# Python Integration via ErlPort

## Overview

This project uses ErlPort to integrate Python workers for HTML parsing and name matching. A sophisticated pooling system manages Python worker processes, provides queueing for busy workers, and ensures fault tolerance through OTP supervision.

## Architecture

### Three-Layer Design

1. **Python Scripts**: HTML parsing and name matching logic
2. **Worker Management**: Individual Python process lifecycle
3. **Pool Management**: Work distribution and queueing

## Supervision Tree

```
python_pools_sup (one_for_one)
├─ python_pool_sup (bbc_pool_sup) (rest_for_one)
│  ├─ python_worker_sup (simple_one_for_one)
│  │  ├─ python_worker[1] (gen_server + Python process)
│  │  └─ python_worker[2] (gen_server + Python process)
│  └─ python_pool (gen_server)
│     ├─ Manages available_workers queue
│     ├─ Tracks busy_workers set
│     └─ Queues pending requests
│
├─ python_pool_sup (fishy_pool_sup) (rest_for_one)
│  ├─ python_worker_sup (simple_one_for_one)
│  │  ├─ python_worker[1]
│  │  └─ python_worker[2]
│  └─ python_pool
│
└─ python_pool_sup (matcher_pool_sup) (rest_for_one)
   ├─ python_worker_sup (simple_one_for_one)
   │  ├─ python_worker[1]
   │  └─ python_worker[2]
   └─ python_pool
```

### Supervision Strategies

| Supervisor | Strategy | Rationale |
|------------|----------|-----------|
| `python_pools_sup` | `one_for_one` | Each pool is independent; failure shouldn't affect others |
| `python_pool_sup` | `rest_for_one` | Worker supervisor must start before pool manager (pool looks up supervisor by name) |
| `python_worker_sup` | `simple_one_for_one` | Dynamic worker creation with identical specifications |

### Why rest_for_one?

The `python_pool_sup` uses `rest_for_one` because:

1. **Start Order**: Worker supervisor must exist before pool manager
2. **Dependency**: Pool manager calls `supervisor:which_children(WorkerSupPid)` during init
3. **Restart Behavior**: If worker supervisor crashes, pool manager must restart (it holds stale worker references)

## Module Descriptions

### python_pools_sup.erl

Top-level supervisor that starts three pool supervisors (BBC, Fishy, Matcher).

**Key Functions:**
- `start_link/1` - Starts with pool configurations from sys.config
- `init/1` - Creates child specs for each pool supervisor

**Configuration:**
```erlang
{python_pools, [
  {bbc_pool, #{pool_size => 2, worker_timeout_ms => 45000}},
  {fishy_pool, #{pool_size => 2, worker_timeout_ms => 45000}},
  {matcher_pool, #{pool_size => 2, worker_timeout_ms => 45000}}
]}
```

### python_pool_sup.erl

Creates a subtree for one pool: worker supervisor + pool manager.

**Key Functions:**
- `start_link/2` - Accepts `PoolName` and pool config
- `init/1` - Creates two children with `rest_for_one` strategy:
  1. Worker supervisor (registered as `{PoolName}_worker_sup`)
  2. Pool manager (registered as `PoolName`)

**Child Specs:**
```erlang
WorkerSup = #{
  id => python_worker_sup,
  start => {python_worker_sup, start_link, [WorkerSupName, Config]},
  restart => permanent,
  shutdown => infinity,  % Supervisor
  type => supervisor
},
PoolManager = #{
  id => python_pool,
  start => {python_pool, start_link, [PoolName, WorkerSupName, Config]},
  restart => permanent,
  shutdown => 5000,      % Worker
  type => worker
}
```

### python_worker_sup.erl

Simple-one-for-one supervisor for Python workers.

**Key Functions:**
- `start_link/2` - Creates supervisor
- `start_worker/1` - Spawns a new worker via `supervisor:start_child/2`

**Worker Spec:**
```erlang
#{
  id => python_worker,
  start => {python_worker, start_link, [PoolName, TimeoutMs]},
  restart => permanent,
  shutdown => 5000,
  type => worker
}
```

### python_pool.erl

Work distribution manager with queueing.

**State:**
```erlang
-record(state, {
    pool_name :: atom(),
    available_workers :: queue:queue(pid()),
    busy_workers :: sets:set(pid()),
    request_queue :: queue:queue({module(), atom(), list(), pid()})
}).
```

**Key Functions:**

| Function | Purpose |
|----------|---------|
| `call_and_await/4` | Synchronous: Request work and wait for result |
| `handle_call({call_python, ...})` | Assign work to available worker or queue request |
| `handle_info({worker_available, Pid})` | Add worker to available pool or assign queued work |
| `handle_info({worker_done, Pid, Result})` | Return result, mark worker available or assign next work |
| `handle_info({'DOWN', ...})` | Remove crashed worker from tracking |

**Work Assignment Flow:**

```
Client calls python_pool:call_and_await/4
  │
  ├─ Available worker exists?
  │  ├─ YES: Assign immediately
  │  │  └─ Send {call_python, M, F, A, Caller, Timeout} to worker
  │  │
  │  └─ NO: Queue request
  │     └─ Add {M, F, A, CallerPid} to request_queue
  │
Worker completes task
  │
  ├─ Send {worker_done, WorkerPid, Result} to pool
  │
  └─ Pool handles worker_done:
     ├─ Send result to original caller
     ├─ Queued work exists?
     │  ├─ YES: Assign next work to this worker
     │  └─ NO: Add worker to available_workers
     │
     └─ Update busy_workers set
```

**Monitoring:**
- Pool monitors all workers with `erlang:monitor(process, WorkerPid)`
- On `{'DOWN', ...}`: Remove from `busy_workers` or `available_workers`
- Supervisor automatically restarts crashed workers
- Restarted workers send `{worker_available, self()}` to rejoin pool

### python_worker.erl

Manages individual Python process and executes calls with timeout protection.

**State:**
```erlang
-record(state, {
    python_pid :: port() | undefined,
    pool_pid :: pid(),
    timeout_ms :: integer()
}).
```

**Key Functions:**

| Function | Purpose |
|----------|---------|
| `start_link/2` | Start worker and Python process |
| `init/1` | Initialize Python via `python:start/1`, notify pool |
| `handle_info({call_python, ...})` | Execute Python call with timeout protection |
| `terminate/2` | Clean shutdown of Python process |

**Timeout Protection Pattern:**

```erlang
% Double-spawn pattern prevents deadlock
spawn_link(fun() ->
    CallPid = spawn_link(fun() ->
        Result = python:call(PythonPid, Module, Function, Args),
        ParentPid ! {CallRef, {ok, Result}}
    end),
    receive
        {CallRef, Result} ->
            CallerPid ! {python_result, self(), Result},
            PoolPid ! {worker_done, self(), Result}
    after TimeoutMs ->
        exit(CallPid, kill),
        Error = {error, timeout},
        CallerPid ! {python_result, self(), Error},
        PoolPid ! {worker_done, self(), Error}
    end
end)
```

**Why Double Spawn?**

1. **Outer process**: Enforces timeout with `receive...after`
2. **Inner process**: Executes Python call (may hang indefinitely)
3. **Timeout behavior**: Outer process kills inner process, sends error to caller
4. **Crash isolation**: Worker process remains alive, ready for next task

**Python Process Lifecycle:**

```
python_worker:start_link/2
  │
  ├─ python:start([{python_path, PythonPath}])
  │  └─ Spawns external Python interpreter via ErlPort
  │
  ├─ Send {worker_available, self()} to pool
  │
  └─ Wait for {call_python, ...} messages
     │
     ├─ Receive work request
     ├─ Execute python:call/4
     ├─ Return result
     └─ Notify pool via {worker_done, ...}
```

## Python Scripts

All Python scripts use JSON for data interchange with Erlang.

### bbc_scraper.py

Parses BBC Sport HTML to extract football results.

**Key Function:**
```python
def parse_bbc_html(html_content: str) -> str
```

**Process:**
1. Parse HTML with `lxml.html.fromstring()`
2. Find `<script>` tag containing `window.__INITIAL_DATA__`
3. Extract JSON from JavaScript
4. Navigate: `data[key]['data']['eventGroups'][*]['secondaryGroups'][*]['events'][*]`
5. Filter: `status == 'PostEvent'` (completed matches)
6. Extract: home/away names, scores, date
7. Format: `{"date": "YYYY-MM-DD", "name": "Team A vs Team B", "score": "X-Y"}`
8. Return: `json.dumps([{...}, {...}, ...])`

**Example Output:**
```json
[
  {
    "date": "2025-11-22",
    "name": "Arsenal vs Liverpool",
    "score": "2-1"
  },
  {
    "date": "2025-11-22",
    "name": "Man City vs Chelsea",
    "score": "1-1"
  }
]
```

### fishy_scraper.py

Parses TheFishy HTML to extract football results.

**Key Function:**
```python
def parse_fishy_html(html_content: str) -> str
```

**Process:**
1. Parse HTML with `lxml.html`
2. Find all `<tr class="results-row">` elements
3. For each row:
   - Extract home team (`.results-home-team .results-team-name`)
   - Extract away team (`.results-away-team .results-team-name`)
   - Extract score (`.results-score`)
   - Extract date (`.results-date`)
4. Format: `{"date": "YYYY-MM-DD", "name": "Team A vs Team B", "score": "X-Y"}`
5. Return: `json.dumps([{...}, {...}, ...])`

**Output Format:** Identical to BBC scraper for consistent processing

### name_matcher.py

Matches scraped team names against canonical names using multiple strategies.

**Key Function:**
```python
def match_matchups_batch(json_input: str) -> str
```

**Input Format:**
```json
{
  "matchups": ["Arsenal vs Liverpool", "Man City vs Chelsea"],
  "teams": [
    {"name": "Arsenal", "altNames": ["The Gunners", "AFC"]},
    {"name": "Manchester City", "altNames": ["Man City", "MCFC"]}
  ]
}
```

**Matching Strategies (applied in order):**

1. **Exact Match**: Clean text identical
   ```python
   clean_text(scraped) == clean_text(canonical)
   ```

2. **Levenshtein Distance**: Edit distance ≤ 2
   ```python
   Levenshtein.distance(scraped, canonical) <= 2
   ```

3. **Abbreviation (Type 0)**: Scraped is abbreviation of canonical
   ```python
   is_abbreviation(scraped, canonical)
   ```

4. **Abbreviation (Type 1)**: Canonical is abbreviation of scraped
   ```python
   is_abbreviation(canonical, scraped)
   ```

5. **Token Match**: ≥50% word overlap
   ```python
   len(tokens_a & tokens_b) / len(tokens_a | tokens_b) >= 0.5
   ```

**Output Format:**
```json
{
  "matched": {
    "Arsenal vs Liverpool": "Arsenal vs Liverpool",
    "Man City vs Chelsea": "Manchester City vs Chelsea"
  },
  "unmatched": [
    "Unknown Team vs Another Unknown"
  ]
}
```

**Helper Functions:**

| Function | Purpose |
|----------|---------|
| `clean_text(text)` | Remove punctuation, lowercase, strip whitespace |
| `is_abbreviation(abbr, full)` | Check if abbr matches first letters of full |
| `match_entity(text, teams)` | Apply all matchers to find canonical name |

## Data Flow

### Complete Request Flow

```
Erlang Scraper (bbc_scraper:scrape/1)
  │
  ├─ 1. HTTP Fetch
  │  └─ http_client:fetch_url(URL) → HTML binary
  │
  ├─ 2. Parse HTML
  │  └─ python_pool:call_and_await(bbc_pool,
  │       {bbc_scraper, parse_bbc_html, [HTML]}, 30000)
  │     │
  │     ├─ python_pool assigns to available worker
  │     │
  │     ├─ python_worker receives {call_python, ...}
  │     │
  │     ├─ Spawns timeout protection processes
  │     │
  │     ├─ python:call(PythonPid, bbc_scraper, parse_bbc_html, [HTML])
  │     │  └─ ErlPort → Python interpreter → bbc_scraper.py
  │     │     └─ Returns JSON string
  │     │
  │     ├─ python_worker sends result back to caller
  │     │
  │     └─ python_pool receives {worker_done, ...}
  │
  ├─ 3. Decode JSON
  │  └─ thoas:decode(JsonString) → List of maps
  │     └─ [#{<<"date">> => ..., <<"name">> => ..., <<"score">> => ...}]
  │
  ├─ 4. Batch Match Names
  │  └─ python_pool:call_and_await(matcher_pool,
  │       {name_matcher, match_matchups_batch, [JsonRequest]}, 30000)
  │     │
  │     └─ Same flow as step 2, different pool
  │        └─ Returns: {"matched": {...}, "unmatched": [...]}
  │
  ├─ 5. Store Results
  │  └─ For each matched result:
  │     └─ event_store:store_event(League, CanonicalName, Date, Source, Score)
  │
  └─ 6. Log Statistics
     └─ Parsed X results, Matched Y/X events
```

### Message Flow Diagram

```
Caller                Pool              Worker             Python Process
  │                    │                  │                      │
  │─call_and_await──>│                  │                      │
  │                    │                  │                      │
  │                    │─call_python──>│                      │
  │                    │                  │                      │
  │                    │                  │─python:call──────>│
  │                    │                  │                      │
  │                    │                  │                      │ (executing)
  │                    │                  │                      │
  │                    │                  │<──result───────────│
  │                    │                  │                      │
  │<───result──────────│<─worker_done──│                      │
  │                    │                  │                      │
  │                    │─call_python──>│  (if queued work)    │
  │                    │                  │                      │
```

## Configuration

### sys.config

```erlang
{python_pools, [
  {bbc_pool, #{
    pool_size => 2,              % Number of Python workers
    worker_timeout_ms => 45000   % Timeout for Python calls (45s)
  }},
  {fishy_pool, #{
    pool_size => 2,
    worker_timeout_ms => 45000
  }},
  {matcher_pool, #{
    pool_size => 2,
    worker_timeout_ms => 45000
  }}
]}
```

### Python Path

Workers automatically discover Python scripts:
```erlang
PythonPath = filename:join([code:priv_dir(erlport_demo), "python"])
% Typically: /path/to/erlport-demo/apps/erlport_demo/priv/python
```

## Error Handling

### Timeout Protection

**Worker-side timeout** (45s) is longer than **caller-side timeout** (30s):
- Caller times out after 30s, receives `{error, timeout}`
- Worker continues for up to 45s, then kills Python call
- Prevents resource leaks from abandoned Python calls

### Worker Crashes

```
Worker crashes during Python call
  │
  ├─ Pool receives {'DOWN', MonitorRef, ...}
  │  └─ Removes worker from busy_workers or available_workers
  │
  ├─ Supervisor detects worker exit
  │  └─ Restarts worker (permanent restart strategy)
  │
  └─ New worker initializes
     ├─ Starts fresh Python process
     └─ Sends {worker_available, NewPid} to pool
        └─ Pool assigns queued work or adds to available_workers
```

### Python Process Crashes

If Python interpreter crashes:
1. Worker's `python:call/4` returns error
2. Worker sends error result to caller
3. Worker remains alive, ready for next task
4. Python process may need restart (handled by next `python:start/1` call)

### Pool Manager Crashes

If pool manager crashes:
- Supervisor restarts it (rest_for_one)
- Workers survive (not supervised by pool)
- New pool manager rediscovers workers via `supervisor:which_children/1`
- Workers send `{worker_available, self()}` to new pool

## Performance Characteristics

### Concurrency

- **Per pool**: Up to `pool_size` concurrent Python calls
- **Cross-pool**: All pools execute independently
- **Example**: With 3 pools × 2 workers = 6 concurrent Python operations

### Queueing

- **Unbounded queue**: Request queue grows indefinitely if workers are busy
- **FIFO ordering**: Requests processed in order received
- **Backpressure**: Callers block until result received (synchronous API)

### Latency

Typical latencies (observed in logs):
- **HTML parsing**: 30-40ms per page
- **Name matching**: 80-100ms per batch (36 names)
- **Total scrape**: 150-200ms per league

## Best Practices

### Pool Sizing

**Rule of thumb**: `pool_size = number_of_concurrent_requests`

- **Too small**: Requests queue, increased latency
- **Too large**: Memory overhead, diminishing returns
- **Current (2 workers)**: Good for 2 leagues with staggered scheduling

### Timeout Configuration

**Hierarchy**: `caller_timeout < worker_timeout < infinity`

```
Scraper timeout:  30s  (caller gives up)
Worker timeout:   45s  (worker kills Python call)
Python timeout:   ∞   (no inherent timeout)
```

### JSON Interchange

**Always use JSON for Erlang ↔ Python**:
- ✅ Well-defined serialization
- ✅ Handles Unicode correctly
- ✅ Clear type mapping
- ❌ Avoid ErlPort's automatic type conversion (unreliable with complex types)

## Monitoring

### Logging

Workers and pools log key events:

```erlang
% Pool assignment
[info] bbc_pool: Request bbc_scraper:parse_bbc_html from <0.317.0>

% Worker execution
[info] Python worker <0.265.0>: Spawned call process <0.333.0>
[info] Python worker <0.265.0>: Starting python:call for bbc_scraper:parse_bbc_html
[info] Python worker <0.265.0>: python:call completed, sending to outer process

% Results
[info] BBC scraper: Parsed 36 results for <<"ENG2">>
[info] BBC scraper: Matched 36/36 events for <<"ENG2">>
```

### Metrics to Track

- **Queue length**: `queue:len(State#state.request_queue)`
- **Available workers**: `queue:len(State#state.available_workers)`
- **Busy workers**: `sets:size(State#state.busy_workers)`
- **Timeout rate**: Count of `{error, timeout}` responses
- **Worker restart rate**: Supervisor reports

## Future Enhancements

Potential improvements (not currently implemented):

- **Dynamic pool sizing**: Scale workers based on load
- **Circuit breaker**: Stop sending work to failing workers
- **Metrics collection**: Expose queue length, latency percentiles
- **Priority queues**: High-priority requests jump the queue
- **Worker warmup**: Pre-initialize Python processes with common imports
- **Result caching**: Cache Python results for identical inputs
- **Graceful degradation**: Return partial results on timeout
- **Health checks**: Periodic validation of Python worker health

## Troubleshooting

### Workers Not Accepting Work

**Symptom**: Requests queue indefinitely

**Check**:
1. Workers sending `{worker_available, Pid}` on startup?
2. Pool receiving `{worker_done, Pid, Result}` after tasks?
3. Workers crashing during execution?

**Debug**:
```erlang
% Check pool state
sys:get_state(bbc_pool).

% Check worker supervisor children
supervisor:which_children(bbc_pool_worker_sup).
```

### Python Import Errors

**Symptom**: Worker crashes with Python ImportError

**Solution**:
1. Verify Python dependencies installed: `pip install -r requirements.txt`
2. Check Python path: Workers log path during initialization
3. Verify priv/python directory exists and contains .py files

### Timeout Issues

**Symptom**: Frequent `{error, timeout}` responses

**Solutions**:
1. Increase worker timeout: `worker_timeout_ms => 60000`
2. Increase pool size: More workers = less queueing
3. Optimize Python code: Profile slow functions
4. Check network: Slow HTTP fetches cause queuing

### Memory Leaks

**Symptom**: Python processes growing in memory

**Solutions**:
1. Restart workers periodically (not currently implemented)
2. Profile Python code for memory issues
3. Implement worker cycling: Kill and restart after N tasks
