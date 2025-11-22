# ErlCracker: Firecracker-Inspired Python Execution Environment

## Overview

This project implements a **Firecracker/Lambda-inspired architecture** using Erlang as the orchestrator and Python processes as the execution environment. Like AWS Firecracker manages microVMs for Lambda functions, our system manages long-lived Python interpreters for on-demand function execution.

## The Firecracker Analogy

### AWS Firecracker + Lambda

```
Firecracker (microVM manager)
├─ Pre-warmed microVMs (persistent, expensive to boot)
├─ Lambda invocation → assigns to available VM
├─ VM executes function in isolation
├─ VM automatically returns to pool when done
└─ Caller receives result, no resource management
```

### ErlCracker (Erlang + Python)

```
Erlang (Python interpreter manager)
├─ Pre-warmed Python interpreters (persistent, expensive to initialize)
├─ Function invocation → assigns to available worker
├─ Worker executes Python function in isolated process
├─ Worker automatically returns to pool when done
└─ Caller receives result, no resource management
```

## Why This Architecture Matches Firecracker/Lambda

### 1. Heavy Initialization, Persistent Runtimes

**Firecracker:** MicroVMs boot once and stay warm between invocations
**ErlCracker:** Python interpreters start once and stay alive across requests

```erlang
init([ScriptName, PoolName]) ->
    {ok, PythonPid} = python:start([{python_path, PythonPath}]),
    % Expensive initialization: load interpreter, import libraries
    % Worker announces readiness when warm
    PoolName ! {worker_available, self()},
    {ok, #state{python_pid = PythonPid, pool_name = PoolName}}.
```

**Cost:** ~100ms to initialize Python vs. ~1ms per invocation after warmup

### 2. Fire-and-Forget Invocation

**Lambda:** Caller invokes function, Lambda runtime handles everything
**ErlCracker:** Caller invokes function, pool handles everything

```erlang
% Simple caller API - no resource management
Result = python_pool:call_and_await(
    bbc_pool,
    {bbc_scraper, parse_bbc_html, [HTML]},
    30000
)
```

Caller doesn't manage:
- Worker checkout/checkin
- Worker lifecycle
- Pool availability
- Failure recovery

### 3. Automatic Return to Pool

**Firecracker:** VM completes, automatically marked available
**ErlCracker:** Worker completes, automatically notifies pool

```erlang
% Worker sends completion to both caller and pool
CallerPid ! {python_result, WorkerPid, Result},
PoolPid ! {worker_done, WorkerPid, Result}

% Pool immediately handles next action
handle_info({worker_done, WorkerPid, Result}, State) ->
    case queue:out(State#state.request_queue) of
        {{value, NextRequest}, NewQueue} ->
            assign_work(WorkerPid, NextRequest);  % Immediate reuse
        {empty, _} ->
            mark_available(WorkerPid)             % Wait for next invocation
    end
```

### 4. Execution Isolation

**Firecracker:** Each invocation isolated in process namespace
**ErlCracker:** Double-spawn pattern isolates execution from worker

```erlang
% Outer process: enforces timeout, manages worker lifecycle
% Inner process: executes Python call (may hang)
spawn_link(fun() ->
    CallPid = spawn_link(fun() ->
        Result = python:call(PythonPid, Module, Function, Args),
        OuterPid ! {CallRef, {ok, Result}}
    end),
    receive
        {CallRef, Result} -> send_result_and_notify_pool(Result)
    after TimeoutMs ->
        exit(CallPid, kill),  % Kill execution, preserve worker
        send_timeout_error()
    end
end)
```

Benefits:
- Worker survives timeout (Python process stays alive)
- Failed execution doesn't crash worker
- Worker ready for next invocation immediately

## Current Implementation Status

### ✅ v0.9 - Core Firecracker Pattern Complete

| Feature | Status | Notes |
|---------|--------|-------|
| **Pre-warmed Python interpreters** | ✅ Complete | Workers persist across invocations |
| **Async worker initialization** | ✅ Complete | Workers announce readiness when warm |
| **Automatic pool return** | ✅ Complete | Workers notify pool on completion |
| **Execution isolation** | ✅ Complete | Double-spawn protects worker from failures |
| **Timeout protection** | ✅ Complete | Worker-side + caller-side timeouts |
| **Work queueing** | ✅ Complete | FIFO queue when all workers busy |
| **Crash recovery** | ✅ Complete | Supervisor restarts workers + Python |
| **Fire-and-forget API** | ✅ Complete | `call_and_await/3` handles everything |

### What's Working Right Now

```erlang
% Three pools configured in sys.config
{python_pools, [
  {bbc_pool, #{pool_size => 2, worker_timeout_ms => 45000}},
  {fishy_pool, #{pool_size => 2, worker_timeout_ms => 45000}},
  {matcher_pool, #{pool_size => 2, worker_timeout_ms => 45000}}
]}

% Production usage - scrapers invoke Python functions
python_pool:call_and_await(bbc_pool, {bbc_scraper, parse_bbc_html, [HTML]}, 30000)
python_pool:call_and_await(matcher_pool, {name_matcher, match_matchups_batch, [JSON]}, 30000)
```

**Performance:**
- HTML parsing: 30-40ms per invocation
- Name matching: 80-100ms per batch (36 names)
- Cold start: ~100ms (first Python initialization)
- Warm invocation: ~1ms overhead (worker assignment + message passing)

## Path to v1.0

### Minor Tweaks Required

#### 1. Configuration Validation (Priority: High)
```erlang
% Add validation in python_pools_sup:init/1
validate_pool_config(Config) ->
    validate_required_keys([pool_size, worker_timeout_ms], Config),
    validate_positive_integer(pool_size, Config),
    validate_positive_integer(worker_timeout_ms, Config).
```

#### 2. Graceful Shutdown (Priority: Medium)
```erlang
% python_worker:terminate/2 already handles this
% Add drain period for in-flight requests
terminate(_Reason, State) ->
    % Allow 5s for current Python call to complete
    timer:sleep(5000),
    python:stop(State#state.python_pid),
    ok.
```

#### 3. Pool Health API (Priority: Low)
```erlang
python_pool:status(PoolName) ->
    gen_server:call(PoolName, status).

% Returns:
#{
  available_workers => 2,
  busy_workers => 0,
  queued_requests => 0,
  total_capacity => 2
}
```

**Estimated effort:** 2-3 days for all three tweaks

### v1.0 Release Criteria

- ✅ Core functionality stable (already achieved)
- ⚠️ Configuration validation
- ⚠️ Graceful shutdown handling
- ⚠️ Basic observability (status API)
- ✅ Documentation (PYTHON.md + ERLCRACKER.md)
- ✅ Production usage (currently scraping in production)

**Assessment:** You are ~90% to v1.0. The system is production-ready now; v1.0 is about polish and observability.

## Future Enhancements

### v1.1 - Dynamic Scaling (Overflow Workers)

**Goal:** Handle burst traffic like Lambda auto-scaling

```erlang
{python_pools, [
  {bbc_pool, #{
    pool_size => 2,           % Sustained capacity
    max_overflow => 3,        % Burst capacity (up to 5 total)
    overflow_ttl => 60000,    % Kill overflow workers after 60s idle
    worker_timeout_ms => 45000
  }}
]}

% Pool implementation
handle_cast({call_python, M, F, A, CallerPid}, State) ->
    case {queue:out(State#state.available_workers), overflow_capacity(State)} of
        {{empty, _}, true} ->
            % Spawn temporary worker for burst
            {ok, TempWorker} = spawn_overflow_worker(),
            assign_work(TempWorker, {M, F, A, CallerPid});
        {{empty, _}, false} ->
            % Queue request (at max capacity)
            queue_request({M, F, A, CallerPid});
        {{value, Worker}, _} ->
            % Use available permanent worker
            assign_work(Worker, {M, F, A, CallerPid})
    end
```

**Benefits:**
- Handle traffic spikes without queueing
- Return to baseline capacity automatically
- Pay (memory) only for what you need

**Lambda equivalence:** Concurrent execution scaling

### v1.2 - Observability & Metrics

**Goal:** Understand system behavior and optimize

```erlang
% Telemetry events
telemetry:execute([erlcracker, pool, request], #{count => 1}, #{pool => PoolName})
telemetry:execute([erlcracker, worker, execution], #{duration => Ms}, Metadata)

% Built-in metrics
python_pool:metrics(PoolName) ->
    #{
      invocations_total => 1547,
      invocations_success => 1540,
      invocations_timeout => 5,
      invocations_error => 2,

      latency_p50 => 35.2,      % ms
      latency_p95 => 89.7,
      latency_p99 => 142.3,

      queue_length_current => 0,
      queue_length_max => 3,

      workers_available => 2,
      workers_busy => 0
    }
```

**Integration points:**
- Prometheus exporter
- StatsD/DataDog
- BEAM telemetry
- Custom dashboards

### v1.3 - Circuit Breaking & Health Checks

**Goal:** Fail fast on unhealthy workers

```erlang
% Circuit breaker per worker
-record(worker_health, {
    pid :: pid(),
    consecutive_failures :: integer(),
    last_success :: integer(),
    state :: open | half_open | closed
}).

% Stop assigning work to failing workers
assign_work(WorkerPid, Request, State) ->
    case worker_health(WorkerPid, State) of
        {ok, open} ->
            % Circuit open, worker healthy
            send_work(WorkerPid, Request);
        {ok, half_open} ->
            % Testing worker after failure
            send_work_with_monitoring(WorkerPid, Request);
        {error, closed} ->
            % Circuit closed, use different worker
            assign_to_next_worker(Request, State)
    end
```

**Health check options:**
- Periodic ping to Python interpreter
- Failure rate thresholds
- Timeout rate thresholds
- Manual circuit tripping

**Lambda equivalence:** Error rate throttling

### v1.4 - Warm/Cold Start Optimization

**Goal:** Maximize use of hot workers (cached state, loaded libraries)

```erlang
% Track worker "temperature"
-record(worker_state, {
    pid :: pid(),
    last_used :: integer(),          % erlang:system_time(millisecond)
    invocation_count :: integer(),
    module_cache :: sets:set()       % Track loaded Python modules
}).

% LIFO strategy: recently used workers first
strategy => lifo  % Stack-like: hot workers stay hot

% Or sophisticated routing
assign_work({Module, Function, Args}, Workers) ->
    % Prefer worker that already has this module loaded
    case find_worker_with_module(Module, Workers) of
        {ok, HotWorker} ->
            {HotWorker, warm_start};
        not_found ->
            {next_available_worker(Workers), cold_start}
    end
```

**Lambda equivalence:** Warm container reuse, execution environment reuse

### v1.5 - Multi-Language Support

**Goal:** Support multiple language runtimes

```erlang
% Generic runtime abstraction
-behaviour(erlcracker_runtime).

% Callbacks
-callback start_runtime(Config :: map()) -> {ok, RuntimePid :: pid()}.
-callback call_function(RuntimePid, Module, Function, Args) -> {ok, Result}.
-callback stop_runtime(RuntimePid) -> ok.

% Implementations
erlcracker_python_runtime.erl   % Current Python/ErlPort
erlcracker_nodejs_runtime.erl   % Future: Node.js via ports
erlcracker_wasm_runtime.erl     % Future: WebAssembly via WASM runtime
```

## Abstraction into Reusable Library

### Proposed Package Structure

```
erlcracker/
├─ src/
│  ├─ erlcracker.erl              % Public API
│  ├─ erlcracker_pool.erl         % Pool manager (generic)
│  ├─ erlcracker_pool_sup.erl     % Pool supervisor
│  ├─ erlcracker_worker.erl       % Worker behavior
│  ├─ erlcracker_worker_sup.erl   % Worker supervisor
│  └─ erlcracker_runtime.erl      % Runtime behavior
│
├─ src/runtimes/
│  └─ erlcracker_python_runtime.erl
│
└─ README.md

rebar.config:
{deps, [
  {erlport, "0.11.0"}
]}
```

### Public API

```erlang
% Start a pool
erlcracker:start_pool(PoolName, RuntimeModule, Config).

% Invoke function
erlcracker:call(PoolName, Module, Function, Args, Timeout).
erlcracker:call_async(PoolName, Module, Function, Args).

% Pool management
erlcracker:status(PoolName).
erlcracker:metrics(PoolName).
erlcracker:resize(PoolName, NewSize).
```

### Usage in Applications

```erlang
% In your app's rebar.config
{deps, [
  {erlcracker, "1.0.0"}
]}.

% In your supervisor
PoolConfig = #{
  pool_size => 5,
  worker_timeout_ms => 30000,
  max_overflow => 10
},
{ok, _} = erlcracker:start_pool(
  my_pool,
  erlcracker_python_runtime,
  PoolConfig
).

% In your business logic
Result = erlcracker:call(my_pool, mymodule, myfunction, [Arg1, Arg2], 5000).
```

## Why This Makes a Great Library

1. **Solves Real Problem:** Managing heavy runtimes efficiently
2. **Clear Use Case:** Python, Node.js, WebAssembly execution from Erlang
3. **Production Proven:** Already working in your scraper system
4. **OTP Compliant:** Proper supervision, gen_server patterns
5. **Simple API:** Fire-and-forget invocation model
6. **Extensible:** Runtime behavior for different languages

## Competitive Landscape

| Solution | Model | Pros | Cons |
|----------|-------|------|------|
| **Poolboy** | Library checkout | Generic, well-tested | Manual lifecycle, not runtime-focused |
| **ErlPort directly** | Direct Python calls | Simple | No pooling, no isolation, no timeout |
| **Ports/NIFs** | OS-level execution | Fast | Unsafe, no crash recovery |
| **ErlCracker** | Firecracker/Lambda | Persistent runtimes, automatic lifecycle, safe isolation | New, single implementation |

## Conclusion

**You are 90% to v1.0** of a production-ready Firecracker-inspired execution environment. The core patterns are implemented and working:

✅ Pre-warmed runtimes
✅ Automatic lifecycle management
✅ Execution isolation
✅ Fire-and-forget invocation
✅ Crash recovery

Minor polish (config validation, observability) gets you to a **1.0 release**. Future versions add scaling, health checks, and multi-language support.

**This architecture is a strong foundation for extracting into a reusable library.** The Firecracker/Lambda analogy is clear, the patterns are proven, and the OTP implementation is solid.
