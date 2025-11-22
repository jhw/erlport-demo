# ErlPort Demo

A demonstration project showcasing **ErlCracker** - a Firecracker/Lambda-inspired architecture for managing Python execution environments in Erlang.

## What Is This?

This project implements a football results scraper that demonstrates advanced Python-Erlang integration patterns:

1. **Pre-warmed Python interpreters** managed like AWS Firecracker microVMs
2. **Fire-and-forget invocation** similar to AWS Lambda
3. **Automatic lifecycle management** with execution isolation
4. **Production-ready pooling** with queueing and fault tolerance
5. **Scheduled scraping** with per-league staggering

## Architecture

### The ErlCracker Pattern

Like AWS Firecracker manages microVMs for Lambda, this system manages long-lived Python interpreters:

```
Erlang (Runtime Manager)
├─ Pre-warmed Python pools (persistent interpreters)
├─ Automatic work assignment
├─ Execution isolation (double-spawn pattern)
├─ Timeout protection
└─ Crash recovery via OTP supervision
```

### Supervision Tree

```
erlport_demo_app
└─ erlport_demo_sup
   ├─ infrastructure_sup
   │  ├─ config_service (league/team data)
   │  ├─ event_store (ETS storage)
   │  ├─ scheduler (staggered scraping)
   │  └─ python_pools_sup
   │     ├─ bbc_pool (HTML parsing)
   │     ├─ fishy_pool (HTML parsing)
   │     └─ matcher_pool (name matching)
   └─ Cowboy HTTP server (:8080)
```

## Key Features

### ErlCracker Runtime Management
- **Worker Pools**: Pre-warmed Python interpreters with async initialization
- **Fire-and-Forget API**: `python_pool:call_and_await/3` - caller doesn't manage workers
- **Execution Isolation**: Double-spawn protects workers from hung Python calls
- **Timeout Protection**: Worker-side (45s) + caller-side (30s) timeouts
- **Automatic Return**: Workers notify pool on completion, immediately get next work
- **Crash Recovery**: OTP supervision restarts workers + Python processes

### Enhanced Scheduling
- **Per-Scraper Configuration**: BBC and Fishy scrapers run independently
- **League Staggering**: ENG1 at T+3s, ENG2 at T+13s (configurable 10s offset)
- **Parallel Execution**: Different scrapers run together for same league
- **Controlled Load**: Distributes HTTP requests over time

### Data Processing
- **HTML Parsing**: lxml-based parsers for BBC Sport and The Fishy
- **Name Matching**: 5 strategies (exact, levenshtein, abbreviation, token)
- **Result Storage**: ETS-backed with upsert semantics
- **HTTP API**: Query results by league via Cowboy

## Performance

**Current production metrics:**
- HTML parsing: 30-40ms per page
- Name matching: 80-100ms per batch (36 names)
- Cold start: ~100ms (Python initialization)
- Warm invocation: ~1ms overhead (Erlang message passing)

## Requirements

- Erlang/OTP 24 or higher
- Rebar3
- Python 3.7 or higher
- lxml, Levenshtein (see `requirements.txt`)

## Quick Start

```bash
# 1. Install Python dependencies
pip install -r requirements.txt

# 2. Compile
rebar3 compile

# 3. Run
rebar3 shell
```

The application will:
- Start on port 8080
- Begin scraping after 3s (ENG1), then 13s (ENG2)
- Repeat every 60s with same staggering
- Log to `log/erlport_demo.log`

## Configuration

All configuration in `config/sys.config`:

### Python Worker Pools

```erlang
{python_pools, [
  {bbc_pool, #{
    pool_size => 2,              % Number of persistent Python workers
    worker_timeout_ms => 45000   % 45s timeout for Python calls
  }},
  {fishy_pool, #{pool_size => 2, worker_timeout_ms => 45000}},
  {matcher_pool, #{pool_size => 2, worker_timeout_ms => 45000}}
]}
```

### Scheduled Scrapers with League Staggering

```erlang
{scrapers, [
  {bbc_scraper, #{
    initial_delay_ms => 3000,      % First league starts after 3s
    interval_ms => 60000,          % Repeat every 60s
    league_stagger_ms => 10000     % Each subsequent league waits +10s
  }},
  {fishy_scraper, #{
    initial_delay_ms => 3000,
    interval_ms => 60000,
    league_stagger_ms => 10000
  }}
]}
```

**Timing Example:**
```
T+3s:  BBC ENG1 + Fishy ENG1 (both scrapers, first league)
T+13s: BBC ENG2 + Fishy ENG2 (both scrapers, second league)
T+63s: BBC ENG1 + Fishy ENG1 (next cycle)
```

## HTTP API

### Get Events by League

```bash
curl http://localhost:8080/events/ENG1
```

**Response:**
```json
{
  "league": "ENG1",
  "count": 15,
  "events": [
    {
      "league": "ENG1",
      "name": "Arsenal vs Liverpool",
      "date": "2025-11-22",
      "bbc_score": "2-1",
      "fishy_score": "2-1",
      "created_at": 1732123456789,
      "updated_at": 1732123478901
    }
  ]
}
```

### Python Pool API (Programmatic)

```erlang
% Fire-and-forget invocation (caller doesn't manage workers)
Result = python_pool:call_and_await(
    bbc_pool,
    {bbc_scraper, parse_bbc_html, [HTMLBinary]},
    30000  % Timeout in ms
).

% Returns: {ok, ParsedResults} | {error, Reason}
```

## Project Structure

```
apps/erlport_demo/
├─ src/
│  ├─ erlport_demo_app.erl       # Application entry point
│  ├─ erlport_demo_sup.erl       # Root supervisor
│  ├─ infrastructure_sup.erl     # Infrastructure services supervisor
│  │
│  ├─ config_service.erl         # Loads league/team data
│  ├─ event_store.erl            # ETS-backed event storage
│  ├─ scheduler.erl              # Staggered task scheduler
│  │
│  ├─ python_pools_sup.erl       # Top-level pool supervisor
│  ├─ python_pool_sup.erl        # Per-pool supervisor (rest_for_one)
│  ├─ python_pool.erl            # Work distribution + queueing
│  ├─ python_worker_sup.erl      # Dynamic worker spawner
│  ├─ python_worker.erl          # Python process manager + timeout
│  │
│  ├─ bbc_scraper.erl            # BBC Sport orchestration
│  ├─ fishy_scraper.erl          # The Fishy orchestration
│  ├─ http_client.erl            # Gun-based HTTP client
│  └─ api_handler.erl            # Cowboy HTTP handler
│
├─ priv/
│  ├─ python/                    # Python execution scripts
│  │  ├─ bbc_scraper.py          # BBC HTML parser (lxml)
│  │  ├─ fishy_scraper.py        # Fishy HTML parser (lxml)
│  │  └─ name_matcher.py         # Fuzzy name matching (5 strategies)
│  └─ data/
│     ├─ leagues/leagues.json   # League configurations
│     └─ teams/*.json            # Canonical team names + alternates
│
└─ test/
   └─ python/                    # Python unit tests
```

## Documentation

Comprehensive documentation available:

- **[ERLCRACKER.md](ERLCRACKER.md)** - Firecracker/Lambda architecture philosophy, v1.0 roadmap, future enhancements
- **[SCHEDULING.md](SCHEDULING.md)** - Enhanced scheduler with per-league staggering
- **[PYTHON.md](PYTHON.md)** - Python pool implementation details, supervision tree, data flow

## Development

### Running Tests

```bash
# Erlang tests
rebar3 eunit

# Python tests
cd apps/erlport_demo/priv/python
python3 -m pytest test_*.py
```

### Watching Logs

```bash
# Colorized log watcher
python3 tools/watch_logs.py

# Or tail directly
tail -f log/erlport_demo.log
```

### Development Shell

```bash
rebar3 shell
```

## Technical Highlights

### ErlCracker Worker Pattern

**Fire-and-Forget API:**
```erlang
python_pool:call_and_await(PoolName, {Module, Function, Args}, Timeout)
```
- No checkout/checkin (unlike poolboy)
- Workers automatically return to pool
- Execution isolated from worker lifecycle

**Double-Spawn Timeout Protection:**
- Outer process: enforces timeout
- Inner process: executes Python call
- Worker survives timeout, Python process stays alive

**Async Worker Registration:**
- Workers initialize Python asynchronously
- Send `{worker_available, self()}` when ready
- Pool assigns queued work immediately

### Name Matching Strategies (Ordered)

1. **Exact**: `clean_text(a) == clean_text(b)`
2. **Levenshtein**: Edit distance ≤ 2
3. **Abbreviation (Type 0)**: Scraped is abbrev of canonical
4. **Abbreviation (Type 1)**: Canonical is abbrev of scraped
5. **Token Match**: ≥50% word overlap

### Data Flow

```
Scheduler (staggered by league)
  ↓
HTTP fetch (Gun client)
  ↓
Python parsing (bbc_pool or fishy_pool)
  ↓
JSON decode (thoas)
  ↓
Batch name matching (matcher_pool)
  ↓
ETS storage (event_store)
  ↓
HTTP API (Cowboy on :8080)
```

## Dependencies

- **cowboy** (2.12.0): HTTP server
- **gun** (2.1.0): HTTP client
- **thoas** (1.2.1): JSON encoding/decoding
- **erlport** (0.11.0): Erlang-Python bridge

## Future: ErlCracker Library

This project demonstrates patterns suitable for extraction into a reusable **erlcracker** library:

```erlang
% Proposed API
erlcracker:start_pool(PoolName, RuntimeModule, Config).
erlcracker:call(PoolName, Module, Function, Args, Timeout).
erlcracker:status(PoolName).
```

**Target use cases:**
- Python execution from Erlang (current)
- Node.js execution
- WebAssembly execution
- Any heavy runtime with persistent processes

See [ERLCRACKER.md](ERLCRACKER.md) for architectural details and roadmap.

## Status

**Current:** v0.9 - Production-ready core (~90% to v1.0)

**To v1.0:**
- Config validation
- Graceful shutdown
- Basic observability (status API)
- Estimated: 2-3 days

## License

MIT
