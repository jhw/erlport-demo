# ErlPort Demo

An Erlang application demonstrating ErlPort integration for scraping football results from BBC Sport and The Fishy, with Python-based parsing and name matching.

## Features

- **ErlPort Integration**: Python process pools for efficient script execution
- **Dual Scrapers**: BBC Sport and The Fishy results scrapers
- **Name Matching**: Fuzzy matching algorithm to normalize team names
- **Timer-based Scheduling**: Automatic scraping every 60 seconds
- **ETS Storage**: In-memory event storage with league/name/date keys
- **HTTP API**: RESTful endpoint to query events by league
- **Gun HTTP Client**: Efficient HTTP/2 capable client for scraping

## Architecture

```
┌──────────────────────────────────────────────┐
│     erlport_demo_sup (rest_for_one)         │
└─┬────────────────────────────────────────────┘
  │
  ├─► [1] infrastructure_sup (one_for_one)
  │        ├─► config_service (static data)
  │        ├─► event_store (ETS)
  │        ├─► scheduler (task manager, monitors workers)
  │        └─► python_pools_sup (rest_for_one)
  │             ├─► bbc_pool_worker_sup (simple_one_for_one)
  │             │   └─► [2 python_workers]
  │             ├─► bbc_pool (pool manager)
  │             ├─► fishy_pool_worker_sup (simple_one_for_one)
  │             │   └─► [2 python_workers]
  │             ├─► fishy_pool (pool manager)
  │             ├─► matcher_pool_worker_sup (simple_one_for_one)
  │             │   └─► [2 python_workers]
  │             └─► matcher_pool (pool manager)
  │
  └─► [2] leagues_sup (one_for_one)
           ├─► league_worker (ENG1) → registers tasks
           └─► league_worker (ENG2) → registers tasks

Scheduler executes: bbc_scraper:scrape/1
                    fishy_scraper:scrape/1
                    (every 60 seconds)

┌─────────────────┐
│  Cowboy HTTP    │  Port 8080
│     API         │  GET /events/:league
└─────────────────┘
```

**OTP Best Practices:**
- **rest_for_one at top level**: Proper dependency ordering with cascading restarts
- **All infrastructure together**: Config, event store, scheduler, and Python pools under one supervisor
- **infrastructure_sup**: Contains ALL foundational services (config, storage, scheduling, Python)
- **config_service**: Centralized static data loading (no file I/O in init/1 elsewhere)
- **python_pools_sup**: Proper supervision hierarchy with simple_one_for_one for workers
- **python_worker**: Individual workers managed by supervisors, not manually restarted
- **Monitored task execution**: Scheduler monitors all task processes, no orphans
- **Message passing**: Python results sent via messages, no callback complexity
- **Flat league supervision**: leagues_sup directly supervises league_workers (no unnecessary nesting)
- **Simple dependency**: infrastructure → leagues (2 levels, clean and clear)
- **Clean shutdown**: Supervisors have infinity timeout, workers have 5s
- **Separation of concerns**: League workers register, scheduler executes, scrapers do work
- **No blocking**: Event store updates via cast, timeouts on all Python calls

## Requirements

- Erlang/OTP 24 or higher
- Rebar3
- Python 3.7 or higher

## Setup

1. Compile the project:
```bash
./scripts/compile.sh
```

2. Run the application:
```bash
./scripts/run.sh
```

## Usage

### HTTP API

Get events for a league:
```bash
curl http://localhost:8080/events/ENG1
curl http://localhost:8080/events/ENG2
```

Response format:
```json
{
  "league": "ENG1",
  "count": 10,
  "events": [
    {
      "league": "ENG1",
      "name": "Arsenal vs Liverpool",
      "date": "2025-11-20",
      "bbc_score": "2-1",
      "fishy_score": "2-1",
      "created_at": 1732128000000,
      "updated_at": 1732128060000
    }
  ]
}
```

### Erlang Shell Commands

Start the application:
```erlang
application:start(erlport_demo).
```

Manual scraping:
```erlang
bbc_scraper:scrape(<<"ENG1">>).
fishy_scraper:scrape(<<"ENG1">>).
```

Query events:
```erlang
event_store:get_events(<<"ENG1">>).
event_store:get_all_events().
```

## Project Structure

```
erlport-demo/
├── apps/
│   └── erlport_demo/
│       └── src/
│           ├── erlport_demo_app.erl      # Application entry point
│           ├── erlport_demo_sup.erl      # Main supervisor (rest_for_one)
│           ├── infrastructure_sup.erl    # Infrastructure supervisor
│           ├── config_service.erl        # Centralized config/data service
│           ├── event_store.erl           # ETS-based event storage
│           ├── scheduler.erl             # Centralized task scheduler (monitors workers)
│           ├── bbc_scraper.erl           # BBC scraping logic
│           ├── fishy_scraper.erl         # Fishy scraping logic
│           ├── scraper_utils.erl         # Shared scraper utilities
│           ├── python_pools_sup.erl      # Python pools supervisor
│           ├── python_pool.erl           # Python pool manager
│           ├── python_pool_worker_sup.erl # Worker supervisor (simple_one_for_one)
│           ├── python_worker.erl         # Individual Python worker
│           ├── leagues_sup.erl           # Leagues supervisor
│           ├── league_worker.erl         # Per-league worker (registers tasks)
│           ├── http_client.erl           # Gun-based HTTP client
│           └── api_handler.erl           # Cowboy HTTP handler
├── priv/
│   ├── python/
│   │   ├── bbc_scraper.py               # BBC HTML parser
│   │   ├── fishy_scraper.py             # Fishy HTML parser
│   │   └── name_matcher.py              # Name normalization
│   └── data/
│       ├── leagues/
│       │   └── leagues.json             # League configuration
│       └── teams/
│           ├── ENG1.json                # Premier League teams
│           └── ENG2.json                # Championship teams
├── scripts/
│   ├── compile.sh                       # Compile project
│   ├── clean.sh                         # Clean build artifacts
│   ├── shell.sh                         # Start Erlang shell
│   └── run.sh                           # Run application
└── rebar.config                         # Dependencies
```

## Dependencies

- **cowboy** (2.12.0): HTTP server for REST API
- **gun** (2.1.0): HTTP client for scraping
- **thoas** (1.2.1): JSON encoding/decoding
- **erlport** (0.11.0): Erlang-Python bridge

## Python Process Pools

The application maintains 3 separate Python process pools with proper OTP supervision:
- **bbc_pool**: 2 workers for BBC HTML parsing
- **fishy_pool**: 2 workers for Fishy HTML parsing
- **matcher_pool**: 2 workers for name matching

Each pool has:
- A `simple_one_for_one` supervisor managing individual `python_worker` processes
- A pool manager (`python_pool`) that coordinates work distribution
- Automatic worker restart on failure (supervised by OTP)
- Request queuing when all workers are busy
- Message-passing interface (results sent as `{python_result, WorkerPid, Result}`)
- Worker monitoring to detect failures and update availability

## Event Storage

Events are stored in ETS with composite keys: `{League, Name, Date}`

Each event tracks:
- Normalized team matchup name
- BBC score (if available)
- Fishy score (if available)
- Creation and update timestamps

Events are created on first scrape and updated when additional data arrives from other sources.

## Name Matching Algorithm

The Python name matcher uses multiple strategies in order of preference:
1. **Exact match**: After text normalization
2. **Levenshtein distance**: ≤2 character edits
3. **Abbreviation**: Check if one name abbreviates the other
4. **Token match**: ≥50% token overlap

This handles variations like:
- "Man City" vs "Manchester City"
- "Nott'm Forest" vs "Notts Forest"
- "Wolves" vs "Wolverhampton Wanderers"

## Scheduler

The centralized `scheduler.erl` manages all recurring tasks with proper process supervision. League workers register their scraping tasks on startup.

To change interval, modify the `scheduler:schedule_task/4` calls in `league_worker.erl`:
```erlang
% In league_worker.erl init/1
scheduler:schedule_task(
    {bbc_scraper, LeagueCode},
    60000,  % Interval in milliseconds
    {bbc_scraper, scrape, [LeagueCode]},
    1000    % Initial delay
)
```

The scheduler provides:
- **Visibility timeout**: Prevents overlapping executions
- **Process monitoring**: All task workers are monitored, no orphaned processes
- **Failure handling**: Tasks automatically rescheduled if worker dies
- **Automatic rescheduling**: Tasks repeat at specified intervals
- **Centralized management**: All tasks in one place
- **Clean shutdown**: Tasks cancelled when workers terminate

## License

See LICENSE.md
