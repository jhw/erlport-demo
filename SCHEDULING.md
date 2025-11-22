# Enhanced Scheduling System

## Overview

The scheduler implements per-scraper scheduling with league-level staggering to distribute HTTP requests over time and avoid overwhelming external services.

## Architecture

The scheduler is a single `gen_server` that manages all scraper tasks using Erlang timers. Tasks are scheduled independently but can be configured to execute with staggered delays.

### Key Features

- **Per-Scraper Configuration**: Each scraper type (BBC, Fishy) has its own timing configuration
- **League Staggering**: Leagues are scraped sequentially with configurable delays between each
- **Parallel Scraper Execution**: Different scraper types (BBC vs Fishy) run in parallel for the same league
- **Automatic Rescheduling**: Tasks automatically reschedule after execution

## Configuration

Scraper timing is configured in `config/sys.config`:

```erlang
{scrapers, [
  {bbc_scraper, #{
    initial_delay_ms => 3000,      % Delay before first league starts
    interval_ms => 60000,          % Time between complete scrape cycles
    league_stagger_ms => 10000     % Delay between each league within a cycle
  }},
  {fishy_scraper, #{
    initial_delay_ms => 3000,
    interval_ms => 60000,
    league_stagger_ms => 10000
  }}
]}
```

### Configuration Parameters

| Parameter | Description | Example |
|-----------|-------------|---------|
| `initial_delay_ms` | Milliseconds to wait before scraping the first league (index 0) | 3000 = 3 seconds |
| `interval_ms` | Milliseconds between complete scrape cycles | 60000 = 60 seconds |
| `league_stagger_ms` | Milliseconds to wait between each subsequent league | 10000 = 10 seconds |

## Timing Calculation

For each scraper task, the initial execution delay is calculated as:

```
delay = initial_delay_ms + (league_index × league_stagger_ms)
```

Where `league_index` is 0-based (ENG1=0, ENG2=1, etc.)

### Example Timeline

With the default configuration and 2 leagues (ENG1, ENG2):

```
T+0s    Application starts
T+3s    BBC ENG1 + Fishy ENG1 execute (initial_delay_ms = 3000)
T+13s   BBC ENG2 + Fishy ENG2 execute (3000 + 1×10000 = 13000)
T+63s   BBC ENG1 + Fishy ENG1 execute (next cycle: 3000 + 60000)
T+73s   BBC ENG2 + Fishy ENG2 execute (next cycle: 13000 + 60000)
```

## Scraper Synchronization

### Same League, Different Scrapers: Parallel

BBC and Fishy scrapers for the same league execute **simultaneously** because they use the same stagger calculation:

```
BBC ENG1:   T+3s, T+63s, T+123s, ...
Fishy ENG1: T+3s, T+63s, T+123s, ...  (same timing)
```

### Different Leagues, Same Scraper: Staggered

Leagues are scraped **sequentially** for each scraper type:

```
BBC ENG1: T+3s,  T+63s,  T+123s, ...
BBC ENG2: T+13s, T+73s,  T+133s, ...  (+10s offset)
```

### Result: Controlled Load Distribution

All ENG1 pages are fetched together, then all ENG2 pages 10 seconds later:

```
T+3s:  BBC ENG1 + Fishy ENG1 (2 HTTP requests)
T+13s: BBC ENG2 + Fishy ENG2 (2 HTTP requests)
```

This prevents overwhelming external services while maintaining efficient parallel execution.

## Implementation Details

### Module: `scheduler.erl`

The scheduler uses a simple but effective design:

1. **Initialization**: Loads all leagues from `config_service` and schedules tasks with calculated staggered delays
2. **Task Execution**: Uses `erlang:send_after/3` for timer-based scheduling
3. **Process Spawning**: Tasks execute in lightweight processes via `proc_lib:spawn_link/1`
4. **Automatic Rescheduling**: After execution, tasks immediately schedule their next run

### Task Identification

Tasks are identified by tuples: `{ScraperType, LeagueCode}`

Examples:
- `{bbc_scraper, <<"ENG1">>}`
- `{fishy_scraper, <<"ENG2">>}`

### State Management

```erlang
-record(task, {
    id :: term(),                      % Task identifier
    interval_ms :: integer(),          % Recurrence interval
    mfa :: {module(), atom(), list()}, % Function to execute
    timer_ref :: reference()           % Active timer reference
}).

-record(state, {
    tasks :: #{term() => #task{}},     % Map of all scheduled tasks
    scraper_config :: map()            % Scraper timing configuration
}).
```

## Logging

The scheduler logs task scheduling and execution:

```
[info] Scheduled task {bbc_scraper,<<"ENG1">>} to run in 3000 ms, then every 60000 ms
[info] Scheduled task {fishy_scraper,<<"ENG1">>} to run in 3000 ms, then every 60000 ms
[info] Scheduled task {bbc_scraper,<<"ENG2">>} to run in 13000 ms, then every 60000 ms
[info] Scheduled task {fishy_scraper,<<"ENG2">>} to run in 13000 ms, then every 60000 ms

[info] Executing task {bbc_scraper,<<"ENG1">>}: bbc_scraper:scrape
[info] Executing task {fishy_scraper,<<"ENG1">>}: fishy_scraper:scrape
[info] Executing task {bbc_scraper,<<"ENG2">>}: bbc_scraper:scrape
[info] Executing task {fishy_scraper,<<"ENG2">>}: fishy_scraper:scrape
```

## Tuning Recommendations

### High-Volume Leagues

If you have many leagues and want to avoid request bursts:

```erlang
{bbc_scraper, #{
  initial_delay_ms => 5000,
  interval_ms => 300000,      % 5 minutes between cycles
  league_stagger_ms => 15000  % 15 seconds between leagues
}}
```

With 10 leagues: Last league starts at 5s + (9 × 15s) = 140 seconds

### Low-Latency Requirements

For faster scraping with fewer leagues:

```erlang
{bbc_scraper, #{
  initial_delay_ms => 1000,
  interval_ms => 30000,       % 30 seconds between cycles
  league_stagger_ms => 5000   % 5 seconds between leagues
}}
```

### No Staggering

To scrape all leagues simultaneously (not recommended):

```erlang
{bbc_scraper, #{
  initial_delay_ms => 3000,
  interval_ms => 60000,
  league_stagger_ms => 0      % All leagues at once
}}
```

## OTP Compliance

This scheduler design follows OTP best practices:

- ✅ Single responsibility: Only manages task scheduling
- ✅ Simple supervision: One gen_server, no additional supervisors needed
- ✅ Fire-and-forget execution: Spawned tasks don't block the scheduler
- ✅ Graceful shutdown: All timers cancelled in `terminate/2`
- ✅ Minimal state: Only tracks task configuration and timer references

## Future Enhancements

Possible improvements (not currently implemented):

- Dynamic interval adjustment based on scraper success/failure
- Exponential backoff on repeated failures
- Priority-based scheduling
- Task execution history and metrics
- Circuit breaker pattern for failing scrapers
- Distributed scheduling for multi-node deployments
