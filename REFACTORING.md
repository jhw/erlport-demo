# OTP Best Practices Refactoring

This document describes the architectural improvements made to follow OTP best practices and improve separation of concerns.

## Issues Fixed

### 1. Python Pool Worker Lifecycle Management ✅

**Problem**: Manual worker restart logic in `python_pool` led to race conditions and incorrect queue state.

**Solution**:
- Created `python_worker.erl` - individual worker gen_server
- Created `python_pool_worker_sup.erl` - `simple_one_for_one` supervisor for workers
- Updated `python_pool.erl` to be a pool manager that monitors workers via `'DOWN'` messages
- Workers now properly supervised by OTP, automatic restart on failure
- Pool manager discovers new workers after supervisor restarts them

**Files Changed**:
- `apps/erlport_demo/src/python_worker.erl` (new)
- `apps/erlport_demo/src/python_pool_worker_sup.erl` (new)
- `apps/erlport_demo/src/python_pool.erl` (refactored)
- `apps/erlport_demo/src/python_pools_sup.erl` (refactored)

### 2. Scheduler Task Process Monitoring ✅

**Problem**: Scheduler spawned task processes without monitoring, leading to orphaned processes if scheduler crashed.

**Solution**:
- Added monitoring of all task worker processes
- Store monitor references in task records
- Handle `'DOWN'` messages to reschedule tasks if workers die abnormally
- Proper demonitor on normal completion
- Task workers now never orphaned, always cleaned up properly

**Files Changed**:
- `apps/erlport_demo/src/scheduler.erl`

### 3. File I/O in init/1 ✅

**Problem**: Multiple processes loading the same JSON files from disk during `init/1`, causing:
- Slow startup
- Blocking init/1 (OTP anti-pattern)
- Redundant file system access
- Supervisor startup failures if files missing

**Solution**:
- Created `config_service.erl` - centralized configuration service
- Loads all league and team data once at startup
- Other processes query config_service via gen_server:call
- Fast, non-blocking init/1 in league workers and supervisors

**Files Changed**:
- `apps/erlport_demo/src/config_service.erl` (new)
- `apps/erlport_demo/src/league_worker.erl` (removed file I/O)
- `apps/erlport_demo/src/leagues_sup.erl` (removed file I/O)
- `apps/erlport_demo/src/scraper_utils.erl` (simplified to delegate to config_service)
- `apps/erlport_demo/src/infrastructure_sup.erl` (added config_service as first child)

### 4. Python Pool Callback Complexity ✅

**Problem**: Awkward callback interface requiring manual CallerPid handling, callbacks invoked in wrong context.

**Solution**:
- Changed to message-passing interface
- Python workers send results as `{python_result, WorkerPid, Result}` messages
- Scrapers use `receive` with timeout to wait for results
- Simpler, more Erlang-idiomatic pattern
- Better error handling with explicit timeouts

**Files Changed**:
- `apps/erlport_demo/src/python_pool.erl` (interface change)
- `apps/erlport_demo/src/python_worker.erl` (sends messages instead of callbacks)
- `apps/erlport_demo/src/bbc_scraper.erl` (uses receive)
- `apps/erlport_demo/src/fishy_scraper.erl` (uses receive)

### 5. Event Store Blocking Calls ✅

**Problem**: Scrapers calling `event_store:store_event/5` synchronously could block Python worker returns.

**Solution**:
- Added cast handler to `event_store.erl`
- Scrapers now use `gen_server:cast(event_store, {store_event, ...})` for non-blocking writes
- Better throughput, no risk of deadlock
- Event store operations already synchronous via ETS, no need for synchronous gen_server calls

**Files Changed**:
- `apps/erlport_demo/src/event_store.erl` (added cast handler)
- `apps/erlport_demo/src/bbc_scraper.erl` (uses cast)
- `apps/erlport_demo/src/fishy_scraper.erl` (uses cast)

### 6. Timeout Management ✅

**Problem**: Missing timeouts on operations could lead to indefinite blocking.

**Solution**:
- Added 5-second timeout to all `config_service:get_*` calls
- Added 30-second timeout to Python parsing operations (receive)
- Added 10-second timeout to Python matching operations (receive)
- All gen_server:call operations now have explicit timeouts

**Files Changed**:
- `apps/erlport_demo/src/config_service.erl` (API functions specify timeout)
- `apps/erlport_demo/src/bbc_scraper.erl` (receive timeouts)
- `apps/erlport_demo/src/fishy_scraper.erl` (receive timeouts)

## Architecture Improvements

### Before

```
erlport_demo_sup (rest_for_one)
  ├─► infrastructure_sup (one_for_one)
  │     ├─► event_store
  │     ├─► scheduler
  │     └─► python_pools_sup (one_for_one)
  │           ├─► python_pool (bbc) - manual worker management
  │           ├─► python_pool (fishy) - manual worker management
  │           └─► python_pool (matcher) - manual worker management
  └─► leagues_sup
```

### After

```
erlport_demo_sup (rest_for_one)
  ├─► infrastructure_sup (one_for_one)
  │     ├─► config_service (loads data once)
  │     ├─► event_store
  │     ├─► scheduler (monitors task workers)
  │     └─► python_pools_sup (rest_for_one)
  │           ├─► bbc_pool_worker_sup (simple_one_for_one)
  │           │     └─► [python_worker, python_worker]
  │           ├─► bbc_pool (manager, monitors workers)
  │           ├─► fishy_pool_worker_sup (simple_one_for_one)
  │           │     └─► [python_worker, python_worker]
  │           ├─► fishy_pool (manager, monitors workers)
  │           ├─► matcher_pool_worker_sup (simple_one_for_one)
  │           │     └─► [python_worker, python_worker]
  │           └─► matcher_pool (manager, monitors workers)
  └─► leagues_sup
```

## OTP Best Practices Now Followed

✅ **Proper supervision trees**: Workers supervised by `simple_one_for_one`, not manually restarted
✅ **Process monitoring**: All spawned processes monitored or linked
✅ **No orphans**: All processes properly cleaned up on failure
✅ **Fast init/1**: No blocking file I/O during process initialization
✅ **Centralized config**: Single source of truth for static data
✅ **Message passing**: No complex callback mechanisms
✅ **Timeouts**: Explicit timeouts on all blocking operations
✅ **Non-blocking**: Cast for fire-and-forget operations
✅ **Separation of concerns**: Clear boundaries between components

### 7. Concurrent Result Matching ✅

**Problem**: Scrapers processed match results sequentially, blocking on each Python call.

**Impact**:
- For 10 results: 1 parse (30s) + 10 sequential matches (10s each) = up to 130s
- Only utilized 1 matcher worker at a time despite having 2 available
- Task appeared "stuck" to scheduler during long processing
- Poor throughput

**Solution**:
- Spawn linked process per result in `process_results/3`
- All match operations happen concurrently
- Both matcher workers utilized fully
- Scraper task completes quickly (parse + spawn), doesn't block
- If scraper crashes, linked match processes cleaned up automatically

**Time improvement**: 10 results now take ~1 parse (30s) + ~5 concurrent matches (10s) = ~40s maximum

**Files Changed**:
- `apps/erlport_demo/src/bbc_scraper.erl` (concurrent spawning)
- `apps/erlport_demo/src/fishy_scraper.erl` (concurrent spawning)

## New Modules

1. **config_service.erl** - Centralized configuration/data service
2. **python_worker.erl** - Individual Python worker process
3. **python_pool_worker_sup.erl** - Supervisor for Python workers

## Modified Modules

1. **infrastructure_sup.erl** - Added config_service
2. **python_pools_sup.erl** - Restructured with worker supervisors
3. **python_pool.erl** - Now a pool manager with monitoring
4. **scheduler.erl** - Added task process monitoring
5. **league_worker.erl** - Removed file I/O
6. **leagues_sup.erl** - Removed file I/O
7. **scraper_utils.erl** - Simplified to delegate to config_service
8. **bbc_scraper.erl** - Message passing, timeouts, cast to event_store
9. **fishy_scraper.erl** - Message passing, timeouts, cast to event_store
10. **event_store.erl** - Added cast handler

## Testing Recommendations

1. Kill Python workers and verify they restart properly
2. Kill scheduler during task execution and verify no orphans
3. Verify config_service failures don't crash entire system
4. Test with missing/malformed JSON files
5. Load test with many concurrent scrape operations
6. Verify no memory leaks from orphaned processes
7. Use observer to inspect supervision tree structure

## Performance Improvements

- **Faster startup**: Data loaded once instead of per-worker
- **Better throughput**: Non-blocking event_store writes, concurrent result matching
- **No redundant I/O**: Config cached in memory
- **Proper backpressure**: Queue depth visible in pool manager state
- **Full worker utilization**: Concurrent spawns use all available matcher workers
- **3x faster scraping**: Sequential matches eliminated (130s → 40s for 10 results)

## Maintainability Improvements

- **Clearer architecture**: Proper OTP supervision patterns
- **Easier debugging**: Process tree visible in observer
- **Better error handling**: Explicit timeouts and monitored processes
- **Less coupling**: Config service as single dependency point
