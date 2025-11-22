# Multi-Language Runtime Support

## Overview

ErlCracker's architecture is **runtime-agnostic** - it can manage any persistent execution environment, not just Python. This document explores how the same patterns apply to Go, Node.js, WebAssembly, and other languages.

## Current Implementation: Python via ErlPort

**How it works:**
```erlang
% ErlPort spawns Python interpreter in same OS process
{ok, Pid} = python:start([{python_path, Path}]),
Result = python:call(Pid, module, function, args)
```

**Characteristics:**
- ✅ Direct function calls (C-level integration)
- ✅ Very fast communication (~microseconds)
- ✅ Shared memory space
- ⚠️ Python GIL limits parallelism
- ❌ Crashes can affect BEAM VM
- ⏱️ Startup: ~100ms (interpreter + imports)

## Go Runtime Support

### Why Go Is a Great Fit

**Advantages over Python:**
1. ✅ **Faster startup** - Compiled binary (1-10ms vs ~100ms)
2. ✅ **Lower memory** - No interpreter overhead
3. ✅ **Better isolation** - Separate process can't crash BEAM
4. ✅ **True concurrency** - No GIL, all cores available
5. ✅ **Type safety** - Compile-time checks
6. ✅ **Simpler deployment** - Single binary, no dependencies

**Trade-offs:**
1. ⚠️ Need custom port protocol (no ErlPort equivalent)
2. ⚠️ Slightly higher latency - Inter-process communication
3. ⚠️ Must compile Go code (vs Python's dynamic loading)

### Implementation Approach: Port-Based

**Erlang side:**
```erlang
-module(erlcracker_go_runtime).
-behaviour(erlcracker_runtime).

start_runtime(Config) ->
    GoBinary = maps:get(binary_path, Config),
    Port = open_port(
        {spawn, GoBinary},
        [binary, {packet, 4}, exit_status]
    ),
    {ok, Port}.

call_function(Port, Module, Function, Args) ->
    Request = encode_request(Module, Function, Args),
    port_command(Port, Request),
    receive
        {Port, {data, Response}} ->
            decode_response(Response)
    after Timeout ->
        {error, timeout}
    end.

stop_runtime(Port) ->
    port_close(Port),
    ok.
```

**Go side (persistent worker):**
```go
package main

import (
    "encoding/binary"
    "encoding/json"
    "io"
    "os"
)

type Request struct {
    Module   string        `json:"module"`
    Function string        `json:"function"`
    Args     []interface{} `json:"args"`
}

type Response struct {
    Result interface{} `json:"result"`
    Error  string      `json:"error,omitempty"`
}

func main() {
    for {
        // Read packet length (4 bytes)
        var length uint32
        binary.Read(os.Stdin, binary.BigEndian, &length)

        // Read packet data
        data := make([]byte, length)
        io.ReadFull(os.Stdin, data)

        // Decode request
        var req Request
        json.Unmarshal(data, &req)

        // Execute function
        result := executeFunction(req.Module, req.Function, req.Args)

        // Encode response
        resp := Response{Result: result}
        respData, _ := json.Marshal(resp)

        // Write packet length + data
        binary.Write(os.Stdout, binary.BigEndian, uint32(len(respData)))
        os.Stdout.Write(respData)
    }
}

func executeFunction(module, function string, args []interface{}) interface{} {
    // Route to appropriate function
    switch module {
    case "math":
        switch function {
        case "add":
            return args[0].(float64) + args[1].(float64)
        case "multiply":
            return args[0].(float64) * args[1].(float64)
        }
    case "string":
        switch function {
        case "reverse":
            s := args[0].(string)
            runes := []rune(s)
            for i, j := 0, len(runes)-1; i < j; i, j = i+1, j-1 {
                runes[i], runes[j] = runes[j], runes[i]
            }
            return string(runes)
        }
    }
    return nil
}
```

**Usage (identical API):**
```erlang
% Start Go pool
{ok, _} = erlcracker:start_pool(
    go_pool,
    erlcracker_go_runtime,
    #{binary_path => "./priv/go_worker", pool_size => 4}
).

% Call Go functions
Result = erlcracker:call(go_pool, math, add, [5, 3], 1000).
% Result = {ok, 8}

Reversed = erlcracker:call(go_pool, string, reverse, ["hello"], 1000).
% Reversed = {ok, "olleh"}
```

### Protocol Options

#### 1. Packet Protocol (Recommended)

**Format:**
```
[4 bytes: length][N bytes: JSON payload]
```

**Pros:**
- ✅ Simple to implement
- ✅ Built-in Erlang support: `{packet, 4}`
- ✅ Efficient framing

**Cons:**
- ⚠️ JSON serialization overhead

#### 2. BERT Protocol

**Format:**
```erlang
% Erlang External Term Format
term_to_binary({call, Module, Function, Args})
```

**Pros:**
- ✅ Preserves Erlang types
- ✅ Libraries available: github.com/processone/go-erlang

**Cons:**
- ⚠️ More complex implementation
- ⚠️ Go doesn't have native atoms

#### 3. Protobuf/MessagePack

**Pros:**
- ✅ Fast serialization
- ✅ Strong typing

**Cons:**
- ⚠️ Requires schema definitions
- ⚠️ Additional dependencies

### Performance Expectations

| Metric | Python (ErlPort) | Go (Port) |
|--------|------------------|-----------|
| **Cold start** | ~100ms | ~1-10ms |
| **Warm call overhead** | ~50μs | ~100-500μs |
| **Throughput** | Limited by GIL | All cores |
| **Memory per worker** | ~30MB | ~5-10MB |
| **Crash isolation** | Can crash BEAM | Cannot crash BEAM |

## Node.js Runtime Support

**Similar to Go approach:**

```javascript
// Node.js worker
const readline = require('readline');

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout
});

rl.on('line', (line) => {
  const request = JSON.parse(line);
  const result = executeFunction(request.module, request.function, request.args);
  console.log(JSON.stringify({result}));
});

function executeFunction(module, func, args) {
  // Route to appropriate function
  if (module === 'lodash') {
    const _ = require('lodash');
    return _[func](...args);
  }
  // ... more modules
}
```

**Characteristics:**
- ⏱️ Startup: ~50-100ms (Node.js + requires)
- 🔄 Single-threaded but non-blocking I/O
- 📦 Access to npm ecosystem
- ⚠️ JSON serialization overhead

## WebAssembly Runtime Support

**Using Wasmtime or similar:**

```erlang
-module(erlcracker_wasm_runtime).

start_runtime(Config) ->
    WasmFile = maps:get(wasm_file, Config),
    % Port to Wasmtime CLI or use Rust NIF
    Port = open_port(
        {spawn, "wasmtime run " ++ WasmFile},
        [binary, {packet, 4}]
    ),
    {ok, Port}.
```

**Characteristics:**
- ⚡ Very fast startup (~1ms)
- 🔒 Sandboxed execution
- 🌐 Language-agnostic (Rust, C, C++, AssemblyScript)
- ⚠️ Limited host interaction

## Runtime Behavior Interface

**Proposed API contract:**

```erlang
-module(erlcracker_runtime).

-callback start_runtime(Config :: map()) ->
    {ok, RuntimeHandle :: term()} | {error, Reason :: term()}.

-callback call_function(
    RuntimeHandle :: term(),
    Module :: atom() | binary(),
    Function :: atom() | binary(),
    Args :: list()
) ->
    {ok, Result :: term()} | {error, Reason :: term()}.

-callback stop_runtime(RuntimeHandle :: term()) -> ok.

% Optional: health check
-callback ping(RuntimeHandle :: term()) ->
    pong | {error, Reason :: term()}.
```

**Implementations:**
- `erlcracker_python_runtime` - ErlPort-based (v1.0)
- `erlcracker_go_runtime` - Port-based (v1.2)
- `erlcracker_nodejs_runtime` - Port-based (v1.3)
- `erlcracker_wasm_runtime` - Wasmtime-based (v1.4)

## Unified API

**The power of runtime abstraction:**

```erlang
% Configure multiple runtime pools
Pools = [
    {python_pool, erlcracker_python_runtime, #{
        pool_size => 4,
        python_path => "priv/python"
    }},
    {go_pool, erlcracker_go_runtime, #{
        pool_size => 8,
        binary_path => "priv/go_worker"
    }},
    {node_pool, erlcracker_nodejs_runtime, #{
        pool_size => 4,
        node_script => "priv/node_worker.js"
    }}
],

% Start all pools
lists:foreach(fun({Name, Runtime, Config}) ->
    erlcracker:start_pool(Name, Runtime, Config)
end, Pools),

% Use them interchangeably
PythonResult = erlcracker:call(python_pool, numpy, array, [[1,2,3]], 5000),
GoResult = erlcracker:call(go_pool, math, fibonacci, [100], 5000),
NodeResult = erlcracker:call(node_pool, lodash, chunk, [[1,2,3,4], 2], 5000).
```

**Benefits:**
- ✅ Same fire-and-forget API
- ✅ Same pooling behavior
- ✅ Same timeout protection
- ✅ Same crash recovery
- ✅ Choose best language for each task

## Comparison Matrix

| Language | Integration | Startup | Memory | Concurrency | Safety | Ecosystem |
|----------|-------------|---------|--------|-------------|--------|-----------|
| **Python** | ErlPort (embedded) | ~100ms | High | GIL-limited | Can crash VM | Excellent (PyPI) |
| **Go** | Port (subprocess) | ~5ms | Low | True parallel | Isolated | Good (stdlib) |
| **Node.js** | Port (subprocess) | ~80ms | Medium | Event loop | Isolated | Excellent (npm) |
| **Rust** | NIF or WASM | ~1ms | Very low | True parallel | NIF=unsafe, WASM=safe | Growing |
| **Ruby** | Port (subprocess) | ~50ms | Medium | GIL-limited | Isolated | Good (gems) |
| **Lua** | Luerl (embedded) | ~10ms | Low | Single-threaded | Can crash VM | Limited |

## Use Case Recommendations

### Choose Python when:
- Need scientific computing (NumPy, SciPy, Pandas)
- ML/AI libraries (TensorFlow, PyTorch, scikit-learn)
- Existing Python codebase
- Rapid prototyping
- **Current ErlCracker implementation** ✓

### Choose Go when:
- Need high throughput
- CPU-intensive operations
- Low latency requirements
- Systems programming
- Concurrent processing

### Choose Node.js when:
- Need npm ecosystem
- Async I/O heavy workloads
- JavaScript developers on team
- Web scraping (Puppeteer)

### Choose WebAssembly when:
- Need maximum sandboxing
- Cross-language compatibility
- Portability requirements
- Security-critical code

## Implementation Roadmap

### v1.0 - Python Runtime (✅ Complete)
- ErlPort-based Python integration
- Fire-and-forget API
- Double-spawn timeout protection
- Production-ready

### v1.1 - Core Enhancements
- Overflow workers (dynamic scaling)
- Metrics and observability
- Circuit breaking

### v1.2 - Go Runtime
- Port-based protocol implementation
- JSON or BERT serialization
- Example Go worker template
- Performance benchmarks vs Python

### v1.3 - Node.js Runtime
- Port-based protocol
- Line-delimited JSON
- npm package integration examples

### v1.4 - WebAssembly Runtime
- Wasmtime integration
- WASI support
- Multi-language WASM examples

### v2.0 - Universal Runtime Manager
- Pluggable runtime backends
- Hot runtime switching
- Multi-runtime coordination
- Runtime-specific optimizations

## Conclusion

**ErlCracker's architecture is fundamentally runtime-agnostic.** The patterns of:

1. Pre-warmed persistent runtimes
2. Fire-and-forget invocation
3. Execution isolation
4. Automatic lifecycle management
5. OTP supervision

...apply equally to Python, Go, Node.js, WebAssembly, and any other execution environment.

The current Python implementation demonstrates the pattern. **Future runtime support is an extension, not a rewrite** - the pooling, queuing, timeout, and supervision logic remains identical.

This makes ErlCracker a **universal execution environment manager** for Erlang, similar to how Firecracker is a universal microVM manager for AWS. 🚀
