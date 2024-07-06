# L6 Compiler

Author(alphabetical order): Xinyou Ji, Zixi An

## Directories

- `src/abs2asm`: Translation from abstract assembly to x86_64 assembly.
- `src/wasmgen`: Translation from AST to webassembly text format
- `src/ast`: The AST structure definitions.
- `src/elaboration`: Elaboration, emits the final AST.
- `src/regalloc`: Register Allocation.
- `src/registers`: X86_64 register conventions help module.
- `src/typecheck`: Typechecker.

## WebAssembly text format compiler

### How to compile to wasm

Use the flag "-ewasm" to specify the wasm compilation target. The other flags works as usual.
For example, to compile the file "sample.l6" with the header "sample.h0", run the command 
`./bin/c0c -ewasm -l sample.h0 sample.l6`. This will produce the output file `sample.l6.wat`.

### How to execute compiled wasm

We use the a custom python script (`link_wasm.py`) and the `wasmtime` module to link and execute
the compiled code. The linking step is required because we need to provide support for heap allocated
memory, and wasm cannot interface with the OS directly. We achieved this by using a custom, simple memory allocation
module (`alloc.wat`) directly written in wasm, and linked to the program during this step.

To run the compiled program, navigate to the `lab6` directory and run `python3 link_wasm.py sample.l6.wat`.

### How to run the test suite

We ported over almost all the tests from l1-large, l2-large, l3-large, and l4-large to ensure the correctness of the new 
compilation target. The omitted tests are typecheck only tests, tests that invokes the c0 runtime library functions, and 
tests that cannot be successfully executed due to the limitations of wasm.

We've provided a python script to run through every test, similar to the grading script. Navigate to the top level `compiler`
directory, and run the following command `python3 run_l6.py tests/l1-large 4`. The first argument specifies the testing folder,
and the second argument specifies the number of parallel threads. Both of them are required parameters.

## Export the compiler for web integration

To showcase the cross platform compatibility of wasm, and to demonstrate the ease of browser integration, we've modified our
compiler code to be built as a wasm module, which can be imported directly in any javascript file for any website frontend.
We've built a simple website to demo this (https://thelongmarch-azx.github.io/).

Alternatively, you can deploy the website locally by running the `deploy_web` script under the lab6 directory.