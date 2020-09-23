# lldb-eval - blazing fast debug expression evaluation
[![Build Status](https://github.com/google/lldb-eval/workflows/Build%20&%20Test/badge.svg)](https://github.com/google/lldb-eval/actions?workflow=Build+%26+Test)

## What

`lldb-eval` is an [LLDB](https://lldb.llvm.org/)-based library for evaluating
debug expressions in the context of the debuggee process. All modern debuggers
support evaluating expressions to inspect the target process state: print out
variables, access member fields, etc. `lldb-eval` is basically a REPL-like
library, that allows to inspect the process state using the familiar C++ syntax.
The primary use-case is IDE integration (for example, Stadia for Visual Studio).

## Why

LLDB has a very powerfull built-in expression evaluator (available via `expr`
command). It can handle almost any valid C++ as well as perform function calls.
But the downside of this power is poor performance, especially for large
programs with lots of debug information. This is not as critical for interactive
use, but doesn't work well for implementing IDE integrations. For example,
Stadia debugger for Visual Studio evaluates dozens and hundreds of expressions
for every "step", so it has to be fast.

`lldb-eval` makes a trade-off between performance and completeness, focusing on
performance. It features a custom expression parser and relies purely on the
debug information, aiming at sub-millisecond evaluation speed.

## Build & Test

### Dependencies

#### Linux

Install the dependencies:

```bash
sudo apt install lld-10 clang-10 lldb-10 llvm-10-dev libclang-10-dev liblldb-10-dev
```

Or build them from source, see the instructions in the [LLDB documentation](https://lldb.llvm.org/resources/build.html#id9).

#### Windows

On Windows we need to build LLDB (and other parts) from source. The steps are
basically the same as for Linux. You will need: `CMake`, `Python3`, `SWIG`,
`Ninja` and `Visual Studio` (tested with Visual Studio 2019 16.6.5).

> **Hint:** You can install the dependencies via [Chocolatey](https://chocolatey.org/).

Run the `x64 Native Tools Command Prompt for VS 2019`:

```bash
git clone https://github.com/llvm/llvm-project.git
cd llvm-project
mkdir build_x64_optdebug
cd build_x64_optdebug

cmake ^
    -DCMAKE_INSTALL_PREFIX='C:\src\llvm-project\build_x64_optdebug\install' ^
    -DLLVM_ENABLE_PROJECTS='lldb;clang;lld' ^
    -DCMAKE_BUILD_TYPE=RelWithDebInfo ^
    -DPYTHON_HOME=C:\Python38 ^
    -GNinja ^
    ../llvm

ninja install
```

### Build

`lldb-eval` uses [Bazel](https://bazel.build/), you can find the installation
instructions on its website.

You need to set the `LLVM_INSTALL_PATH` environmental variable with a location
to your LLVM installation:

```bash
# If you installed the packages via "apt install".
export LLVM_INSTALL_PATH=/usr/lib/llvm-10

# If you built it from source using the instructions above.
export LLVM_INSTALL_PATH=C:\src\llvm-project\build_x64_optdebug\install
```

Now you can build and test `lldb-eval`:

```bash
# Build and run all tests
bazel test :all

# Evaluate a sample expression
bazel run :main -- "(1 + 2) * 42 / 4"
```

## Disclamer

This is not an officially supported Google product.
