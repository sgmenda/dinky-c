# DinkyC: A Dinky C Compiler in Rust

![Build](https://github.com/sgmenda/dinky-c/workflows/Rust/badge.svg)
[![Coverage Status](https://coveralls.io/repos/github/sgmenda/dinky-c/badge.svg?branch=master)](https://coveralls.io/github/sgmenda/dinky-c?branch=master)
[![License](https://img.shields.io/badge/license-BSD--3-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

* A compiler for a small subset of `C`.
* Written in `rust`.
* Targets `x86_64`.
* I wrote this following Nora Sandler's [Writing a C Compiler
  series](https://norasandler.com/2017/11/29/Write-a-Compiler.html) which in
  turn is based on Abdulaziz Ghuloum’s [An Incremental Approach to Compiler
  Construction](http://scheme2006.cs.uchicago.edu/11-ghuloum.pdf).
* I also picked up some rusty ideas from
  * Tristan Hume's [Writing a Compiler in
    Rust](https://thume.ca/2019/04/18/writing-a-compiler-in-rust/)
  * Shuhei Kagawa's [Writing an Interpreter and a Compiler in
    Rust](https://shuheikagawa.com/blog/2019/10/06/interpreter-and-compiler-in-rust/).
* some x86 guidance from
  * kesgin's [Intel Instruction Set
    pages](https://web.itu.edu.tr/kesgin/mul06/intel/index.html)
  * Doeppner's [x64 Cheat
    Sheet](https://cs.brown.edu/courses/cs033/docs/guides/x64_cheatsheet.pdf)
  * the [Compiler Explorer](https://godbolt.org/)

### Design

Compilation is broken down into 9 steps.

1. lexing (`lexer`): converts source code into a token stream.
2. parsing (`parser`): converts token stream into an abstract syntax tree (AST).
3. (not implemented yet) semantic analysis (`sema`): performs semantic 
   analysis on the AST, that is:
   1. ensure that variables are defined before use; and
   2. type checking.
4. (not implemented yet) optimization (`astopt`): performs optimizations on the
   AST.
5. intermediate code generation (`codegen`): generates pseudo-assembly from the
   AST.
6. (not implemented yet) optimization (`asmopt`): performs optimizations on the
   pseudo-assembly.
7. generate object files (`x86_64_gen`): converts pseudo-assembly to x86_64.
8. (not implemented yet) optimization (`x86_64opt`): performs optimizations on
   the generated x86_64.
9. linking/loading: generate executable from x86_64. Currently, performed inside
`main.rs` using `gcc`.

#### Why Rust?

I wanna learn rust. I have played around with rust in the past, but never did
anything non-trivial with it. This seemed like a good project for rust (pattern
matching, enums, speed). I started this project in OCaml (the language [Nora's
series](https://norasandler.com/2017/11/29/Write-a-Compiler.html) recommends)
and quickly switched because OCaml didn't seem that much better than rust for
this, and I wanted to learn rust.

#### Why C?

I like C. C is simple. My long term intention is to mess with security stuff
(like spectre mitigations; see wishlist below) and C seems to be the best
language for that (almost all cryptography software is written in C.)

I have not run into version specific stuff yet, but for when I do, I am gonna
pick [ISO C18](https://en.wikipedia.org/wiki/C18_(C_standard_revision)).

#### Why x86_64?

I wanna learn x86_64. I have just started playing CTFs and x86_64 knowledge is
invaluable when disassembling binaries.

I find intel syntax to be easier to read, so the assembly produced is in the
intel syntax. I started off with ATT syntax which is the one that [Nora's
series](https://norasandler.com/2017/11/29/Write-a-Compiler.html) uses, but
quickly switched because reading the produced assembly was harder than I
wanted it to be.

### Wishlist

#### Security

1. Implement [Spectre variant
1](https://cve.mitre.org/cgi-bin/cvename.cgi?name=CVE-2017-5753) mitigations
like [retpoline](https://support.google.com/faqs/answer/7625886). In
particular, I would like to be able to define `secret int x;` and then have
code that is independent of the value of `x`. Crypto peeps call this
"constant-time", but I want constant time even in the presence of speculative
execution by the processor. It might be useful to take a look the static
analysis tool mentioned in [this paper](https://arxiv.org/abs/1910.01755).
2. Building on the last point, if I define a variable as `secret` then it should
be cleared before being freed. This would prevent secret data from being leaked
into the heap or the stack.

#### Speed

1. Implement register allocation. Currently, all return values are stored on the
stack. Check if we can store it in a register, and if so, store it in a
register. This will save tonnes of time at runtime since `store`s and `load`s
are really expensive.
2. Implement auto-vectorization, [a la
LLVM](https://llvm.org/docs/Vectorizers.html).
3. Implement fixed-width math. Currently, everything is 64-bit. For instance,
`int x` is a 64-bit int (the spec allows this!) Implement `uint8_t`,
`uint16_t`, `uint32_t`, and `uint64_t`.

#### More Targets

1. It would be cool to compile to LLVM and then make use of its passes.
2. It would be nice to be able to compile to WASM, so one can run `C` on the web
and other WASM targets. (if this seems absurd, ask yourself, why do we use
docker?)
3. It would be cool to compile to ARM. This shouldn't be that difficult cuz of
the pseudoassembly, but I dunno, I haven't tried. I would also need to find a
way to link and run ARM executables on my computer
([qemu](https://www.qemu.org/) looks promising.)

### Testing

I have been testing this compiler against nlsandler's [Write a C Compiler!
tests](https://github.com/nlsandler/write_a_c_compiler).  `master` passes stages
1-4 of the tests.
