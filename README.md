# core-comp-unit

This is the compilation unit from a (Haskell) Core program towards different targets which can be:
- Assembly code
- Llvm IR
- C code (or better, C-- code)
- Bytecode

The codebase is very experimental and lacks of some parts and correctness checks. I suggest NOT to trust it.

### Why this project?
Before answering to this question, if you don't know what Core is, what you need to know is that GHC (the Glasgow Haskell
Compiler) uses different intermediate representations to compile a Haskell program and one of these is Core. If you are
interested in this topic, I suggest you this read, where the steps for a Haskell program compilation are well shown:  
https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/hsc-main  
As you may know, GHC exposes an API which allows the programmer to use some of the compiler facilities; this project aims
to, given a Core program, compile it into different targets with a single call (```FromCore.coreCompUnit```) in a similar
way GHC would do with a Haskell source.  
NB: there are several ways GHC can compile a Haskell source and the way this project does the Core source compilation is
very restricted.

### Thanks
https://www.stephendiehl.com/posts/ghc_03.html  
source code at:  
https://github.com/sdiehl/dive-into-ghc/tree/master/03-core

### License
MIT
