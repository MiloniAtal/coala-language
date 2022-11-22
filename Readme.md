# Coala Language Compiler Frontend

![alt text](https://github.com/MiloniAtal/coala-PLT/blob/main/coala.jpg)

This repository contains the compiler frontend to translate a program written in the language Coala, to LLVM IR. This is a project for the course COMS W 4115 Programming Languages and Translators, Fall 2022, Columbia University, under the guidance of Professor Ronghui Gu and our mentor, Zixuan Zhang.

Coala is a new language we have developed, whose main focus is to provide garbage collection. It will try to unburden the programmer from the task of de-allocating unused memory. We want Coala to have this feature in an attempt to make a piece of code robust to memory leaks, irrespective of whether it was carefully written or not. By freeing the programmers from manual memory management, it will not only make their task easy but will also reduce the length of written code, which is always beneficial, especially in large software codebases.

Coala is an imperative language. It is strongly and statically typed, and has static scopes and a strict evaluation order.

## File structure:

    |- coala
        |- bin
            |- main.ml
            |- dune

        |- lib
            |- ast.ml
            |- coalaparse.mly
            |- codegen.ml
            |- sast.ml
            |- scanner.ml
            |- semant.ml

        |- test
            |- tests
            |- testall.sh
            |- dune


    The coala/lib contains the main scanner, parser, semant, sast, ast and codegen files. The coala/bin contains the main file used to run the compiler. 
    The coala/test/tests contain all the test cases. 

## Method to run:
    1. Install llvm, dune
    2. Clone the repository or download the code
    3. Move into coala folder
    4. Run:
        dune build bin/main.exe 
        dune exec -- bin/main.exe [options] [inputfile] > [outputfile]
        lli [outputfile]

## Testing:
    dune test
