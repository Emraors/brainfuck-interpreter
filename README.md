# Brainfuck Interpreter

This is a Haskell project that implements a brainfuck interpreter. The project consists of two modules:
- Parser: Parses a brainfuck program from a string.
- Evaluator: Evaluates a brainfuck program. 

# Requirements
To build and run this project, you will need:
- GHC; 
- Stack; 

# Building 
To build the project, run the following command:
```bash
stack build 
```
# Running
To run the brainfuck interpreter, run the following command:
```bash
stack exec brainfuck-interpreter-exe 
```
This will start the interpreter, which will read a brainfuck program and evaluates it. 

# Testing 
To run the tests, run the following command: 
```
stack test
```
# TODO 
- Improve error handling for invalid brainfuck programs.
- Implement A REPL. 
- Use free monads. 
- Improve the test suite.


