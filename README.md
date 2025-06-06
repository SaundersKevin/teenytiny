# About

This is my own Haskell implimentation of [Austin Henley's Teeny Tiny Compiler](https://austinhenley.com/blog/teenytinycompiler1.html)

Teeny Tiny is based on BASIC, though with a very small amount of keywords. This allows it to be created very easily, and is a really fun way to start learning about compilers!

This compiler uses C as it's target.

# The Language

IF and WHILE behave just like expected. The loop is ended with an ENDIF or ENDWHILE

GOTO takes an label and jumps to where LABEL states that

PRINT takes either a variable or a string

LET is used whenever assigning a variable to a numerical value
All variables are stored as float values

INPUT assigns user input to a given variable. As of <=1.1.0 this has to be a new variable

The stack has a default size of 32. It is interacted with using POP, PUSH, and PEEK. POP will take the top off the stack and assigns the result to a variable. PUSH pushes a value onto the stack. PEEK assigns the value of the top of the stack to a variable.

See the examples/ folder for some example programs

# TODO

- Add more explicit error messages
- Optimizations
