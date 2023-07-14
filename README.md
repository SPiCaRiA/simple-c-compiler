# Simple C Compiler

A compiler for the Simple C language (a subset of the C programming langauge) that generates code for 64-bit Intel processor. (The project of my compiler class.)

## The Language

Note: `e` for epsilon.

```
translation-unit -> e
                  | global-declaration translation-unit
                  | function-definition translation-unit

global-declaration -> specifier global-declarator-list ;

global-declarator-list -> global-declarator
                        | global-declarator , global-declarator-list

global-declarator -> pointers id
                   | pointers id ( )
                   | pointers id [ num ]

pointers -> e
          | * pointers

specifier -> int
           | char
           | long
           | void

function-definition -> specifier pointers id ( parameters ) { declarations statements }

parameters -> void
            | parameter-list

parameter -> specifier pointers id

declarations -> e
              | declaration declarations

declaration -> specifier declarator-list ;

declarator-list -> declarator
                 | declarator , declarator-list

declarator -> pointers id
            | pointers id [ num ]

statements -> e
            | statement statements

statement -> { declarations statements }
           | return expression ;
           | while ( expression ) statement
           | for ( assignment ; expression ; assignment ) statement
           | if ( expression ) statement
           | if ( expression ) statement else statement
           | assignment ;

assignment -> expression = expression
            | expression

expression -> expression || expression
            | expression && expression
            | expression == expression
            | expression != expression
            | expression <= expression
            | expression >= expression
            | expression < expression
            | expression > expression
            | expression + expression
            | expression - expression
            | expression * expression
            | expression / expression
            | expression % expression
            | ! expression
            | - expression
            | & expression
            | * expression
            | sizeof expression
            | expression [ expression ]
            | id ( expression-list )
            | id ( )
            | id
            | num
            | string
            | character
            | ( expression )

expression-list -> expression
                 | expression , expression-list
```


## Usage

Generate Intel assembly code with this compiler and compile executable with GCC.

```sh
scc < file.c > file.s 2> /dev/null
gcc file.s [additional-source-files]
./a.out
```


## Demo

```sh
# Compile executable
scc < demo/calc.c > demo/calc.s 2> /dev/null
gcc demo/calc.s

# Expect no output
./a.out < demo/calc.in | diff calc.out -
```

