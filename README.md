# Bear Basic
A custom TinyBasic parser and interpreter written in Haskell.

## Usage
To build from source you'll need GHC to be installed.
Then you can run `ghc BearBasic.hs`, to get the `BearBasic` binary.

Or if you wish to run it from GHCi, just load `BearBasic.hs` and start the main function.

## Grammar
BearBasic uses mostly the same grammar and syntax as TinyBasic.

```
statement ::= PRINT expr-list
              IF expression relop expression THEN statement
              GOTO expression
              INPUT var-list
              LET var = expression
              GOSUB expression
              RETURN
              CLEAR
              LIST
              RUN
              END

expression ::= term + expression | term - expression | term
expr-list  ::= (string|expression) (, (string|expression) )*
term       ::= factor * term | factor / term | factor
factor     ::= var | number | (expression)
number     ::= (+|-|ε) 0 | 1 | 2 | 3 | ...
var        ::= A | B | C | ... | Z
```

Currently the only implemented statements are `PRINT`, `LET`, and `END` (a great selection i know). More comming soon.