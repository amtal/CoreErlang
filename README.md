# CoreErlang

CoreErlang is a haskell library which consists on a parser and pretty-printer for the intermediate language used by Erlang.

The parser uses the Parsec library and the pretty-printer was modelled after the corresponding module of the haskell-src package. It also exposes a Syntax module which allows easy manipulation of terms.

It is able to parse and pretty print all of Core Erlang. Remaining work includes customizing the pretty printer and refining the syntax interface.

# Syntax

*module*  ::=  **module** *Atom* [*fname<sub>i<sub>1</sub></sub>* , ... , *fname<sub>i<sub>k</sub></sub>*] **attributes** [*attr<sub>1</sub>*, ..., *attr<sub>m</sub>*]  *fundef<sub>1</sub>* ... *fundef<sub>n</sub>* **end**

*attr*  ::=  *Atom* = *const*

*fundef*  ::=  *fname* = *fun*

*fname*  ::=  *Atom* / *Integer*

*const*  ::=  *lit* | *clist* | *ctuple* | *cmap*

*clist*  ::=  [*const<sub>1</sub>* | *const<sub>2</sub>*]

*ctuple*  ::=  {*const<sub>1</sub>* , ..., *const<sub>n</sub>*}

*cmap*  ::=  \~{*const<sub>1</sub>* => *const<sub>2</sub>* , ..., *const<sub>m</sub>* => *const<sub>n</sub>*}\~

*lit*  ::=  *Atom* | *Integer* | *Float* | *Char* | *String* | [ ]

*fun*  ::=  **fun** (*var<sub>1</sub>*, ..., *var<sub>n</sub>*) -> *exprs*

*var*  ::=  *VariableName*

*exprs*  ::=  *expr* | <*expr<sub>1</sub>* , ..., *expr<sub>n</sub>*>

*expr*  ::=  *var* | *fname* | *lit* | *fun*

​ &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; |  [*exprs<sub>1</sub>* | *exprs<sub>2</sub>*]

​ &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; |  {*exprs<sub>1</sub>*, ...,  *exprs<sub>n</sub>*}

​ &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; |  \~{*exprs<sub>1</sub>* => *exprs<sub>2</sub>* , ..., *exprs<sub>m</sub>* => *exprs<sub>n</sub>*}\~

​ &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; |  **let** *vars* = *exprs<sub>1</sub>* **in** *exprs<sub>2</sub>*

​ &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; |  **case** *exprs* **of** *clause<sub>1</sub>* · · · *clause<sub>n</sub>* **end**

​ &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; |  **letrec** *fname<sub>1</sub>* = *fun<sub>1</sub>* · · · *fname<sub>n</sub>* = *fun<sub>n</sub>* **in** *exprs*

​ &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; |  **apply** *exprs<sub>0</sub>* (*exprs<sub>1</sub>*, . . ., *exprs<sub>n</sub>*)

​ &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; |  **call** *exprs′<sub>1</sub>*:*exprs′<sub>2</sub>*(*exprs<sub>1</sub>*, . . ., *exprs<sub>n</sub>*)

​ &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; |  **primop** *Atom*(*exprs<sub>1</sub>*, . . ., *exprs<sub>n</sub>*)

​ &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; |  **receive** *clause<sub>1</sub>* · · · *clause<sub>n</sub>* **after** *exprs<sub>1</sub>* -> *exprs<sub>2</sub>*

​ &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; |  **try** *exprs<sub>1</sub>* **of** <*var<sub>1</sub>*, . . .*var<sub>n</sub>*> -> *exprs<sub>2</sub>*

​ &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  **catch** <*var<sub>n+1</sub>*, . . .*var<sub>n+m</sub>*> -> *exprs<sub>3</sub>*

​ &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; |  **do** *exprs<sub>1</sub>* *exprs<sub>2</sub>*

​ &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; |  **catch** *exprs*

*vars*  ::=  *var* | <*var<sub>1</sub>*, ..., *var<sub>n</sub>*>

*clause*  ::=  *pats* **when** *exprs<sub>1</sub>* -> *exprs<sub>2</sub>*

*pats*  ::=  *pat* | <*pat<sub>1</sub>*, ..., *pat<sub>n</sub>*>

*pat*  ::=  *var* | *lit* | *var* = *pat* | *plist* | *ptuple* | *pmap*

*plist*  ::=  [*pat<sub>1</sub>* | *pat<sub>2</sub>*]

*ptuple*  ::=  {*pat<sub>1</sub>*, ...,  *pat<sub>n</sub>*}

*pmap*  ::=  \~{*pkey<sub>1</sub>* := *pat<sub>1</sub>*, ..., *pkey<sub>n</sub>* := *pat<sub>n</sub>*}\~

*pkey* ::= *var* | *lit*

## License

BSD3

## Authors

- David Castro Pérez <dcastrop@udc.es>
- Henrique Ferreiro García <hferreiro@udc.es>

