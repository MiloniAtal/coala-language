### Build the Coala parser

```
cd coala
dune build
```
or

Building:
```
dune build bin/main.exe
```

Building Codegen IR and compiling with lli
```
dune exec -- bin/main.exe -l test/example.mc > test/example.out
lli test/example.out

```

### Library files
-  `ast.ml`: abstract syntax tree (AST)
-  `scanner.mll`: scanner
-  `coalaparse.mly`: parser

### Other files

- `main.ml`: top-level file to run the scanner and parser
- `example.mc`: a sample Coala source code
- `example.out`: a sample parsed code of example.mc
