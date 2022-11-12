### Build the Coala parser

```
ocamlbuild test.native
```

### Run the Coala parser
```
./test.native
```

### Compiler files
-  `ast.ml`: abstract syntax tree (AST)
-  `scanner.mll`: scanner
-  `coalaparse.mly`: parser

### Other files

- `test.ml`: top-level file to test and run the scanner
- `example.mc`: a sample Coala source code
- `example.out`: a sample parsed code of example.mc
