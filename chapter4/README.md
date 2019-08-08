Look at the flags for "menhir", in particular at the `--dump`
and `--explain`. You can find the `parser.automaton` and
`parser.conflicts` files at the `_build` directory.


test4.tig:

```
LET FUNCTION ID LPAREN ID COLON ID RPAREN COLON ID EQ IF ID EQ INT THEN INT ELSE INT TIMES ID LPAREN ID MINUS INT RPAREN IN ID LPAREN INT RPAREN END
```
