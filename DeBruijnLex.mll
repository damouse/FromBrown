{
 open DeBruijnParse
}
  rule tokenize =
parse [' ' '\t' '\n' '\r'] {tokenize lexbuf}
| ['0'-'9']+ as s          {Var (int_of_string s)}
| '('                      {LParen}
| ')'                      {RParen}
| '\\'                     {Slash}
| eof                      {Eof}
