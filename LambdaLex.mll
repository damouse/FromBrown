{
 open LambdaParse
}
  rule tokenize =
parse [' ' '\t' '\n' '\r'] {tokenize lexbuf}
| ['a'-'z'] as s           {Var s}
| '.'                      {Dot}
| '('                      {LParen}
| ')'                      {RParen}
| '\\'                     {Slash}
| eof                      {Eof}
