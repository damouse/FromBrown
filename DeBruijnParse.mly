%token LParen RParen Slash Eof
%token <int> Var

%start term

%type <DeBruijn.expr> term
%%

term: 
  application Eof    {$1}
| Eof                {raise DeBruijn.Empty}
;

application:
| application sub_expr       { DeBruijn.Apply ($1, $2) }
| sub_expr                   { $1 }
;

sub_expr:
| LParen application RParen  { $2 }
| Slash application          { DeBruijn.Lambda $2 }
| Var                        { DeBruijn.Var $1 }
;
