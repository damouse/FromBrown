%token Dot LParen RParen Slash Eof
%token <char> Var
%start term
%type <Lambda.expr> term
%%
term: 
  | application Eof {$1}
  | Eof             {raise Lambda.Empty}
;

application:
  | application sub_expr        { Lambda.Apply ($1, $2) }
  | sub_expr                    { $1 }
;

sub_expr:
  | LParen application RParen   { $2 }
  | Slash Var Dot application   { Lambda.Lambda ($2, $4) }
  | Var                         { Lambda.Var($1) }
;