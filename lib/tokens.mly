(* varible identifiers *)
%token <string> ID

(* literals *)
%token <char> CHAR_LIT
%token <int> INT_LIT
%token <string> STR_LIT

(* reserved type names *)
%token CHAR "char" 
%token INT "int"
%token NOTHING "nothing"

(* statement keywords *)
%token VAR "var"
%token FUN "fun"
%token REF "ref"
%token RETURN "return"
%token IF "if"
%token THEN "then"
%token ELSE "else"
%token WHILE "while"
%token DO "do"

(* comparator operators *)
%token EQUAL "="
%token NOT_EQUAL "#"
%token GREATER ">"
%token LESSER "<"
%token GREATER_EQUAL ">="
%token LESSER_EQUAL "<="

(* logical operators *)
%token AND "and"
%token OR "or"
%token NOT "not"

(* arithmetic operators *)
%token PLUS "+"
%token MINUS "-"
%token STAR "*"
%token DIV "div"
%token MOD "mod"

(* structural symbols *)
%token LPAREN "("
%token RPAREN ")"
%token LBRACKET "["
%token RBRACKET "]"
%token LBRACE "{"
%token RBRACE "}"
%token COMMA ","
%token COLON ":"
%token SEMICOLON ";"
%token LARROW "<-"

(* end of file *)
%token EOF "eof"

%left "+" "-" "*"
%left "div" "mod"
%left "and" "or"
%nonassoc "not"

%%
