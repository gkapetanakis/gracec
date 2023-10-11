%token CHAR "char" 
%token INT "int"
%token NOTHING "nothing"
%token <char> CHAR_LIT
%token <int> INT_LIT
%token <string> STRING_LIT
%token <string> ID
%token VAR "var"
%token FUN "fun"
%token REF "ref"
%token RETURN "return"
%token IF "if"
%token THEN "then"
%token ELSE "else"
%token WHILE "while"
%token DO "do"
%token EQUAL "="
%token NOT_EQUAL "#"
%token GREATER ">"
%token LESSER "<"
%token GREATER_EQUAL ">="
%token LESSER_EQUAL "<="
%token AND "and"
%token OR "or"
%token NOT "not"
%token PLUS "+"
%token MINUS "-"
%token STAR "*"
%token DIV "div"
%token MOD "mod"
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
%token EOF "eof"

%left "+" "-" "*" "div" "mod"
%left "and" "or"

%%