%token <string> ATOM
%token <string> STRING
%token SLASH
%token SEMICOLON
%token EQUAL
%token EOF

%start content_type_with_eof
%type <ContentType.t> content_type_with_eof

%%

content_type_with_eof:
  | x = content_type EOF
  { x }

content_type:
  | ty = ATOM SLASH subty = ATOM parameters = parameter*
  { ContentType.make ~parameters ty subty }

parameter:
  | SEMICOLON attribute = ATOM EQUAL value = value
    { (attribute, value) }

value:
  | atom = ATOM
    { atom }
  | string = STRING
    { string }
