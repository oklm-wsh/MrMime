%token <string> ATOM
%token <string> STRING
%token <int>    DIGIT
%token          SLASH
%token          SEMICOLON
%token          EQUAL
%token          DOT
%token          EOF

%start content_type_with_eof
%type <ContentType.t> content_type_with_eof

%%

content_type_with_eof:
  | x = content_type EOF
  { x }

(** See RFC 2045 § 5.1 and Lexer.content_type

    content   := "Content-Type" ":" type "/" subtype
                 *(";" parameter)
                 ; Matching of media type and subtype
                 ; is ALWAYS case-insensitive.

    parameter := attribute "=" value
*)

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
