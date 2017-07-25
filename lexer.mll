{
  open Parser;;        (* le type "token" est défini dans parser.mli *)
  (* ce n'est pas à vous d'écrire ce fichier, il est engendré automatiquement *)
  exception Eof;;
  let incr_linenum lexbuf =
      let pos = lexbuf.Lexing.lex_curr_p in
      lexbuf.Lexing.lex_curr_p <- {pos with
        Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
        Lexing.pos_bol = pos.Lexing.pos_cnum;
      }

let ponctuators_hashtbl  =
  let tbl = Hashtbl.create 0
  in let _ = List.iter
         (fun (k, w) ->
            Hashtbl.add tbl k w)
         ["!=", NEQ;
          "==", EQ;
          ".", DOT;
          "...", THREEDOT;
          "[", LBRACKET;
          "]", RBRACKET;
          "(", LPAREN;
          ")", RPAREN;
          "{", LCURLY;
          "}", RCURLY;
          "*=", MULASSIGN;
          "/=", DIVASSIGN;
          "%=", MODASSIGN;
          "+=", ADDASSIGN;
          "-=", SUBASSIGN;
          "<<=", LSASSIGN;
          ">>=", RSASSIGN;
          "&=", ANDASSIGN;
          "^=", XORASSIGN;
          "|=", ORASSIGN;
          "=", ASSIGN;
          "<",  SLT ;
          ">",  SGT ;
          "<=",  LEQ ;
          "=>",  GEQ ;
          "++",  INCR ;
          "--",  DECR ;
          "->",  ARROW ;
          "*",  MUL ;
          "&&",  AND ;
          "||",  OR ;
          "&", ANDBIN;
          "|", ORBIN;
          "^", XORBIN;
          "/", DIV;
          "%", MOD;
          "?", QUESTION;
          ",", COMA;
          "~", NEG;
          "!", NOT;
          "+", ADD;
          "-", SUB;
          ":", COLON;
          ";", ENDLINE;]
  in tbl


let keywords_hashtbl  =
  let tbl = Hashtbl.create 0
  in let _ = List.iter
         (fun (k, w) ->
            Hashtbl.add tbl k w)
         ["sizeof" , SIZEOF;
          "typedef", TYPEDEF;
          "extern", EXTERN;
          "static", STATIC;
          "auto", AUTO;
          "register", REGISTER;
          "void", VOID;
          "char", CHAR;
          "int", INT;
          "short", SHORT;
          "long", LONG;
          "float", FLOAT;
          "double", DOUBLE;
          "signed", SIGNED;
          "unsigned", UNSIGNED;
          "__Bool", BOOL;
          "__Complex", COMPLEX;
          "struct", STRUCT;
          "union", UNION;
          "enum", ENUM;
          "const", CONST;
          "volatile", VOLATILE;
          "restrict", RESTRICT;
          "inline", INLINE;
          "case", CASE;
          "switch", SWITCH;
          "goto", GOTO;
          "for", FOR;
          "while", WHILE;
          "if", IF;
          "else", ELSE;
          "break", BREAK;
          "continue", CONTINUE;
          "return", RETURN;
          "default", DEFAULT;
          "do", DO; ]
  in tbl
}


let nondigit = ['_' 'a'-'z' 'A'-'Z']
let digit = ['0' - '9']
let hexadecimal_digit = ['0'-'9' 'a'-'f' 'A' - 'F']
let nonzero_digit = ['1' - '9']
let octal_digit = ['0' - '7']
let hex_quad = hexadecimal_digit hexadecimal_digit hexadecimal_digit hexadecimal_digit
let universal_character_name = ("\\u"hex_quad| "\\U"hex_quad hex_quad)

let identifier_nondigit = (nondigit | universal_character_name)











(*** constants ***)
let integer_suffix = 
    ("u" | "U")(("l"|"L")? | ("LL"|"ll"))
    |  (("l"|"L") | ("LL"|"ll"))("u" | "U")?

let decimal_constant = nonzero_digit digit*
let octal_constant = '0' octal_digit* 
let hexadecimal_constant = ("0x" | "0X") hexadecimal_digit+

let hexadecimal_digit_sequence = hexadecimal_digit+
let digit_sequence = digit+

let simple_escape_sequence = ("\'" | "\"" | "\\?" | "\\" | "\\a" | "\b" | "\\f" | "\n" | "\r" | "\t" | "\\v")
let octal_escape_sequence = "\\" octal_digit octal_digit? octal_digit?
let hexadecimal_escape_sequence = "\\x" (hexadecimal_digit)+
let escape_sequence = simple_escape_sequence | octal_escape_sequence | universal_character_name | hexadecimal_escape_sequence
let sign = ['+' '-']

let c_char = [^'\'' '\\' '\n'] | escape_sequence
let c_char_sequence = c_char+
let character_constant = "L"? "'" c_char_sequence "'"


(*** string litteral ***)
let s_char_sequence = [^ '"'  ]+
let string_litteral = "L"? '"' s_char_sequence '"'



(*** headers name ***)
let q_char_sequence = [^ '\n' '"']+
let h_char_sequence = [^ '\n' '>']+ 
let header_name = ('"' q_char_sequence '"' | "<" h_char_sequence ">")


(*** preprocessing numbers ***)
let pp_number = (digit | "." digit) (digit | identifier_nondigit | "e" sign | "E" sign | "p" sign | "P" sign | ".")


(*** ponctuators ***)
let ponctuators = "[" | "(" | ")" | "]" | "{" | "}" | "." | "->" | "++" | "--" | "&" | "*" | "+" | "-" | "~" | "!" | "/" | "%" | "<<" | ">>" | "<" | ">" | "<=" | ">=" | "==" | "!=" | "^" | "|" | "&&" | "||" | "?" | ":" | ";" | "..." | "=" | "*=" | "/=" | "%=" | "+=" | "-=" | "<<=" | ">>=" | "&=" | "^=" | "|=" | "," | "#" | "##" | "<:" | ":>" | "<%" | "%>" | "%:" | "%:%:"



let pragma_type = 
    "if" | "ifndef" | "ifdef" | "elif" | "else" | "include" | "define" | "undef" | "line" | "error"

let skip_empty = [' ' '\t']*
let preproc_word = [^' ' '\t' '\n']+

rule token = parse    (* la "fonction" aussi s'appelle token .. *)
  | [' ' '\t']     { token lexbuf }
  | '\n' {print_endline "newline"; incr_linenum lexbuf; token lexbuf}

  | '\n' skip_empty "#" "pragma" skip_empty "NOM" {PREPROC_CUSTOM}
  | '\n' [' ' '\t']* "#" (['a'-'z''A'-'Z']+ [^'\n']+ as first_word) {print_endline @@ first_word ^ "bouh"; PREPROC first_word}
  (* we don't pass with single comment anymore because it is easier for preprocessors this way *)
  | "//"[^'\n']*          { token lexbuf }
  | "/*"          { multiline_comment lexbuf; token lexbuf }

  | ponctuators as s  {if Hashtbl.mem ponctuators_hashtbl s
    then Hashtbl.find ponctuators_hashtbl s
    else failwith "oups"
  }

  | identifier_nondigit (identifier_nondigit | digit)* as s 
    { if Hashtbl.mem keywords_hashtbl s then
        Hashtbl.find keywords_hashtbl s
        else IDENT(s)}
  | string_litteral as s
    { STRING_LIT(s)}



    | "0"
        { CONSTANT(Ast.CInt(Ast.Dec, Num.num_of_string "0", ""))}


  | character_constant as s
    { CONSTANT(Ast.CChar(s))}

    (** lexing floats**)
  | (digit_sequence? as a) ((("e" | "E") as exp_word) (sign as sgn) (digit_sequence as dgts)) (['f' 'F' 'l' 'L']? as suffix)
  { 
        let num_a = Num.num_of_string (if a = "" then "0" else a) in
        let num_b = Num.num_of_string "0" in
        let exp = Some (Ast.Exponent(exp_word, (if sgn = '+' then 1 else -1), Num.num_of_string dgts)) 
        in 
        CONSTANT(Ast.CFloat(Ast.Dec, num_a, num_b, exp, suffix))
    }

  | (digit_sequence? as a) ('.' (digit_sequence? as b))  ((("e" | "E") as exp_word) (sign as sgn) (digit_sequence as dgts))? (['f' 'F' 'l' 'L']? as suffix)
  { 
        let num_a = Num.num_of_string (if a = "" then "0" else a) in
        let num_b = Num.num_of_string (match b with
            | "" -> "0"
            | b  -> b) in
        let exp = match (exp_word, sgn, dgts) with
            | Some exp_word, Some sgn, Some dgts ->
                    Some (Ast.Exponent(exp_word, (if sgn = '+' then 1 else -1), Num.num_of_string dgts)) 
            | _ -> None
        in 
        CONSTANT(Ast.CFloat(Ast.Dec, num_a, num_b, exp, suffix))
  }

    | ("0x" | "0X") (hexadecimal_digit_sequence? as a) ('.' (hexadecimal_digit_sequence? as b))?  ((("p" | "P") as exp_word) (sign as sgn) (hexadecimal_digit_sequence as dgts)) (['f' 'F' 'l' 'L']? as suffix)
    { 
        let num_a = Num.num_of_string (if a = "" then "0" else "0x" ^ a) in
        let num_b = Num.num_of_string @@ "0x" ^ (match b with
            | None | Some "" -> "0"
            | Some b -> b) in
        let exp = Some (Ast.Exponent(exp_word, (if sgn = '+' then 1 else -1), Num.num_of_string dgts)) 
        in 
        CONSTANT(Ast.CFloat(Ast.Hex, num_a, num_b, exp, suffix))
  }


    (** int **)
    | (octal_constant as s) (integer_suffix? as suffix)
    { let t = "0o" ^ String.sub s 1 (String.length s - 1)
     in CONSTANT(Ast.CInt(Ast.Oct, Num.num_of_string t, suffix))}
    | (decimal_constant as s) (integer_suffix? as suffix)
    { CONSTANT(Ast.CInt(Ast.Dec, Num.num_of_string s, suffix))}
    | (hexadecimal_constant as s) (integer_suffix? as suffix)
    {CONSTANT(Ast.CInt(Ast.Dec, Num.num_of_string s, suffix))}

and multiline_comment = parse
  | "*/"   { () }
  | eof    { failwith "unterminated comment" }
  | '\n'   { incr_linenum lexbuf; multiline_comment lexbuf }
  | _      { multiline_comment lexbuf }

(* Single-line comment terminated by a newline *)
and singleline_comment = parse
  | '\n'   { incr_linenum lexbuf }
  | eof    { () }
  | _ as x { print_endline (String.make 1 x) ; singleline_comment lexbuf }




