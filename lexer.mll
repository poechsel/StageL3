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
let s_char_sequence = [^ '"' '\\' '\n']+
let string_litteral = "L"? "\"" s_char_sequence "\""



(*** headers name ***)
let q_char_sequence = [^ '\n' '"']+
let h_char_sequence = [^ '\n' '>']+ 
let header_name = ('"' q_char_sequence '"' | "<" h_char_sequence ">")


(*** preprocessing numbers ***)
let pp_number = (digit | "." digit) (digit | identifier_nondigit | "e" sign | "E" sign | "p" sign | "P" sign | ".")


(*** ponctuators ***)
let ponctuators = "[" | "(" | ")" | "]" | "{" | "}" | "." | "->" | "++" | "--" | "&" | "*" | "+" | "-" | "~" | "!" | "/" | "%" | "<<" | ">>" | "<" | ">" | "<=" | ">=" | "==" | "!=" | "^" | "|" | "&&" | "||" | "?" | ":" | ";" | "..." | "=" | "*=" | "/=" | "%=" | "+=" | "-=" | "<<=" | ">>=" | "&=" | "^=" | "|=" | "," | "#" | "##" | "<:" | ":>" | "<%" | "%>" | "%:" | "%:%:"


rule token = parse    (* la "fonction" aussi s'appelle token .. *)
  | [' ' '\t']     { token lexbuf }
  | '\n' {incr_linenum lexbuf; token lexbuf}




  | '.' { DOT }

  | identifier_nondigit (identifier_nondigit | digit)* as s 
    { IDENT(s)}






  | (digit_sequence? as a) ((("e" | "E") as exp_word) (sign as sgn) (digit_sequence as dgts)) (['f' 'F' 'l' 'L']? as suffix)
  { 
        let num_a = Num.num_of_string (if a = "" then "0" else a) in
        let num_b = Num.num_of_string "0" in
        let exp = Some (Ast.Exponent(exp_word, (if sgn = '+' then 1 else -1), int_of_string dgts)) 
        in 
        DEC_CST_FRAC(num_a, num_b, exp, suffix)
    }

  | (digit_sequence? as a) ('.' (digit_sequence? as b))  ((("e" | "E") as exp_word) (sign as sgn) (digit_sequence as dgts))? (['f' 'F' 'l' 'L']? as suffix)
  { 
        let num_a = Num.num_of_string (if a = "" then "0" else a) in
        let num_b = Num.num_of_string (match b with
            | "" -> "0"
            | b  -> b) in
        let exp = match (exp_word, sgn, dgts) with
            | Some exp_word, Some sgn, Some dgts ->
                    Some (Ast.Exponent(exp_word, (if sgn = '+' then 1 else -1), int_of_string dgts)) 
            | _ -> None
        in 
        DEC_CST_FRAC(num_a, num_b, exp, suffix)
  }

    | ("0x" | "0X") (hexadecimal_digit_sequence? as a) ('.' (hexadecimal_digit_sequence? as b))?  ((("p" | "P") as exp_word) (sign as sgn) (hexadecimal_digit_sequence as dgts)) (['f' 'F' 'l' 'L']? as suffix)
    { 
        let num_a = Num.num_of_string (if a = "" then "0" else "0x" ^ a) in
        let num_b = Num.num_of_string @@ "0x" ^ (match b with
            | None | Some "" -> "0"
            | Some b -> b) in
        let exp = Some (Ast.Exponent(exp_word, (if sgn = '+' then 1 else -1), int_of_string dgts)) 
        in 
        HEX_CST_FRAC(num_a, num_b, exp, suffix)
  }


    | octal_constant as s
    {OCT_CST(String.sub s 1 (String.length s - 1))}
    | decimal_constant as s
    {DEC_CST(s)}
    | hexadecimal_constant as s
    {HEX_CST(s)}





