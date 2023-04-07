open TokenTypes
open Str

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let tokenize input =
  let re_lpar = Str.regexp "(" in
  let re_rpar = Str.regexp ")" in
  let re_equal = Str.regexp "=" in
  let re_notEqual = Str.regexp "<>" in
  let re_greater = Str.regexp ">" in
  let re_less = Str.regexp "<" in
  let re_greaterEqual = Str.regexp ">=" in
  let re_lessEqual = Str.regexp "<=" in
  let re_or = Str.regexp "||" in
  let re_and = Str.regexp "&&" in
  let re_not = Str.regexp "not" in
  let re_if = Str.regexp "if" in
  let re_then = Str.regexp "then" in
  let re_else = Str.regexp "else" in
  let re_add = Str.regexp "\\+" in
  let re_sub = Str.regexp "-" in 
  let re_mult = Str.regexp "\\*" in
  let re_div = Str.regexp "/" in
  let re_concat = Str.regexp "\\^" in
  let re_let = Str.regexp "let" in
  let re_def = Str.regexp "def" in
  let re_in = Str.regexp "in" in
  let re_rec = Str.regexp "rec" in
  let re_fun = Str.regexp "fun" in
  let re_arrow = Str.regexp "->" in
  let re_doubleSemi = Str.regexp ";;" in
  let re_whitespace = Str.regexp "\t\\|\n\\| " in

  (* complex tokens *)
  let re_int = Str.regexp "[-]?[0-9]+" in
  let re_bool = Str.regexp "true\\|false" in
  let re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*" in
  let re_extra = Str.regexp "[a-zA-Z0-9]+" in  
  let re_string = Str.regexp "\"[^\"]*\"" in

  let rec next_token str index = 
    if index >= String.length str then
            []
    else if (Str.string_match re_whitespace str index) then
            (next_token str (index+1))
    else if (Str.string_match re_lpar str index) then
            if ((Str.string_match re_sub str (index+1)) && (Str.string_match re_int str (index+1))) then
              let token = Str.matched_string str in
              (Tok_Int (int_of_string token))::(next_token str (Str.match_end()+1))
            else
              (Tok_LParen)::(next_token str (index+1)) 

    else if (Str.string_match re_rpar str index) then
            (Tok_RParen)::(next_token str (index+1))
    else if (Str.string_match re_equal str index) then
            (Tok_Equal)::(next_token str (index+1))
    else if (Str.string_match re_notEqual str index) then
            (Tok_NotEqual)::(next_token str (index+2))
    else if (Str.string_match re_less str index) then
            (Tok_Less)::(next_token str (index+1))
    else if (Str.string_match re_greater str index) then
            (Tok_Greater)::(next_token str (index+1))
    else if (Str.string_match re_greaterEqual str index) then
            (Tok_GreaterEqual)::(next_token str (index+2))
    else if (Str.string_match re_lessEqual str index) then
            (Tok_LessEqual)::(next_token str (index+2))
    else if (Str.string_match re_or str index) then
            (Tok_Or)::(next_token str (index+2))
    else if (Str.string_match re_and str index) then
            (Tok_And)::(next_token str (index+3))
    else if (Str.string_match re_not str index) then
            (Tok_Not)::(next_token str (index+3))
    else if (Str.string_match re_if str index) then
            (Tok_If)::(next_token str (index+2))
    else if (Str.string_match re_then str index) then
            (Tok_Then)::(next_token str (index+4))
    else if (Str.string_match re_else str index) then
            (Tok_Else)::(next_token str (index+4))
    else if (Str.string_match re_add str index) then
            (Tok_Add)::(next_token str (index+1))
    else if (Str.string_match re_arrow str index) then
            (Tok_Arrow)::(next_token str (index+2))
    else if (Str.string_match re_sub str index) then
            (Tok_Sub)::(next_token str (index+1))
    else if (Str.string_match re_mult str index) then
            (Tok_Mult)::(next_token str (index+1))
    else if (Str.string_match re_div str index) then
            (Tok_Div)::(next_token str (index+1))
    else if (Str.string_match re_concat str index) then
            (Tok_Concat)::(next_token str (index+1))
    else if (Str.string_match re_doubleSemi str index) then
            (Tok_DoubleSemi)::(next_token str (index+2))
    (* Complex token match *)
    else if (Str.string_match re_let str index) then
            let token = Str.matched_string str in
            let pos = Str.match_end() in
            if ((Str.string_match re_extra str pos)) then
              let token2 = token^(Str.matched_string str) in
              (Tok_ID token2)::(next_token str (Str.match_end()))
            else
              (Tok_Let)::(next_token str pos)

    else if (Str.string_match re_def str index) then
            let token = Str.matched_string str in
            let pos = Str.match_end() in
            if ((Str.string_match re_extra str pos)) then
              let token2 = token^(Str.matched_string str) in
              (Tok_ID token2)::(next_token str (Str.match_end()))
            else
              (Tok_Def)::(next_token str pos)
    else if (Str.string_match re_in str index) then
            let token = Str.matched_string str in
            let pos = Str.match_end() in
            if ((Str.string_match re_extra str pos)) then
              let token2 = token^(Str.matched_string str) in
              (Tok_ID token2)::(next_token str (Str.match_end()))
            else
              (Tok_In)::(next_token str pos)

    else if (Str.string_match re_rec str index) then
            let token = Str.matched_string str in
            let pos = Str.match_end() in
            if ((Str.string_match re_extra str pos)) then
              let token2 = token^(Str.matched_string str) in
              (Tok_ID token2)::(next_token str (Str.match_end()))
            else
              (Tok_Rec)::(next_token str pos)
    else if (Str.string_match re_fun str index) then
            let token = Str.matched_string str in
            let pos = Str.match_end() in
            if ((Str.string_match re_extra str pos)) then
              let token2 = token^(Str.matched_string str) in
              (Tok_ID token2)::(next_token str (Str.match_end()))
            else
              (Tok_Fun)::(next_token str (index+3))
             

    else if (Str.string_match re_int str index) then
            let tok = Str.matched_string str in
            (Tok_Int (int_of_string tok))::(next_token str (Str.match_end()))

    else if (Str.string_match re_bool str index) then
            let token = Str.matched_string str in 
            let pos = Str.match_end() in
            
            if ((Str.string_match re_extra str pos)) then
              let token2 = (token^(Str.matched_string str)) in
              (Tok_ID token2)::(next_token str (Str.match_end()))
            else
              (Tok_Bool (bool_of_string token))::(next_token str pos)
    else if (Str.string_match re_id str index) then
            let token = Str.matched_string str in
            let pos = Str.match_end() in
            (Tok_ID token)::(next_token str pos)
    else if (Str.string_match re_string str index) then
            let token = Str.matched_string str in
            let pos = Str.match_end() in 
            let token2 = String.sub token 1 ((String.length token)-2) in
            (Tok_String token2)::(next_token str pos)
    else
            raise (InvalidInputException "Lex Error")


  (* initial call *)
  in next_token input 0
;;
