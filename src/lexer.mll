{
  open Parser

  let line = ref 1
  let buf = Buffer.create 1024
  let init_buffer () = Buffer.reset buf
  let add_char_to_buffer = Buffer.add_char buf
  let add_string_to_buffer = Buffer.add_string buf
  let get_buffer_contents () = Buffer.contents buf
  let clear_buffer () = Buffer.clear buf
}

let blank = [' ' '\t']
let newline = ['\n' '\r']
let substr = [^ '"' '\\' '\n']+

rule token = parse
  | blank     { token lexbuf }
  | newline   { incr line; token lexbuf }
  | "="       { EQ }
  | "/="       { SEQ }
  | "\""      { clear_buffer ();
                string lexbuf }
  | eof       { EOF }

and string = parse
    "\\\"" {
      (* This double quote is escaped from COIL string context, not
         source language context. i.e. original string contains a
         double quote as itself, not concatenated with a backslash. *)
      add_string_to_buffer "\"";
      string lexbuf
    }
  | '"'    { let s = get_buffer_contents() in STR s }
  | newline as c { add_char_to_buffer c; Lexing.new_line lexbuf; string lexbuf }
  | eof    { failwith ("Unterminated_string") }
  | "\\\\" {
      add_char_to_buffer '\\';
      add_char_to_buffer '\\';
      string lexbuf
    }
  | substr as s {
      add_string_to_buffer s;
      string lexbuf
    }
  | _ as c {
      add_char_to_buffer c;
      string lexbuf
    }
