(* Input module that provides parsing facilities over files where the
   information can be read line by line *)

(* shortcuts *)
let fos = float_of_string
let ios = int_of_string

(* same as failwith but uses a format instead *)
let fail_fmt fmt = Format.kasprintf failwith fmt

(* We redefine input_line to make it ignore lines that begin with '//'.
   We also hides side-effects used for error printing *)
let open_in, input_line, error_message =
  let last = ref "" in
  let number = ref 0 in
  let fn = ref "" in
  (fun filename ->
    number:=0;
    fn:=filename;
    open_in filename),
  (fun ic ->
    let rec aux () =
      let l = input_line ic in
      last := l;
      incr number;
      if String.length l > 2 && String.sub l 0 2 = "//" then aux ()
      else l
    in aux ()),
  (fun msg ->
    fail_fmt "File %s, line %i, on input\n%s\nError: %s" !fn !number !last msg)

(* redfining split_on_char to make it filter empty strings *)
let split_on_char c l =
  String.split_on_char c l |> List.filter ((<>) "")

(* different versions of input line that read a line with words
   separated commas and builds different target: int, float, int list,
   float array etc.*)
let list ic = input_line ic |> split_on_char ','
let array ic = list ic |> Array.of_list

let int ic =
  match list ic with
  | [x] -> ios x
  | _ -> error_message "was expecting a single integer"

let float ic =
  match list ic with
  | [x] -> fos x
  | _ -> error_message "was expecting a single float"

let int2 ic =
  match list ic with
  | [x;y] -> (ios x),(ios y)
  | _ -> error_message "was expecting two integers"

let float2 ic =
  match list ic with
  | [x;y] -> (fos x),(fos y)
  | _ -> error_message "was expecting two floats"

let int_list ic = list ic |> List.map ios
let float_list ic = list ic |> List.map fos
let int_array ic = array ic |> Array.map ios
let float_array ic = array ic |> Array.map fos

(* checks that we have indeed reached the end of the file, and close
   the channel*)
let check_end ic =
  match input_line ic with
  | _ -> error_message "file should be ended"
  | exception End_of_file -> close_in ic
