let pred x = x - 1

let succ = (+) 1

let digits = 
  [|
    "zero"; "one"; "two"; "three"; "four";
    "five"; "six"; "seven"; "eight"; "nine"
  |]

let is_numeric_digit c =
  let open Char in
  code '0' <= code c && code c <= code '9'

let is_digit str pos = 
  if is_numeric_digit str.[pos] then Some str.[pos]
  else begin
    let i = ref 0 and found = ref false and result = ref None in
    while !i <= 9 && not !found do
      let digit = digits.(!i) in
      begin
        match String.sub str pos (String.length digit) with
        | s when s=digit ->
          (result := Some (Char.chr ((Char.code '0') + !i));
          found := true)
        | _
        | exception Invalid_argument _ -> ()
      end;
      incr i;
    done;
    !result
  end

let lookup s start next stop p f default_value =
  let rec test_char pos =
    if stop pos then default_value
    else begin
      match p s pos with
      | None -> test_char (next pos)
      | Some res -> f res
    end
  in
  test_char start

let make_string ch = String.make 1 ch

let find_first_digit s n =
  let stop i = i >= n in
  lookup s 0 succ stop is_digit make_string ""

let find_last_digit s n =
  let stop x = x < 0 in
  lookup s (n-1) pred stop is_digit make_string ""

let number_of_line line =
  let n = String.length line in
  int_of_string
  (
    (find_first_digit line n) ^
    (find_last_digit line n)
  )

let fold_file filename f init =
  let chan = open_in filename in
  let rec loop acc =
    match input_line chan with
    | line -> loop (f acc line)
    | exception End_of_file -> acc
  in
  let result = loop init in
  close_in chan;
  result

let f acc line = acc + (number_of_line line)

let main () =
  let result = fold_file "input" f 0 in 
  Printf.printf "Result: %d\n" result

let _ = main ()
