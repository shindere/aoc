let pred x = x - 1

let succ = (+) 1

let is_digit c =
  let open Char in
  code '0' <= code c && code c <= code '9'

let find_char s start next stop p f default_value =
  let rec test_char pos =
    if stop pos then default_value
    else if p s.[pos] then f s pos
    else test_char (next pos)
  in
  test_char start

let make_string s pos = String.make 1 s.[pos]

let find_first_digit s n =
  let stop i = i >= n in
  find_char s 0 succ stop is_digit make_string ""

let find_last_digit s n =
  let stop x = x < 0 in
  find_char s (n-1) pred stop is_digit make_string ""

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
