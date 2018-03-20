open MyStdlib
open OUnit2

let assert_equal
    ~printer:(printer:'a -> string)
    ~cmp:(cmp:'a comparer)
  : 'a -> 'a -> unit =
  assert_equal
    ~printer:printer
    ~cmp:(comparer_to_equality_check cmp)

let assert_not_equal (printer:'a -> string) (expectednot:'a) (actual:'a) =
  assert_bool
    ("Expected and Actual are not equal, value=" ^
       (printer expectednot))
    (expectednot <> actual)

let assert_bool_equal (expected:bool) (actual:bool) =
  assert_equal
    ~printer:string_of_bool
    ~cmp:compare_bool
    expected
    actual

let assert_true (actual:bool) =
  assert_bool_equal
    true
    actual

let assert_false (actual:bool) =
  assert_bool_equal
    false
    actual

let assert_not_equal_bool (expected_not:bool) (actual:bool) =
  assert_not_equal string_of_bool expected_not actual

let assert_not_equal_int (expected_not:int) (actual:int) =
  assert_not_equal string_of_int expected_not actual

let assert_int_equal = assert_equal ~printer:string_of_int ~cmp:compare_int

let assert_float_equal =
  assert_equal
    ~printer:Float.to_string
    ~cmp:(fun x y ->
        let z = Float.abs (x -. y) in
        if z > 0.00000000001 then
          1
        else
          0)

let assert_int_option_equal =
  assert_equal
    ~printer:(string_of_option string_of_int)
    ~cmp:(option_compare compare_int)

let assert_float_option_equal =
  assert_equal
    ~printer:(string_of_option Float.to_string)
    ~cmp:(option_compare Float.compare)

let assert_string_double_option_equal
  (expected:(string*string) option) (actual:(string*string) option) =
  assert_equal
    ~printer:(fun x -> begin match x with
              | None -> "None"
              | Some (x1,x2) -> "(" ^ x1 ^ "," ^ x2 ^ ")" end)
    expected
    actual

let assert_string_list_option_equal
  (expected:(string list) option) (actual:(string list) option) =
  assert_equal
    ~printer:(fun x -> begin match x with
              | None -> "None"
              | Some xs -> "[" ^ (String.concat xs ~sep:";") ^ "]" end)
    expected
    actual


let assert_comparison_equal =
  assert_equal
    ~printer:show_comparison
    ~cmp:compare_comparison

let assert_char_list_list_equal = assert_equal
    ~printer:string_of_char_list_list
    ~cmp:(compare_list ~cmp:(compare_list ~cmp:compare_char))

let assert_char_list_double_equal =
  let char_list_printer = string_of_list string_of_char in
  let char_list_comparer = compare_list ~cmp:compare_char in
  assert_equal
    ~printer:(string_of_pair char_list_printer char_list_printer)
    ~cmp:(pair_compare char_list_comparer char_list_comparer)

let assert_string_equal = assert_equal
    ~printer:ident
    ~cmp:compare_string
