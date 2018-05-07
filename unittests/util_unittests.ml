open MyStdlib
open Ounit_general_extensions
open OUnit

module IntHeap = HeapOf(IntModule)

let push_pop_same _ =
  let h = IntHeap.empty in
  let h = IntHeap.push h 1 in
  let h = snd @$ Option.value_exn (IntHeap.pop h) in
  assert_int_equal
    0
    (IntHeap.size h)

let push_2x_pop_same _ =
  let h = IntHeap.empty in
  let h = IntHeap.push h 1 in
  let h = IntHeap.push h 1 in
  let h = snd @$ Option.value_exn (IntHeap.pop h) in
  assert_int_equal
    1
    (IntHeap.size h)

let push_many_pop _ =
  let h = IntHeap.empty in
  let h = IntHeap.push h 1 in
  let h = IntHeap.push h 2 in
  let h = IntHeap.push h 3 in
  let h = IntHeap.push h 4 in
  let h = IntHeap.push h 5 in
  let h = IntHeap.push h 1 in
  let h = IntHeap.push h 2 in
  let h = IntHeap.push h 3 in
  let h = IntHeap.push h 4 in
  let h = IntHeap.push h 5 in
  let h = IntHeap.push h 8 in
  let h = IntHeap.push h 3 in
  let h = snd @$ Option.value_exn (IntHeap.pop h) in
  assert_int_equal
    11
    (IntHeap.size h)


let stochastic_regex_from_regex_suite = "Test Heap" >:::
  [
    "push_pop_same" >:: push_pop_same;
    "push_2x_pop_same" >:: push_2x_pop_same;
    "push_many_pop" >:: push_many_pop;
  ]

let _ = run_test_tt_main stochastic_regex_from_regex_suite
