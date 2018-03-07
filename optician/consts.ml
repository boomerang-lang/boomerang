(*******************************************************************************
 * consts.ml - global constants
 ******************************************************************************)

let use_naive_expansion_search : bool ref = ref false
let use_only_forced_expansions : bool ref = ref false

let use_iterative_deepen_strategy : bool ref = ref false
let force_expand_regexps : bool ref = ref false

(* The set of directories to search for includes *)
let include_directories : string list ref = ref ["."]

let use_lens_context : bool ref = ref false

let generate_io_count : int ref = ref 0

let verbose : bool ref = ref false

let gen_symmetric : bool ref = ref false

let simplify_generated_lens : bool ref = ref true
