val wholename : string ref
val basename : string ref
val created_files : string ref
val current : Buffer.t
val emit : string -> unit
val dump : unit -> unit

val lex : Lexing.lexbuf -> unit

val fcl_of_src : string -> string
val fcl_of_src_str : string -> string
