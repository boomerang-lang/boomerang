type 'a t
  
exception Stopped
  
val create : unit -> 'a t
  
val stop : 'a t -> unit
  
val enq : (unit -> 'a) -> 'a t -> unit
  
val wait : 'a t -> unit
  
val results : 'a t -> 'a list
