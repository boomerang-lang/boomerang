let list_of_queue (q:'a Queue.t) : 'a list =
  List.rev (Queue.fold (fun l v -> v::l) [] q)

type 'a t = { 
  thread : Thread.t; 
  lock : Mutex.t; 
  tasks : (unit -> 'a) Queue.t;
  results : 'a Queue.t;
  stopped : bool ref;
}

exception Stopped

let with_lock (l:Mutex.t) (f:unit -> unit) : unit =
  Mutex.lock l;
  (try f () with _ -> ());
  Mutex.unlock l

let wq_run (l:Mutex.t) (q:(unit -> 'a) Queue.t) (r:'a Queue.t) (stopped:bool ref) : unit -> unit =
  let rec loop () =
    Thread.yield ();
    (* find a task *)
    let fo = ref None in
    with_lock l (fun () -> fo := try Some (Queue.take q) with Queue.Empty -> None);
    (* run the task *)
    match !fo with
      | None -> if !stopped then () else loop ()
      | Some f -> 
          with_lock l (fun () -> Queue.add (f ()) r);
          loop ()
  in
  loop  
    
let create () : 'a t =
  let l = Mutex.create () in
  let q = Queue.create () in
  let r = Queue.create () in
  let s = ref false in
  let t = Thread.create (wq_run l q r s) () in
  { thread = t; lock = l; tasks = q; results = r; stopped = s }

let stop (wq:'a t) : unit =
  wq.stopped := true

let enq (f:unit -> 'a) (wq:'a t) : unit =
  if !(wq.stopped) then raise Stopped;
  with_lock wq.lock (fun () -> Queue.add f wq.tasks)

let wait (wq:'a t) : unit =
  stop wq;
  Thread.join wq.thread

let results (wq:'a t) : 'a list =
  let r = Queue.create () in
  with_lock wq.lock (fun () -> Queue.transfer wq.results r);
  list_of_queue r

