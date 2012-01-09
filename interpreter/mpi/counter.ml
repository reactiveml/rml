
type counter = {
  mutable c_counter : int;
  c_mutex : Mutex.t;
  c_is_zero : Condition.t;
}

let mk_counter () = {
  c_counter = 0;
  c_mutex = Mutex.create ();
  c_is_zero = Condition.create ();
}

let await_zero c =
  Mutex.lock c.c_mutex;
  if c.c_counter > 0 then
    Condition.wait c.c_is_zero c.c_mutex;
  Mutex.unlock c.c_mutex

let is_zero c =
  c.c_counter = 0

let incr c =
  Mutex.lock c.c_mutex;
  c.c_counter <- c.c_counter + 1;
  Mutex.unlock c.c_mutex

let set c v =
  Mutex.lock c.c_mutex;
  c.c_counter <- v;
  Mutex.unlock c.c_mutex

let decr c =
  Mutex.lock c.c_mutex;
  c.c_counter <- c.c_counter - 1;
  if c.c_counter = 0 then
    Condition.signal c.c_is_zero;
  Mutex.unlock c.c_mutex

let apply f c =
  Mutex.lock c.c_mutex;
  c.c_counter <- f c.c_counter;
  Mutex.unlock c.c_mutex
