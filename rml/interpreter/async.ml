(**********************************************************************)
(*                                                                    *)
(*                           ReactiveML                               *)
(*                    http://reactiveML.org                           *)
(*                    http://rml.inria.fr                             *)
(*                                                                    *)
(*                          Louis Mandel                              *)
(*                                                                    *)
(*  Copyright 2002, 2012 Louis Mandel.  All rights reserved.          *)
(*  This file is distributed under the terms of the GNU Library       *)
(*  General Public License, with the special exception on linking     *)
(*  described in file ../LICENSE.                                     *)
(*                                                                    *)
(*  ReactiveML has been done in the following labs:                   *)
(*  - theme SPI, Laboratoire d'Informatique de Paris 6 (2002-2005)    *)
(*  - Verimag, CNRS Grenoble (2005-2006)                              *)
(*  - projet Moscova, INRIA Rocquencourt (2006-2007)                  *)
(*                                                                    *)
(**********************************************************************)

let l = Mutex.create ()
let h = Hashtbl.create 97

let with_lock f = begin
  Mutex.lock l;
  f ();
  Mutex.unlock l
end

let save id fnu = with_lock (fun () ->
  Hashtbl.add h id fnu
)

let release () =
  with_lock (fun () ->
    Hashtbl.iter (fun _ fnu -> fnu ()) h;
    Hashtbl.clear h
  )

let async send func () =
  let tid = Thread.id (Thread.self ()) in
  let result = func () in
  save tid (send result)

let run send func =
  let _ (* tid *) = Thread.create (async send func) () in
  ()
