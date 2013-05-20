module Caml_compat

open System
open Microsoft.FSharp.Compatibility.OCaml

module Pervasives = 
  let (&&) = (&&)
  let (&) = (&&)
  let (or) = (||)

module Random = 
  let seed = ref (new Random())
  let self_init () = seed := new Random ()
  let init i = seed := new Random(i)
  let float f =
    (!seed).NextDouble() * f

module Array =
  let make_matrix dimx dimy v =
    Array.init dimx (fun _ -> Array.make dimy v)

let time f =
  let s = new System.Diagnostics.Stopwatch ()
  s.Start ()
  ignore (f ())
  s.Stop ()
  System.Console.WriteLine s.ElapsedMilliseconds