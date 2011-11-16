module Caml_compat

open System
open Microsoft.FSharp.Compatibility.OCaml

module Pervasives = 
  let (&&) = (&&)
  let (&) = (&&)
  let (or) = (||)

module Random = 
  let seed = new Random(0)
  let self_init () = ()
  let float f =
    seed.NextDouble() * f

let time f =
  let s = new System.Diagnostics.Stopwatch ()
  s.Start ()
  ignore (f ())
  s.Stop ()
  System.Console.WriteLine s.ElapsedMilliseconds