
module type S = sig
  type key
  type 'a t

  val empty : 'a t

  val add : key -> 'a -> 'a t -> 'a t
  val find : key -> 'a t -> 'a
  val clean : 'a t -> 'a t
end


module Make (Ord : Map.OrderedType) = struct
  module MyMap = Map.Make(Ord)

  type key = Ord.t
  type 'a t = ('a Weak.t) MyMap.t

  let empty = MyMap.empty

  let add key v m =
    let ptr = Weak.create 1 in
      Weak.set ptr 0 (Some v);
      MyMap.add key ptr m

  let find key m =
    match Weak.get (MyMap.find key m) 0 with
      | None -> raise Not_found
      | Some v -> v

  let clean m =
    let alive_m, _ = MyMap.partition (fun _ ptr -> Weak.check ptr 0) m in
      alive_m
end
