open Utils

module QuadMap = Map.Make (struct
  type t = Utils.quad
  let compare = compare
end)

type 'a t = { q_map : 'a QuadMap.t; q_rect : Utils.rect }
type key = Utils.quad

let empty r =
  { q_map = QuadMap.empty; q_rect = r }

let add k v m =
  { m with q_map = QuadMap.add k v m.q_map }

let find k m =
  QuadMap.find k m.q_map

let find_by_pos pos m =
  QuadMap.find (quad_from_pos m.q_rect pos) m.q_map
