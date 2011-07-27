exception Fallback

(** Is used to stop the pass at this level *)
let stop _ acc x = x, acc
