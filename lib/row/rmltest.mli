type 'a behaviour =
    O of 'a
  | P of 'a behaviour list
  | S of 'a behaviour * 'a behaviour
  | N
