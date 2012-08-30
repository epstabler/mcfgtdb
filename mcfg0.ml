(* g0.ml Example grammar
   It would be easy to automate the numbering of categories,
   but for now we just define the grammar this way
*)

open Mcfgtdb (* for definition of grammar type *)

let (g0Cat: int->string) = function
  | 0 -> "S" | 1 -> "AC" | 2 -> "BD" | 3 -> "A" | 4 -> "B" | 5 -> "C" | 6 -> "D"
  | _ -> failwith "Error: g0Cat"

let (g0:ing) = [
  ((0, IN [(1, [(0, 0); (0, 2)]); (2, [(0, 1); (0, 3)])]), 1.);
  ((1, IN [(3, [(0, 0)]); (5, [(1, 0)]); (1, [(0, 1); (1, 1)])]), 0.5);
  ((1, IN [(3, [(0, -1)]); (5, [(1, -1)])]), 0.5);
  ((2, IN [(4, [(0, 0)]); (6, [(1, 0)]); (2, [(0, 1); (1, 1)])]), 0.5);
  ((2, IN [(4, [(0, -1)]); (6, [(1, -1)])]), 0.5);
  ((3, IT "a"), 1.);
  ((4, IT "b"), 1.);
  ((5, IT "c"), 1.);
  ((6, IT "d"), 1.)
]
