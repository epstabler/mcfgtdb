(*  file: albro2beam.ml
 creator: E Stabler, stabler@ucla.edu
 updated: 2011-07-06
 purpose: convert grammar with Albro-style map function into our format.
*)
(* types for the external grammar notation, int version *)
type rhs = T of string | E | N of cat list * (int*int) list list;;
type production = cat * rhs;;
type prule = production * float;;
type exg = prule list;;

(*** begin EXAMPLE **)
let (g1Cat: int->string) = function (* maps g1 categories to strings, for printing *)
| 0 -> "S" | 1 -> "AC" | 2 -> "BD" | 3 -> "A" | 4 -> "B" | 5 -> "C" | 6 -> "D"
| _ -> failwith "error:g1Cat";;

let (xg1:exg) = [
(0, E), 0.5;
(0, N ([1; 2], [[0,0;1,0;0,1;1,1]])), 0.5;
(1, N ([3; 5; 1], [[0,0;2,0];[1,0;2,1]])), 0.5;
(1, N ([3; 5], [[0,0];[1,0]])), 0.5;
(2, N ([4; 6; 2], [[0,0;2,0];[1,0;2,1]])), 0.5;
(2, N ([4; 6], [[0,0];[1,0]])), 0.5;
(3, T "a"), 1.;
(4, T "b"), 1.;
(5, T "c"), 1.;
(6, T "d"), 1.;
];;
(*** end EXAMPLE -- with example here, we can put print trace function into def of derive *)

let rec jkp cat k p = function
| ((c,j)::[])::more when cat=c && p=0 -> (j,k,-1)::jkp cat k (p+1) more
| ((c,j)::cis)::more when cat=c -> (j,k,p)::jkp cat k (p+1) (cis::more)
| (_::cis)::more -> jkp cat k (p+1) (cis::more)
| []::more -> jkp cat (k+1) 0 more
| [] -> [];;

(* example: jkp 2 0 0 [[0,0;2,0];[1,0;2,1]];; *)

(* sort the jkp list to get the components of cat in consecutive order,
 and then delete the component numbers j *)
let kp cat iill = 
List.map (fun (_,x,y) -> (x,y)) (List.sort compare (jkp cat 0 0 iill));;

(* examples, contrast:  kp 2 [[0,0;2,0];[1,0;2,1]];; 
                      kp 2 [[0,0;2,1];[1,0;2,0]];; *)

let rec nrhs2in i iill = function
| [] -> []
| cat::cats -> (cat,(kp i iill))::nrhs2in (i+1) iill cats;;

let rhs2in = function
| T s -> IT s
| E -> IE
| N (cats,iill) -> IN (nrhs2in 0 iill cats);;

let prule2in ((lhs,rhs),p) = match rhs with
| T s -> ((lhs,IT s),p)
| E -> ((lhs,IE),p)
| N (cats,iill) -> ((lhs,IN (nrhs2in 0 iill cats)),p);;

(* prule2in ((2, N ([4; 6; 2], [[0,0;2,0];[1,0;2,1]])), 0.5);; *)

let g2in = List.map prule2in;;

let printIIe out (i1,i2) = Printf.fprintf out "%i,%i;" i1 i2;;
let printL elementPrinter out list =
Printf.fprintf out "[";
List.iter (elementPrinter out) list;
Printf.fprintf out "]";;
let printIILe out = printL printIIe out;;
let printIILL out = printL printIILe out;;
let printRHS out f = function 
| E -> Printf.fprintf out "\"\""
| T s -> Printf.fprintf out "\"%s\"" s
| N (cats,iill) -> List.iter (fun x -> Printf.fprintf out "%s " (f x)) cats; printIILL out iill;;
let printPRule out f ((lhs,rhs),p) =
  Printf.fprintf out "%f: %s --> " p (f lhs);
  printRHS out f rhs;;
let printEXG f (g:exg) =
List.iter (fun x -> (printPRule stdout f x; Printf.fprintf stdout "\n")) g;;

(*
printEXG g1Cat xg1;;
printING g1Cat (g2in xg1);;
*)
