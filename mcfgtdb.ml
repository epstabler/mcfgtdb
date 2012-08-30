(*  file: td-mcfg.ml  implementing Stabler 2011 "Top-down recognizers for MCFGs and MGs"
 creator: E Stabler, stabler@ucla.edu
 creation date: 2011-05-12
       updated: 2011-07-06
 purpose: top-down beam parser for MCFG
*)

open Makeq

type cat = int
type imap = (int * int) list    
(* to specify string functions, an imap element (k,i) in a string component on the rhs of a rule
   indicates that the string is the i'th element of the k'th string component of the lhs *)
type nrhs = (cat * imap) list
type irhs = IT of string | IE | IN of nrhs
type iproduction = cat * irhs
type iprule = iproduction * float
type ing = iprule list          (* "internal" grammar *)
type expansion = irhs * float
type expansions = expansion list
type indices = int list list
type iCat = cat * indices       (* the type of predicted categories *)
type input = string list

(*** optional print functions *)
let printIlist out = List.iter (Printf.fprintf out "%i")

let printImap out = List.iter (fun (k,i) -> 
  if i<0 then Printf.fprintf out "(%i,)" k else Printf.fprintf out "(%i,%i)" k i)

let printInrhs out f =  List.iter (fun (cat,imap) -> 
  (Printf.fprintf out " %s" (f cat);printImap out imap))

let printIRHS out f = function 
| IE -> Printf.fprintf out "\"\""
| IT s -> Printf.fprintf out "\"%s\"" s
| IN nrhs -> printInrhs out f nrhs

let printIPRule out f ((lhs,irhs),p) =
  Printf.fprintf out "%f: %s --> " p (f lhs);
  printIRHS out f irhs

let printING f (g:ing) =
   List.iter (fun x -> (printIPRule stdout f x; Printf.fprintf stdout "\n")) g
(*** END optional print functions ****)
let least = let rec least0 sofar = function [] -> sofar | x::xs -> least0 (min sofar x) xs
  in function [] -> failwith "Error: least" | x::xs -> least0 x xs

module ICatPoset = (* order by comparing least index, an int list *)
  struct
    type t = iCat
    let le = fun ((_,i1):iCat) -> fun ((_,i2):iCat) -> compare (least i1) (least i2) < 1
  end

module IQ = Makeq (ICatPoset)    (* for the queues of predictions *)

type der = input * IQ.t * float  (* the type of partial derivations *)

module DerivationPoset = (* order by (decreasing) probability, a float *)
  struct
    type t = der
    let le = fun ((_,_,p1):der) -> fun ((_,_,p2):der) -> compare p2 p1 < 1
  end

module DQ = Makeq (DerivationPoset)  (* for the queue of (partial) derivations *)

(*** optional print functions *)
let ilist out = fun x -> 
      Printf.fprintf out "(";
      List.iter (Printf.fprintf out "%i") x;
      Printf.fprintf out ")"

let rec printIQ out f = fun iq0 ->
if IQ.is_empty iq0 then ()
else let ((c,indices),iq1) = IQ.extract_min iq0 in 
       Printf.fprintf out "%s" (f c);
       List.iter (ilist out) indices; Printf.fprintf out " ";
       printIQ out f iq1

let rec printDQ out f = fun dq0 ->
if DQ.is_empty dq0 then ()
else let ((input,iq,p),dq1) = DQ.extract_min dq0 in 
       Printf.fprintf out "%f" p; Printf.fprintf out ":";
       List.iter (Printf.fprintf out "%s") input; Printf.fprintf out ":";
       printIQ out f iq; Printf.fprintf out "\n";
       printDQ out f dq1
(*** END optional print functions *)

(* we assume categories numbered sequentially up from 0 *)
let rec numberOfCats = function 
  | [] -> 0
  | ((n0, _),_)::more -> max n0 (numberOfCats more)

(* define expansions array from mcfg rules *)
let ingExpansions : ing -> expansions array = fun g -> 
  let a = Array.make ((numberOfCats g)+1) [] in
    List.iter (fun ((c, rhs), p) -> (a.(c) <- (rhs, p)::a.(c))) g; a

(* OK, now the indexing for linear order -- as in Stabler'11 *)
let rec extendIndices : indices -> imap -> indices =  fun i0 -> function
  | [] -> []
  | (k,x)::more when x<0 -> (List.nth i0 k)::extendIndices i0 more
  | (k,x)::more -> ((List.nth i0 k)@[x])::extendIndices i0 more

let rec insertPredictions : indices -> IQ.t -> nrhs -> IQ.t = fun i0 -> fun iq -> function
  | [] -> iq
  | (cat,ill)::more ->
    insertPredictions i0 (IQ.add (cat, extendIndices i0 ill) iq) more

(* pruning and trimming functions *)
exception InitialIndexDiffers
let rec trimIndices: int -> indices -> indices = fun i -> function
  | (x::xs)::more -> if x=i then xs::trimIndices i more else raise InitialIndexDiffers
  | []::_ -> raise InitialIndexDiffers
  | [] -> []

let rec trimmedElements : int -> IQ.t -> iCat list = fun i -> fun iq0 -> 
  if IQ.is_empty iq0
  then []
  else let ((c,ixs),iq) = IQ.extract_min iq0 in (c,trimIndices i ixs)::trimmedElements i iq

(* we might want to turn this off (i.e. remove call from prunedInsert)..
   certainly the pruning slows performance in many cases *)
let trimIQ : IQ.t -> IQ.t = fun iq0 -> 
  if IQ.is_empty iq0
  then iq0
  else let ((c,ixs),iq1) = IQ.extract_min iq0 in 
         if ixs=[]
         then iq0
         else let ix = List.hd ixs in
                if ix = []
                then iq0
                else let i = List.hd ix in (* trim i from every index, else no change on error *)
                       try List.fold_right IQ.add (trimmedElements i iq0) IQ.empty
                       with InitialIndexDiffers -> iq0

let prunedInsert : DQ.t -> float -> der -> DQ.t = (* prune derivation if newP not above bound *)
   fun dq -> fun pb -> fun (s,iq,newP) -> 
     if newP > pb then DQ.add (s,trimIQ iq,newP) dq else dq

(* recognizer core function *)
let extendDerivation : DQ.t -> float -> indices -> input -> IQ.t -> float -> expansion -> DQ.t =
  fun dq -> fun pb -> fun i0 -> fun input -> fun iq -> fun p0 -> function
    | IT s, p -> (match input with
        | head::tail -> if s=head then prunedInsert dq pb (tail, iq, p0 *. p) else dq (* scan *)
        | [] -> dq)
    | IE, p -> prunedInsert dq pb (input, iq, p0 *. p)                  (* scan empty element *)
    | IN nrhs, p -> prunedInsert dq pb (input, insertPredictions i0 iq nrhs, p0 *. p) (* expand *)

let rec derive : (int -> string) -> expansions array -> float -> DQ.t -> bool =
  fun f exps pb dq0 -> 
  if (DQ.is_empty dq0)
  then false
  else 
    let ((in0,iq0,p0),dq1) = DQ.extract_min dq0 in
      Printf.fprintf stdout "--\n"; printDQ stdout f dq0; (* uncomment for trace! *)
      if IQ.is_empty iq0 && in0=[] then true
      else if IQ.is_empty iq0
      then derive f exps pb dq1
      else
        let ((cat,i0),iq1) = IQ.extract_min iq0 in
          derive f exps pb (List.fold_left
                         (fun dq x -> extendDerivation dq pb i0 in0 iq1 p0 x)
                         dq1
                         exps.(cat))

(* create the initial derivation queue and parse -- 
  minbound is for our first simple pruning rule: analyses with p < this bound are discarded *)
let recognize g f minbound input = 
  let exps = ingExpansions g in
  let init  = IQ.add (0,[[]]) IQ.empty in
  let prob = 1. in
  let dq = (DQ.add (input,init,prob) DQ.empty) in
    derive f exps minbound dq 
(*    time (derive f exps minbound) dq *)

(* examples
OK: 
  recognize g0 g0Cat 0.0000001 ["a";"b";"c";"d"];;
NO:
  recognize g0 g0Cat 0.5 ["a";"b";"c";"d"];;
OK: this example is used in Stabler'11
  recognize g0 g0Cat 0.0000001 ["a";"b";"b";"c";"d";"d"];;
NO:
  recognize g0 g0Cat 0.0000001 ["a";"a";"b";"c";"d"];;
*)
