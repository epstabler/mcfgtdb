open Str  (* for parsing the input *)
open Mcfgtdb (* the recognizer *)
open G0

let time f x =
    let t = Sys.time() in
    let fx = f x in
    ( Printf.printf "time: %fs\n" (Sys.time() -. t); flush stdout ; fx; )

let rec loop0 f exps min0 iq0 =  (* get input and set up mutable parts of initial dq *)
  print_string ("\n: ");
  let linestring = read_line() in
  let input = split (regexp "[\ \t]+") linestring in
    List.iter print_string input;
    if List.length input > 0 && List.hd input = "q"
    then ()
    else
      let dq = (DQ.add (input,iq0,1.0) DQ.empty) in
      let accepted = time (derive f exps min0) dq in
	Printf.fprintf stdout "accepted: %b" accepted;
	loop0 f exps min0 iq0;;

let go g f min0 =
  let exps = ingExpansions g in
  let init  = IQ.add (0,[[]]) IQ.empty in
    loop0 f exps min0 init 

let _ = go g0 g0Cat 0.00000001
