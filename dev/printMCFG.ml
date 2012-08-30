(*  file: printMCFG.ml
 creator: E Stabler, stabler@ucla.edu
 updated: 2011-07-06
*)
let printIlist out = List.iter (Printf.fprintf out "%i");;
let printImap out = List.iter (fun (k,i) -> 
  if i<0 then Printf.fprintf out "(%i,)" k else Printf.fprintf out "(%i,%i)" k i);;
let printInrhs out f =  List.iter (fun (cat,imap) -> 
  (Printf.fprintf out " %s" (f cat);printImap out imap));;
let printIRHS out f = function 
| IE -> Printf.fprintf out "\"\""
| IT s -> Printf.fprintf out "\"%s\"" s
| IN nrhs -> printInrhs out f nrhs;;
let printIPRule out f ((lhs,irhs),p) =
  Printf.fprintf out "%f: %s --> " p (f lhs);
  printIRHS out f irhs;;
let printING f (g:ing) =
List.iter (fun x -> (printIPRule stdout f x; Printf.fprintf stdout "\n")) g;;
