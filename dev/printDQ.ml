(*  file: printDQ.ml
 creator: E Stabler, stabler@ucla.edu
 updated: 2011-07-06
*)

let ilist out = fun x -> 
      Printf.fprintf out "(";
      List.iter (Printf.fprintf out "%i") x;
      Printf.fprintf out ")";;

let rec printIQ out f = fun iq0 ->
if IQ.is_empty iq0 then ()
else let ((c,indices),iq1) = IQ.extract_min iq0 in 
       Printf.fprintf out "%s" (f c);
       List.iter (ilist out) indices; Printf.fprintf out " ";
       printIQ out f iq1;;

let rec printDQ out f = fun dq0 ->
if DQ.is_empty dq0 then ()
else let ((input,iq,p),dq1) = DQ.extract_min dq0 in 
       Printf.fprintf out "%f" p; Printf.fprintf out ":";
       List.iter (Printf.fprintf out "%s") input; Printf.fprintf out ":";
       printIQ out f iq; Printf.fprintf out "\n";
       printDQ out f dq1;;
