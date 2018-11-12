let array_rev arr =
  if (Array.length arr = 1 || Array.length arr = 0) then arr
  else
    let u = Array.length (arr) in
    for i=0 to (u/2)-1 do
    let temp = arr.(i) in
    arr.(i) <- arr.(u-i-1);
    arr.(u-i-1) <- temp;
    done;
    arr
;;
(* val array_rev : 'a array -> unit

   Reverses the given array in place. Uses iteration and mutation to
   do so efficiently. DOES NOT generate any internal copies of the
   parameter array.

   REPL EXAMPLES:
   # let a1 = [|1; 2; 3;|];;
   val a1 : int array = [|1; 2; 3|]
   # array_rev a1;;
   - : unit = ()
   # a1;;
   - : int array = [|3; 2; 1|]
   # let a2 = [|"a"; "b"; "c"; "d"; "e"; "f"|];;
   val a2 : string array = [|"a"; "b"; "c"; "d"; "e"; "f"|]
   # array_rev a2;;
   - : unit = ()
   # a2;;
   - : string array = [|"f"; "e"; "d"; "c"; "b"; "a"|]
   # let a3 = [|true; true; false; false; true;|];;
   val a3 : bool array = [|true; true; false; false; true|]
   # array_rev a3;;
   - : unit = ()
   # a3;;
   - : bool array = [|true; false; false; true; true|]
*)

let list_rev lst =
  if (lst = []) then lst
  else
    let out_list = [List.hd (lst)] in
    let tail = List.tl lst in
    let rec reverse_list out_list tail = (*helper*)
      if (tail=[]) then out_list
      else
        let hed = List.hd tail in
        let out_list = hed :: out_list in
        reverse_list (out_list) (List.tl tail)
        in
      reverse_list out_list tail
;;
