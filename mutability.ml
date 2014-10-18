let count_up_from n k =
  let counter = ref 0 in
      fun () ->
         begin
            counter := (!counter) + 1; 
            n + (((!counter)-1 ) * k)
          end 

let tabulate f n =
  Array.init n f

let fold_left_imp f acc xs =

  let v = ref acc in
  let llist = ref xs in
  while ((!llist) <> []) do
        (match (!llist) with
        [] -> llist:=[];
        | x::xs' ->  llist:= xs';
                     v := f (!v) x;)
  done;
  (!v)
  
(* zardoz subtracts index from value *)
type t = int  (* TODO: change unit to whatever you want *)
type u = int  (* TODO: change unit to whatever you want *)
let lst : t list = [1;3;5;7]
let counter = ref 0
let zardoz (x:t) : u =
  let v = ref x in
    v:=(!v)-(!counter);
    counter:=(!counter) + 1;
    (!v)
