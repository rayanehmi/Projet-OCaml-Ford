open Graph 
open Tools
open Gfile
open Fordfulk
open Printf

let outcomeNode = (-1);; (* Our outcomeNode *)
let incomeNode = (-2);; (* Our incomeNode *)
let infinity = 100000;; (* Infinity label for arc between payers *)



(* Read a Payer line*) 
let readPayer graph line =  
  Scanf.sscanf line "payer %d %d" (fun payer amount -> new_node graph payer)

(* Read a comment line*)
let readComment graph line =
  Scanf.sscanf line "%%" graph

(* Read a given file and create the graph assiociate *)
let readFile file = 
  let open_file = open_in file in

  let partialGraph = empty_graph in

  let rec loop graph = 
    try 
      let line = input_line open_file in

      let line = String.trim line in

      let graphUpdate = 
        if line = "" then graph
        else match line.[0] with
          |'p' -> readPayer graph line
          |_ -> readComment graph line
      in loop graphUpdate
    with End_of_file -> graph
  in let graphResult = loop partialGraph in close_in open_file; 
  graphResult



let addPayerId list line =
  try 
    Scanf.sscanf line "payer %d %d" (fun id amount -> List.append list [id]) 
  with e ->
    Printf.printf "Impossible to read student in line : \n%s\n" line;
    list;;

let addAmountPayer list line =
  try 
    Scanf.sscanf line "payer %d %d" (fun id amount -> List.append list [amount]) 
  with e ->
    Printf.printf "Impossible to read student in line : \n%s\n" line;
    list;;

let getListOfIdPayers file =
  (* Open the file *)
  let open_file = open_in file in 

  (* Initialize the list *)
  let partialList = [] in 

  (* Read all lines until end of file *)
  let rec loop list =
    try

      (* Read a line *)
      let line = input_line open_file in 

      (* Remove spaces from line*)
      let line = String.trim line in

      let listUpdate = 

        (* Ignore empty lines *)
        if line = "" then list

        (* Match the first character of the line *)
        else match line.[0] with 
          | 'p' -> addPayerId list line
          | _ -> list
      in
      loop listUpdate

    with End_of_file -> list 

  in

  let result = loop partialList in
  close_in open_file ;
  result

let readAmount file = 
  let open_file = open_in file in

  let amountList = [] in

  let rec loop accu = 
    try 
      let line = input_line open_file in

      let line = String.trim line in

      let amountUpdate = 
        if line = "" then accu
        else match line.[0] with
          |'p' -> (addAmountPayer accu line)
          |_ -> accu
      in loop amountUpdate
    with End_of_file -> accu
  in let amountResultList = loop amountList in close_in open_file; 
  amountResultList

(* Create arcs between all nodes of the partialGraph *)
let rec setupArcBetweenPayers graph nodelist =
  match nodelist with
  |[] -> graph
  |x1::[] -> graph
  |x1::x2::rest -> setupArcBetweenPayers (new_arc graph x1 x2 infinity) (x2::rest);;

let rec setupArcBetweenPayersReturn graph nodelist = 
  match nodelist with
  |[] -> graph
  |x1::[] -> graph
  |x1::x2::rest -> setupArcBetweenPayers (new_arc graph x2 x1 infinity) (x2::rest);;

let nodeGraph = readFile "paymentfile.txt"
let totalAmountList = readAmount "paymentfile.txt"
let nodeIdList = getListOfIdPayers "paymentfile.txt"
let nodeGraphWithArcs = setupArcBetweenPayers nodeGraph nodeIdList
let nodeGraphWithAllArcs = setupArcBetweenPayersReturn nodeGraphWithArcs nodeIdList


let totalAmount = List.fold_left (fun accu x -> accu+x) 0 totalAmountList

let supposedPayment = totalAmount / (List.length nodeIdList)

(** 20 [40;10;10] *)
let diff supposedPaymentAmount listPaydAmount = List.map (fun x -> x-supposedPaymentAmount) listPaydAmount