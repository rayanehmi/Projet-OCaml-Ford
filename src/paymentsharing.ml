open Graph 
open Tools
open Gfile
open Fordfulk
open Printf

type outcomeNode = Graph.id;;
type incomeNode = Graph.id;;

let outcomeNode = -1;; (* Our outcomeNode *)
let incomeNode = -2;; (* Our incomeNode *)
let infinity = 100000;; (* Infinity label for arc between payers *)



(* Read a Payer line*) 
let readPayer graph line =  
  Scanf.sscanf line "payer %d %d" (fun payer amount -> new_node graph payer)

(* Read a comment line*)
let readComment graph line =
  Scanf.sscanf line "%%" graph

(* Read a given file and create the graph assiociate *)
let readFile file = 
  Printf.printf "je lis le fichier %s" file;
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

let setupArcBetweenPayers graph nodeList = 
  let res = 
    List.fold_left (fun acc x -> List.fold_left (fun acc y -> (x,y)::acc)acc nodeList) [] nodeList in 
  List.rev res

let rec setupArcBetweenPayersRec graph nodeListTuple = 
  match nodeListTuple with
  |[] -> graph
  |(x1,x2)::rest when x1 != x2 -> setupArcBetweenPayersRec (new_arc graph x1 x2 infinity) rest 
  |(x1,x2)::rest -> setupArcBetweenPayersRec graph rest


let diff supposedPaymentAmount listPaydAmount = List.map (fun x -> x-supposedPaymentAmount) listPaydAmount


let rec graphSetArcsWithSS graph diffList accu = match diffList with
  |[] -> graph
  |x1::rest when x1 > 0 -> graphSetArcsWithSS (new_arc graph (accu+1) incomeNode x1) rest (accu+1)
  |x1::rest when x1 <= 0 -> graphSetArcsWithSS (new_arc graph outcomeNode (accu+1) (-x1)) rest (accu+1)


(** APPEL FONCTION *)

let createAllGraph file = 
  let nodeGraph = readFile file in

  let totalAmountList = readAmount file in 

  let nodeIdList = getListOfIdPayers file in 

  let listeTuple = setupArcBetweenPayers nodeGraph nodeIdList in

  let nodeGraphWithArcs = setupArcBetweenPayersRec nodeGraph listeTuple in

  let totalAmount = List.fold_left (fun accu x -> accu+x) 0 totalAmountList in

  let supposedPayment = totalAmount / (List.length nodeIdList) in

  let listPaymentDiff = diff supposedPayment totalAmountList in

  let graphWithSourceAndSink = (new_node (new_node nodeGraphWithArcs incomeNode) outcomeNode) in

  let graphUpdated = graphSetArcsWithSS graphWithSourceAndSink listPaymentDiff 0 in 

  let graphFinal = fordfulk graphUpdated outcomeNode incomeNode in graphFinal;



