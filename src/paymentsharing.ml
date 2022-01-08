open Graph 
open Tools
open Gfile
open Fordfulk
open Printf

type outcomeNode = Graph.id;;
type incomeNode = Graph.id;;

let outcomeNode = -1;; (* Notre noeud reliant les personnes n'ayant pas payé assez *)
let incomeNode = -2;; (* Notre noeud reliant les personnes qui ont trop payé *)
let infinity = 100000;; (* valeur de base pour les arcs entre chaque payeurs du graphe *)



(* Permet d'interpréter une ligne du fichier de données et de créer un noeud pour chaque payeur dans le graph *) 
let readPayer graph line =  
  Scanf.sscanf line "payer %d %d" (fun payer amount -> new_node graph payer)

(* Permet d'interpréter une ligne du ficher comme étant un commentaire *)
let readComment graph line =
  Scanf.sscanf line "%%" graph

(* Permet de lire un fichier de données et de créer un graphe associé avec la fonction readPayer *)
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


(* Permet d'interpréter une ligne de notre fichier de données et ajouter chaque PayerId à une liste *)
let addPayerId list line =
  try 
    Scanf.sscanf line "payer %d %d" (fun id amount -> List.append list [id]) 
  with e ->
    Printf.printf "Impossible to read student in line : \n%s\n" line;
    list;;

(* Permet d'interpréter une ligne de notre fichier de données et ajouter chaque montant payé par payeurs à une liste *)
let addAmountPayer list line =
  try 
    Scanf.sscanf line "payer %d %d" (fun id amount -> List.append list [amount]) 
  with e ->
    Printf.printf "Impossible to read student in line : \n%s\n" line;
    list;;

(* Permet d'obtenir la liste des PayerId à partir d'un fichier de données *)
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

(* Permet d'obtenir la liste du montant payé par chaque payeurs à partir d'un fichier de données *)
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

(* Permet d'obtenir tout les tuples possibles entre chaque éléments d'une liste de noeuds  *)
let allTuplePossible nodeList = 
  let res = 
    List.fold_left (fun acc x -> List.fold_left (fun acc y -> (x,y)::acc)acc nodeList) [] nodeList in 
  List.rev res

(* Permet de créer tout les arcs entre les noeuds de payeurs d'un graphe avec le label infinity *)
let rec setupArcBetweenPayersRec graph nodeListTuple = 
  match nodeListTuple with
  |[] -> graph
  |(x1,x2)::rest when x1 != x2 -> setupArcBetweenPayersRec (new_arc graph x1 x2 infinity) rest 
  |(x1,x2)::rest -> setupArcBetweenPayersRec graph rest

(* Permet d'obtenir la différence entre ce que chaque payeur à payé et ce qu'il devait normalement payé *)
let diff supposedPaymentAmount listPaydAmount = List.map (fun x -> x-supposedPaymentAmount) listPaydAmount

(* Permet de créer les arcs entre un payeur et un noeud incomeNode ou outcomeNode en fonction du signe de la différence de ce qu'il a payé et ce qu'il devait payé *)
let rec graphSetArcsWithSS graph diffList accu = match diffList with
  |[] -> graph
  |x1::rest when x1 > 0 -> graphSetArcsWithSS (new_arc graph (accu+1) incomeNode x1) rest (accu+1)
  |x1::rest when x1 <= 0 -> graphSetArcsWithSS (new_arc graph outcomeNode (accu+1) (-x1)) rest (accu+1)
  |_::_ -> assert false


(** APPEL FONCTION *)
(* Permet de créer le graph final en appliquant fordfulkerson avec utilisation des fonctions précédentes *)
let createAllGraph file = 
  let nodeGraph = readFile file in

  let totalAmountList = readAmount file in 

  let nodeIdList = getListOfIdPayers file in 

  let listeTuple = allTuplePossible nodeIdList in

  let nodeGraphWithArcs = setupArcBetweenPayersRec nodeGraph listeTuple in

  let totalAmount = List.fold_left (fun accu x -> accu+x) 0 totalAmountList in

  let supposedPayment = totalAmount / (List.length nodeIdList) in

  let listPaymentDiff = diff supposedPayment totalAmountList in

  let graphWithSourceAndSink = (new_node (new_node nodeGraphWithArcs incomeNode) outcomeNode) in

  let graphUpdated = graphSetArcsWithSS graphWithSourceAndSink listPaymentDiff 0 in 

  let graphFinal = fordfulk graphUpdated outcomeNode incomeNode in graphFinal;



