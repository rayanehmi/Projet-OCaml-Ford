open Gfile
open Tools
open Graph
open Findpath
open Fordfulk
open Printf
open Option
open Paymentsharing



let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0      ()
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)

  (* These command-line arguments are not used for the moment. *)
  and source = int_of_string Sys.argv.(2)
  and sink = int_of_string Sys.argv.(3)
  in


  (*
   *            ###########################
   *            ###### FORDFULKERSON ######
   *            ###########################
   *)

  (*  Permet de lire le fichier infile *)
  let graph = from_file infile in

  (* Transforme le graph de string vers int pour manipulation *)
  let graph = gmap graph (fun x -> int_of_string x) in

  (* Applique l'algorithme de fordfulkerson sur le graph entre les noeuds source et puits défini *)
  let updateGraph = fordfulk graph source sink in

  (* Transforme le graph de int vers string pour lecture des résultats *)
  let updateGraphStringMapped = gmap updateGraph (fun x -> string_of_int x) in

  (* Affichage du graphe *)
  let () = export outfile updateGraphStringMapped in




  (*
   *            ###########################
   *            ##### PAYMENT_SHARING #####
   *            ###########################
   *)
(*
  let graphFinal = createAllGraph infile in

  let updateGraphStringMapped = gmap graphFinal (fun x -> string_of_int x) in

  let () = export outfile updateGraphStringMapped in

  (**
   *            https://graphviz.org/Gallery/directed/fsm.html
  *)
*)
  ()

