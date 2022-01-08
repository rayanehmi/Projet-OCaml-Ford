open Graph


type outcomeNode = Graph.id;;
type incomeNode = Graph.id;;

val readPayer : 'a Graph.graph -> string -> 'a Graph.graph

val readComment : 'a -> string -> 'a

val readFile : string -> 'a Graph.graph

val addPayerId : int list -> string -> int list

val addAmountPayer : int list -> string -> int list

val getListOfIdPayers : string -> int list

val readAmount : string -> int list

(*val setupArcBetweenPayers : 'a -> 'b list -> ('b * 'b) list*)

val setupArcBetweenPayersRec : int Graph.graph -> (Graph.id * Graph.id) list -> int Graph.graph

(*val setupArcBetweenPayersReturn : int Graph.graph -> Graph.id list -> int Graph.graph*)

val diff : int -> int list -> int list

val graphSetArcsWithSS : int Graph.graph -> int list -> int -> int Graph.graph

val createAllGraph : string -> int Graph.graph