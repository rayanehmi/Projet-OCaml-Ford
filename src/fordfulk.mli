open Graph
open Printf

val graph_ecart : 'a Graph.graph -> int Graph.graph

val minLabel : 'a Graph.graph -> Graph.id list -> 'a option -> 'a option

val add_path : int Graph.graph -> Graph.id list -> int -> int Graph.graph

val fordfulk : int Graph.graph -> Graph.id -> Graph.id -> int Graph.graph
