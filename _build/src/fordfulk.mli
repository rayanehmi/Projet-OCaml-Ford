open Graph
open Printf

val graph_ecart : 'a Graph.graph -> int Graph.graph

val minLabel : 'a Graph.graph -> Graph.id list -> 'a option -> 'a option

val getLabelPath : 'a Graph.graph -> Graph.id list -> 'a list -> 'a list

val addFlowToPath : int Graph.graph -> Graph.id list -> int -> int Graph.graph

val addFlowToPathReturn : int Graph.graph -> Graph.id list -> int -> int Graph.graph

val fordfulkCore : int Graph.graph -> Graph.id -> Graph.id -> int Graph.graph

val fordfulk : int Graph.graph -> Graph.id -> Graph.id -> int Graph.graph


