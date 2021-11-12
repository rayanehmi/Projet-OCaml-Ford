open Graph

(* assert false is of type ∀α.α, so the type-checker is happy. *)
let clone_nodes gr = assert false
let gmap gr f = assert false

let clone_nodes gr = List.map (fun f (a,b) -> a) gr;;
