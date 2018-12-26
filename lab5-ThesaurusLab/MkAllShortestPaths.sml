functor MkAllShortestPaths (Table : TABLE) : ALL_SHORTEST_PATHS =
struct
  open Table
  open Seq

  (* Table.key defines our vertex type *)
  type vertex = key
  type edge = vertex * vertex

  exception NYI
  (* You must define the following two types and
   * explain your decision here with comments.
   *)
  type graph = vertex seq table
  type asp = vertex seq table

  (* Task 2.1 *)
  fun makeGraph (E : edge seq) : graph =
    Table.collect E

  (* Task 2.2 *)
  fun numEdges (G : graph) : int =
    let val new_table = Table.map Seq.length G
        fun sum (x, y) = x + y
    in (Table.reduce sum 0) new_table
    end

  fun numVertices (G : graph) : int =    (* W = O(m+n) S = O(log(n)) *)
    let val key_set = domain G
        val value_seq = range G
        val new_value_seq = Seq.flatten value_seq
        val value_set = Set.fromSeq new_value_seq
        val vertices = Set.union (key_set, value_set)
    in length (Set.toSeq vertices)
    end

  (* Task 2.3 *)
  fun outNeighbors (G : graph) (v : vertex) : vertex seq =
    case find G v of
      NONE => Seq.empty()
    | SOME(seq) => seq

  (* Task 2.4 *)
  fun makeASP (G : graph) (v : vertex) : asp =
    case (Table.find G v) of
      NONE => Table.empty()
    | SOME _ => let fun mkASP_if_not_none (visited, frontier, ASP) =
                    if Set.size frontier = 0 then ASP
                    else let val frontier_seq = Set.toSeq(frontier)
                            val out_neighbors = Seq.map (outNeighbors G) (frontier_seq)
                            val out_neighbors = Seq.map (fn seq => Set.toSeq(Set.difference(Set.fromSeq seq, visited))) out_neighbors 
                            val back_neighbors = Seq.flatten (Seq.tabulate (fn i => (Seq.map (fn v => (v, nth frontier_seq i)) (nth out_neighbors i))) (Seq.length frontier_seq))
                            val out_neighbors = Set.fromSeq (Seq.flatten(out_neighbors))
                            val back_edges = Table.collect (back_neighbors)
                            val new_visited = Set.union (visited, frontier)
                            val new_frontier = Set.difference (out_neighbors, new_visited)
                            val new_ASP = Table.merge (fn (a, _) => a) (ASP, back_edges)
                        in  
                            mkASP_if_not_none (new_visited, new_frontier, new_ASP)
                        end
                in mkASP_if_not_none (Set.empty(), Set.singleton v, Table.empty())
                end

  (* Task 2.5 *)
  fun report (A : asp) (v : vertex) : vertex seq seq =
    case (Table.find A v) of 
      NONE => Seq.empty()
    | SOME _ => let fun find_all_path A v = 
                    if Seq.length (outNeighbors A v) = 0 then Seq.singleton (Seq.singleton v)
                    else 
                      let fun append_self seq v = Seq.map (fn seq1 => Seq.append(seq1, Seq.singleton v)) seq
                          val out_neighbors = outNeighbors A v
                          val new_seq = Seq.flatten (Seq.map (fn v1 => append_self (find_all_path A v1) v) out_neighbors) 
                      in new_seq
                      end
                in find_all_path A v 
                end

end
