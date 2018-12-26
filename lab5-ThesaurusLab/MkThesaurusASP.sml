functor MkThesaurusASP (ASP : ALL_SHORTEST_PATHS where type vertex = string)
  : THESAURUS =
struct
  structure Seq = ASP.Seq
  open Seq
  open ASP

  (* Remove the following two lines when you're done! *)
  exception NYI

  (* You must define the following type and
   * explain your decision here with a comment.
   *)
  type thesaurus = graph

  (* Task 3.1 *)
  fun make (S : (string * string seq) seq) : thesaurus =
    let val new_seq = Seq.flatten(Seq.map (fn (v, v_seq) => (Seq.map (fn v1 => (v, v1)) v_seq)) S)
    in ASP.makeGraph new_seq
    end 

  (* Task 3.2 *)
  fun numWords (T : thesaurus) : int =
    ASP.numVertices T

  fun synonyms (T : thesaurus) (w : string) : string seq =
    ASP.outNeighbors T w

  (* Task 3.3 *)
  fun query (T : thesaurus) (w1 : string) (w2 : string) : string seq seq =
    ASP.report (ASP.makeASP T w1) w2

end
