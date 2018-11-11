functor MkSkyline(structure S : SEQUENCE) : SKYLINE =
struct
  structure Seq = S
  open Primitives
  open Seq

  exception NotYetImplemented
  
  fun skyline (buildings : (int * int * int) seq) : (int * int) seq =
    raise NotYetImplemented
end
