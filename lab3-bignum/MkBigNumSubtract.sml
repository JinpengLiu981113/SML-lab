functor MkBigNumSubtract(structure BNA : BIGNUM_ADD) : BIGNUM_SUBTRACT =
struct
  structure Util = BNA.Util
  open Util
  open Seq

  exception NotYetImplemented
  infix 6 ++ --
  fun x ++ y = BNA.add (x, y)
  fun x -- y = 
      let fun reverse ZERO = ONE
            | reverse ONE = ZERO
          val new_y = map reverse (append(y, tabulate (fn i => ZERO) (length x - length y)))
          val new_y_plus_1 = new_y ++ singleton ONE
        in take(x ++ new_y_plus_1, length x)
        end
      
  val sub = op--
end

(*W=O(n), S=O(logn)  ++ *)

(*
sample：
00101 --
0011

00110
11001
00101

00101 ++ 
00101
000101

result
00010
*)
