functor MkBigNumAdd(structure U : BIGNUM_UTIL) : BIGNUM_ADD =
struct
  structure Util = U
  open Util
  open Seq

  infix 6 ++
  exception NotYetImplemented
  datatype carry = GEN | PROP | STOP

  fun (x ++ y) = 
    let fun first (i,_) = i
        fun second (_,i) = i
        fun mapToPair ZERO ZERO = (ZERO,STOP)
          | mapToPair ZERO ONE = (ONE,PROP)
          | mapToPair ONE ZERO = (ONE,PROP)
          | mapToPair ONE ONE = (ZERO,GEN)
        fun addedDigit i = if(i < Int.min(length x, length y)) then mapToPair (nth x i) (nth y i)
              else if(i<length x) then mapToPair ZERO (nth x i)
              else if (i<length y) then mapToPair ZERO (nth y i)
              else (ZERO,STOP)
        fun ctlcarry (_, STOP) = STOP
          | ctlcarry (c, PROP) = c
          | ctlcarry (_, GEN) = GEN
        fun reverse_bit ONE = ZERO
          | reverse_bit ZERO = ONE
        fun mapper GEN = true
          | mapper _ = false
        val beforeCarrying = tabulate addedDigit (Int.max(length x, length y))      (*W=O(n), S=O(1)*)
        val carriesBefore = map second beforeCarrying                               (*W=O(n), S=O(1)*)
        val addxy = scani ctlcarry STOP carriesBefore                               (*W=O(n), S=O(logn)*)
        val map_add = append(singleton false, map mapper addxy)                     (*W=O(n), S=O(1)*)
        val not_carried = append((map first beforeCarrying), singleton ZERO)
        fun digitcarrying i = if (nth map_add i) = true then reverse_bit (nth not_carried i)
                              else (nth not_carried i)
        val results = tabulate digitcarrying (length not_carried)
    in results
    end

  val add = op++

end

(*
sample：

00101 ++ 
01111

(carry and not_carry)
01010
SPGPG

(process of scani)
SP     GPG
S   P   G   PG
S,S   S,P   S,G   S,P   S,G
S,S,S    S,G    S,S,G
SSSG  SSG
SSSSG

010100
SSSSSG

(result)
010101
*)
