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
            | reverse ONE = ZERO     (*位翻转*)
          val new_y = map reverse (append(y, tabulate (fn i => ZERO) (length x - length y)))    (*将减数补0到与被减数一样长，然后对其逐位反转*)
          val new_y_plus_1 = new_y ++ singleton ONE   (*求得减数的补数*)
        in take(x ++ new_y_plus_1, length x)   (*由于计算后的结果的长度不可能比被减数更长，故取被减数的长度作为结果*)
        end
      
  val sub = op--
end

(*W=O(n), S=O(logn)  ++ *)
(*
实例分析：
00101 --
0011

00110 (减数补0)
11001 (减数反转)
00101 (减数加1，得到补数)

00101 ++ 
00101
000101
00010 (取出减数长度的串，得到结果)
*)
