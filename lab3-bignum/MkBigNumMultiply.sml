functor MkBigNumMultiply(structure BNA : BIGNUM_ADD
                         structure BNS : BIGNUM_SUBTRACT
                         sharing BNA.Util = BNS.Util) : BIGNUM_MULTIPLY =
struct
  structure Util = BNA.Util
  open Util
  open Seq
  open Primitives
  exception NotYetImplemented

  infix 6 ++ --
  infix 7 **

  fun x ++ y = BNA.add (x, y)
  fun x -- y = BNS.sub (x, y)
  fun complete (x, y) = if length x < length y then (append(x, tabulate (fn i => ZERO) (length y - length x)), y)
                        else (x, append(y, tabulate (fn i => ZERO) (length x - length y)))  (*将两个位串补0到同样长*)
  fun complete2 (x, y) = if length x div 2 = 1 then (append(x, singleton ZERO), append(y, singleton ZERO)) else (x, y)  (*判断两个数组是否为偶数长度，
                                                                                                                          若不是偶数长度，在前面补0，
                                                                                                                          否则直接返回*)
  fun multiply (x, y) = let val (new_x, new_y) = complete(x, y)
                        in case (showt new_x, showt new_y) of 
                          (_, ELT ZERO) => singleton ZERO     (*任何数乘0均得到0*)
                        | (ELT ZERO, _) => singleton ZERO     (*同理*)
                        | (EMPTY, EMPTY) => singleton ZERO    (*两空数组返回0*)
                        | (EMPTY, ELT ONE) => singleton ONE   (*空数组与1相乘得到1*)
                        | (ELT ONE, EMPTY) => singleton ONE   (*同理*)
                        | (ELT ONE, ELT ONE) => singleton ONE (*1x1=1*)  (*几种基本情况*)
                        | (NODE(p, q), NODE(r, s)) => 
                                    let val (pr, p_q_r_s, qs) = par3((fn () => multiply(p, r)), (fn () => multiply((p ++ q), (r ++ s))), (fn () => multiply(q, s)))  (*并行计算三个部分的乘法*)
                                        val pqrs = p_q_r_s -- pr -- qs
                                        val new_number = append(tabulate (fn i => ZERO) (2 * length p), qs) ++ 
                                                        append(tabulate (fn i => ZERO) (length p), pqrs) ++ 
                                                        pr  (*计算加法*)
                                    in new_number end
                        end
  fun x ** y = 
    multiply(complete2(complete(x, y)))
	       
  val mul = op**

end
