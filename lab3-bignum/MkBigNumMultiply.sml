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
                        else (x, append(y, tabulate (fn i => ZERO) (length x - length y)))
  fun complete2 (x, y) = if length x div 2 = 1 then (append(x, singleton ZERO), append(y, singleton ZERO)) else (x, y)
  fun multiply (x, y) = let val (new_x, new_y) = complete(x, y)
                        in case (showt new_x, showt new_y) of 
                          (_, ELT ZERO) => singleton ZERO
                        | (ELT ZERO, _) => singleton ZERO
                        | (EMPTY, EMPTY) => singleton ZERO
                        | (EMPTY, ELT ONE) => singleton ONE
                        | (ELT ONE, EMPTY) => singleton ONE
                        | (ELT ONE, ELT ONE) => singleton ONE 
                        | (NODE(p, q), NODE(r, s)) => 
                                    let val (pr, p_q_r_s, qs) = par3((fn () => multiply(p, r)), (fn () => multiply((p ++ q), (r ++ s))), (fn () => multiply(q, s)))  (*并行计算三个部分的乘法*)
                                        val pqrs = p_q_r_s -- pr -- qs
                                        val new_number = append(tabulate (fn i => ZERO) (2 * length p), qs) ++ 
                                                        append(tabulate (fn i => ZERO) (length p), pqrs) ++ 
                                                        pr
                                    in new_number end
                        end
  fun x ** y = 
    multiply(complete2(complete(x, y)))
	       
  val mul = op**

end
