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
        fun mapToPair ZERO ZERO = (ZERO,STOP)  (*按位相加，0+0=0， 生成stop*)
          | mapToPair ZERO ONE = (ONE,PROP)    (*1+0=1, 生成prop*)
          | mapToPair ONE ZERO = (ONE,PROP)    (*0+1=1, prop*)
          | mapToPair ONE ONE = (ZERO,GEN)     (*1+1=0, gen*)
        fun addedDigit i = if(i < Int.min(length x, length y)) then mapToPair (nth x i) (nth y i)
              else if(i<length x) then mapToPair ZERO (nth x i)
              else if (i<length y) then mapToPair ZERO (nth y i)
              else (ZERO,STOP)                 (*对两个bignum进行逐位相加，若有一个较长，则多出来的部分逐位与0相加*)
        fun ctlcarry (_, STOP) = STOP
          | ctlcarry (c, PROP) = c
          | ctlcarry (_, GEN) = GEN            (*对carry的处理*)
        fun reverse_bit ONE = ZERO
          | reverse_bit ZERO = ONE             (*对位进行逆转*)
        fun mapper GEN = true
          | mapper _ = false                   (*在计算完carry后，对其逐个进行判断，若为GEN，记为true, 否则记为false*)
        val beforeCarrying = tabulate addedDigit (Int.max(length x, length y))      (*W=O(n), S=O(1)*)
        val carriesBefore = map second beforeCarrying   (*对逐位相加的结果，取后面生成carry的部分*)   (*W=O(n), S=O(1)*)
        val addxy = scani ctlcarry STOP carriesBefore      (*W=O(n), S=O(logn)*)
        val map_add = append(singleton false, map mapper addxy)    (*由于scani将id除去，但carry携带的进位，也即carry串的长度，必须比addxy大，在串的开始加上false，表示最低位没有传过来的进位值*)  (*W=O(n), S=O(1)*)
        val not_carried = append((map first beforeCarrying), singleton ZERO)   (*若最后有进位，即最后的结果比之前两数都要大，需先在最前面补0*)
        fun digitcarrying i = if (nth map_add i) = true then reverse_bit (nth not_carried i)
                              else (nth not_carried i)  (*对于carry的每一个元素，若其为true,说明在当前位有进位，则将原来没有carry的结果翻转，表示将进位值加上，否则进位为0，则原来的值不变*)
        val results = tabulate digitcarrying (length not_carried)
    in results
    end

    (*对两个bignum进行逐位相加，若有一个较长，则多出来的部分逐位与0相加，
    逐位相加的计算方法为：
    若为ZERO ZERO，生成(ZERO,STOP)
    若为ZERO ONE，生成(ONE,PROP)
    若为ONE ZERO，生成(ONE,PROP)
    若为ONE ONE，生成(ZERO,GEN)
    然后先对逐位相加的结果，取后面生成carry的部分，
    然后用scani函数，对每一位相加生成的carry值进行计算，具体计算方法为：
    对于任意carry值，若其遇到STOP，返回STOP,
    对于任意carry值，若其遇到PROP，返回其本身,
    对于任意carry值，若其遇到GEN，返回GEN，
    由于scani将id除去，但carry携带的进位，也即carry串的长度，必须比addxy大，
    在串的开始加上false，表示最低位没有传过来的进位值，
    若最后有进位，即最后的结果比之前两数都要大，需先在最前面补0，
    对于carry的每一个元素，若其为true,说明在当前位有进位，
    则将原来没有carry的结果翻转，表示将进位值加上，否则进位为0，则原来的值不变。*)

  val add = op++

end

(*
实际样例分析：
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
