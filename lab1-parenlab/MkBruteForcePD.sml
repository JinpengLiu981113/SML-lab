functor MkBruteForcePD (structure P : PAREN_PACKAGE) : PAREN_DIST =
struct
  structure P = P
  open P
  open Seq
  open Option210

  fun parenDist (parens : paren seq) : int option = 
    if length parens = 0 then NONE
    else let fun getAllSubseq(parens, i, len, seq_list) = 
                if i < length parens then 
                if len <= length parens - i then getAllSubseq(parens, i, len+1, Seq.subseq parens (i, len) :: seq_list) 
                else getAllSubseq(parens, i+1, 0, seq_list)
                else seq_list

              val allSubseqList = getAllSubseq(parens, 0, 0, [])

              fun calculatePD([], pdlist) = pdlist
                | calculatePD(seq::allSubseqList, pdlist) = 
                  let fun calPD(seq, i, num, []) = 
                      if length seq = 0 then NONE
                      else if i < length seq then 
                        if nth seq i = OPAREN then 
                            if num = 0 then calPD(seq, i+1, num+1, [OPAREN])
                            else SOME(num)
                        else NONE 
                      else SOME(num)
                        | calPD(seq, i, num, OPAREN::stack_list) = 
                      if i < length seq then 
                        if nth seq i = OPAREN then calPD(seq, i+1, num+1, OPAREN::OPAREN::stack_list)
                        else calPD(seq, i+1, num+1, stack_list)
                      else NONE
                  in calculatePD(allSubseqList, calPD(seq, 0, 0, [])::pdlist) end
              
              val allIntOptions = calculatePD(allSubseqList, [])

          in iter intMax NONE (Seq.fromList(allIntOptions))
          end 

    
end

(* 
line 9: 如果sequence为空，则其中没有括号是匹配的，返回NONE, W=O(1)  S=O(1) 
line 10-14: function getAllSubseq, 如果sequence不为空，则生成其所有的子串, 返回值存放在allSubseqList， W=O(n^2), S=O(1)
line 20-32: function calPD, 对生成的某一子串求其int option, 方法：若子串为空，则返回NONE, 若子串不为空，从子串的左边开始，
            若一开始为右括号，子串不匹配，返回NONE，遇到左括号压入栈中，num+1, 遇到右括号分以下情况：1.若栈为空，子串不匹配，返回NONE；
            2.若栈不为空，则把之前栈中的一个左括号弹出, num+1。最后返回SOME(num), W=O(n), S=O(n)
line 18-33: function calculatePD, 对sequence生成的所有子串求其最大的int option, 返回值存放在allIntOptions，W=O(n*2^n), S=O(n)
line 37: 求allIntOptions中最大的SOME(n)，W=O(n) S=O(n)
*)
