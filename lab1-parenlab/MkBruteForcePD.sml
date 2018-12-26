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
