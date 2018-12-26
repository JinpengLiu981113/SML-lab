functor MkDivideAndConquerPD (structure P : PAREN_PACKAGE) : PAREN_DIST =
struct
  structure P = P
  open Primitives
  open P
  open Seq
  open Option210


  fun parenDist (parens : paren seq) : int option = 
    let fun calPD(parens) = 
      let val parens_tree = showt(parens)
      in case parens_tree of
          EMPTY => (0, 0, 0, 0, 0)
        | ELT OPAREN => (0, 1, 0, 1, 0)
        | ELT CPAREN => (0, 0, 1, 0, 1)
        | NODE(L, R) => 
            let val ((Lnum, Lo, Lc, Lodis, Lcdis),
                    (Rnum, Ro, Rc, Rodis, Rcdis)) = par(fn () => calPD(L), fn () => calPD(R))
            in if Lo = Rc then (Int.max(Lodis+Rcdis, Int.max(Lnum, Rnum)), Ro, Lc, Rodis, Lcdis)
               else if Lo < Rc then (Int.max(Lodis+Rcdis+Lo-Rc, Int.max(Lnum, Rnum)), Ro, Lc+Rc-Lo, Rodis, length L + Rcdis) 
                    else (Int.max(Lodis+Rcdis+Rc-Lo, Int.max(Lnum, Rnum)), Ro+Lo-Rc, Lc, length R + Lodis, Lcdis)
            end
      end
        val (A, _, _, _, _) = calPD(parens)
    in if A = 0 then NONE
       else SOME(A)
    end


end
