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

(*
function calPD:
line 12: 把parens分成两部分，W(n) = Wshowt(n) S(n) = Sshowt(n)
line 13-23: 匹配parens_tree的几种模式，每一种都返回参数
            (匹配的数目，左括号的数目，右括号的数目，左括号到右侧的距离，右括号到左侧的距离)， 
            如果其为EMPTY，则返回(0, 0, 0, 0, 0),
            如果为左括号，则返回(0, 1, 0, 1, 0),
            如果为右括号，则返回(0, 0, 1, 0, 1),
            对于递归返回的左右两组括号，分为三种情况：
            1.若返回的左边一组括号的左括号的个数等于右边一组返回的右括号的个数，
              则最大匹配数可能在左边，可能在中间，也可能在右边，计算他们的最大值
              返回(最大值，右边一组左括号数，左边一组右括号数，左括号到右侧的距离，右括号到左侧的距离)
            2.若返回的左边一组括号的左括号的个数小于右边一组返回的右括号的个数，
              则最大匹配数可能在左边，可能在中间，也可能在右边，计算他们的最大值
              返回(最大值，右边一组左括号数，左组右括号数+右组右括号数-左组左括号数，右组左括号到右侧的距离，右组右括号到左侧的距离)
            3.若返回的左边一组括号的左括号的个数大于右边一组返回的右括号的个数，
              则最大匹配数可能在左边，可能在中间，也可能在右边，计算他们的最大值
              返回(最大值，右组左括号数+右组左括号数-右组右括号数，左边一组右括号数，左组左括号到右侧的距离，左组右括号到左侧的距离) 
            W(n) = 2W(n/2), S(n) = max(S(n/2))
line 25-27: 接受返回参数最大值，若其为0，则返回NONE，否则返回SOME(n) W(n) = O(1), S(n) = O(1)
*)
