functor MkRangeCount(structure OrdTable : ORD_TABLE) : RANGE_COUNT =
struct
  structure OrdTable = OrdTable
  open OrdTable


  (* Ordered table type: *)
  type 'a table = 'a table
  type 'a seq = 'a Seq.seq
  type point = Key.t * Key.t

  (* Use this to compare x- or y-values. *)
  val compareKey : (Key.t * Key.t) -> order = Key.compare

  (* Define this yourself *)
  type countTable = Key.t table table table  (*{{x1→{y1→(), y2->(), y3->(), ... , yn->()}, ..., 
                                                {x2->{y1→(), y2->(), y3->(), ... , yn->()}}}*)

  (* Remove this line before submitting! *)
  exception NYI

  fun makeCountTable (S : point seq) : countTable =
    let fun sort_func ((x1, y1), (x2, y2)) = compareKey (x1, x2)
        fun compute_one (x, y) = first(singleton (x, singleton (y, empty())))
        fun join_table (T1, T2) = 
            case (T1, T2) of 
              (NONE, NONE) => NONE
            | (NONE, a) => a 
            | (a, NONE) => a
            | (SOME (x1, T), SOME(x2, T')) => 
                    SOME(x2, join (T, T'))
        fun opt_convert T = case T of SOME(x, T') => (x, T') 
                    
        val sorted = Seq.sort sort_func S
        val table_elements = Seq.map compute_one sorted
        val mk_count_table = Seq.scani join_table NONE table_elements
        val opt_to_table = Seq.map opt_convert mk_count_table
    in fromSeq opt_to_table
    end

  fun count (T : countTable)
                   ((xLeft, yHi) : point, (xRight, yLo) : point) : int  =
    let fun get_value T = case T of NONE => empty() | SOME (key, value) => value
        val (xr_left, optr, xr_right) = split(T, xRight)
        val new_xr_left = case optr of
                        NONE => xr_left
                      | SOME v => join (xr_left, singleton (xRight, v))
        val (xl_left, _, xl_right) = split(T, xLeft)
        val new_xr_left_with_y = getRange (get_value(last new_xr_left)) (yLo, yHi)
        val new_xl_left_with_y = getRange (get_value(last xl_left)) (yLo, yHi)
    in size new_xr_left_with_y - size new_xl_left_with_y
    end

end
