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
                    
        val sorted = Seq.sort sort_func S  (*对point sequence进行merge sort*)
        val table_elements = Seq.map compute_one sorted  (*对seq进行map操作，将每个point变成{x→{y→()}}的option seq*)
        val mk_count_table = Seq.scani join_table NONE table_elements  (*用scani 对 table进行join操作，
                                                                  生成一个横坐标排好序的表，其key为横坐标对应的值，
                                                            value为横坐标比当前对应的横坐标小的点的纵坐标生成的table*)
        val opt_to_table = Seq.map opt_convert mk_count_table  (*将option变成{x→{y→()}}*)
    in fromSeq opt_to_table  (*将seq变成一个表*)
    end

  fun count (T : countTable)
                   ((xLeft, yHi) : point, (xRight, yLo) : point) : int  =
    let fun get_value T = case T of NONE => empty() | SOME (key, value) => value
        val (xr_left, optr, xr_right) = split(T, xRight)  (*对countTable对xRight进行split，
                                                            得到其左半部分，记为xr_left，
                                                            一个option，记为optr，
                                                            其右半部分，记为xr_right*)
        val new_xr_left = case optr of
                        NONE => xr_left
                      | SOME v => join (xr_left, singleton (xRight, v)) (*判断若optr为SOME v，说明xRight在countTable中，
                                                                          要将其加回到xr_left中，若optr为NONE，
                                                                          说明xRight不在countTable中，不需要将其加入xr_left*)
        val (xl_left, _, xl_right) = split(T, xLeft)  (*对countTable对xLeft进行split，
                                                        得到其左半部分，记为xl_left，
                                                        一个option，记为optl，
                                                        其右半部分，记为xl_right*)
        val new_xr_left_with_y = getRange (get_value(last new_xr_left)) (yLo, yHi)  (*得到所有在xRight左侧，yLo，yHi之间点的y坐标*)
        val new_xl_left_with_y = getRange (get_value(last xl_left)) (yLo, yHi)  (*得到所有在xLeft左侧，yLo，yHi之间点的y坐标*)
    in size new_xr_left_with_y - size new_xl_left_with_y  (*将他们的size相减，就得到所有在矩形里面的点的个数*)
    end

end