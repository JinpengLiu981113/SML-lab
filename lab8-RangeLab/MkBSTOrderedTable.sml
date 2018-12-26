functor MkBSTOrderedTable (structure Tree : BSTREE
                           structure Seq : SEQUENCE) : ORD_TABLE =
struct

  structure Table = MkBSTTable(structure Tree = Tree
                               structure Seq = Seq)

  (* Include all the functionalities of the standard Table *)
  open Table

  (* This is defined after "open" so it doesn't get overwritten *)
  structure Key = Tree.Key
  type key = Key.t

  (* Remember, type 'a table = 'a Tree.bst *)

  (* Remove this line before submitting! *)
  exception NYI

  fun first (T : 'a table) : (key * 'a) option = 
    let fun findfirst (T, opt) = 
            case Tree.expose T of 
              NONE => opt    (*左子树为空，返回记录下来的节点，此时返回的就是其第一个节点*)
            | SOME {left, key, value, right} => findfirst (left, SOME (key, value))   (*访问其左子树，并记录下左子树的根节点*)
    in findfirst (T, NONE)  (*若table为空，返回NONE*)
    end

  fun last (T : 'a table) : (key * 'a) option =  
    let fun findlast (T, opt) = 
            case Tree.expose T of 
              NONE => opt  (*右子树为空，返回记录下来的节点*)
            | SOME {left, key, value, right} => findlast (right, SOME (key, value))  (*访问其右子树，并记录下右子树的根节点*)
    in findlast (T, NONE)  (*若table为空，返回NONE*)
    end
		      
  fun previous (T : 'a table) (k : key) : (key * 'a) option =
    let val (left, _, right) = Tree.splitAt (T, k)  (*对table进行split，得到原来table左边部分和右边部分*)
    in last left  (*对左边部分求最后一个元素，得到table中k元素的前一个元素*)
    end

  fun next (T : 'a table) (k : key) : (key * 'a) option =
    let val (left, _, right) = Tree.splitAt (T, k)  (*对table进行split，得到原来table左边部分和右边部分*)
    in first right  (*对右边部分求第一个元素，得到table中k元素的后一个元素*)
    end

  fun join (L : 'a table, R : 'a table) : 'a table =
    Tree.join (L, R)

  fun split (T : 'a table, k : key) : 'a table * 'a option * 'a table =
    Tree.splitAt (T, k)

  fun getRange (T : 'a table) (low : key, high : key) : 'a table =
    let val (left1, b1, right1) = split (T, low)  (*对table对low进行split操作，得到其左半部分，记为left1，
                                                    一个关于low的option，记为b1，和右半部分，记为right1*)
        val (left2, b2, right2) = split (right1, high)  (*对right1对high进行split操作，得到其左半部分，记为left2，
                                                          一个关于high的option，记为b2，和右半部分，记为right2*)
    val new_left = case b1 of
        NONE => left2
      | SOME v => join(singleton (low, v), left2)  (*判断若b1不为NONE，说明low在table里，需要把low加到结果中，
                                                  join(singleton (low, v), left2)，否则不把low加到table里*)
    in case b2 of
        NONE => new_left
      | SOME v => join(new_left, singleton (high, v))  (*若b2不为NONE，说明high在table里，需要把high加到结果中，
                                                join(new_left, singleton (high, v))，否则不把high加到table里。*)
    end				       

end
