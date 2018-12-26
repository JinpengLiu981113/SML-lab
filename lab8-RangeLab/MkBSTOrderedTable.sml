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

  fun first (T : 'a table) : (key * 'a) option = 
    let fun findfirst (T, opt) = 
            case Tree.expose T of 
              NONE => opt
            | SOME {left, key, value, right} => findfirst (left, SOME (key, value))
    in findfirst (T, NONE)
    end

  fun last (T : 'a table) : (key * 'a) option =  
    let fun findlast (T, opt) = 
            case Tree.expose T of 
              NONE => opt
            | SOME {left, key, value, right} => findlast (right, SOME (key, value))
    in findlast (T, NONE)
    end
		      
  fun previous (T : 'a table) (k : key) : (key * 'a) option =
    let val (left, _, right) = Tree.splitAt (T, k)
    in last left
    end

  fun next (T : 'a table) (k : key) : (key * 'a) option =
    let val (left, _, right) = Tree.splitAt (T, k)
    in first right
    end

  fun join (L : 'a table, R : 'a table) : 'a table =
    Tree.join (L, R)

  fun split (T : 'a table, k : key) : 'a table * 'a option * 'a table =
    Tree.splitAt (T, k)

  fun getRange (T : 'a table) (low : key, high : key) : 'a table =
    let val (left1, b1, right1) = split (T, low)
        val (left2, b2, right2) = split (right1, high)
    val new_left = case b1 of
        NONE => left2
      | SOME v => join(singleton (low, v), left2)
    in case b2 of
        NONE => new_left
      | SOME v => join(new_left, singleton (high, v))
    end				       

end
