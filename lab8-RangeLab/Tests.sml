structure Tests =
struct
  structure Seq = ArraySequence
  open Seq

  type point = int * int

  (* Here are a couple of test structures. ordSet1 is a test ordered set
   * (table), while points1 is a test collection of points in the plane.
   *
   * Note that for an ordered table test, you should just specify a sequence
   * of keys (and values will be automatically set to unit). *)

  val ordSet1 = % [5, 7, 2, 8, 9, 1]
  val my_test_set_1 = % [11]
  val my_test_set_2 = % [3, 15]
  val my_test_set_3 = % [30, 22, 6, 4, 98, 16, 34, 76, 19, 48, 46, 37, 49, 10, 25, 88]

  val testsFirst = [
    ordSet1,
    my_test_set_1,
    my_test_set_2,
    my_test_set_3,
    % []
  ]
  val testsLast = [
    ordSet1,
    my_test_set_1,
    my_test_set_2,
    my_test_set_3,
    % []
  ]
  val testsPrev = [
    (ordSet1, 8),
    (ordSet1, 1),
    (my_test_set_1, 1),
    (my_test_set_2, 1),
    (my_test_set_2, 3),
    (my_test_set_3, 1),
    (my_test_set_3, 16),
    (my_test_set_3, 88),
    (my_test_set_3, 2),
    (% [], 8)
  ]
  val testsNext = [
    (ordSet1, 8),
    (ordSet1, 9),
    (my_test_set_1, 1),
    (my_test_set_2, 11),
    (my_test_set_2, 3),
    (my_test_set_3, 1),
    (my_test_set_3, 16),
    (my_test_set_3, 88),
    (my_test_set_3, 2),
    (% [], 8)
  ]
  val testsJoin = [
    (ordSet1, % [100]),
    (ordSet1, % [3]),
    (my_test_set_1, my_test_set_2),
    (my_test_set_2, my_test_set_3),
    (my_test_set_1, my_test_set_3),
    (my_test_set_3, ordSet1),
    (my_test_set_2, ordSet1),
    (my_test_set_1, ordSet1),
    (% [], % [100])
  ]
  val testsSplit = [
    (ordSet1, 7),
    (ordSet1, 100),
    (my_test_set_1, 1),
    (my_test_set_2, 1),
    (my_test_set_2, 3),
    (my_test_set_3, 1),
    (my_test_set_3, 16),
    (my_test_set_3, 88),
    (my_test_set_3, 26),
    (% [], 7)
  ]
  val testsRange = [
    (ordSet1, (5,8)),
    (ordSet1, (10,12)),
    (my_test_set_2, (1, 2)),
    (my_test_set_2, (2, 2)),
    (my_test_set_2, (2, 3)),
    (my_test_set_2, (0, 8)),
    (my_test_set_2, (5, 6)),
    (% [], (5,8))
  ]


  val points1 = % [(0,0),(1,2),(3,3),(4,4),(5,1)]
  val points2 : point seq = % []
  val points3 = % [(10000,10000),(0,0)]
  val points4 = tabulate (fn i => (i,i)) 1000
  val my_points = map (fn (a, b) => ((12*a+23)mod 59, (13*b+37)mod 101)) points4

  val testsCount = [
    (points1, ((1,3),(5,1))),
    (points1, ((2,4),(4,2))),
    (points1, ((100,101),(101,100))),

    (points2, ((0,10),(10,0))),
    (points3, ((0,10000),(10000,0))),
    (points4, ((0,500),(1000,0))),
    (my_points, ((15, 30), (55, 90)))
  ]


end
