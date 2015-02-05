(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw1.sml";

val test1_1 = is_older((1,2,3),(2,3,4)) = true
val test1_2 = is_older((1,2,3),(1,2,3)) = false
val test1_3 = is_older((1,2,3),(1,2,4)) = true
val test1_4 = is_older((1,12,31),(2,1,1)) = true
val test1_5 = is_older((1,12,31),(1,11,30)) = false

val test2_1 = number_in_month([(2012,2,28),(2013,12,1)],2) = 1
val test2_2 = number_in_month([],2) = 0
val test2_3 = number_in_month([(2012,2,28)],4) = 0

val test3_1 = number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3
val test3_2 = number_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[]) = 0
val test3_3 = number_in_months([],[]) = 0
val test3_4 = number_in_months([],[1]) = 0
val test3_5 = number_in_months([(2012,2,28),(2013,2,1),(2011,3,31),(2011,4,28),(7,7,7)],[2,3,4]) = 4

val test4 = dates_in_month([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]

val test5 = dates_in_months([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6_1 = get_nth(["hi", "there", "how", "are", "you"], 2) = "there"
val test6_2 = get_nth(["hi", "there", "how", "are", "you"], 4) = "are"

val test7_1 = date_to_string((2013, 6, 1)) = "June 1, 2013"
val test7_2 = date_to_string((2014, 1, 31)) = "January 31, 2014"

val test8_1 = number_before_reaching_sum(10, [1,2,3,4,5]) = 3
val test8_2 = number_before_reaching_sum(10, [10]) = 0
val test8_3 = number_before_reaching_sum(10, [9,2]) = 1
val test8_4 = number_before_reaching_sum(10, [1,1,1,1,1,1,1,1,1,1,1]) = 9

val test9_1 = what_month(70) = 3
val test9_2 = what_month(31) = 1
val test9_3 = what_month(32) = 2
val test9_4 = what_month(365) = 12
val test9_5 = what_month(1) = 1
val test9_6 = what_month(59) = 2
val test9_7 = what_month(60) = 3

val test10_1 = month_range(31, 34) = [1,2,2,2]
val test10_2 = month_range(31, 30) = []
val test10_3 = month_range(32, 32) = [2]
val test10_4 = month_range(1, 5) = [1,1,1,1,1]

val test11_1 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
val test11_2 = oldest([]) = NONE
val test11_3 = oldest([(2012,2,28)]) = SOME (2012,2,28)
val test11_4 = oldest([(2012,2,28),(2014,3,28)]) = SOME (2012,2,28)

