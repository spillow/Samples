use "hw3provided.sml";

(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1_1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test1_2 = only_capitals ["Ad","ba","C"] = ["Ad","C"]

val test2_1 = longest_string1 ["A","bc","C"] = "bc"
val test2_2 = longest_string1 ["A","bc","qd","C"] = "bc"
val test2_3 = longest_string1 [] = ""


val test3_1 = longest_string2 ["A","bc","C"] = "bc"
val test3_2 = longest_string2 ["A","bc","qd","C"] = "qd"


val test4a_1 = longest_string3 ["A","bc","C"] = "bc"
val test4a_2 = longest_string3 ["A","bc","qd","C"] = "bc"
val test4a_3 = longest_string3 [] = ""

val test4b_1 = longest_string4 ["A","B","C"] = "C"
val test4b_2 = longest_string4 ["A","bc","qd","C"] = "qd"

val test5 = longest_capitalized ["A","bc","C"] = "A";

val test6 = rev_string "abc" = "cba";

val test7_1 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val test7_2 = first_answer (fn x => if x > 8 then SOME x else NONE) [1,2,3,4,5] = 0 handle NoAnswer => true

val test8_1 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8_2 = all_answers (fn x => if x = 2 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8_3 = all_answers (fn x => if x < 10 then SOME [x] else NONE) [2,3,4,5,6,7] = SOME([2,3,4,5,6,7])
val test8_4 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [] = SOME([])

val test9a_1 = count_wildcards Wildcard = 1
val test9a_2 = count_wildcards (TupleP [Wildcard, Wildcard]) = 2
val test9a_3 = count_wildcards (TupleP [Wildcard, Variable("x"), ConstructorP("Thing",UnitP)]) = 1

val test9b_1 = count_wild_and_variable_lengths (Variable("a")) = 1
val test9b_2 = count_wild_and_variable_lengths (Variable("mlvar")) = 5
val test9b_3 = count_wild_and_variable_lengths (TupleP [Variable("mlvar"), Variable("to")]) = 7

val test9c_1 = count_some_var ("x", Variable("x")) = 1;
val test9c_2 = count_some_var ("x", TupleP [Variable("x"), Variable("x")]) = 2;

val test10_1 = check_pat (Variable("x")) = true
val test10_2 = check_pat (TupleP [Variable("x"), Variable("x")]) = false;
val test10_3 = check_pat (TupleP [Variable("x"), Variable("y")]) = true;
val test10_4 = check_pat (TupleP [ConstructorP("x", Wildcard), Variable("x"), Variable("y")]) = true;

val test11_1 = match (Const(1), UnitP) = NONE
val test11_2 = match (Const(1), ConstP(1)) = SOME([])
val test11_3 = match (Const(8), Variable("x")) = SOME([("x", Const(8))])
val test11_4 = match (Tuple([Const(7), Unit]), TupleP([Variable("x"),Variable("y")]))
                        = SOME([("x", Const(7)),("y",Unit)])
val test11_5 = match (Tuple([Const(7), Unit]), TupleP([Variable("x"),ConstructorP("y", Wildcard)]))
                        = NONE
val test11_6 = match (Tuple([Const(7), Unit]), TupleP([Variable("x"),Variable("x")]))
                        = NONE

val test12_1 = first_match Unit [UnitP] = SOME []
val test12_2 = first_match (Const 8) [UnitP] = NONE
val test12_3 = first_match (Tuple [Const 6]) [UnitP, TupleP ([Variable "x", Variable "y"]), TupleP ([ConstP 6])] = SOME []

