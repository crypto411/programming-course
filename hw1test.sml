(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)


val test1 = is_older ((1,2,3),(2,3,4)) = true
val test1_2 = is_older ((1,2,3),(1,2,2)) = false
val test1_3 = is_older ((1,2,3),(1,2,3)) = false
val test1_4 = is_older ((1,2,3),(1,2,4)) = true
val test1_5 = is_older ((1,2,3),(1,3,2)) = true

val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1
val test2_2 = number_in_month ([(2012,2,28),(2013,12,1),(2012,2,1)],2) = 2
val test2_3 = number_in_month ([(2012,2,28),(2013,12,1),(2012,2,1),(2023,2,1)],2) = 3
val test2_4 = number_in_month ([(2012,3,28),(2013,12,1),(2012,3,1),(2023,3,1)],2) = 0
val test2_5 = number_in_month ([],2) = 0

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3
val test3_1 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,3,28)],[2,3]) = 3
val test3_2 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,3,28)],[2]) = 1
val test3_3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,3,28)],[]) = 0
val test3_4 = number_in_months ([],[]) = 0
val test3_5 = number_in_months ([],[1]) = 0
val test3_6 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4,5,2,3]) = 3

val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
val test4_1 = dates_in_month ([(2012,2,28),(2013,12,1)],12) = [(2013,12,1)]
val test4_2 = dates_in_month ([(2012,2,28),(2013,12,1)],12) = [(2013,12,1)]
val test4_3 = dates_in_month ([(2012,2,28),(2013,12,1),(2015,12,1),(2020,12,1)],12) = [(2013,12,1),(2015,12,1),(2020,12,1)]
val test4_4 = dates_in_month ([],12) = []

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]
val test5_1 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4,2,3,4,2]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"

val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3
val test8_1 = number_before_reaching_sum (15, [1,2,3,4,5]) = 4
val test8_2 = number_before_reaching_sum (6, [1,2,3,4,5]) = 2
val test8_3 = number_before_reaching_sum (16, [1,2,3,4,5]) = 5
val test8_4 = number_before_reaching_sum (100, [1,2,3,4,5]) = 5
val test8_5 = number_before_reaching_sum (100, [1,32,50,20,5]) = 50

val test9 = what_month 70 = 3
val test9_1 = what_month 90 = 3
val test9_2 = what_month 150 = 5
val test9_3 = what_month 360 = 12

val test10 = month_range (31, 34) = [1,2,2,2]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
val test11_1 = oldest([(2012,2,28),(1880,2,3),(2011,3,31),(2011,4,28),(1992,3,2)]) = SOME (1880,2,3)