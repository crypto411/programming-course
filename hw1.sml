(* Helper functions *)
val month_names = 
    [
        "January", 
        "February", 
        "March", 
        "April", 
        "May", 
        "June",
        "July", 
        "August", 
        "September", 
        "October", 
        "November", 
        "Desember"
    ]
val month_days = 
    [
        31, (* January *)
        28, (* February *)
        31, (* March *)
        30, (* April *)
        31, (* May *)
        30, (* June *)
        31, (* July *)
        31, (* August *)
        30, (* September *)
        31, (* October *)
        30, (* November *)
        31  (* Desember *)
    ]
fun get_month_name(month: int, month_names: string list) = 
    if month <= 0 orelse month > 12
    then ""
    else if month = 1 then hd month_names else get_month_name(month - 1, tl month_names)

fun get_year(date: int * int * int) = #1 date
fun get_month(date: int * int * int) = #2 date
fun det_day(date: int * int * int) = #3 date
fun append(xs,ys) = 
    if xs = []
    then ys
    else (hd xs)::append(tl xs, ys)

fun eliminate_int(number: int, xs: int list) = 
    if null xs
    then []
    else
        if number = hd xs 
        then eliminate_int(number, tl xs)
        else (hd xs)::eliminate_int(number, tl xs)

fun distinct_int(xs: int list) =
    if null xs
    then []
    else 
        let
            val eliminated = eliminate_int(hd xs, tl xs)
        in
            if null eliminated
            then [hd xs]
            else (hd xs)::distinct_int(eliminated)
        end

(* Question No. 1 *)
fun is_older(date1: int * int * int, date2: int * int * int) = 
    let
        val year1  = #1 date1
        val month1 = #2 date1
        val day1 = #3 date1
        val year2  = #1 date2
        val month2 = #2 date2
        val day2 = #3 date2
    in
        if year1 < year2
        then true 
        else if year1 = year2 andalso month1 < month2
        then true 
        else if month1 = month2 andalso day1 < day2
        then true
        else false
    end

(* Question No. 2 *)
fun number_in_month(list_dates: (int * int * int) list, month: int) = 
    if null list_dates 
    then 0
    else 
        let
            val month_in_head = get_month(hd list_dates)
            val sum_compare = if month_in_head = month then 1 else 0
        in
            sum_compare + number_in_month(tl list_dates, month)
        end

(* Question No. 3 *)
fun number_in_months(list_dates: (int * int * int) list, months: int list) = 
    if null months
    then 0
    else 
        let
            val distinct_months = distinct_int(months)
            val sum_of_month = number_in_month(list_dates, hd distinct_months)
        in
            sum_of_month + number_in_months(list_dates, tl distinct_months)
        end 

(* Question No. 4 *)
fun dates_in_month(list_dates: (int * int * int) list, month: int) = 
    if null list_dates
    then []
    else 
        let
            val month_in_head = get_month(hd list_dates)
        in 
            if month = month_in_head
            then (hd list_dates)::dates_in_month(tl list_dates, month)
            else dates_in_month(tl list_dates, month)
        end 

(* Question No. 5 *)
fun dates_in_months(list_dates: (int * int * int) list, months: int list) = 
    if null months
    then []
    else 
        let
            val distinct_months = distinct_int(months)
            val list_of_month = dates_in_month(list_dates, hd distinct_months)
        in
            append(list_of_month, dates_in_months(list_dates, tl distinct_months))
        end

(* Question No. 6 *)
fun get_nth(xs: string list, element: int) = 
    if element <= 0 orelse null xs
    then ""
    else if element = 1 then hd xs else get_nth(tl xs, element - 1)

(* Question No. 7 *)
fun date_to_string(date: int * int * int) = 
    get_month_name(#2 date, month_names) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)

(* Question No. 8 *)
fun number_before_reaching_sum(n: int, xs: int list) = 
    if null xs
    then 0
    else
        let 
            val tl_xs = tl xs
            val next_value = if null tl_xs then 0 else hd tl_xs
            val diff = n - hd xs
        in
            if diff > 0
            then 
                if next_value <> 0 andalso (diff - next_value) <= 0
                then hd xs
                else 
                    if next_value = 0
                    then hd xs
                    else number_before_reaching_sum(diff, tl xs)
            else 0
        end

(* Question No. 9 *)
fun what_month(days: int) =
    if days <= 0
    then 0
    else
        let
            fun calculate_month(days: int, month_days: int list) = 
                if days <= 0
                then 0
                else 
                    let
                        val days_mod = if days > 365 then days mod 365 else days
                        val days_left = days_mod - (hd month_days)
                    in
                        if days_left >= 0
                        then 1 + calculate_month(days_left, tl month_days)
                        else 1
                    end
        in
            calculate_month(days, month_days)
        end

(* Question No. 10 *)
fun month_range(day1: int, day2: int) = 
    if day1 <= 0 orelse day1 > day2
    then []
    else what_month(day1)::month_range(day1+1, day2)

(* Question No. 11 *)
fun oldest(date_list: (int * int * int) list) = 
    if null date_list 
    then NONE 
    else 
        let val tl_date = oldest(tl date_list)
        in if isSome tl_date andalso is_older(valOf tl_date, hd date_list) 
            then tl_date
            else SOME (hd date_list)
        end