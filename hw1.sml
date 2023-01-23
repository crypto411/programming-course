(* Helper functions *)
fun get_year(date: int * int * int) = #1 date
fun get_month(date: int * int * int) = #2 date
fun det_day(date: int * int * int) = #3 date
fun append(xs,ys) = 
    if xs = []
    then ys
    else (hd xs)::append(tl xs, ys)

(* in progress *)
fun eliminate_int(number: int, xs: int list) = 
    if null xs
    then []
    else 
        let
            val isEqual = number = hd xs
            val selector = if isEqual then (tl xs) else xs
        in 
            (hd selector)::eliminate_int(number, tl selector)
        end


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

(* not complete (lacking distinct function) *)
fun number_in_months(list_dates: (int * int * int) list, months: int list) = 
    if null months
    then 0
    else 
        let
            val sum_of_month = number_in_month(list_dates, hd months)
        in
            sum_of_month + number_in_months(list_dates, tl months)
        end 

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

(* not complete (lacking distinct function) *)
fun dates_in_months(list_dates: (int * int * int) list, months: int list) = 
    if null months
    then []
    else 
        let
             val list_of_month = dates_in_month(list_dates, hd months)
        in
            append(list_of_month, dates_in_months(list_dates, tl months))
        end

fun get_nth(xs: string list, element: int) = 
    if element <= 0 orelse null xs
    then ""
    else if element = 1 then hd xs else get_nth(tl xs, element - 1)

fun date_to_string() 