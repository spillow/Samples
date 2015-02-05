fun is_older(date1 : (int * int * int), date2 : (int * int * int)) =
    let
        val year1  = (#1 date1)
        val month1 = (#2 date1)
        val day1   = (#3 date1)
        val year2  = (#1 date2)
        val month2 = (#2 date2)
        val day2   = (#3 date2)
    in
        if year1 < year2 then true
        else if year1 > year2 then false
        else
            if month1 < month2 then true
            else if month1 > month2 then false
            else
                if day1 < day2 then true
                else if day1 > day2 then false
                else false
    end

fun number_in_month(dates : (int * int * int) list, month : int) =
    if null dates then 0
    else
        if month = (#2 (hd dates))
        then 1 + number_in_month(tl dates, month)
        else number_in_month(tl dates, month)

fun number_in_months(dates : (int * int * int) list, months : int list) =
    if null months
    then 0
    else
        number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month(dates : (int * int * int) list, month : int) =
    let
        fun f(dates : (int * int * int) list) =
            if null dates then []
            else
                if month = (#2 (hd dates))
                then (hd dates) :: f(tl dates)
                else f(tl dates)
    in
        f(dates)
    end

fun dates_in_months(dates : (int * int * int) list, months : int list) =
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth(strings : string list, n : int) =
    if n = 1
    then hd strings
    else get_nth(tl strings, n - 1)

fun date_to_string(date : (int * int * int)) =
    let
        val months = ["January", "February", "March", "April", "May", "June", "July",
                      "August", "September", "October", "November", "December"]
        val year  = (#1 date)
        val month = (#2 date)
        val day   = (#3 date)

        val strMonth = get_nth(months, month)
        val strDay   = Int.toString(day)
        val strYear  = Int.toString(year)
    in
        strMonth ^ " " ^ strDay ^ ", " ^ strYear
    end

fun number_before_reaching_sum(sum : int, nums : int list) =
    let
        fun f(nums, accsum, acccnt) =
            if (hd nums) + accsum >= sum
            then acccnt
            else f(tl nums, (hd nums) + accsum, acccnt + 1)
    in
        f(nums, 0, 0)
    end

fun what_month(day : int) =
    let
        val monthlen = [31,28,31,30,31,30,31,31,30,31,30,31]
        val month = number_before_reaching_sum(day, monthlen)
    in
        month + 1
    end

fun month_range(day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest(dates : (int * int * int) list) =
    if null dates
    then NONE
    else
        let
            fun f(dates) =
                if null (tl dates)
                then hd dates
                else
                    let
                        val maxold = f(tl dates)
                    in
                        if is_older(hd dates, maxold)
                        then (hd dates)
                        else maxold
                    end
        in
            SOME (f(dates))
        end

fun number_in_months_challenge(dates : (int * int * int) list, months : int list) = 0

fun reasonable_date(date : (int * int * int)) = true
