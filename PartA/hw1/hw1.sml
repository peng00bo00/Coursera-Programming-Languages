fun Year (date: int * int * int) = #1 date
fun Month (date: int * int * int) = #2 date
fun Date (date: int * int * int) = #3 date

(*1*)
fun is_older (date1: int * int * int, date2: int * int * int) = 
    let
	val y1 = Year date1
	val y2 = Year date2
	val m1 = Month date1
	val m2 = Month date2
	val d1 = Date date1
	val d2 = Date date2
    in
	y1 < y2 orelse (y1 = y2 andalso m1 < m2)
		orelse (y1 = y2 andalso m1 = m2 andalso d1 < d2)
    end

(*2*)
fun number_in_month (dates: (int * int * int) list, month: int) =
    if null dates
    then 0
    else if Month (hd dates) = month
         then 1 + number_in_month (tl dates, month)
	 else number_in_month (tl dates, month)

(*3*)
fun number_in_months (dates: (int * int * int) list, months: int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

(*4*)
fun dates_in_month (dates: (int * int* int) list, month: int) =
    if null dates
    then []
    else if Month(hd dates) = month
         then hd dates :: dates_in_month(tl dates, month)
         else dates_in_month(tl dates, month)

(*5*)
fun dates_in_months (dates: (int * int * int) list, months: int list) =
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

(*6*)
fun get_nth (strings: string list, n: int) =
    if n = 1
    then hd strings
    else get_nth(tl strings, n-1)
		
(*7*)
fun date_to_string (date: (int * int * int)) =
    let
        val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
        val y = Year date;
        val m = get_nth(months, Month date);
        val d = Date date;
    in
        m ^ " " ^ Int.toString(d) ^ ", " ^ Int.toString(y)
    end
    
(*8*)
fun number_before_reaching_sum (sum: int, numbers: int list) =
    if sum <= hd numbers
    then 0
    else 1 + number_before_reaching_sum (sum-(hd numbers), tl numbers)

(*9*)
fun what_month (day: int) =
    let
	val days_of_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	number_before_reaching_sum (day, days_of_month) + 1
    end

(*10*)
fun month_range (day1: int, day2: int) =
    if day1 > day2
    then []
    else what_month (day1) :: month_range(day1+1, day2)

(*11*)
fun oldest (dates: (int * int * int) list) =
    if null dates
    then NONE
    else
	let
	    val date1 = oldest(tl dates)
	    (* note that date1 and date2 have different types. *)
	    val date2 = hd dates
	in
	    if
		isSome date1 andalso is_older(valOf date1, date2)
	    then date1
	    else SOME(date2)
	end

	
(*12*)
fun remove_duplication (x: int list) =
    if null x
    then []
    else
	let
	    (* Remove the element in the lst *)
	    (* remove_element([1,1,2,3], 1) -> [2, 3] *)
	    fun remove_element (lst: int list, element: int) =
		if null lst
		then []
		else if hd lst = element
		     then remove_element(tl lst, element)
		     else hd lst :: remove_element(tl lst, element)
	in
	    hd x :: remove_duplication(remove_element(x, hd x))
	end

fun number_in_months_challenge (dates: (int * int * int) list, months: int list) =
    number_in_months(dates, remove_duplication(months))

fun dates_in_months_challenge (dates: (int * int * int) list, months: int list) =
    dates_in_months (dates,  remove_duplication(months))
	
(*13*)

	
fun reasonable_date (date: (int * int * int)) =
    let

	fun valid_year (date: (int * int * int)) =
	    if Year(date) > 0
	    then true
	    else false

	fun leap_year (date: (int * int * int)) =
	    let
		val y = Year(date)
	    in
		if y mod 400 = 0
		then true
		else if (y mod 4) = 0 andalso (y mod 100) <> 0
		     then true
	             else false
	    end
		
	fun valid_month (date: (int * int * int)) =
	    if Month(date) > 0 andalso Month(date) <= 12
	    then true
	    else false

	fun get_nth_list (lst: int list, n: int) =
	    if n = 1
	    then hd lst
	    else get_nth_list(tl lst, n-1)

	fun valid_day (date: (int * int * int)) =
	    let
		val m = Month(date)
		val days_of_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
		val days_of_month_leap = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
	    in
		if leap_year(date)
		then
		    if Date(date) > 0 andalso Date(date) <= get_nth_list(days_of_month_leap, m)
		    then true
		    else false
		else if Date(date) > 0 andalso Date(date) <= get_nth_list(days_of_month, m)
		     then true
	             else false
	    end
    in
	if valid_year(date) andalso valid_month(date) andalso valid_day(date)
	then true
        else false
    end
