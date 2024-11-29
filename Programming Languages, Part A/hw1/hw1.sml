fun is_older ((y1,m1,d1), (y2, m2, d2)) =
    if y1 < y2
	  then true
    else if  y1 = y2 andalso m1 < m2
	  then true
    else if  y1 = y2 andalso m1 = m2 andalso d1 < d2
	  then true
    else
          false



fun number_in_month (list, month) =
    if null list
        then 0
    else 
	let 
	    val (year, m, d) = hd list
	in
	    if m = month
		then 1 + number_in_month((tl list), month)
	    else
		number_in_month((tl list), month)
	end



fun number_in_months (list, months) =
    if null months
        then 0
    else
	number_in_month(list, hd months) + number_in_months(list, tl months)



(* I am using the latest version of Emacs, and using #1 to retrieve tuple values will result in an error, so I am using pattern matching *)					   
fun dates_in_month (dates, month) =
    if null dates
        then []
    else
	let
	    val (year, m, day) = hd dates
	in
	    if m = month
	        then (year, m, day) ::dates_in_month(tl dates, month)
	    else
		dates_in_month(tl dates, month)
	end



fun dates_in_months (dates, months) =
    if null months
	then []
    else
	let
	    val month = hd months
	in
	    dates_in_month(dates, month) @ dates_in_months(dates, tl months)
	end



fun get_nth (strings, number) =
    if number = 1
	then hd strings
    else
	get_nth(tl strings, number-1)



(* I am using the latest version of Emacs, and using #1 to retrieve tuple values will result in an error, so I am using pattern matching *)	
fun date_to_string (date) = 
    let
	val monthsList = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
	val (year, month, day) = date
    in
	get_nth(monthsList, month) ^ " " ^ Int.toString(day) ^ ", " ^ Int.toString(year)
    end



fun number_before_reaching_sum (sum, list) =
    let
        fun recur(list, index, result) = 
	    if result + (hd list) >= sum
	        then index-1
	    else recur(tl list, index+1, result+(hd list))
    in 
	recur(list, 1, 0)
    end



fun what_month (day) =
    let
        val months_day = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        number_before_reaching_sum(day, months_day) + 1
    end



fun month_range (day1, day2) =
    if day1 > day2
	then []
    else
	what_month(day1) :: month_range(day1+1, day2)



(* I am using the latest version of Emacs, and using #1 to retrieve tuple values will result in an error, so I am using pattern matching *)	
fun oldest (list) =
    if null list
	then NONE
    else if null (tl list)
	then SOME(hd list)
    else
	let 
	    val cur = hd list
	    val after = oldest(tl list)
	in
	    case after of
		NONE => SOME cur
	    |	SOME afterVal =>
		    if is_older (cur, afterVal)
			then SOME cur
		    else
			SOME afterVal
	end



