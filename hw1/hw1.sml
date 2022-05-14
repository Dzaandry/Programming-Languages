fun is_older (date1 : int * int * int, date2 : int * int * int) =
    if (#1 date1) < (#1 date2)
    then true
    else if (#1 date2) < (#1 date1)
    then false
    else if (#2 date1)  < (#2 date2)
    then true
    else if (#2 date2) < (#2 date1)
    then false
    else if (#3 date1) < (#3 date2)
    then true
    else false

fun number_in_month (ds : (int * int * int) list, month : int) =
    if null ds
    then 0
    else if (#2 (hd ds)) = month
    then 1 + number_in_month (tl ds, month)
    else number_in_month (tl ds, month)
			  
fun number_in_months (ds : (int * int * int) list, ms : int list) =
    if null ms
    then 0
    else number_in_month (ds, hd ms) + number_in_months (ds, tl ms)
							
fun dates_in_month (ds : (int * int * int) list, m : int) =
    if null ds
    then []
    else if (#2 (hd ds)) = m
    then hd ds :: dates_in_month (tl ds, m)
    else dates_in_month (tl ds, m)

fun append (xs : (int * int * int) list, ys : (int * int * int)  list) =
    if null xs
    then ys
    else (hd xs) :: append (tl xs, ys)
			
fun dates_in_months (ds : (int * int * int) list, ms : int list) =
    if null ms
    then []
    else append (dates_in_month (ds, hd ms), dates_in_months (ds, tl ms))
						      
fun get_nth (strings : string list, n : int) =
    if n < 1
    then ""
    else  if n = 1
    then hd strings
    else get_nth (tl strings, n - 1)
		 
fun date_to_string (date : int * int * int) =
    get_nth (["January", "February", "March", "April", "May", "June",
	      "July", "August", "September", "October", "November", "December"], (#2 date)) ^
    " " ^ Int.toString (#3 date) ^ ", " ^ Int.toString (#1 date)

fun number_before_reaching_sum (sum : int, ns : int list) =
    let
	fun helper_with_accumulators (ns1 : int list, sum_so_far : int, ind : int) =
	    if sum_so_far >= sum
	    then ind
	    else helper_with_accumulators (tl ns1, sum_so_far + hd ns1, ind + 1)
    in helper_with_accumulators (tl ns, hd ns, 0)
    end
	
fun what_month (day : int) =
    number_before_reaching_sum(day, [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]) + 1 
			      
fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month (day1) :: month_range (day1 + 1, day2)
					  
fun oldest (ds : (int * int * int) list) =
    if null ds
    then NONE
    else
	let
	    fun oldest_nonempty (ds : (int * int * int) list) =
		if null (tl ds)
		then hd ds
		else let val tl_ans = oldest_nonempty (tl ds)
		     in
			 if is_older (hd ds, tl_ans)
			 then hd ds
			 else tl_ans
		     end
	in
	    SOME (oldest_nonempty ds)
	end
	    
(* 2 Helper functions, useful for 2 different main functions, therefore not in let *)					
fun member_of (m : int, ms : int list) =
    if null ms
    then false
    else if m = (hd ms)
    then true
    else member_of (m, tl ms)
		   
fun remove_dups (ms : int list) =
    if null ms
    then []
    else if member_of (hd ms, tl ms)
    then remove_dups (tl ms)
    else (hd ms) :: remove_dups (tl ms)

fun number_in_months_challenge (ds : (int * int * int) list, ms : int list) =
    number_in_months (ds, remove_dups ms)

fun dates_in_months_challenge (ds : (int * int * int) list, ms : int list) =
    dates_in_months (ds, remove_dups ms)
		    
fun reasonable_date (date : int * int * int) =
    if (#1 date) <= 0 orelse (#2 date) > 12 orelse (#2 date) < 1 orelse (#3 date) < 1
    then false
    else if (#2 date) = 2 andalso (#3 date) = 29
    then (#1 date) mod 400 = 0 orelse ((#1 date) mod 4 = 0 andalso (#1 date) mod 100 <> 0)				       
    else let
	val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	fun get_nth (i : int, l : int list) =
	    if i = 1
	    then hd l
	    else get_nth (i - 1, tl l)
    in
	(#3 date) <= get_nth ((#2 date), days)
    end
	     
