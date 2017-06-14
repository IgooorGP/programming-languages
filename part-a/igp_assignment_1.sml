(* Assignment 1 by Igor Grillo Peternella *)

(* ex 1 *)
fun is_older (dt1 : int * int * int, dt2 : int * int * int) =
  if #1 dt1 < #1 dt2 
  then true
  else if #1 dt1 = #1 dt2 andalso #2 dt1 < #2 dt2
  then true
  else if #1 dt1 = #1 dt2 andalso #2 dt1 = #2 dt2 andalso #3 dt1 < #3 dt2
  then true
  else false

(* ex2 *)
fun number_in_month (dates : (int * int * int) list, month : int) =
  if null dates
  then 0
  else
      if #2(hd dates) = month
      then number_in_month(tl dates, month) + 1
      else number_in_month(tl dates, month)
			  
(* ex3 *)
fun number_in_months (dates : (int * int * int) list, months : int list) =
  if null months
  then 0
  else number_in_month(dates, hd months) + number_in_months(dates, tl months)

(* ex4 *)
fun dates_in_month (dates : (int * int * int) list, month: int) =
  if null dates
  then []
  else
      if #2(hd dates) = month
      then hd dates::dates_in_month(tl dates, month)
      else dates_in_month(tl dates, month)

(* ex 5 *)			 
fun get_nth (xs : string list, n : int) =
  if n = 1
  then hd xs
  else get_nth(tl xs, n - 1)
	      
(*
      ** uses an aux function ***  			 
fun get_nth (xs : string list, n : int) =
  let
      fun aux (xs: string list, k : int) =
	if k = n
	then hd xs
	else aux(tl xs, k + 1)
  in
      aux(xs, 1)
  end 
*)

(* ex7 *)
fun date_to_string (date : int * int * int) =
  let
      val months = ["January", "February", "March",
		    "April","May", "June",
		    "July", "August", "September",
		    "October", "November", "December"]
  in
      get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^
      ", " ^ Int.toString(#1 date)
  end

(* ex8 *)
fun number_before_reaching_sum (sum : int, xs : int list) =
  let
      fun aux (sum : int, xs : int list, k : int) =
	if hd xs >= sum
	then k
	else aux(sum - hd xs, tl xs, k + 1) 
  in
      aux(sum, xs, 0)
  end

(* ex9 *)
fun what_month (doy : int) =
  let
      val days_per_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
      number_before_reaching_sum(doy, days_per_month) + 1
  end

(* ex10 *)
fun month_range (doy1 : int, doy2 : int) =
  if doy1 > doy2
  then []
  else
      if doy1 = doy2
      then what_month(doy2)::[]
      else what_month(doy1)::month_range(doy1 + 1, doy2)

(* ex11 *)
fun oldest (dates : (int * int * int) list) =
  if null dates
  then NONE
  else
      if null (tl dates)
      then SOME(hd dates)
      else 
	  if is_older(hd dates, hd (tl dates))
	  then
	      let
		  val new_dates = hd dates::tl(tl dates) (*hd dates is older so remove the hd(tl dates) and build a new list*)
	      in
		 oldest(new_dates)
	      end
	  else oldest(tl dates)
