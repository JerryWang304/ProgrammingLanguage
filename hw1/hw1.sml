(* 1 *)
fun is_older(x:int*int*int, y:int*int*int) = 
  if #1 x < #1 y
  then true
  else if #1 x = #1 y andalso #2 x < #2 y
       then true
       else if #1 x = #1 y andalso #2 x = #2 y andalso #3 x < #3 y
            then true
            else false

(*2*)
fun number_in_month(dates:(int*int*int) list, month:int) =
  if #2 (hd dates) = month andalso null (tl dates)
  then 1
  else if #2 (hd dates) <> month andalso null (tl dates)
       then 0
       else if #2 (hd dates) = month 
            then 1+ number_in_month(tl dates, month)
            else number_in_month(tl dates,month)

(*3*)
fun number_in_months(dates:(int*int*int) list, months: int list) =
  if null months
  then 0
  else number_in_month(dates,hd months) + number_in_months(dates,tl months)
(*4*)
fun dates_in_month(dates: (int*int*int) list, month: int) =
  if null dates
  then []
  else
    if #2 (hd dates) = month
    then (hd dates)::dates_in_month(tl dates,month)
    else dates_in_month(tl dates,month)
(*5*)
fun dates_in_months(dates: (int*int*int) list, months: int list) =
  if null months
  then []
  else
    (*@ append another list*)
    dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)
(*6*)
fun get_nth(strs: string list, num: int) = 
  if num = 1
  then hd strs
  else get_nth(tl strs, num-1)
(*7*)
fun date_to_string(year:int,month:int,day:int) = 
  let 
    val ms =
      ["January","February","March","April","May","June","July","August","September","October","November","December"]
  in
    get_nth(ms,month) ^ " " ^ Int.toString(day) ^ ", " ^ Int.toString(year)
  end

(*8*)
fun number_before_reaching_sum(sum:int,nums: int list) = 
   (*
    * [1,2,3,4,5] , 10, 0 -> [2,3,4,5], 9, 1 -> [3,4,5],7,2 ->
    * [4,5], 4 , 3 -> [5], 0 , 4 -> end
    * O(n) I think :) 
    *)
   let 
     fun recursive(sum:int, nums: int list, depth:int) = 
       if sum <= 0
       then depth
       else recursive(sum - (hd nums), tl nums, depth+1)
   in
     recursive(sum,nums,0)-1
   end
(*9*)
fun what_month(day: int) =
  let
    val months = [31,28,31,30,31,30,31,31,30,31,30,31];
  in
    1+number_before_reaching_sum(day,months)
  end

(* 10 *)
fun month_range(x: int, y: int) = 
  if x > y
  then []
  else if x = y
       then what_month(x)::[]
       else what_month(x)::month_range(x+1,y)

(* 11 *)
fun oldest(dates: (int*int*int) list) = 
  let 
    fun recursive_max(dates: (int*int*int) list, max: int*int*int) = 
       if null dates
       then max
       else if is_older(max,hd dates)
            then recursive_max(tl dates, max)
            else recursive_max(tl dates, hd dates)
  in
    if null dates
    then NONE
    else SOME (recursive_max(dates, hd dates))
  end 
       

(* for challenge *)
fun is_in(xs: int list, x: int) =
    if null  xs
    then false
    else if x = hd xs
         then true
         else is_in(tl xs, x)
(* for challenge *)  
(* [1,2,2,3,3,4] -> [1,2,3,4] *)
fun no_duplicate(xs: int list, return_list: int list) = 
    if null xs
    then return_list
    else
      if is_in(return_list, hd xs)
      then no_duplicate(tl xs, return_list)
      else no_duplicate(tl xs, return_list @ [hd xs])

(* 12: challenge *)
fun number_in_months_challenge(dates: (int*int*int) list, months: int list) = 
     number_in_months(dates, no_duplicate(months,[]))
    (*  no_duplicate(months,[]) *)

fun dates_in_months_challenge(dates: (int*int*int) list, months: int list) =
   dates_in_months(dates,no_duplicate(months,[]))
(* 13: challenge *)
fun reasonable_date(year, month, day) = 
  if year <= 0 orelse month <=0 orelse month >12 orelse day <=0 orelse day > 31
  then false
  else if is_leap(year) 
         
