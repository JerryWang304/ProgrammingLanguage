(* this is a comment *)
val x = 34;
val y = 17;
val z = 0-y;
val minus5 = ~5;
val abs_of_x = if z<0 then 0-z else z;
fun pow(x:int, y:int) = 
  if y = 0
  then 1
  else x*pow(x,y-1)
fun cube(x:int) = pow(x,3)

fun swap(pr: int*int) = 
  (#2 pr, #1 pr)

fun sum_list(xs: int list) = 
  if null xs
  then 0
  else hd xs + sum_list(tl xs)
fun silly3() = 
  let 
    val x = (let val x = 5 in x+10 end);
  in
    (x, let val x = 2 in x end, let val x = 10 in let val x = 2 in x end end)
  end

(* input: 4, output: 1,2,3,4 *)

fun count_to(x: int) =
  let 
    fun count_from_to(from: int, to: int) =
      if from = to
      then to::[]
      else
        from::count_from_to(from+1,to)
    in
      count_from_to(1,x)
    end

(* return the max element*)
(* int list -> int option*)
fun max(xs: int list) =
  if null xs
  then NONE
  else
    let val temp = max(tl xs)
    in if isSome temp andalso valOf temp > hd xs
       then temp
       else SOME (hd xs)
    end



