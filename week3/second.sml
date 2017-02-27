(* datatype binding *)
datatype mytype = TwoInts of int*int
                | Str of string
                | Pizza
(* TwoInts, Str ans Pizza are constructors
 * A constructor is a function that makes values of the new type or is the value
 * of the new type 
 * *)
fun eval_type x = 
  case x of Pizza => 3
          | TwoInts(x,y) => x+y
          | Str s => 6

datatype exp = Constant of int
            |  Negate of exp 
            |  Add of exp * exp
            |  Multiply of exp * exp

fun max_constant e = 
  case e of
       Constant i => i

     | Negate e2 => max_constant e2 
     | Add(e1, e2) => Int.max(max_constant e1, max_constant e2)
     | Multiply(e1, e2) => Int.max(max_constant e1, max_constant e2)

val max_cons = max_constant(Add(Constant 19, Multiply(Constant 80,
Negate(Constant 1000))))


(* type alias *)

type date = int*int*int


fun sum_of_list xs = 
  case xs of 
       [] => 0
     | x::xs' => x + sum_of_list xs'

val check_sum_of_list = sum_of_list([1,2,3]) = 6

(* Binary tree *)
datatype ('a, 'b) tree = 
          Node of 'a * ('a, 'b) tree * ('a,'b) tree
        | Leaf of 'b

fun sum_triple (x,y,z) = x+y+z

fun rotate(x,y,z) = (z,y,x)
(* everything takes one argument *)

(* append two lists *)
fun append(xs, ys) =
  case xs of
       [] => ys
     | x::xs' => x::append(xs',ys)

(* ''a ''a the same type *)
exception ListLengthMismatch
fun  old_zip3(x,y,z) = 
   if null x andalso null y andalso null z
   then []
   else if null x orelse null y orelse null z
        then raise ListLengthMismatch
        else (hd x, hd y, hd z)::old_zip3(tl x, tl y, tl z)
(* use pattern match *)
fun zip3 triple = 
  case triple of 
       ([], [], []) => []
     | (hd1::tl1, hd2::tl2, hd3::tl3) => (hd1, hd2, hd3)::zip3(tl1, tl2, tl3)
     | _ => raise ListLengthMismatch 
(*
 * Input: [(x1,y1,z1), (x2,y2,z2), (x3,y3,z3) ... (xn, yn, zn)]
 * Output: ([x1,x2,x3,...,xn], [y1,y2,y3,...,yn],...,[z1,z2,z3,...,zn])
 * recursion is amazing and wonderful
 *)
fun unzip3 lists =
  case lists of 
       [] => ([],[],[])
     | (a,b,c)::tail => let val (x,y,z) = unzip3 tail
                        in  (a::x, b::y,c::z)
                        end  
                        

(* non decereasing *)
fun nondecreasing xs = (* int list -> bool *)
  case xs of 
       [] => true
     | _::[] => true (* only one element *)
     | x::y::tail => x <= y andalso nondecreasing(y::tail)
     
(* length of a list *)
fun len xs =
  case xs of 
       [] => 0
     | x::xs' => 1+len xs'

(* implement hd*)
fun hd xs = 
  case xs of 
       [] => raise List.Empty
     |x::xs' => x

(* Exception *)
exception Mine of int * int
exception MyEmptyList
fun max_in_list (xs, some_exception) = 
  case xs of 
       [] => raise some_exception 
     | x::[] => x
     | x::tail => Int.max(x, max_in_list(tail, some_exception))
(* e = 0 *)
val e = max_in_list([], MyEmptyList) handle MyEmptyList => 0

(* tail recursion *)

