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
