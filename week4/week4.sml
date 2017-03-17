(* introduction to first-class funtion *)
fun double x = x*2


fun incr x = x+1
(* first class function: as a value *)
val a_tuple = (double, incr, double(incr 3))

val sixteen = (#1 a_tuple) 8


(* first-calss functions: use them as values *)
(* higher-order function: takes or returns other functions *)
(* apply f on x n times *)
fun n_times(f,n,x) = 
  if n=0
  then x
  else f(n_times(f,n-1,x))

fun increase_n_times(n,x) = n_times(incr,n,x)
fun double_n_times(n,x) = n_times(double,n,x)

(* polymorphic types and functions as arguments *)
(* a higher-order function which is not polymorphic
 * x is int; f: fn int -> int
 * *)
fun times_untilf_zero(f,x) = 
  if x = 0
  then 0
  else 1 + times_untilf_zero(f,f(x))

(* not a higer-order funtion but polymorphic *)
fun len xs = 
  case xs of
       [] => 0
     | _::nexts => 1+len(nexts)
(* anonymous funtions *)
fun triple x = x*3

fun triple_n_times (n,x) = 
  (*
  let
    fun triple x  = x*3
  in
    n_times(triple,n,x)
  end
 *)
  (* n_times(let fun triple x=x*3 in triple end, n,x) *)

  (* fn not fun (lambda)*)
  n_times((fn x=>3*x),n,x)


(* unnecessary function wrapping *)
fun nth_tail(n,xs) (* = n_times((fn y=> tl y), n,xs) *)
                   = n_times(tl, n,xs)


(* map and fliter *)
fun map(f,xs) =
  case xs of 
       [] => []
     | x::tail => f(x)::map(f,tail)

val xx = map((fn x=>x+1),[1,2])

fun filter(f,xs) =
  case xs of
       [] => []
     | x::xs' => if f x then x::filter(f,xs') else filter(f,xs')

val even = filter((fn x=> (x mod 2 = 0)),[1,2,3,4])


(* generalizing *)
(* return other function *)
fun double_or_triple f = 
  if f 7
  then fn x => x*2
  else fn x => x*3


(* predicate: return true or false for something *)



(* lexical scope: use environment where the function is defined *)
(* dynamic scope: use environment where the function is called *)
(* function has two parts: the code and the environment *)

(* lexical scope and higher-order function *)

(* lexical scope style in ML return 10 *)
fun f g = let val x = 9 in g() end
val x = 7
fun h() = x+1
val y = f h
(*  dynamic scope style will return 8 *) 

(* call String.size s for each el in xs *)
fun all_shorter_than(xs,s) = 
  filter (fn x => String.size x < String.size s, xs)
(* closure: avoid repeating computations that do not depend on function argument
 *)
fun all_shorter_than2(xs,s) = 
  let
    val i = String.size s
  in
    filter(fn x=> String.size x < i,xs)
  end
(* fold function (reduce ) *)

fun fold (f, acc, xs) =
  case xs of
       [] => acc
     | x::tail => fold ( f, f(acc,x), tail)
(* are all elements non-negative *)
fun f2 xs = fold(fn (x,y) => x andalso y>=0, true, xs)

fun f3(xs,low, high) =
  fold(fn (x,y) => x+(if y>=low andalso y<=high then 1 else 0),0,xs)

fun f4(xs,s) = 
  let 
    val i = String.size s
  in
    fold(fn (x,y) => x andalso String.size y < i,true,xs)
  end

(* combining funtions *)
fun compose(f,g) = fn x => f(g x)

fun sqrt_of_abs i = Math.sqrt(Real.fromInt(abs i))
fun sqrt_of_abs i = (Math.sqrt o Real.fromInt o abs) i


(* pipelines of function *)
infix !>
fun  x !> f = f x 

fun sqrt_of_abs i = i !> abs !> Real.fromInt !> Math.sqrt


fun backup(f,g) = fn x=> case f x of
                              NONE => g x
                            | SOME y => y
(* curring *)
fun sorted3_tuple (x,y,z) = z>=y andalso y>=x
(* curried way *)
(* type: fn: int->int->int ->bool *)
fun sorted3_nicer x y z = z>=y andalso y>=x

val sorted3 = fn x => fn y => fn z => z>= y andalso y >= x

(* fun range (i, j) = if i>j then [] else i::(range i+1 j) *)

val x = ref 1
val z = x
val _ = x:=10
val ret = !x + !z
(* callbacks: library takes some function to apply later, when an event occurs *)
val cbs: (int -> unit) list ref = ref []
fun onKeyEvent f = cbs := f::(!cbs)
fun onEvent i = 
