(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
         | Variable of string
         | UnitP
         | ConstP of int
         | TupleP of pattern list
         | ConstructorP of string * pattern

datatype valu = Const of int
          | Unit
          | Tuple of valu list
          | Constructor of string * valu

fun g f1 f2 p =
    let 
    val r = g f1 f2 
    in
    case p of
        Wildcard          => f1 ()
      | Variable x        => f2 x
      | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
      | ConstructorP(_,p) => r p
      | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
         | UnitT
         | IntT
         | TupleT of typ list
         | Datatype of string

(**** you can put all your code here ****)

(* problem 1 *)
fun only_capitals str_list = 
  (*
  let
    fun f str = Char.isUpper(String.sub(str,0))
  in
    List.filter f str_list 
  end *)
    List.filter (fn str => Char.isUpper(String.sub(str,0))) str_list 


(* problem 2 *)
fun longest_string1(str_list) =
  foldl (fn (x,y) => if String.size x > String.size y then x else y) "" str_list

(* problem 3 *)
fun longest_string2(str_list) =
  foldl (fn (x,y) => if String.size x >=  String.size y then x else y) "" str_list


