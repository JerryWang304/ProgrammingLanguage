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
  List.foldl (fn (x,y) => if String.size x > String.size y then x else y) "" str_list

(* problem 3 *)
fun longest_string2(str_list) =
  List.foldl (fn (x,y) => if String.size x >=  String.size y then x else y) "" str_list

(* problem 4 *)
fun longest_string_helper f str_list =
  List.foldl (fn (x,y) => if f(String.size x, String.size y) then x else y)
  "" str_list

fun longest_string3 str_list = 
  longest_string_helper (fn (x,y) => x>y) str_list

fun longest_string4 str_list = 
  longest_string_helper (fn (x,y) => x>=y) str_list


(* problem 5 *)
fun longest_capitalized str_list = 
  (*
  List.foldl (fn (x,y) => if String.size x >  String.size y andalso
  Char.isUpper(String.sub(x,0)) then x else y) "" str_list
  *)
  longest_string1(only_capitals(str_list))

(* problem 6 *)
(* explode : string -> chars 
 * implode: chars -> string
 *)
fun rev_string str = String.implode(List.rev(String.explode str))

(* problem 7 *)
(* first_answer: ('a -> 'b option ) -> 'a list -> 'b *)
fun first_answer f some_list = 
  case some_list of 
       [] => raise NoAnswer
     | head::tail => case f(head) of
                          SOME(result) => result
                        | NONE => first_answer f tail
(* problem 8 *)
(* all_answers: ('a -> 'b list option ) -> 'a list -> 'b list optioin *)
fun all_answers f some_list = 
  let
    fun helper (the_list,ret) = 
      case the_list of 
           [] => ret
         | x::xs => case f(x) of
                         SOME r => helper(xs,r::ret)
                       | NONE => helper(xs,ret)
  in
    if some_list = [] 
    then SOME []
    else case helper(some_list,[]) of
         [] => NONE
        | ret => SOME(ret)
  end 

(* problem 9 *)
fun count_wildcards p = 
  g (fn x => 1) (fn x=> 0) p

fun count_wild_and_variable_lengths p = 
  g (fn x => 1) (fn x => String.size(x)) p 

fun count_some_var(str,p) = 
  g (fn x=> 1) (fn x => if x=str then 1 else 0) p

(* problem 10 *)
fun  check_pat p = 
  let
    fun get_variables p = 
      case p of
           Variable x => [x]
         | TupleP xs => List.foldl (fn(x,y) => get_variables(x) @ y ) [] xs
         | ConstructorP(_,pa) => get_variables(pa)
         | _ => []

    fun unique lst = 
      case lst of
           [] => true
         | head::tail => (not (List.exists (fn s => s = head) tail)) andalso unique tail 

  in 
    unique(get_variables(p))
  end

(* problem 11 *)
(*fun match (va_p,p_p) = *)

(*   case (va_p,p_p) of
        (_, Wildcard) => SOME []
      | (v,Variable va) => SOME [(va,v)]
      | (Unit, UnitP) => SOME []
      | (Const i, ConstP j) => if i=j then SOME [] else NONE
      | (Tuple vs, TupleP ps) => if List.length vs = List.length ps
                                       then all_answers match (ListPair.zip(vs, ps))
                                       else NONE
      | (Constructor(s2, v), ConstructorP(s1, p)) => if s1 = s2
                                                     then match(v, p)
                                                     else NONE 
      | (_,_) => NONE
*)
fun match (va, pat) =
    case (va, pat) of
        (_, Wildcard) => SOME []
      | (v, Variable s) => SOME [(s, v)]
      | (Unit, UnitP) => SOME []
      | (Const i, ConstP j) => if i = j then SOME [] else NONE
      | (Tuple vs, TupleP ps) => if List.length vs = List.length ps
                                 then all_answers match (ListPair.zip(vs, ps))
                                 else NONE
      | (Constructor(s2, v), ConstructorP(s1, p)) => if s1 = s2
                                                     then match(v, p)
                                                     else NONE
      | (_, _) => NONE

(* problem 12 *)
fun first_match(v,ps) =
  SOME (first_answer (fn x => match(v,x)) ps) handle NoAnswer => NONE 
