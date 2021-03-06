(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
 *    string), then you avoid several of the functions in problem 1 having
 *       polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
      s1 = s2

(* put your solutions for problem 1 here *)

(* 1-a *)
(* input: "2", ["1","2"]
 * output: SOME ["1"]
 * *)
fun all_except_option(str, str_list) =
  let 
    fun is_in(str, str_list,find,left) = 
      case str_list of
           [] => if find=false then NONE else SOME left 
         | head::tail => if same_string(head,str) 
                         then is_in(str, tail, true, left)
                         else is_in(str, tail, find, head::left)
  in 
    is_in(str,str_list,false,[])
    (* note that the list order is reversed*)
  end

                     
(* 1-b *)
fun get_substitutions1(listss, str) = 
  case listss of
       [] => []
    (* head: string list *)
     | head::tail => case  all_except_option(str, head) of
                          NONE => get_substitutions1(tail, str)
                        | SOME left => left @ get_substitutions1(tail, str)
(* 1-c *)
fun get_substitutions2(listss, str) = 
  let 
    fun helper(listss, str, ret) = 
      case listss of
           [] => ret
         | head::tail => case all_except_option(str, head) of
                              NONE => helper(tail,str,ret)
                            | SOME left => helper(tail,str,left @ ret)
  in
      helper(listss, str, [])
  end

(* 1-d *)
fun similar_names(listss, full_name) = 
  let
    (*
    val first_names = get_substitutions2(listss, #first full_name)
    val middle_name = #middle full_name 
    val last_name = #last full_name 
     *)
    val {first=f,middle=middle_name,last=last_name} = full_name;
    val first_names = get_substitutions2(listss, f);
    fun helper(names,ret) = 
      case names of 

           [] => ret
         | name::tail => 
           helper(tail, {first=name, middle = middle_name,last=last_name }::ret)
  in 
    full_name::helper(first_names, []) 
  end
  

(* you may assume that Num is always used with values 2, 3, ..., 10
  *    though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank
datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(* 2-a *)
fun card_color card = 
  case card of
       (Spades, _) => Black 
     | (Clubs,_) => Black 
     | (Diamonds,_) => Red 
     | (Hearts,_) => Red 

(* 2-b *)
fun card_value card = 
  case card of
       (_, Ace) => 11
     | (_, Num n) => n
     | _ => 10

(* 2-c *)
fun remove_card(cs, c, e) = (* cs: card list, c: card, e: exception *)
  let 
    fun help(card_list, some_card, ret, has_found) = 
      case card_list of
           [] => if has_found = false then NONE else SOME ret 
         | head::tail => if head = some_card andalso has_found = false (* first
         time to see some_card *)
                         then help(tail, some_card, ret, true)
                         else help(tail, some_card, head::ret, has_found)
  in 
    case help(cs, c, [], false) of
         NONE => raise e
       | SOME ret => ret 
  end

(* 2-d *)
fun all_same_color cards = 
    case cards of
       [] => true 
    (* [_] => true *)
     | _::[] => true 
     | a::(b::tail) => if card_color(a) = card_color(b) then all_same_color(b::tail) else false

(* 2-e *)
fun sum_cards cards = 
  let 
    fun sum(nums,ret) = 
      case nums of
           [] => ret
         | head::tail => sum(tail,head+ret)
    fun values(cards, ret) = 
      case cards of 
           [] => ret
         | head::tail => values(tail, card_value(head)::ret)
      
  in 
    sum(values(cards,[]),0)
  end

(* 2-f *)
fun score(cards, goal) = 
  let
    val sum = sum_cards(cards)
    val pre_score = if sum > goal then 3*(sum-goal) else goal-sum
  in
    if all_same_color(cards)
    then pre_score div 2
    else pre_score
  end

(* 2-g *)
(* card_list: all the cards in front of the player 
 * move_list: the given operation sequences, which could be Draw of Discard(some
 * card)
 * *)

fun officiate(card_list, move_list, goal) = 
  let
    fun state(card_list, move_list, goal,held_list) = 
      case move_list of
           [] => score(held_list, goal)
         | Draw::next_movements => (case card_list of
                                      [] => score(held_list,goal)
                                      | head::left_cards => if sum_cards(held_list) +
                                                    card_value(head) > goal
                                                  then score(head::held_list, goal) 
                                                  else state(left_cards, next_movements, goal,head::held_list))
        | (Discard card)::next_movements => 
            let 
              fun is_in(cards, c) = 
                case cards of
                     [] => false
                   | head::tail => if head = c then true else
                     is_in(tail,c)

            in
              if false = is_in(held_list, card)
              then raise IllegalMove 
              else state(card_list, next_movements, goal, remove_card(held_list, card, IllegalMove))
            end

  in
      state(card_list, move_list, goal, [])
  end
