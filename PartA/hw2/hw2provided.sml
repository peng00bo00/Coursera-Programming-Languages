(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* 1.a *)
fun all_except_option (str, []) = NONE
  | all_except_option (str, x::xs) =
    case (same_string(str, x), all_except_option(str, xs)) of (true, _) => SOME(xs)
							    | (false, NONE) => NONE
							    | (false, SOME(lst)) => SOME(x::lst)
(*1.b*)
fun get_substitutions1 ([], str) = []
  | get_substitutions1 (x::xs, str) =
    case all_except_option(str, x) of NONE => get_substitutions1(xs, str)
				    | SOME(lst) => lst @ get_substitutions1(xs, str)
(*1.c*)
fun get_substitutions2 ([], str) = []
  | get_substitutions2 (substitutions, str) =
    let
	(* use strings to save substitutions to record*)
	fun helper ([], strings) = strings
	  | helper (x::xs, strings) =
	    case all_except_option(str, x) of NONE => helper(xs, strings)
					    | SOME(lst) => helper(xs, strings @ lst)
    in
	helper(substitutions, [])
    end
(*1.d*)
fun similar_names (substitutions, {first, middle, last}) =
    let
	fun helper ([], str) = str
	  | helper (x::xs, str) = helper(xs, {first=x, middle=middle, last=last} :: str)
    in
	helper(first::get_substitutions1(substitutions, first), [])
    end
											      

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
(* 2.a *)
fun card_color card =
    case card of (Clubs, _)  => Black
	       | (Diamonds, _) => Red
	       | (Hearts, _) => Red
	       | (Spades, _) => Black

(* 2.b *)
fun card_value card =
    case card of (_, Ace) => 11
	       | (_, Num i) => i
	       | (_, _) => 10
(* 2.c *)
fun remove_card ([], c, e) = raise e
  | remove_card (x::xs, c, e) =
    case x = c
     of true => xs
      | false => x::remove_card(xs, c, e)
			       
(* 2.d *)
fun all_same_color cs =
    case cs of [] => true
	     | c::[] => true
	     | c::c'::cs' => card_color(c)=card_color(c') andalso all_same_color(c'::cs')
(* 2.e *)
fun sum_cards [] = 0
  | sum_cards cs =
    let
	fun helper ([], num) = num
	  | helper (c::cs, num) = helper(cs, num+card_value(c))
    in
	helper(cs, 0)
    end
(* 2.f *)
fun score (cs, goal) =
    let
	val sum = sum_cards cs
	fun preliminary_score (sum) =
	    case sum > goal of true => 3 * (sum - goal)
			        | _ => goal - sum
    in
	case all_same_color cs of true => preliminary_score(sum) div 2
			        | _ => preliminary_score(sum)
    end
(* 2.g *)
fun officiate (cs, ms, goal) =
    let
	fun step (cs, [], hs) = score(hs, goal)
	  | step (cs, m::ms, hs) =
	    case (cs, m) of ([], Draw) => score(hs, goal)
			  | (c::cs', Draw) => if sum_cards(hs) + card_value(c) > goal
					      then score(c::hs, goal)
					      else step(cs', ms, c::hs)
		          | (_, Discard c) => step(cs, ms, remove_card(hs, c, IllegalMove))
    in
	step(cs, ms, [])
    end
(* 3.a *)
fun score_challenge (cs, goal) =
    let
	fun preliminary_score (sum) =
	    case sum > goal of true => 3 * (sum - goal)
			        | _ => goal - sum
	
	(* find the least possible socre for Ace *)
	fun possible_score ([], sum, score) = score
	  | possible_score (c::cs', sum, score) =
	    case c of (_, Ace) => if preliminary_score(sum-10) < score then possible_score(cs', sum-10, preliminary_score(sum-10))
				                  else score
		    | (_, _) => possible_score(cs', sum, score)
	
	val score = possible_score(cs, sum_cards(cs), preliminary_score(sum_cards(cs)))
    in
	case all_same_color cs of true => score div 2
			        | _ => score
    end

fun officiate_challenge (cs, ms, goal) =
    let
	fun step (cs, [], hs) = score_challenge(hs, goal)
	  | step (cs, m::ms, hs) =
	    case (cs, m) of ([], Draw) => score_challenge(hs, goal)
			  (**)
			  | (c::cs', Draw) => if sum_cards(hs) + card_value(c) > goal
					      then score_challenge(c::hs, goal)
					      else step(cs', ms, c::hs)
		          | (_, Discard c) => step(cs, ms, remove_card(hs, c, IllegalMove))
    in
	step(cs, ms, [])
    end

(* 3.b *)
fun careful_player (cs, goal) =
    let
	(* whether to discard and draw *)
	fun discard ([], c, value) = NONE
	  | discard (h::hs, c, value) =
	    if value - card_value(h) + card_value(c) = goal
	    then SOME([Discard h, Draw])
	    else discard(hs, c, value)
	
	(* move for each step *)
	fun step (cs, ms, hs) =
	    case (cs, score(hs, goal)=0) of (_, true) => ms
				          | ([], false) => ms
					  | (c::cs', false) => if goal - sum_cards(hs) > 10
							       then step(cs', ms @ [Draw], hs @ [c])
							       else case discard(hs, c, sum_cards(hs)) of NONE => ms
													| SOME(m) => step(cs', ms @ m, hs @ [c])
    in
	step(cs, [], [])
    end
