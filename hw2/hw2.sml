(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
	     
fun all_except_option (str, los) =
    let
	fun helper_with_acc (los1, acc) =
	    case los1 of
		[] => NONE
	      | s::los1' => if same_string(str, s) 
			    then SOME ( acc @ los1')
			    else helper_with_acc (los1', s::acc)
    in
	helper_with_acc (los, [])
		      end

fun get_substitutions1 (losl, str) =
    case losl of
	[] => []
     | los:: losl' => case all_except_option(str, los) of
				     NONE => get_substitutions1 (losl', str) 
				   | SOME lst => lst @ get_substitutions1 (losl', str)

fun get_substitutions2 (losl, str) =
    let
	fun helper_with_acc (losl1, rsf) =
	    case losl1 of
		[] => rsf
	       |los :: losl1'  => case all_except_option (str, los) of
				      NONE => helper_with_acc (losl1', rsf) 
				    | SOME lst => helper_with_acc (losl1', rsf @ lst)
    in helper_with_acc(losl, [])
		      end
								  
fun similar_names (loss, {first=f, middle=m, last=l}) =
    let
	val subs = get_substitutions2 (loss, f)
	fun helper_with_acc (los, rsf) =
	    case los of
		[] => rsf
	      (*have to do rsf @ [x] instead of x :: rsf to keep the order for the autograder, maybe order matters idk *)
	      | s::los' => helper_with_acc(los', rsf @ [{first=s, middle=m, last=l}]) 
    in
	helper_with_acc(subs, [{first=f, middle=m, last=l}])
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
fun card_color (s, r) =
    if s = Clubs orelse s = Spades
    then Black
    else Red

fun card_value (s, r) =
    case r of
	Num i => i 
     | Ace => 11 
     | _ => 10
		       
fun remove_card (cs, c, e) =
    let
	fun helper_with_acc (cs1, acc) =
	    case cs1 of
		[] => raise e 
	      | card::cs1' => if card = c
			     then acc @ cs1'
			     else helper_with_acc(cs1', card::acc)
    in
	helper_with_acc (cs, [])
    end
	
fun all_same_color cs =
    case cs of
	[] => true
     | c::[] => true 
     | c1::c2::rest =>  card_color(c1) = card_color(c2) andalso all_same_color (c2::rest)
									       
fun sum_cards cs =
    let
	fun helper_with_acc (cs1, acc) =
	    case cs1 of
		[] => acc 
	      | c::cs1' =>  helper_with_acc(cs1' , card_value(c) + acc)
    in
	helper_with_acc (cs, 0)
    end
	
fun score (cs, goal) =
    let
	val sum = sum_cards cs
	val same_color = all_same_color cs
	val prel_score = if goal >= sum then (goal - sum) else 3 * (sum - goal)
    in
	if same_color then prel_score div 2 else prel_score
    end
	
fun officiate (cs, ms, goal) =
    let
	fun helper_with_acc (cs1, ms1, acc, e) =
	    case ms1 of
		[] => score (acc, goal)
	      | m::ms1' => case m of
			       Draw => (case cs1 of
					   [] => score (acc, goal)
					 | c::cs1' => if sum_cards(c::acc) > goal
						      then score (c::acc, goal)
						      else helper_with_acc (cs1', ms1', c::acc, e))
			     | Discard c => helper_with_acc (cs1, ms1', remove_card (acc, c, e), e)
    in
	helper_with_acc (cs, ms, [], IllegalMove)
    end

(* A million helpers for careful_player and its helpers - probably there is a much easier way, but this is my clumsy  work*)
	
(* consume a hand e.g. [card1, card2, card3], produce a list of hands like [ [card1, card2], [card1, card3], [card2, card3] ] *)	
fun discard_each cs =
    let
	fun helper_with_accs (cs1, acc, loh) =
	    case cs1 of
		[] => loh
	      | c::cs1' => helper_with_accs (cs1', c::acc, (acc @ cs1')::loh)
    in
	helper_with_accs (cs, [], [])
    end
(* consume a card and a list of hands like [hand1, hand2, ..., handn], produce a list of hands like [c::hand1, c::hand2...]*)
fun add_card_to_each (c, loh) =
    let
	fun helper_with_acc (loh1, acc) =
	    case loh1 of
		[] => acc
	      | hand::loh1' => helper_with_acc (loh1', (c::hand)::acc)
    in
	helper_with_acc (loh, [])
    end
(* Given a goal and list of hands, return SOME (winning hand), NONE if there's no winning hand*)
fun find_win (goal, loh) =
    case loh of
	[] => NONE
      | hand::loh' =>  if score (hand, goal) = 0
		       then SOME hand
		       else find_win (goal, loh')
(*Helper for find_discarded *)
fun is_in (c, hand) =
    case hand of
	[] => false
      | card::hand' => c = card orelse is_in (c, hand')
					     				     
exception ThatCouldntHappen (* A special exception for find_discarded *)
(*now I have an original hand and a winning hand, I need to find a card I discarded from the original hand*)
fun find_discarded (h1, h2) =
    case h1 of
	[] => raise ThatCouldntHappen
      | c::h1' => if is_in (c, h2)
		  then find_discarded(h1', h2)
		  else c
			   
				     
fun discard_and_draw (hand, cs, goal) =
    if null hand then NONE
    else
	case cs of
	    [] => NONE
	  | c::cs' => let
	     val  new_hand = find_win (goal, add_card_to_each (c, discard_each hand))
	  in
	      case new_hand of
		  NONE => NONE
		| SOME h => SOME (find_discarded (hand, h))
	  end
		      
	      
fun careful_player (cs, goal) =
    let
	fun helper_with_accs (cs1, ms, hand) =
	    if score (hand, goal) = 0
	    then ms
	    else
		case discard_and_draw (hand, cs1, goal) of
		    NONE => let val diff = goal -  sum_cards (hand)
			    in
				case cs1 of
				    [] => if diff > 10
					  then ms @ [Draw]
					  else ms
				  | c::cs1' => if diff > 10
					       then helper_with_accs (cs1', ms @ [Draw], c::hand)
					       else ms
			    end
		   |SOME c  => ms @ (Discard c::Draw::[])
    in
	helper_with_accs (cs, [], [])
    end
	
		
	
							    
			   
