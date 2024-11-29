(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(*答案做法*)
fun all_except_option(string, list) = 
	case list of
		[] => NONE
		|x::xs =>
			if same_string(x, string)
				then SOME xs
			else
				case all_except_option(string, xs) of
					NONE => NONE
					|SOME res => SOME(x::res)



fun get_substitutions1(list: string list list, s: string) =
    case list of
		[] => []
    	|(x::xs) =>
	    	case all_except_option(s, x) of
				NONE => get_substitutions1(xs, s)
	       		|SOME list => list @ get_substitutions1(xs, s)



(* the version of tail recursion *)
fun get_substitutions2(list: string list list, s: string) =
    let 
	fun recurHelper(list: string list list, result: string list) =
	    case list of
		[] => result
               |(x::xs) =>
	            case all_except_option(s, x) of
		        NONE => recurHelper(xs, result)
		       |SOME list => recurHelper(xs, result @ list)
    in
	recurHelper(list, [])
    end



(* 答案思路 *)
fun similar_names(substitutions: string list list, fullName: {first:string, middle:string, last:string}) =
    let
        val {first, middle, last} = fullName
        val possible_firsts = get_substitutions2(substitutions, first)
        
        fun recurHelper(names_list) =
            case names_list of
                [] => []
              | x::xs => {first=x, middle=middle, last=last} :: recurHelper(xs)
    in
        fullName :: recurHelper(possible_firsts)
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


(* 函数参数，有其他写的方法吗 *)
fun card_color((suit,_): card): color = 
    case suit of
	Spades => Black
       |Clubs => Black
       |_ => Red


fun card_value((_,rank): card) =
    case rank of
	Num n => n
       |Ace => 11
       |_ => 10

(*答案思路*)
fun remove_card(cards: card list, card: card, e: exn) =
	case cards of
		[] => raise e
		|x::xs =>
			if x = card 
				then xs
			else x::remove_card(xs, card, e)



(* 答案思路 *)
fun all_same_color(cards: card list) =
    case cards of
		[] => true
		|x::[] => true
		|h::n::tail => 
			card_color(h) = card_color(n) 
			andalso all_same_color(n::tail)



fun sum_cards(cards: card list) = 
    let
	fun recurHelper(cards, result) =
	    case cards of
		[] => result
	       |x::xs =>
		    recurHelper(xs, result+card_value(x))
		    
    in
		recurHelper(cards, 0)
    end



fun score(cards: card list, goal: int) =
    let 
		val sum = sum_cards(cards)
    in
	let
	    val preliminary_score = 
		if sum > goal
		    then 3 * (sum - goal)
		else 
		    goal - sum
	in
	    let
		val final_score = 
		    if all_same_color(cards)
			then preliminary_score div 2
		    else 
			preliminary_score
	    in
		final_score
	    end
	end
    end



fun officiate(card_list: card list, move_list: move list, goal: int) =
	let
	    fun roundRecur(card_list, move_list, held_cards, held_sum) =
		case move_list of
		    (* 统计手牌分数 *)
		    [] => score(held_cards, goal)
		   |(x::xs) => 
			case x of 
			    (* 丢弃 *)
			    Discard card =>
				roundRecur(card_list, xs, remove_card(held_cards, card, IllegalMove), held_sum-card_value(card))
			    (* 抽牌 *)
			   |Draw =>
				case card_list of
				    [] => score(held_cards, goal)
				   |(c::rest) =>
					if held_sum + card_value(c) > goal
					    then score(c::held_cards, goal)
					else
					    (* cardlist删除第一个元素，movelist删除第一个元素，heldcards增加该元素 *)
					    roundRecur(rest, xs, c::held_cards, held_sum + card_value(c))
	in
	    roundRecur(card_list, move_list, [], 0)
	end


	
	

