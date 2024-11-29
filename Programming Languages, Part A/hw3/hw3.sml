exception NoAnswer


fun only_capitals lst = List.filter (fn s => Char.isUpper(String.sub(s,0))) lst
	
fun longest_string1(strings: string list) = 
	List.foldl (fn (x, acc) => if String.size(x) > String.size(acc) then x else acc) "" strings

fun longest_string2(strings: string list) =
	List.foldl (fn (x, acc) => if String.size(x) >= String.size(acc) then x else acc) "" strings

fun longest_string_helper compare strings = 
	List.foldl (fn (x, acc) => if compare(String.size(x),String.size(acc)) then x else acc) "" strings

val longest_string3 = longest_string_helper(fn (x, y) => x > y)

val longest_string4 = longest_string_helper(fn (x, y) => x >= y)


val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o List.rev o String.explode


fun first_answer f lst =
    case lst of
        [] => raise NoAnswer
        |x::xs => case f(x) of
                    SOME v => v
                    |NONE => first_answer f xs


fun all_answers f list = 
	let 
		fun helper(list, result) =
			case list of 
				[] => SOME result
				|x::xs =>
					case f(x) of
						SOME y => helper(xs, result @ y)
						|NONE => NONE
	in
		helper(list, [])
	end


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

(* 这个函数很巧妙 *)
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


fun count_wildcards(p: pattern) =
	g (fn () => 1)
	  (fn _ => 0)
	  p


fun count_wild_and_variable_lengths(p: pattern) =
	g (fn () => 1)
	  String.size
	  p


fun count_some_var(str: string, p: pattern) =
	g (fn () => 0)
	  (fn x => if x = str then 1 else 0)
	  p


fun check_pat pat =
    let
        fun get_vars pat =
            case pat of
                Variable s => [s]
              | TupleP pats => List.foldl (fn (p,acc) => (get_vars p) @ acc) [] pats
              | ConstructorP(_,p) => get_vars p
              | _ => [] 

        (* 检查列表中是否有重复 *)
        fun has_repeats xs =
            case xs of
                [] => false
              | x::xs' => List.exists (fn y => x = y) xs' orelse has_repeats xs'
    in
        not (has_repeats (get_vars pat))
    end


(* AI写的 *)
(* 1.理解一下这个函数的含义。
   2.学习一下什么叫模式匹配 *)
fun match(v, p) =
    case (v, p) of
        (_, Wildcard) => SOME []     
        |(_, Variable s) => SOME [(s, v)]
        |(Unit, UnitP) => SOME []
        |(Const n, ConstP m) => if m = n then SOME [] else NONE
        |(Tuple vs, TupleP ps) => 
            if List.length vs = List.length ps
            	then all_answers match (ListPair.zip(vs, ps))
            else NONE
        |(Constructor(s1,v1), ConstructorP(s2,p1)) =>
            if s1 = s2
            	then match(v1, p1)
            else NONE
        |_ => NONE


fun first_match v patterns =
	SOME (first_answer (fn p => match(v,p)) patterns)
	handle NoAnswer => NONE


(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

