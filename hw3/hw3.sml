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

fun only_capitals sl =
    List.filter (fn s => Char.isUpper (String.sub (s, 0))) sl

fun longest_string1 sl =
    foldl (fn (x, y) => if String.size y >= String.size x then y else x) "" sl
	  
fun longest_string2 sl =
    foldl (fn (x, y) => if String.size x >= String.size y then x else y) "" sl

fun longest_string_helper func sl =
    foldl (fn (x, y) => if func (String.size x, String.size  y) then x else y) "" sl

val longest_string3 = longest_string_helper (fn (x, y) => x  >  y)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

val longest_capitalized = longest_string1 o only_capitals;
						
val rev_string  = String.implode o List.rev o String.explode
						 
fun first_answer f lst =
    case lst of
	[] => raise NoAnswer
      | x :: lst' => case (f x) of
			 NONE => first_answer f lst'
		       | SOME v  => v
					
		
fun all_answers f lst =
    let
	fun helper_answers lst1 acc =
	    case lst1 of
		[] => SOME acc
	      | x :: lst1' => case (f x) of
				 NONE => NONE
			       | SOME v => helper_answers lst1' ( v  @  acc)
    in
	helper_answers lst []
		       end

fun count_wildcards p =
    g (fn u => 1) (fn s => 0) p
	    
fun count_wild_and_variable_lengths p =
    g (fn u => 1) (fn s => String.size s) p
      
fun count_some_var (str, p) =
    g (fn u => 0) (fn s => if s = str then 1 else 0) p

fun check_pat p =
    let
	fun pattern_to_los p1 =
	    case p1 of
		Variable x  =>  [x]
	       |TupleP ps  =>  List.foldl (fn (p, i) => pattern_to_los p @ i) [] ps
	       |ConstructorP(_, p)  => pattern_to_los p
	       | _ => []					      
	fun has_duplicates los =
	    case los of
		[] => false
	      | s :: los' => (List.exists (fn x => s = x) los') orelse has_duplicates los'
    in
	not (has_duplicates (pattern_to_los p))
    end
	
fun match (v, p) =
    case p of
	Wildcard => SOME []
      | Variable s =>SOME  [(s, v)]
      | UnitP => (case v of
		      Unit => SOME []
		    | _ => NONE)
      | ConstP i => (case v of
			 Const x => if x = i then SOME [] else NONE
		       | _ => NONE)
			
      | TupleP ps =>  (case v of
			  Tuple vs => if List.length ps <> List.length vs then NONE
				      else all_answers (fn x => match (#1 x, #2 x)) (ListPair.zip (vs, ps))
			| _ => NONE)
      |ConstructorP (s1, p) => (case v of
				    Constructor (s2, v) => if s1 = s2 then match (v, p) else NONE
				  | _ => NONE) 
				   
fun first_match v ps =
    SOME ( first_answer (fn p => match (v, p)) ps)
    handle NoAnswer => NONE
		     
    
	
			 
	
