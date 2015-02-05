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

(* pipe operator *)
infix |>
fun x |> f = f x

(* 1 *)
fun only_capitals strings =
    List.filter (fn s => String.sub(s, 0) |> Char.isUpper) strings

(* 2 *)
fun longest_string1 strings =
    foldl (fn (v, acc) => if String.size v > String.size acc then v else acc) "" strings

(* 3 *)
fun longest_string2 strings =
    foldl (fn (v, acc) => if String.size v >= String.size acc then v else acc) "" strings

(* 4 *)
fun longest_string_helper cmp strings =
    foldl (fn (v, acc) => if cmp(String.size v,String.size acc) then v else acc) "" strings

val longest_string3 = longest_string_helper (fn (x,y) => x > y)
val longest_string4 = longest_string_helper (fn (x,y) => x >= y)

(* 5 *)
val longest_capitalized = longest_string1 o only_capitals

(* 6 *)
val rev_string = implode o rev o explode

(* 7 *)
fun first_answer f l =
    case l |> map f |> List.filter isSome of
      []         => raise NoAnswer
    | SOME(x)::_ => x

(* 8 *)
fun all_answers f l =
    l |> map f |> foldl (fn (x, acc) => case (x,acc) of
                              (NONE,_) => NONE
                            | (_,NONE) => NONE
                            | (SOME(x),SOME(y)) => SOME(y @ x)) (SOME([]))

(* 9a *)
val count_wildcards = g (fn _ => 1) (fn _ => 0)

(* 9b *)
fun count_wild_and_variable_lengths p =
    count_wildcards p + g (fn _ => 0) (fn x => String.size x) p

(* 9c *)
fun count_some_var (s, p) =
    g (fn _ => 0) (fn x => if x = s then 1 else 0) p

(* 10 *)
fun check_pat p =
    let
        fun dups l = case l of
          []    => false
        | x::xs => if List.exists (fn v => x = v) xs
                   then true
                   else dups xs
        fun names p = case p of
          Variable x        => [x]
        | TupleP ps         => List.foldl (fn (p,acc) => names p @ acc) [] ps
        | ConstructorP(_,p) => names p
        | _                 => []
    in
        not (p |> names |> dups)
    end

(* 11 *)
fun match (value,pat) =
    let
        fun matchaux (value, pat) =
        case (value, pat) of
          (_, Wildcard)   => SOME([])
        | (v, Variable s) => SOME([(s, v)])
        | (Unit, UnitP)   => SOME([])
        | (Const x, ConstP y) => if x = y then SOME([]) else NONE
        | (Tuple vs, TupleP ps) => if length vs <> length ps then NONE else
            all_answers matchaux (ListPair.zip(vs, ps))
        | (Constructor(s1, v), ConstructorP(s2, p)) => if s1 <> s2 then NONE else
            all_answers matchaux [(v, p)]
        | _ => NONE
    in
        if check_pat pat
        then matchaux(value, pat)
        else NONE
    end

(* 12 *)
fun first_match v ps =
    SOME(first_answer (fn p => match(v,p)) ps) handle NoAnswer => NONE
