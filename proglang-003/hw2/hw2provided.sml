(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* a *)
fun contains(elem : ''a, l : ''a list) =
    case l of
        []    => false
      | x::xs => if x = elem then true else contains(elem, xs)

fun all_except_option(s, strings) =
    let
        fun all_but(s, strings) =
            case strings of
                []    => []
              | x::xs => if same_string(x, s) then all_but(s, xs) else x::all_but(s, xs)
    in
        if not (contains(s, strings))
        then NONE
        else SOME(all_but(s, strings))
    end

(* b *)
fun get_substitutions1(subs, s) =
    case subs of
        []    => []
      | x::xs => case all_except_option(s, x) of
                    NONE     => get_substitutions1(xs, s)
                  | SOME(ss) => ss @ get_substitutions1(xs, s)

(* c *)
fun get_substitutions2(subs, s) =
    let
        fun aux(subs, acc) =
            case subs of
                []    => acc
              | x::xs => case all_except_option(s, x) of
                            NONE     => aux(xs, acc)
                          | SOME(ss) => aux(xs, acc @ ss)
    in
        aux(subs, [])
    end

(* d *)
fun similar_names(subs, fullName : {first:string, middle:string, last:string}) =
    let
        val {first=firstName,middle=_,last=_} = fullName
        val nameSubs = get_substitutions1(subs, firstName)
        fun build(names) =
            let
                val {first=_,middle=middle,last=last} = fullName
            in
                case names of
                    []    => []
                  | x::xs => {first=x,middle=middle,last=last} :: build(xs)
            end
    in
        fullName :: build(nameSubs)
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

(* a *)
fun card_color(card) =
    case card of
          (Clubs, _)  => Black
        | (Spades, _) => Black
        | otherwise   => Red

(* b *)
fun card_value(card) =
    case card of
        (_, Num x) => x
      | (_, Ace)   => 11
      | otherwise  => 10

(* c *)
fun remove_card(cs, c, e) =
    case cs of
         []    => raise e
       | x::xs => if x = c then xs else x::remove_card(xs, c, e)

(* d *)
fun all_same_color(cs) =
    case cs of
         [] => true
       | x::y::xs => if card_color(x) = card_color(y) then all_same_color(y::xs) else false
       | otherwise => true

(* e *)
fun sum_cards(cs) =
    let
        fun sum(cs, acc) =
            case cs of
                 []    => acc
               | x::xs => sum(xs, card_value(x) + acc)
    in
        sum(cs, 0)
    end

(* f *)
fun score(cs, goal) =
    let
        val sum = sum_cards(cs)
        val prelim =
            if sum > goal
            then 3 * (sum - goal)
            else goal - sum
    in
        if all_same_color(cs)
        then prelim div 2
        else prelim
    end

(* g *)
fun officiate(cards, moves, goal) =
    let
        fun aux(cards, moves, held) =
            case moves of
                []    => score(held, goal)
              | m::ms => case m of
                           Discard(c) => aux(cards, ms, remove_card(held, c, IllegalMove))
                         | Draw       => case cards of
                                             []    => score(held, goal)
                                           | c::cs => if sum_cards(c::held) > goal
                                                      then score(c::held, goal)
                                                      else aux(cs, ms, c::held)
    in
        aux(cards, moves, [])
    end

