(* Igor G. Peternella - assignment 2 *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
  s1 = s2
           
(* ex 1a *)
(*

fun all_except_option (str, str_lst) =
  let fun same_length (lst1, lst2) =
        case (lst1, lst2) of
            ([], [])             => true (* same length *)
           |(hd1::tl1, hd2::tl2) => same_length(tl1, tl2)
           | _                   => false
      fun remove_str (xs) =
        case xs of
            []       => []
          | hd :: tl => if same_string(hd, str)
                        then remove_str(tl) (* dont append if str has been found *)
                        else hd :: remove_str(tl)
      val result_lst = remove_str(str_lst)
  in
      if same_length(result_lst, str_lst) (* same length means the str was not found*)
      then NONE
      else SOME result_lst 
  end
*)
           
(* string * string list -> OPTION string list *)
fun all_except_option (str, str_lst) =
  let fun remove_str (str_lst, removed) =
        case str_lst of
            []       => if removed then SOME [] else NONE
          | hd :: tl => if same_string(hd, str)
                        then case remove_str(tl, true) of
                                 NONE     => NONE 
                               | SOME lst => SOME lst
                        else case remove_str(tl, removed) of
                                 NONE     => NONE
                               | SOME lst => SOME (hd :: lst)
  in
      remove_str(str_lst, false)
  end

(* ex 1b *)
(* string list list -> string list *)
fun get_substitutions1 (str_lsts, s) =
  case str_lsts of
      [] => []
    | str_lst :: str_lsts_tl => case all_except_option(s, str_lst) of
                                    NONE => get_substitutions1(str_lsts_tl, s)
                                  | SOME lst => lst @ get_substitutions1(str_lsts_tl, s)
                                                                        
(* ex 1c *)
fun get_substitutions2 (str_lsts, s) =
  let fun aux (xs, acc) =
        case xs of
            [] => acc @ []
          | str_lst :: str_lsts_tl => case all_except_option(s, str_lst) of
                                          NONE => aux(str_lsts_tl, acc)
                                        | SOME lst => aux(str_lsts_tl, acc @ lst)
  in
      aux(str_lsts, [])
  end

(* ex 1d *)
fun similar_names (str_lsts, {first=x, middle=y, last=z}) =
  let fun create_names(names_lst) =
        case names_lst of
            []       => []
          | hd :: tl => {first=hd, middle=y, last=z} :: create_names(tl)
  in
      {first=x, middle=y,last=z} :: create_names(get_substitutions2(str_lsts, x))
  end

(* you may assume that Num is always used with values 2, 3, ..., 10
       though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
                                      
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* ex 2a *)
fun card_color card =
  case card of
      (Clubs, _)  => Black
    | (Spades, _) => Black
    | _           => Red

(* ex 2b *)
fun card_value card =
  case card of
      (_, Num x) => x
    | (_, Ace)   => 11
    | _          => 10

(* ex 2c *)
fun remove_card (cs, c, e) =
  let fun aux(cs, removed) =
        case cs of
            [] => if removed then [] else raise e
          | crd :: tl => if crd = c (* compares card *)
                         then aux(tl, true)
                         else crd :: aux(tl, removed)
  in
      aux(cs, false)
  end
      
(* ex 2d *)
(* fun all_same_color cs = *)
  

      
                                     











      








                           
(* tests *)
(*                                     
val test1 = all_except_option ("string", ["string"]) = SOME []

val test2 = get_substitutions1 ([["foo"],["there"]], "foo") = []

val test3 = get_substitutions2 ([["foo"],["there"]], "foo") = []

val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

val test5 = card_color (Clubs, Num 2) = Black

val test6 = card_value (Clubs, Num 2) = 2

val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []

val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true

val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4

val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4

val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)
             
                                                  

      
*)
