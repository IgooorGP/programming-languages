(* Igor G. Peternella - assignment 2 *)

(* Datatype bindings *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
                                      
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove
              
fun same_string(s1 : string, s2 : string) =
  s1 = s2
              
(* ex 1a:  Write a function all_except_option, which takes a string and a string list. Return NONE if the
string is not in the list, else return SOME lst where lst is identical to the argument list except the string
is not in it. You may assume the string is in the list at most once. Use same_string, provided to you,
to compare strings. *)
           
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

(* ex 1b: Write a function get_substitutions1, which takes a string list list (a list of list of strings, the
substitutions) and a string s and returns a string list. The result has all the strings that are in
some list in substitutions that also has s, but s itself should not be in the result. Assume each list
in substitutions has no repeats. The result will have repeats if s and another string are both in more than
one list in substitutions *)
      
fun get_substitutions1 (str_lsts, s) =
  case str_lsts of
      [] => []
    | str_lst :: str_lsts_tl => case all_except_option(s, str_lst) of
                                    NONE => get_substitutions1(str_lsts_tl, s)
                                  | SOME lst => lst @ get_substitutions1(str_lsts_tl, s)
                                                                        
(* ex 1c: Write a function get_substitutions2, which is like get_substitutions1 except it uses a tail-recursive
local helper function *)
                                                                        
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

(* ex 1d: Write a function similar_names, which takes a string list list of substitutions (as in parts (b) and
(c)) and a full name of type {first:string,middle:string,last:string} and returns a list of full
names (type {first:string,middle:string,last:string} list). The result is all the full names you
can produce by substituting for the first name (and only the first name) using substitutions and parts (b)
or (c). The answer should begin with the original name (then have 0 or more other names). Do not
eliminate duplicates from the answer. *)

fun similar_names (str_lsts, {first=x, middle=y, last=z}) =
  let fun create_names(names_lst) =
        case names_lst of
            []       => []
          | hd :: tl => {first=hd, middle=y, last=z} :: create_names(tl)
  in
      {first=x, middle=y,last=z} :: create_names(get_substitutions2(str_lsts, x))
  end

(* A game is played with a card-list and a goal. The player has a list of held-cards, initially empty. The player
makes a move by either drawing, which means removing the first card in the card-list from the card-list and
adding it to the held-cards, or discarding, which means choosing one of the held-cards to remove. The game
ends either when the player chooses to make no more moves or when the sum of the values of the held-cards
is greater than the goal.
The objective is to end the game with a low score (0 is best). Scoring works as follows: Let sum be the sum
of the values of the held-cards. If sum is greater than goal, the preliminary score is three times (sum-goal),
else the preliminary score is (goal - sum). The score is the preliminary score unless all the held-cards are
the same color, in which case the score is the preliminary score divided by 2 (and rounded down as usual
with integer division; use ML's div operator). *)
      
(* ex 2a: Write a function card_color, which takes a card and returns its color (spades and clubs are black,
diamonds and hearts are red). Note: One case-expression is enough. *)
      
fun card_color card =
  case card of
      (Clubs, _)  => Black
    | (Spades, _) => Black
    | _           => Red

(* ex 2b: Write a function card_value, which takes a card and returns its value (numbered cards have their
number as the value, aces are 11, everything else is 10). Note: One case-expression is enough. *)
                         
fun card_value card =
  case card of
      (_, Num x) => x
    | (_, Ace)   => 11
    | _          => 10

(* ex 2c: Write a function remove_card, which takes a list of cards cs, a card c, and an exception e. It returns a
list that has all the elements of cs except c. If c is in the list more than once, remove only the first one.
If c is not in the list, raise the exception e. You can compare cards with =. *)
                        
fun remove_card (cs, c, e) =
  let fun aux(cs, removed) =
        case cs of
            [] => if removed then [] else raise e
          | crd :: tl => if crd = c andalso not removed (* removes ONLY the first match *)
                         then aux(tl, true)
                         else crd :: aux(tl, removed)
  in
      aux(cs, false)
  end
      
(* ex 2d:  Write a function all_same_color, which takes a list of cards and returns true if all the cards in the
list are the same color. Hint: An elegant solution is very similar to one of the functions using nested
pattern-matching in the lectures. *)
      
fun all_same_color cs =
  case cs of
      hd :: (nk :: []) => card_color(hd) = card_color(nk) (* two cards only *)
    | hd :: (nk :: tl) => let val is_previous_same = all_same_color(nk :: tl)
                          in 
                              is_previous_same andalso (card_color(hd) = card_color(nk))
                          end
    | _ => true
               
(* ex 2e: Write a function sum_cards, which takes a list of cards and returns the sum of their values. Use a 
locally defined helper function that is tail recursive. *)
               
fun sum_cards cs =
  let fun aux(cs, acc) =
        case cs of
            []       => acc
          | hd :: tl => aux(tl, acc + (card_value hd)) (* tail call --> no aux(tl) + card_value(hd) *)
  in         
      aux(cs, 0)
  end

(* ex 2f: Write a function score, which takes a card list (the held-cards) and an int (the goal) and computes
the score as described above. *)
      
fun score (cs, goal) =
  let
      val sum = sum_cards cs
      val pre_score = if sum > goal then 3 * (sum - goal)  (* game over: exceeded goal *)
                      else goal - sum                      (* game is on *)
  in 
      if (all_same_color cs) then pre_score div 2 
      else pre_score      
  end

(* ex 2g: Write a function officiate, which "runs a game." It takes a card list (the card-list) a move list
(what the player does at each point), and an int (the goal) and returns the score at the end of the
game after processing (some or all of) the moves in the move list in order. Use a locally defined recursive
helper function that takes several arguments that together represent the current state of the game. As
described above:

1. The game starts with the held-cards being the empty list.
2. The game ends if there are no more moves. (The player chose to stop since the move list is empty.)
3. If the player discards some card c, play continues (i.e., make a recursive call) with the held-cards
not having c and the card-list unchanged. If c is not in the held-cards, raise the IllegalMove
exception.
4. If the player draws and the card-list is (already) empty, the game is over. Else if drawing causes
the sum of the held-cards to exceed the goal, the game is over (after drawing). Else play continues
with a larger held-cards and a smaller card-list. *)
                                                
fun officiate (card_lst, mv_lst, goal) =
  let fun update_game (card_lst, mv_lst, held_cards) =
        if (sum_cards held_cards) > goal
        then score(held_cards, goal) (* game is over: exceeded the goal *)
        else case mv_lst of
                 [] => score(held_cards, goal) (* game is over: no more moves *)
               | (Discard card) :: mv_tl => update_game(card_lst, mv_tl, remove_card(held_cards, card, IllegalMove))
               | (Draw) :: mv_tl => case card_lst of
                                        [] => score(held_cards, goal) (* game is over: no more cards to draw *)
                                      | card :: card_tl => update_game(remove_card(card_lst, card, IllegalMove), mv_tl, card::held_cards) (* adds new card *)
  in
      update_game(card_lst, mv_lst, []) (* game starts with an empty hand *)
  end
