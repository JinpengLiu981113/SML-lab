structure CardProblem =
struct
open CardType
exception NYI
exception IllegalMove

fun hd cs = 
    case cs of 
        [] => raise IllegalMove
      | (x::_) => x

fun tl cs = 
    case cs of 
        [] => raise IllegalMove
      | (_::cs') => cs'

fun card_color c = 
    case c of 
        (Spades, _) => Black
    | (Clubs, _) => Black
    | (Diamonds, _) => Red
    | (Hearts, _) => Red

fun card_value c = 
    case c of 
        (_, Ace) => 11
    | (_, Jack) => 10
    | (_, Queen) => 10
    | (_, King) => 10
    | (_, Num (v)) => v

fun remove_card cs c = 
    case cs of 
        [] => raise IllegalMove
    | _ => let val x = hd cs
                val cs' = tl cs
            in if card_color x = card_color c andalso card_value x = card_value c then cs'
                else x::(remove_card cs' c)
            end

fun all_same_color cs = 
    let val x = hd cs
        val start_color = card_color x
        fun is_all_same_color (cs, bool) = 
            case cs of
                [] => bool
            | (x1::cs') => if card_color x1 = start_color then is_all_same_color(cs', true)
                            else is_all_same_color(cs', false)
    in is_all_same_color(cs, true)
    end

fun sum_cards cs = 
    case cs of 
        [] => 0
    | (x::cs') => (card_value x) + sum_cards cs'

fun score cs goal = 
    let val sum = sum_cards cs
        val preliminary = if sum > goal then 3 * (sum - goal) else (goal - sum)
    in if all_same_color cs then preliminary div 2
        else preliminary
    end

fun officiate (cs, moves, goal) = 
    let val hand = []
        fun handle_moves (cs, hand, moves) = 
            case moves of
                [] => hand 
            | _ => case hd moves of
                        Draw => if cs = [] then hand
                                else if (sum_cards hand) > goal then hand
                                else handle_moves ((tl cs), ((hd cs)::hand), (tl moves))
                    | Discard c => handle_moves (cs, (remove_card hand c), (tl moves))
        val hand_cards = handle_moves (cs, hand, moves)
    in 
        score hand_cards goal
    end
end