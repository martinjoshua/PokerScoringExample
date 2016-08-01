module Poker

open System.Collections.Generic

type Suit =
    | Spades
    | Diamonds
    | Clubs
    | Hearts

type Card =
    | Ace of Suit
    | King of Suit
    | Queen of Suit
    | Jack of Suit
    | Pip of int * Suit 

type Score =
    | RoyalFlush 
    // all other scores should contain the highest card
    // of the match - in case of a tie (doesn't apply to Royal flush).
    | StraightFlush of Card
    | FourOfAKind of Card
    | FullHouse of Card
    | Flush of Card
    | Straight of Card
    | ThreeOfAKind of Card
    | TwoPair of Card
    | Pair of Card
    | HighCard of Card

let CreateSuite = function 
    | 0 -> Spades
    | 1 -> Diamonds
    | 2 -> Clubs
    | 3 -> Hearts
    | _ -> failwith "cannot create suite"

let CreateCard number suite = 
    match number with 
    | 11 -> Jack(suite)
    | 12 -> Queen(suite)
    | 13 -> King(suite)
    | 14 -> Ace(suite)
    | i when 1 < i && i < 11 -> Pip(i, suite)
    | _ -> failwith "cannot create card"

// returns integer value of Card
let CardValue = function
    | Ace(s) -> 14
    | King(s) -> 13
    | Queen(s) -> 12
    | Jack(s) -> 11
    | Pip(i,s) -> i

// returns Suite of Card
let CardSuite = function
    | Ace(s) -> s
    | King(s) -> s
    | Queen(s) -> s
    | Jack(s) -> s
    | Pip(i,s) -> s

let CreateDeck = [ for s in 0..3 do for v in 2..14 -> CreateSuite s |> CreateCard v ]

// Input Card list, shuffles and outputs a Stack object
let ShuffleDeck cards = 
    let rnd = new System.Random()
    cards |> List.sortBy (fun i -> rnd.Next()) |> Stack
    
let DrawHand (deck: Stack<_>) = [ for i in 0..4 -> deck.Pop() ]
         
let GetCardString = function
    | Ace(s) -> sprintf "Ace of %s" (s.ToString())
    | King(s) -> sprintf "King of %s" (s.ToString())
    | Queen(s) -> sprintf "Queen of %s" (s.ToString())
    | Jack(s) -> sprintf "Jack of %s" (s.ToString())
    | Pip(i,s) -> sprintf "%i of %s" i (s.ToString())

let private GetRankedCard op cards = 
    cards 
        |> List.tail 
        |> List.fold (fun c1 c2 -> if op (CardValue c1) (CardValue c2) then c1 else c2) (List.head(cards))

let GetHighCard = List.ofSeq >> GetRankedCard (>)

let GetLowCard = List.ofSeq >> GetRankedCard (<)

// returns Some(Card) where Card is the highest card of the sequence, or None (not sequential)
let (|Sequential|_|) cards =
    // returns a tuple (isSequential, nextCard). Input is previous tuple match and next card. Note logic for Ace=1 or 14
    let folder (seq, c1) c2 = (CardValue c2) = (CardValue c1) - 1 || ((CardValue c1) = 14 && (CardValue c2) = 5), c2
    cards 
        |> List.sortByDescending CardValue
        |> function 
            | h :: tail -> 
                if tail 
                    |> List.scan folder (true, h) 
                    |> Seq.forall (fun (isSeq, _) -> isSeq) then cards |> GetHighCard |> Some else None
            | _ -> None
  
// returns Some(Card) where Card is the highest card of the match, or None (not one of a kind)
let (|OneOfAKind|_|) cards = 
    if cards 
        |> Seq.map CardSuite 
        |> Seq.distinct 
        |> Seq.length = 1 then cards |> GetHighCard |> Some else None

// i = num matching cards, k = number of matches 
// returns Some(Card) where Card is the highest card of the match, or None (no matches)
let (|HasMatch|_|) i k cards = 
    let matches = 
        cards 
            |> Seq.groupBy CardValue
            |> Seq.filter (fun (k, v) -> v |> Seq.length = i)        
    if matches |> Seq.length = k then matches |> Seq.head |> (fun (k, v) -> v |> Seq.head) |> Some else None

let GetHandScore (cards:Card list) = 
    match cards with
    | Sequential c & OneOfAKind _ -> if cards |> GetLowCard |> CardValue = 10 then RoyalFlush else StraightFlush c
    | HasMatch 4 1 c -> FourOfAKind c
    | HasMatch 3 1 c & HasMatch 2 1 _ -> FullHouse c
    | OneOfAKind c -> Flush c
    | Sequential c -> Straight c
    | HasMatch 3 1 c -> ThreeOfAKind c
    | HasMatch 2 2 c -> TwoPair c
    | HasMatch 2 1 c -> Pair c
    | _ -> cards |> GetHighCard |> HighCard


// EXTRAS

type Player = { Name: string; Hand: Card list }

let (|HighestCardValue|) (StraightFlush c|FourOfAKind c|FullHouse c|Flush c|Straight c|ThreeOfAKind c|TwoPair c|Pair c|HighCard c) = CardValue c

// returns tuple (player, score, integer val of highest card of score)
let private decompose player =
    let score = player.Hand |> GetHandScore
    (player, score, match score with | RoyalFlush -> 0 | HighestCardValue c -> c)

let GetWinningHand players =
    players 
    |> List.map decompose
    // sort by Score asc, then by highest card value desc
    |> List.sortBy (fun (p, s, c) -> s, -c) 
    |> List.map (fun (p, s, c) -> p) 
    |> List.head
