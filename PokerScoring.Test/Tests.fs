namespace PokerScoring.Test

open System.Collections.Generic
open Xunit
open Poker

module Tests =
    [<Fact>]
    let ``Can Create Standard 52 deck``() = 
        let deck = CreateDeck
        Assert.True(deck |> Seq.length = 52)

    [<Fact>]
    let ``Can Shuffle Deck``() = 
        let deck1 = CreateDeck 
        let deck2 = CreateDeck |> ShuffleDeck
        Assert.False(deck1 |> Seq.zip deck2 |> Seq.forall (fun (a, b) -> a = b))

    [<Fact>]
    let ``Can Draw Hand``() = 
        let cards = CreateDeck |> ShuffleDeck |> DrawHand
        Assert.True(cards |> Seq.length = 5)

    [<Fact>]
    let ``Can Draw Card``() = 
        let cards = [Pip(4, Spades); Pip(6, Hearts); Ace(Hearts); Jack(Hearts); Pip(4, Clubs)] |> Stack
        let topCard = cards.Pop()
        Assert.True((Pip(4, Clubs)) = topCard)

    [<Fact>]
    let ``Can Get High Card``() = 
        let highCard = [Pip(4, Spades); Pip(6, Hearts); Ace(Hearts); Jack(Hearts); Pip(4, Clubs)] |> GetHighCard
        Assert.True((Ace(Hearts)) = highCard)

    [<Fact>]
    let ``Can Get Low Card``() = 
        let lowCard = [Pip(2, Spades); Pip(6, Hearts); Ace(Hearts); Jack(Hearts); Pip(4, Clubs)] |> GetLowCard
        Assert.True((Pip(2, Spades)) = lowCard)

    [<Fact>]
    let ``Can Score Royal Flush``() = 
        let deck = [Ace(Spades); King(Spades); Queen(Spades); Jack(Spades); Pip(10, Spades)] |> ShuffleDeck 
        let hand = deck |> DrawHand
        Assert.True(deck |> Seq.length = 0)
        match hand |> GetHandScore with
            | RoyalFlush -> ()
            | _ -> failwith "Failed"

    [<Fact>]
    let ``Can Score Straight Flush``() = 
        let score = [Pip(2, Spades); Pip(3, Spades); Ace(Spades); Pip(4, Spades); Pip(5, Spades)] |> ShuffleDeck |> DrawHand |> GetHandScore
        match score with
            | StraightFlush c -> ()
            | _ -> failwith "Failed"

    [<Fact>]
    let ``Can Score Flush``() = 
        let score = [Pip(2, Spades); Pip(3, Spades); Ace(Spades); Pip(7, Spades); Pip(8, Spades)] |> ShuffleDeck |> DrawHand |> GetHandScore
        match score with
            | Flush c -> ()
            | _ -> failwith "Failed"

    [<Fact>]
    let ``Can Score High Card``() = 
        let score = [Pip(9, Spades); Pip(3, Spades); Ace(Hearts); Pip(4, Spades); Pip(5, Spades)] |> ShuffleDeck |> DrawHand |> GetHandScore
        match score with
            | HighCard c -> ()
            | _ -> failwith "Failed"

    [<Fact>]
    let ``Can Score Two Pairs``() = 
        let score = [Pip(2, Spades); Pip(2, Hearts); Ace(Spades); Pip(5, Hearts); Pip(5, Spades)] |> ShuffleDeck |> DrawHand |> GetHandScore
        match score with
            | TwoPair c -> ()
            | _ -> failwith "Failed"

    [<Fact>]
    let ``Can Score Pair``() = 
        let score = [Pip(2, Spades); Pip(2, Hearts); Ace(Spades); Pip(3, Hearts); Pip(5, Spades)] |> ShuffleDeck |> DrawHand |> GetHandScore
        match score with
            | Pair c -> ()
            | _ -> failwith "Failed"

    [<Fact>]
    let ``Can Score Three of a Kind``() = 
        let score = [Pip(2, Spades); Pip(2, Hearts); Pip(2, Diamonds); Pip(4, Spades); Pip(5, Spades)] |> ShuffleDeck |> DrawHand |> GetHandScore
        match score with
            | ThreeOfAKind c -> ()
            | _ -> failwith "Failed"

    [<Fact>]
    let ``Can Score Four of a Kind``() = 
        let score = [Pip(2, Spades); Pip(2, Hearts); Pip(2, Diamonds); Pip(2, Clubs); Pip(5, Spades)] |> ShuffleDeck |> DrawHand |> GetHandScore
        match score with
            | FourOfAKind c -> ()
            | _ -> failwith "Failed"

    [<Fact>]
    let ``Can Score Full House``() = 
        let score = [Pip(2, Spades); Pip(2, Hearts); Pip(2, Diamonds); Pip(10, Clubs); Pip(10, Spades)] |> ShuffleDeck |> DrawHand |> GetHandScore
        match score with
            | FullHouse c -> ()
            | _ -> failwith "Failed"

    [<Fact>]
    let ``Can Score Straight (10,J,Q,K,A)``() = 
        let score = [Pip(10, Spades); Jack(Hearts); Queen(Diamonds); King(Clubs); Ace(Spades)] |> ShuffleDeck |> DrawHand |> GetHandScore
        match score with
            | Straight c -> ()
            | _ -> failwith "Failed"

    [<Fact>]
    let ``Can Score Straight (A,2,3,4,5)``() = 
        let score = [Pip(2, Hearts); Pip(3, Spades); Pip(4, Spades); Pip(5, Spades); Ace(Spades)] |> ShuffleDeck |> DrawHand |> GetHandScore
        match score with
            | Straight c -> ()
            | _ -> failwith "Failed"

    [<Fact>]
    let ``Can Not Score Straight (A,2,3,Q,K)``() = 
        let score = [Pip(2, Hearts); Pip(3, Spades); King(Spades); Queen(Spades); Ace(Spades)] |> ShuffleDeck |> DrawHand |> GetHandScore
        match score with
            | HighCard c -> ()
            | _ -> failwith "Failed"



// EXTRAS
    let player1 = { Name = "Straight Flush (AS,2H,3S,4S,5S)"; Hand = [Pip(2, Spades); Pip(3, Spades); Pip(4, Spades); Pip(5, Spades); Ace(Spades)] }
    let player2 = { Name = "Royal Flush (10S,JS,QS,KS,AS)"; Hand = [Pip(10, Spades); Jack(Spades); Queen(Spades); King(Spades); Ace(Spades)] }
    let player3 = { Name = "Four of a Kind (AS,AH,AC,AD,5S)"; Hand = [Pip(2, Hearts); Ace(Spades); Ace(Hearts); Ace(Clubs); Ace(Diamonds)] }
    let player4 = { Name = "Flush (AS,2H,3S,4S,5S)"; Hand = [Pip(9, Spades); Pip(3, Spades); Pip(4, Spades); Pip(5, Spades); Ace(Spades)] }
    let player5 = { Name = "Straight (AS,2H,3S,4S,5S)"; Hand = [Pip(2, Hearts); Pip(3, Spades); Pip(4, Spades); Pip(5, Spades); Ace(Spades)] }
    let player6 = { Name = "Low Pair (AS,2H,2S,4S,5S)"; Hand = [Pip(2, Spades); Pip(2, Hearts); Pip(4, Spades); Pip(5, Spades); Ace(Spades)] }
    let player7 = { Name = "High Pair (2H,3S,4S,AH,AS)"; Hand = [Pip(2, Hearts); Pip(3, Spades); Pip(4, Spades); Ace(Hearts); Ace(Spades)] }

    [<Fact>]
    let ``Winning hand (Royal Flush)``() = 
        let players = [ player1; player2; player3; player4; player5; player6; player7 ]
        let winner = players |> GetWinningHand 
        Assert.True(winner.Name = player2.Name)

    [<Fact>]
    let ``Winning hand (Straight Flush)``() = 
        let players = [ player1; player3; player4; player5; player6; player7 ]
        let winner = players |> GetWinningHand 
        Assert.True(winner.Name = player1.Name)

    [<Fact>]
    let ``Winning hand (Four of a Kind)``() = 
        let players = [ player3; player4; player5; player6; player7 ]
        let winner = players |> GetWinningHand 
        Assert.True(winner.Name = player3.Name)

    [<Fact>]
    let ``Winning hand (Flush)``() = 
        let players = [ player4; player5; player6; player7 ]
        let winner = players |> GetWinningHand 
        Assert.True(winner.Name = player4.Name)

    [<Fact>]
    let ``Winning hand (Straight)``() = 
        let players = [ player5; player6; player7 ]
        let winner = players |> GetWinningHand 
        Assert.True(winner.Name = player5.Name)

    [<Fact>]
    let ``Winning hand (High Pair)``() = 
        let players = [ player6; player7 ]
        let winner = players |> GetWinningHand 
        Assert.True(winner.Name = player7.Name)