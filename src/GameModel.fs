module GameModel

open System
open Yahtzee.Data

module Seq =
  let flatten (seqs: seq<seq<'a>>): seq<'a> =
    seq{
      for xs in seqs do
        for x in xs do
          yield x
    }

let scoreFaces (faceNum: int) (dice: int array): int =
  dice
  |> Seq.filter (fun n -> n = faceNum)
  |> Seq.sum

let scoreNOfAKind (numOfKind: int) (dice: int array): int =
  let counts = Array.init 7 (fun _ -> 0)
  for d in dice do
    counts.[d] <- counts.[d] + 1
  let mutable out = 0
  let total = if numOfKind = 5 then 50 else Seq.sum dice
  for n in counts do
    if numOfKind <= n then out <- total
  out

let scoreFullHouse (dice: int array): int =
  let dice = Array.sort dice
  if (
      dice.[0] = dice.[1] && dice.[3] = dice.[4] &&
      (dice.[1] = dice.[2] || dice.[2] = dice.[3])
  )
  then
    25
  else
    0

let scoreNStraight (len: int) (dice: int array): int =
  let sortedDice = Array.sort dice
  let mutable maxStreakLen = 1
  let mutable streakLen = 1
  for (d1, d2) in Seq.zip sortedDice (Seq.skip 1 sortedDice) do
    if d1 = d2 then
      ()
    else if d1 + 1 = d2 then
      streakLen <- streakLen + 1
      maxStreakLen <- max maxStreakLen streakLen
    else
      streakLen <- 1
  if len <= maxStreakLen then
    match len with
    | 4 -> 30
    | 5 -> 40
    | _ -> failwith "straight length must be 4 or 5"
  else 0

let scoreChance (dice: int array): int =
  Array.sum dice


type Die =
  {
      value: int
      held: bool
  }

let newDie() = { value= 6; held= false }

let newRolledDice (rnd: Random): array<int> =
    Array.init 5 (fun _ -> rnd.Next(1, 7))


type ScoringSlot =
  {
    name: string
    score: array<int> -> int
    fill: int option
    max: int
  }
type ScoreCardCell =
  {
    score: array<int> -> int
    fill: option<int>
  }


type GameState =
  {
    rnd: Random
    activePlayer: int
    rolls: int
    dice: Die array
    areDiceShowingYahtzee: bool

    upperRowLabels: list<string>
    upperTbl: Table<ScoreCardCell>

    lowerRowLabels: list<string>
    lowerTbl: Table<ScoreCardCell>
    numYahtzeeBonuses: array<int>
  }

  member this.upperRows(): seq<seq<ScoringSlot>> =
    failwith "unimplemented"

  member this.lowerRows(): seq<seq<ScoringSlot>> =
    failwith "unimplemented"


let newPlayerUpperColumn() =
  [|
    { name= "ones";   score= (scoreFaces 1); fill= None; max= 5*1 }
    { name= "twos";   score= (scoreFaces 2); fill= None; max= 5*2 }
    { name= "threes"; score= (scoreFaces 3); fill= None; max= 5*3 }
    { name= "fours";  score= (scoreFaces 4); fill= None; max= 5*4 }
    { name= "fives";  score= (scoreFaces 5); fill= None; max= 5*5 }
    { name= "sixes";  score= (scoreFaces 6); fill= None; max= 5*6 }
  |]


let newPlayerLowerColumn() =
  [|
    { name= "3 of a kind"; score= (scoreNOfAKind 3); fill= None; max= 5*6 }
    { name= "4 of a kind"; score= (scoreNOfAKind 4); fill= None; max= 5*6 }
    { name= "full house"; score= scoreFullHouse; fill= None; max= 25 }
    { name= "small straight"; score= (scoreNStraight 4); fill= None; max= 30 }
    { name= "large straight"; score= (scoreNStraight 5); fill= None; max= 40 }
    { name= "Yahtzee!"; score= (scoreNOfAKind 5); fill= None; max= 50 }
    { name= "chance"; score= scoreChance; fill= None; max= 5*6 }
  |]

type Points = int
type ScoreCellFill =
  | PotentialScore of Points * (GameState -> GameState)
  | Unavailable of option<int>


module GameState =

  let numOfPlayers (st: GameState): int =
    (Table.numColumns st.upperTbl)

  let playerIndices (st: GameState): seq<int> =
    seq{ for playerIdx in 0..(Table.numColumns st.upperTbl)-1 do playerIdx }

  let isYahtzeeBonusAvailable (st: GameState): bool =
    let isYahtzeeFilled() =
      let yahtzeeRowIdx =
        Seq.indexed st.lowerRowLabels
        |> Seq.find (fun (_, lbl) -> lbl = "Yahtzee!")
        |> fst
      (st.lowerTbl[yahtzeeRowIdx, st.activePlayer]).fill |> Option.isSome
    let isYahtzee() = 0 < (st.dice |> Array.map (fun d -> d.value) |> scoreNOfAKind 5)
    isYahtzeeFilled() && isYahtzee()

  let scoringChoices (st: GameState): (array<ScoreCellFill> * array<ScoreCellFill>) =
    (
      [|
        PotentialScore(0, fun st -> st)
        PotentialScore(0, fun st -> st)
        PotentialScore(0, fun st -> st)
        PotentialScore(0, fun st -> st)
        PotentialScore(0, fun st -> st)
        PotentialScore(0, fun st -> st)
      |], [|
        PotentialScore(0, fun st -> st)
        PotentialScore(0, fun st -> st)
        PotentialScore(0, fun st -> st)
        PotentialScore(0, fun st -> st)
        PotentialScore(0, fun st -> st)
        PotentialScore(0, fun st -> st)
        PotentialScore(0, fun st -> st)
      |]
    )

  let passTurn (st: GameState): GameState =
    { st
      with
        activePlayer= (st.activePlayer + 1) % numOfPlayers st
        rolls= 0
        dice= Array.empty
    }

  // reference: https://en.wikipedia.org/wiki/Yahtzee#Free_choice_Joker_rule
  let jokerScoringRule (st: GameState) rowIdx colIdx origScoringFn dice =
    let isUpperNumFilled =
        let dieFace = st.dice[0].value
        Option.isSome st.upperTbl[dieFace-1, colIdx].fill
    if isYahtzeeBonusAvailable st && isUpperNumFilled then
      match rowIdx with
      | 2 -> 25
      | 3 -> 30
      | 4 -> 40
      | _ -> origScoringFn dice
    else
      origScoringFn dice

  let upperTableScoring (st: GameState): Table<ScoreCellFill> =
    st.upperTbl
    |> Table.mapi (fun (rowIdx, colIdx) cell ->
      if not (Array.isEmpty st.dice) && colIdx = st.activePlayer then
        match cell.fill with
        | None ->
          let y =
            st.dice
            |> Array.map (fun d -> d.value)
            |> cell.score
          PotentialScore (y, fun st ->
            { st
              with
                upperTbl = st.upperTbl |> Table.updateAt (rowIdx, colIdx) (fun _ -> { cell with fill= Some(y) })
            }
          )
        | Some(_) -> Unavailable(cell.fill)
      else
        Unavailable(cell.fill)
    )

  let lowerTableScoring (st: GameState): Table<ScoreCellFill> =
    st.lowerTbl
    |> Table.mapi (fun (rowIdx, colIdx) cell ->
      if not (Array.isEmpty st.dice) && colIdx = st.activePlayer && Option.isNone cell.fill then
        let y =
          st.dice
          |> Array.map (fun d -> d.value)
          |> jokerScoringRule st rowIdx colIdx cell.score
        PotentialScore (y, (fun st ->
          { st
            with
              lowerTbl = st.lowerTbl |> Table.updateAt (rowIdx, colIdx) (fun _ -> { cell with fill= Some(y) })
          }
        ))
      else
        Unavailable(cell.fill)
    )

let newGameState (numPlayers: int) =
  {
    rnd= Random()
    // upperColumns= List.init numPlayers (fun _ -> newPlayerUpperColumn())
    // lowerColumns= List.init numPlayers (fun _ -> newPlayerLowerColumn())
    numYahtzeeBonuses= Array.init numPlayers (fun _ -> 0)
    activePlayer= 0
    rolls= 0
    dice= Array.empty
    areDiceShowingYahtzee= false

    upperTbl= { rows= [|
      for n in 1..6 do
        [| for _ in 1..numPlayers do { score= (scoreFaces n); fill= None } |]
    |] }
    upperRowLabels= ["ones"; "twos"; "threes"; "fours"; "fives"; "sixes"]
    lowerTbl = { rows= [|
      [| for _ in 1..numPlayers do { score= (scoreNOfAKind 3);  fill= None } |]
      [| for _ in 1..numPlayers do { score= (scoreNOfAKind 4);  fill= None } |]
      [| for _ in 1..numPlayers do { score= scoreFullHouse;     fill= None } |]
      [| for _ in 1..numPlayers do { score= (scoreNStraight 4); fill= None } |]
      [| for _ in 1..numPlayers do { score= (scoreNStraight 5); fill= None } |]
      [| for _ in 1..numPlayers do { score= (scoreNOfAKind 5);  fill= None } |]
      [| for _ in 1..numPlayers do { score= scoreChance;        fill= None } |]
    |] }
    lowerRowLabels= [
      "3 of a kind"; "4 of a kind"; "full house"; "small straight"; "large straight";
      "Yahtzee!"; "chance"
    ]
  }

let rerollDice (st: GameState) =
  let newDice =
    if st.dice.Length = 0 then
      newRolledDice st.rnd |> Array.map (fun n -> { value= n; held= false })
    else
      [| for d in st.dice do
          if d.held then
            d
          else
            { d with value= st.rnd.Next(1, 7) }
      |]
  let yahtzeeBonus =
    0 < (newDice |> Array.map (fun d -> d.value) |> scoreNOfAKind 5)
  { st
    with
      dice= newDice;
      rolls= st.rolls + 1
      areDiceShowingYahtzee= yahtzeeBonus
  }

type ScoreTotals =
  {
    upperSectionSubtotal: int
    upperSectionBonus: int
    upperSectionTotal: int
    lowerSectionTotal: int
    grandTotal: int
  }

let calcTotals st =
  let getColumns = Table.transpose >> Table.rows
  let columns =
    Seq.zip (getColumns st.upperTbl) (getColumns st.lowerTbl)
    |> Seq.indexed
  [|
    for (playerIdx, (upperCol, lowerCol)) in columns do
      let upSub =
        upperCol
        |> Seq.map (fun s -> Option.defaultValue 0 s.fill)
        |> Seq.sum
      let upBonus = if 63 <= upSub then 35 else 0
      let upTotal = upSub + upBonus

      let lowTotal =
        lowerCol
        |> Seq.map (fun s -> Option.defaultValue 0 s.fill)
        |> Seq.sum
        |> (fun x -> x + st.numYahtzeeBonuses.[playerIdx] * 100)

      {
        upperSectionSubtotal= upSub
        upperSectionBonus= upBonus
        upperSectionTotal= upTotal
        lowerSectionTotal= lowTotal
        grandTotal= upTotal + lowTotal
      }
  |]

let totalSection (cols: array<ScoringSlot>): int =
  cols
  |> Seq.map (fun s -> Option.defaultValue 0 s.fill)
  |> Seq.sum

let upperSectionBonus col =
  if 63 <= totalSection col then 35 else 0


type PlayerIdx = int

let findWinner (st: GameState): Option<PlayerIdx * Points> =
  let isTableFull =
    Table.rows
    >> Seq.flatten
    >> Seq.map (fun cell -> Option.isSome cell.fill)
    >> Seq.reduce (fun x y -> x && y)
  if isTableFull st.upperTbl && isTableFull st.lowerTbl then
    Some(
      calcTotals st
      |> Seq.map (fun t -> t.grandTotal)
      |> Seq.indexed
      |> Seq.maxBy snd
    )
  else
    None
