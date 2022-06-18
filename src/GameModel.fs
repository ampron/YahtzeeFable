module GameModel

open System
open Yahtzee.Data

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
    [| 6; 6; 6; 6; 6 |]
    // Array.init 5 (fun _ -> rnd.Next(1, 7))


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
    upperColumns: list<array<ScoringSlot>>
    upperBonuses: array<int>
    lowerColumns: list<array<ScoringSlot>>
    numYahtzeeBonuses: array<int>
    activePlayer: int
    rolls: int
    dice: Die array
    areDiceShowingYahtzee: bool

    // experimental
    upperTbl: Table<ScoreCardCell>
    upperRowLabels: list<string>
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

  let isYahtzeeBonusAvailable (st: GameState): bool =
    let isYahtzeeFilled() =
      st.lowerColumns.[st.activePlayer]
      |> Seq.find (fun slot -> slot.name = "Yahtzee!")
      |> (fun slot -> Option.isNone slot.fill)
      |> not
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

  let jokerRule (st: GameState) (upperRows, lowerRows): (array<ScoreCellFill> * array<ScoreCellFill>) =
    // reference: https://en.wikipedia.org/wiki/Yahtzee#Yahtzee_bonuses_and_Joker_rules
    let isUpperNumFilled() =
      let dieFace = st.dice[0].value
      Option.isSome st.upperColumns[st.activePlayer].[dieFace].fill
    if isYahtzeeBonusAvailable st && isUpperNumFilled() then
      let updatePotentialScoreAt rowIdx n =
        Array.updateAt rowIdx (PotentialScore(n, fun st ->
          { st with
              lowerColumns=
                st.lowerColumns
                |> List.updateAt st.activePlayer (
                  st.lowerColumns[st.activePlayer]
                  |> Array.updateAt 4 { st.lowerColumns[st.activePlayer].[rowIdx] with fill= Some(n) }
                )
          }
        ))
      ( upperRows,
        lowerRows
        |> updatePotentialScoreAt 2 25
        |> updatePotentialScoreAt 3 30
        |> updatePotentialScoreAt 4 40
      )
    else
      (upperRows, lowerRows)

  let upperTableScoring (st: GameState): Table<ScoreCellFill> =
    st.upperTbl
    |> Table.mapi (fun (rowIdx, colIdx) cell ->
      if colIdx = st.activePlayer then
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

  let lowerTableScoring
    (st: GameState)
    (upperTbl: Table<ScoringSlot>)
    (lowerTbl: Table<ScoringSlot>)
    : Table<ScoreCellFill>
    =
    failwith "unimplemented"

let newGameState (numPlayers: int) =
  {
    rnd= Random()
    upperColumns= List.init numPlayers (fun _ -> newPlayerUpperColumn())
    upperBonuses= Array.init numPlayers (fun _ -> 0)
    lowerColumns= List.init numPlayers (fun _ -> newPlayerLowerColumn())
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
  let columns =
    Seq.zip st.upperColumns st.lowerColumns
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
