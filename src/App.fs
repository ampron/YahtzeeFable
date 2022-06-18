module App

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props

open Yahtzee.Data
open GameModel

// MODEL
//------------------------------------------------------------------------------
type Model =
  | Loby
  | InGame of GameState

type Index = int

type Score = int

type Msg =
| StartGame of int
| HoldAll
| RollDice
| SwapDie of int
| ChooseScoringRow of array<ScoringSlot> * int * int
| ChooseScoringOption of (GameState -> GameState)

let init() : Model = Loby

// UPDATE
//------------------------------------------------------------------------------
let update (msg: Msg) (model: Model) =
  match model with
  | Loby ->
    match msg with
    | StartGame(n) -> InGame(newGameState n)
    | _ -> failwith "unsupported Loby message"
  | InGame(st) ->
    match msg with
    | HoldAll ->
      for (i, d) in Seq.indexed st.dice do
        st.dice[i] <- { d with held= true }
      InGame({ st with rolls= 3 })
    | RollDice ->
      match st.rolls with
      | 0 | 1 | 2 -> InGame(rerollDice st)
      | _ -> failwith "invaild state, # of rolls"
    | SwapDie(swapIdx) ->
      let newDice =
        [| for (i, d) in Seq.indexed st.dice do
            if i = swapIdx then
              { d with held= not d.held }
            else
              d
        |]
      InGame({ st with dice= newDice })
    | ChooseScoringRow (col, i, y) ->
      if GameState.isYahtzeeBonusAvailable st then
        st.numYahtzeeBonuses.[st.activePlayer] <- st.numYahtzeeBonuses.[st.activePlayer] + 1
      col.[i] <- { col[i] with fill= Some(y) }
      InGame(
        { st
          with
            activePlayer= (st.activePlayer + 1) % st.upperColumns.Length
            rolls= 0
            dice= Array.empty
        }
      )
    | ChooseScoringOption(updateScoreCard) ->
      InGame(
        { updateScoreCard st
          with
            activePlayer= (st.activePlayer + 1) % st.upperColumns.Length
            rolls= 0
            dice= Array.empty
        }
      )


// VIEW (rendered with React)
//------------------------------------------------------------------------------
let lobyView dispatch =
  div [] [
    p [] [str "How many players?"]
    for n in 2..6 do
      button [ OnClick (fun _ -> StartGame(n) |> dispatch) ] [ str $"{n}" ]
  ]

let inGameView (st: GameState) dispatch =
  let calculatedCell n = td [Class "calculated-cell"] [ str (n.ToString())]
  let grandTotalCell n = td [Class "grand-total"] [str (n.ToString())]
  let rowLabelCell s = td [Class "row-label"] [str s]
  let spacerRow() =
    tr [Class "spacer-row"] [for _ in 0..st.upperColumns.Length do td [] []]
  let possibleScoreButton col rowIdx y =
    button [
      Class "possible-score"
      OnClick(fun _ -> ChooseScoringRow(col, rowIdx, y) |> dispatch)
    ] [str $"{y}"]

  let totals = calcTotals st
  div [] [
    div [] [
      table [] [
        colgroup [] [
          col []
          for (i, _) in Seq.indexed st.upperColumns do
            col [if i = st.activePlayer then Class "active-player"]
        ]
        tbody [] [
          tr [] [
            th [] []
            for playerNum in 1..st.upperColumns.Length do
              th [] [str $"Player %d{playerNum}"]
          ]
          spacerRow()
          // alternate upper table rendering
          let upperScoring = GameState.upperTableScoring st
          for (rowLabel, row) in Seq.zip st.upperRowLabels (upperScoring |> Table.rows) do
            tr [] [
              td [] [str rowLabel]
              for (colIdx, cell) in row |> Seq.indexed do td [] [
                match cell with
                | PotentialScore(points, updateState) ->
                    button [
                      Class "possible-score"
                      OnClick(fun _ -> ChooseScoringOption(updateState) |> dispatch)
                    ] [str $"{points}"]
                | Unavailable(None) ->
                    str ""
                | Unavailable(Some(n)) ->
                    str $"{n}"
              ]
            ]

          spacerRow()
          tr [] [
            rowLabelCell "Upper Section Subtotal"
            for tot in totals do calculatedCell tot.upperSectionSubtotal
          ]
          tr [] [
            rowLabelCell "Upper Section Bonus"
            for tot in totals do calculatedCell tot.upperSectionBonus
          ]
          tr [] [
            rowLabelCell "Upper Section Total"
            for tot in totals do calculatedCell tot.upperSectionTotal
          ]
          spacerRow()

          // lower rows
          for (rowIdx, firstColCell) in Seq.indexed st.lowerColumns.Head do
            tr [] [
              td [] [str firstColCell.name]
              for (colIdx, col) in Seq.indexed st.lowerColumns do
                let cell = col[rowIdx]
                td [] [
                  match cell.fill with
                  | None ->
                    if st.rolls = 3 && colIdx = st.activePlayer then
                      let y =
                        st.dice
                        |> Array.map (fun d -> d.value)
                        |> cell.score
                      possibleScoreButton col rowIdx y
                  | Some(n) ->
                    str $"{n}"
                ]
            ]

          spacerRow()
          tr [] [
            rowLabelCell "Yahtzee Bonuses"
            for (playerIdx, n) in Seq.indexed st.numYahtzeeBonuses do
              td [] [
                if playerIdx = st.activePlayer && GameState.isYahtzeeBonusAvailable st then
                  str ($"%d{n + 1}! x 100")
                else
                  str ($"%d{n} x 100")
              ]
          ]
          tr [] [
            rowLabelCell "Lower Section Total"
            for tot in totals do calculatedCell tot.lowerSectionTotal
          ]
          tr [] [
            rowLabelCell "Upper Section Total"
            for tot in totals do calculatedCell tot.upperSectionTotal
          ]
          tr [] [
            rowLabelCell "Grand Total"
            for tot in totals do grandTotalCell tot.grandTotal
          ]
          spacerRow()
        ]
      ]
    ]
    div [] [
      p [] [str (sprintf "Player %d's turn" (st.activePlayer + 1))]
      div [] [
        match st.rolls with
        | 0 -> button [OnClick(fun _ -> RollDice |> dispatch)] [str "Roll!"]
        | 1 -> button [OnClick(fun _ -> RollDice |> dispatch)] [str "Roll again!"]
        | 2 -> button [OnClick(fun _ -> RollDice |> dispatch)] [str "Final roll..."]
        | 3 -> ()
        | _ -> failwith "excessive rolling"
        if 0 < st.dice.Length then
          button [OnClick(fun _ -> HoldAll |> dispatch)] [str "Hold all"]
      ]
      span [Class "dice-cup"] [
        for (i, d) in Seq.indexed st.dice do
          button [
            if d.held || st.rolls = 3 then Class "held"
            OnClick(fun _ -> SwapDie(i) |> dispatch)
          ] [
            let dieChars = [| ""; "⚀"; "⚁"; "⚂"; "⚃"; "⚄"; "⚅" |]
            str dieChars[d.value]
          ]
      ]
    ]
  ]

let view (model: Model) dispatch =
  match model with
    | Loby -> lobyView dispatch
    | InGame(st) -> inGameView st dispatch


// App
//------------------------------------------------------------------------------
Program.mkSimple init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.run
