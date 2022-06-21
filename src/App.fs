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
    | ChooseScoringOption(updateScoreCard) ->
      if GameState.isYahtzeeBonusAvailable st then
        st.numYahtzeeBonuses.[st.activePlayer] <- st.numYahtzeeBonuses.[st.activePlayer] + 1
      InGame(st |> updateScoreCard |> GameState.passTurn)


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
    tr [Class "spacer-row"] [
      td [] []
      for _ in GameState.playerIndices st do
        td [] []
    ]
  let totals = calcTotals st

  div [] [
    div [] [
      table [] [
        colgroup [] [
          col []
          for playerIdx in GameState.playerIndices st do
            col [if playerIdx = st.activePlayer then Class "active-player"]
        ]
        tbody [] [
          tr [] [
            th [] []
            for playerId in 1 .. Table.numColumns st.upperTbl do
              th [] [str $"Player %d{playerId}"]
          ]
          spacerRow()

          let upperScoring = GameState.upperTableScoring st
          for (rowLabel, row) in Seq.zip st.upperRowLabels (upperScoring |> Table.rows) do
            tr [] [
              td [] [str rowLabel]
              for cell in row do td [] [
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

          let lowerScoring = GameState.lowerTableScoring st
          for (rowLabel, row) in Seq.zip st.lowerRowLabels (lowerScoring |> Table.rows) do
            tr [] [
              td [] [str rowLabel]
              for cell in row do td [] [
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
