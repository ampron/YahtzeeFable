module View

open Fable.React
open Fable.React.Props

open YahtzeeFable.Collections
open Yahtzee.Data
open AppModel
open GameModel

let lobyView model dispatch =
  div [
    Class(if model.developerModeEnabled then "developer-mode" else "normal-mode")
  ] [
    p [] [str "How many players?"]
    for n in 2..6 do
      button [ OnClick (fun _ -> StartGame(n) |> dispatch) ] [ str $"{n}" ]
  ]

let inGameView (model: Model) (st: GameState) dispatch =
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

  div [
    Class(if model.developerModeEnabled then "developer-mode" else "normal-mode")
  ] [
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
            for (playerIdx, n) in st.numYahtzeeBonuses |> ImArray.toSeq |> Seq.indexed do
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

let endGameView (st: GameState, winnerIdx: PlayerIdx, winningScore: Points) dispatch =
  p [] [str $"Player {winnerIdx+1} wins with {winningScore} points!"]
