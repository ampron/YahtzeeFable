module App

open Elmish
open Elmish.React

open AppModel
open GameModel
open View

// MODEL
//------------------------------------------------------------------------------
let init() : Model =
  {
    ystage= Loby
    keyMap= Control.initialKeyMap.Value
    developerModeEnabled= false
  }

// reference: https://elmish.github.io/elmish/docs/subscription.html
let keyEvents _initialModel =
  let subscription dispatch =
    Browser.Dom.document.addEventListener("keyup", (fun e ->
      let ke = e :?> Browser.Types.KeyboardEvent
      SingleKeyPress(ke) |> dispatch
    ))

  Cmd.ofSub subscription

// UPDATE
//------------------------------------------------------------------------------
let update (msg: Msg) (model: Model) : Model =
  match msg with
  | SingleKeyPress(ke) ->
    model.keyMap[int(ke.keyCode)] model

  | _ ->
    match model.ystage with
    | Loby ->
      match msg with
      | StartGame(n) -> { model with ystage= InGame(newGameState n) }
      | _ -> failwith "unsupported Loby message"

    | InGame(st) ->
      match msg with
      | HoldAll ->
        for (i, d) in Seq.indexed st.dice do
          st.dice[i] <- { d with held= true }
        { model with ystage= InGame({ st with rolls= 3 }) }

      | RollDice ->
        match st.rolls with
        | 0 | 1 | 2 -> { model with ystage= InGame(rerollDice st) }
        | _ -> failwith "invaild state, # of rolls"

      | SwapDie(swapIdx) ->
        let newDice =
          [| for (i, d) in Seq.indexed st.dice do
              if i = swapIdx then
                { d with held= not d.held }
              else
                d
          |]
        { model with ystage= InGame({ st with dice= newDice }) }

      | ChooseScoringOption(updateScoreCard) ->
        if GameState.isYahtzeeBonusAvailable st then
          st.numYahtzeeBonuses.[st.activePlayer] <- st.numYahtzeeBonuses.[st.activePlayer] + 1
        let newSt = st |> updateScoreCard |> GameState.passTurn
        match findWinner newSt with
        | None -> { model with ystage= InGame(newSt) }
        | Some((playerIdx, score)) ->
          { model with ystage= GameOver(newSt, playerIdx, score) }

      | _ -> failwith "unsupported InGame message"

    | GameOver(x, i, y) ->
      { model with ystage= GameOver(x, i, y) }


// VIEW (rendered with React)
//------------------------------------------------------------------------------
let view (model: Model) =
  match model.ystage with
    | Loby -> lobyView model
    | InGame(st) -> inGameView model st
    | GameOver(st, i, y) -> endGameView (st, i, y)

// App
//------------------------------------------------------------------------------
Program.mkSimple init update view
|> Program.withSubscription keyEvents
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.run
