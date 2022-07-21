module AppModel

open GameModel

// MODEL
//------------------------------------------------------------------------------
type YahtzeeStage =
  | Loby
  | InGame of GameState
  | GameOver of GameState * PlayerIdx * Points


type Model =
  {
    ystage: YahtzeeStage
    keyMap: KeyMap
    developerModeEnabled: bool
  }

and KeyMap = (Model -> Model) array

type Index = int

type Score = int

type Msg =
| StartGame of int
| HoldAll
| RollDice
| SwapDie of int
| ChooseScoringOption of (GameState -> GameState)
| SingleKeyPress of Browser.Types.KeyboardEvent
