module Control

open AppModel

// Reference for Javascript keycodes
// http://gcctech.org/csc/javascript/javascript_keycodes.htm

let keyNoOp st = st

let cycleDie keyCode model =
  let idx = keyCode - 49
  { model with
      ystage=
        match model.ystage with
        | InGame(st) -> InGame(
            { st with
                dice=
                  if Array.isEmpty st.dice then
                    st.dice
                  else
                    st.dice |> Array.updateAt idx (
                      { st.dice[idx] with value= (st.dice[idx].value % 6) + 1}
                )
            }
          )
        | _ -> model.ystage
  }

let rec setDeveloperKeys (handlers: KeyMap) =
  let devKeyCode = 192
  handlers[devKeyCode] <-
    (fun model ->
    { model with
        developerModeEnabled= true
        keyMap=
          let handlers = Array.copy model.keyMap
          // add die cycling keys
          for keyCode in 49..53 do
            handlers[keyCode] <- cycleDie keyCode
          // add disabling toggle
          handlers[devKeyCode] <-
            (fun model ->
              { model with
                  developerModeEnabled= false
                  keyMap=
                  let handlers = Array.copy model.keyMap
                  // remove die cycling keys
                  for keyCode in 49..53 do
                    handlers[keyCode] <- keyNoOp
                  setDeveloperKeys handlers
              }
            )
          handlers
    }
    )
  handlers

let initialKeyMap =
  lazy
    let mutable handlers = Array.init 256 (fun _ -> keyNoOp)
    setDeveloperKeys handlers
