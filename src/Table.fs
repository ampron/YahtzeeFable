namespace Yahtzee.Data


type Table<'t> =
  {
    rows: array<array<'t>>
  }

  member this.Item(rowIdx: int, colIdx: int): 't =
    this.rows[rowIdx].[colIdx]

  member this.GetSlice(a: int option, b: int option): 't =
    failwith "not implmented"

module Table =
  let empty() = { rows= Array.empty }

  let numRows (tbl: Table<'t>): int = tbl.rows.Length

  let numColumns (tbl: Table<'t>): int = tbl.rows[0].Length

  let copy (tbl: Table<'t>): Table<'t> =
    { rows= [| for row in tbl.rows do Array.copy row |] }

  let mapi (f: (int * int) -> 't -> 'u) (tbl: Table<'t>): Table<'u> =
    { rows=
        tbl.rows
        |> Array.mapi (fun rowIdx row ->
          row |> Array.mapi (fun colIdx x ->
            f (rowIdx, colIdx) x
          )
        )
    }

  let indexedCells (tbl: Table<'t>) =
    seq{
      for (rowIdx, row) in Seq.indexed tbl.rows do
        for (colIdx, element) in Seq.indexed row do
          yield ((rowIdx, colIdx), element)
    }

  let rows (tbl: Table<'t>) =
    seq{
      for row in tbl.rows do
        yield Seq.ofArray row
    }

  let transpose (tbl: Table<'t>) : Table<'t> =
    { rows=
      [| for colIdx in tbl.rows[0] |> Seq.indexed |> Seq.map fst do
          [| for rowIdx in tbl.rows |> Seq.indexed |> Seq.map fst do
                tbl.rows[rowIdx].[colIdx]
          |]
      |]
    }

  let updateAt
    ((rowIdx, colIdx): int * int)
    (updateCell: 't -> 't)
    (tbl: Table<'t>)
    =
    let y = updateCell tbl.rows[rowIdx].[colIdx]
    { tbl with
        rows=
          tbl.rows
          |> Array.updateAt rowIdx (
            tbl.rows[rowIdx]
            |> Array.updateAt colIdx y
          )
    }

  let updateMany
    (instructions: seq<int * int * ('t -> 't)>)
    (tbl: Table<'t>)
    =
    let tbl = copy tbl
    for (rowIdx, colIdx, updateFn) in instructions do
      tbl.rows.[rowIdx].[colIdx] <- updateFn tbl.rows.[rowIdx].[colIdx]
    tbl
