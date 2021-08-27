// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open Elmish
open Microsoft.Xna.Framework
open Xelmish.Model
open Xelmish.Viewables
let startPos = 0,0
let BLOCK_WIDTH = 10
let WINDOW_HEIGHT = 600
let WINDOW_WIDTH = 1000
let mutable lastTick = 0L
let mutable lastTreat = 0L

let rand = System.Random ()

type Model = {
    tickInterval: int64
    treatInterval: int64
    position: (int * int) list
    direction: int * int
    treatPosition: int * int
}

let initialDirection = 1,0
let shiftBy pos dir = fst pos + (fst dir) * BLOCK_WIDTH, snd pos + (snd dir) * BLOCK_WIDTH
let rec moveLastToNewFirst (lis: (int * int) list) (pos: int * int) =
    match lis with
    | [] -> []
    | [x] -> [pos]
    | x::xs -> (moveLastToNewFirst xs pos) @ [x]
//let moveLastToShiftedFirst lis dir = moveLastToNewFirst lis (shiftBy lis.[0] dir)
let moveLastToShiftedFirst (lis: (int * int) list) dir = [shiftBy lis.[0] dir] @ lis.GetSlice (None, Some (lis.Length - 2))
    
let init () = ({
    tickInterval = 500L
    treatInterval = 50_000L
    position = [shiftBy (0,0) initialDirection; 0,0]
    direction = 1,0
    treatPosition = WINDOW_WIDTH / 10, WINDOW_HEIGHT / 10
}, Cmd.none)

type Message =
    | Tick
    | Left
    | Right
    | Up
    | Down
    | MoveTreat
    | EatTreat
let update message model =
    match message with
    //| Tick -> { model with position = (List.map (fun (x,y) -> x + (fst model.direction) * BLOCK_WIDTH, y + (snd model.direction) * BLOCK_WIDTH) model.position) }
    | Tick -> { model with position = moveLastToShiftedFirst model.position model.direction}
    | Left -> { model with direction = -1,0 }
    | Right -> { model with direction = 1,0 }
    | Up -> { model with direction = 0,-1 }
    | Down -> { model with direction = 0,1 }
    | MoveTreat -> { model with treatPosition = rand.Next(WINDOW_WIDTH), rand.Next(WINDOW_HEIGHT) }
    | EatTreat -> { model with
                       position = [shiftBy model.position.[0] model.direction] @ model.position
                       treatPosition = rand.Next(WINDOW_WIDTH), rand.Next(WINDOW_HEIGHT) }
    , Cmd.none
    

let view model dispatch =
    [
        //yield colour Colour.Aqua (BLOCK_WIDTH, BLOCK_WIDTH) (fst model.position.[0], snd model.position.[0])
        for (x,y) in model.position do yield colour Colour.Aqua (BLOCK_WIDTH, BLOCK_WIDTH) (x,y)
        yield colour Colour.Yellow (BLOCK_WIDTH, BLOCK_WIDTH) (fst model.treatPosition, snd model.treatPosition)
        yield onkeydown Keys.Left (fun _ -> dispatch Left)
        yield onkeydown Keys.Right (fun _ -> dispatch Right)
        yield onkeydown Keys.Down (fun _ -> dispatch Down)
        yield onkeydown Keys.Up (fun _ -> dispatch Up)
        yield onupdate (fun inputs ->
            let timeInterval =
                if List.fold (fun acc elem -> acc || inputs.keyboardState.IsKeyDown elem) false [Keys.Up; Keys.Down; Keys.Left; Keys.Right]
                then 100L
                else model.tickInterval
            if (inputs.totalGameTime - lastTick) >= timeInterval then
                lastTick <- inputs.totalGameTime
                dispatch Tick
            if (inputs.totalGameTime - lastTreat) >= model.treatInterval then
                lastTreat <- inputs.totalGameTime
                dispatch MoveTreat
            if Math.Abs(fst model.position.[0] - fst model.treatPosition) < BLOCK_WIDTH && Math.Abs(snd model.position.[0] - snd model.treatPosition) < BLOCK_WIDTH
            then dispatch EatTreat
            )
    ]

[<EntryPoint; STAThread>]
let main _ =
    let config = {
        resolution = Windowed (1000, 600)
        clearColour = Some Color.Black
        mouseVisible = true
        assetsToLoad = []
    }
    
    Program.mkProgram init update view
    |> Program.withConsoleTrace
    |> Xelmish.Program.runGameLoop config
    0 // return an integer exit code
