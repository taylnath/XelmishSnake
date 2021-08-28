// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp
module PlayScreen

open System
open Elmish
open Microsoft.Xna.Framework
open Xelmish.Model
open Xelmish.Viewables
open Constants
let startPos = 0,0
let mutable lastTick = 0L
let mutable lastTreat = 0L

let rand = System.Random ()

type Model = {
    tickInterval: int64
    treatInterval: int64
    position: (int * int) list
    direction: int * int
    treatPosition: int * int
    extraTreatPosition: int * int
    score: int
}

let initialDirection = 1,0
let shiftBy pos dir = fst pos + (fst dir) * blockWidth, snd pos + (snd dir) * blockWidth
let rec moveLastToNewFirst (lis: (int * int) list) (pos: int * int) =
    match lis with
    | [] -> []
    | [x] -> [pos]
    | x::xs -> (moveLastToNewFirst xs pos) @ [x]
//let moveLastToShiftedFirst lis dir = moveLastToNewFirst lis (shiftBy lis.[0] dir)
let moveLastToShiftedFirst (lis: (int * int) list) dir = [shiftBy lis.[0] dir] @ lis.GetSlice (None, Some (lis.Length - 2))
    
let init () = {
    tickInterval = 400L
    treatInterval = 50_000L
    position = [shiftBy (0,0) initialDirection; 0,0]
    direction = 1,0
    treatPosition = windowWidth / 10, windowHeight / 10
    extraTreatPosition = windowWidth / 2, windowHeight / 2
    score = 2
}

type Message =
    | Tick
    | Left
    | Right
    | Up
    | Down
    | MoveTreat
    | EatTreat
    | GameOver of int
    
let update message model =
    match message with
    //| Tick -> { model with position = (List.map (fun (x,y) -> x + (fst model.direction) * BLOCK_WIDTH, y + (snd model.direction) * BLOCK_WIDTH) model.position) }
    | Tick -> { model with position = moveLastToShiftedFirst model.position model.direction}
    | Left -> { model with direction = -1,0 }
    | Right -> { model with direction = 1,0 }
    | Up -> { model with direction = 0,-1 }
    | Down -> { model with direction = 0,1 }
    | MoveTreat -> { model with treatPosition = rand.Next(windowWidth), rand.Next(windowHeight) }
    | EatTreat -> { model with
                       position = [shiftBy model.position.[0] model.direction] @ model.position
                       treatPosition = rand.Next(windowWidth), rand.Next(windowHeight)
                       extraTreatPosition = rand.Next(windowWidth), rand.Next(windowHeight)
                       score = model.score + 1 }
    | GameOver _ -> model
    , Cmd.none
    

let tuplesWithinDist (tup1: int * int) (tup2: int * int) (dist: int) =
    Math.Abs(fst tup1 - fst tup2) < dist && Math.Abs(snd tup1 - snd tup2) < dist
let view model dispatch =
    [
        //yield colour Colour.Aqua (BLOCK_WIDTH, BLOCK_WIDTH) (fst model.position.[0], snd model.position.[0])
        for (x,y) in model.position do yield colour Colour.Aqua (blockWidth, blockWidth) (x,y)
        yield colour Colour.Yellow (blockWidth, blockWidth) (fst model.treatPosition, snd model.treatPosition)
        yield colour Colour.Yellow (blockWidth, blockWidth) (fst model.extraTreatPosition, snd model.extraTreatPosition)
        yield onkeydown Keys.Left (fun _ -> if model.direction <> (1,0) then dispatch Left)
        yield onkeydown Keys.Right (fun _ -> if model.direction <> (-1, 0) then dispatch Right)
        yield onkeydown Keys.Down (fun _ -> if model.direction <> (0, -1) then dispatch Down)
        yield onkeydown Keys.Up (fun _ -> if model.direction <> (0, 1) then dispatch Up)
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
            if List.fold (fun acc elem -> acc || (tuplesWithinDist model.position.[0] elem blockWidth)) false model.position.[1..]
            then dispatch (GameOver model.score)
            if (tuplesWithinDist model.position.[0] model.treatPosition blockWidth || tuplesWithinDist model.position.[0] model.extraTreatPosition blockWidth)
            then dispatch EatTreat
            )
    ]
