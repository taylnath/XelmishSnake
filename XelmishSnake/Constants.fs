module Constants

open Xelmish.Model

let highScoreFile = "./highscore.txt"

let gridWidth = 10
let gridHeight = 20
let padding = 30
let tiledim = 20
let resWidth = padding + (tiledim * gridWidth) + padding + (tiledim * 6) + padding
let resHeight = padding + (tiledim * gridHeight) + padding

let blockWidth = 20
let windowHeight = 400
let windowWidth = 700