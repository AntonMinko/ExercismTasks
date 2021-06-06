module QueenAttack

open System 

let create (position: int * int) = 
  match position with
  | (x, y) -> x >= 0 && x < 8 && y >= 0 && y < 8

let canAttack (queen1: int * int) (queen2: int * int) = 
  let (x1, y1) = queen1
  let (x2, y2) = queen2

  x1 = x2 || y1 = y2 || Math.Abs(x1 - x2) = Math.Abs(y1 - y2)