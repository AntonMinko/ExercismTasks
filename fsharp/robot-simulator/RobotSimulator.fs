module RobotSimulator

type Direction = North | East | South | West
type Position = int * int
type Robot = { direction: Direction; position: Position }

let create direction position = { direction = direction; position = position }

let move instructions robot =
    let processInstruction robot inst =
        let dir =
            match inst with
            | 'R' -> match robot.direction with
                     | North -> East
                     | East -> South
                     | South -> West
                     | West -> North
            | 'L' -> match robot.direction with
                     | North -> West
                     | East -> North
                     | South -> East
                     | West -> South
            | _ -> robot.direction
        let pos =
            let (x, y) = robot.position
            match inst with
            | 'A' -> match robot.direction with
                     | North -> (x, y+1)
                     | East -> (x+1, y)
                     | South -> (x, y-1)
                     | West -> (x-1, y)
            | _ -> robot.position
        { direction = dir; position = pos }
    
    instructions
    |> Seq.fold processInstruction robot