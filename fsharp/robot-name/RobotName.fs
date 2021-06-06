module RobotName
open System
open System.Collections.Generic

type Robot =
    {
        name: string
    }

let usedNames = new HashSet<string>()
let r = new Random(DateTime.Now.Millisecond)

let generateName() =
    let l1 = char (r.Next(int 'A', int 'Z' + 1))
    let l2 = char (r.Next(int 'A', int 'Z' + 1))
    let num = r.Next(100, 1000)
    $"{l1}{l2}{num}"

let rec generateUniqueName() =
    let name = generateName()
    if (usedNames.Contains(name)) then generateUniqueName()
    else
        usedNames.Add(name) |> ignore
        name

let mkRobot() = { name = generateUniqueName() }

let name robot = robot.name

let reset robot = { robot with name = generateUniqueName() }