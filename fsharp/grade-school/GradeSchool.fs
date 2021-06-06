module GradeSchool

type School = Map<int, string list>

let empty: School = Map.empty

let add (student: string) (grade: int) (school: School): School = 
    if school.ContainsKey grade then
        let students = student :: school.[grade] |> List.sort
        school |> Map.add grade students
    else
        school |> Map.add grade [student]

let roster (school: School): string list = 
    school |> Map.toList |> List.sortBy fst |> List.collect snd

let grade (number: int) (school: School): string list =
    if school.ContainsKey number then
        school.[number]
    else
        List.empty