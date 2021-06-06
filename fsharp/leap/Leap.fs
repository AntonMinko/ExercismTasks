module Leap

let leapYear (year: int): bool = 
  let divisibleBy4 = year % 4 = 0
  let divisibleBy100 = year % 100 = 0
  let divisibleBy400 = year % 400 = 0

  (divisibleBy4 && not divisibleBy100) || (divisibleBy100 && divisibleBy400)