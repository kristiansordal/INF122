data Month
  = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December
  deriving (Eq, Show)

nextYear :: Year -> Year
nextYear (Year y) = Year (y + 1)

nextMonth January = February
nextMonth February = March
nextMonth March = April
nextMonth April = May
nextMonth May = June
nextMonth June = July
nextMonth July = August
nextMonth August = September
nextMonth September = October
nextMonth October = November
nextMonth November = December
nextMonth December = January

newtype Year = Year Integer
  deriving (Eq, Show)

divisible :: Integer -> Integer -> Bool
divisible x y = mod x y == 0

isLeapYear :: Year -> Bool
isLeapYear (Year y) = divisible y 4 && (not (divisible y 100) || divisible y 400)

daysOf :: Year -> Month -> Integer
daysOf _ January = 31
daysOf y February = if isLeapYear y then 29 else 28
daysOf _ March = 31
daysOf _ April = 30
daysOf _ May = 31
daysOf _ June = 30
daysOf _ July = 31
daysOf _ August = 31
daysOf _ September = 30
daysOf _ October = 31
daysOf _ November = 30
daysOf _ December = 31

data Date = Date Year Month Integer
  deriving (Eq, Show)

isValidDate :: Year -> Month -> Integer -> Bool
isValidDate y m d = 1 <= d && d <= daysOf y m

date :: Year -> Month -> Integer -> Maybe Date
date y m d =
  if isValidDate y m d
    then Just (Date y m d)
    else Nothing

nextDay :: Date -> Date
nextDay (Date y m d) =
  if d == daysOf y m
    then
      if m == December
        then Date (nextYear y) January 1
        else Date y (nextMonth m) 1
    else Date y m (d + 1) -- myDate = Date (Year 2022) September 22

main = do
  let d = Date (Year 2112) December 41823
  print d
