data Door = A | B | C
    deriving (Eq, Show, Ord)

data GameResult = Win | Lose

door = uniform [A,B,C]

type Strategy = Door -> Door

stay, switch :: Strategy

stay reveal = A
switch B = C
switch C = B

computeResult :: Door -> Door -> GameResult
computeResult winDoor guess
    = if winDoor == guess
        then Win
        else Lose

game strategy = bind door (\winDoor -> 
    (!(computeResult strategy))
        $ case winDoor of
            A -> uniform [B,C]
            B -> force C
            C -> force B)

