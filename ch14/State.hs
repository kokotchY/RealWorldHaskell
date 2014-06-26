import Control.Monad.State

tick :: State Int Int
tick = do
    n <- get
    put (n+1)
    return n

plusOne :: Int -> Int
plusOne n = execState tick n

plus :: Int -> Int -> Int
plus n x = execState (sequence $ replicate n tick) x
