cincoInt :: Int -> Int
cincoInt x = 5

cincoChar :: Char -> Int
cincoChar c = 5

cincoBool :: Bool -> Int
cincoBool b = 5

odd :: Int -> Bool
odd x = (mod x 0 == 1)

app :: (Int -> Int) -> Int -> Int
app f x = f x

appEither :: (Int -> Int) -> (Int -> Int) -> Int -> Int -> Int
appEither f g x y = if(x >= 0) then f y else g y
