import System.Random

randomRange :: Semigroup m => SqrtTree m -> IO m
randomRange t = do
  l <- randomRIO (0, 999999)
  r <- randomRIO (l, 999999)
  pure $ range t l r

main = do
  ns <- replicateM 1000000 (randomRIO (0, 1000 :: Int))
  let t = fromList (map Sum ns)
  rs <- replicateM 1000000 (randomRange t)
  print (getSum $ mconcat rs)
