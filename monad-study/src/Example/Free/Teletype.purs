module Example.Free.Teletype
  ( TeletypeF(..)
  , main
  )
  where

import Prelude

import Control.Monad.Rec.Class (class MonadRec)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Study.Control.Monad.Free.Free (Free, foldFree, liftF, resume)

data TeletypeF a = PutStrLn String a | GetLine (String -> a)

type Teletype a = Free TeletypeF a

-- 呼び出しと、呼び出しに対応する処理をつなぐための型を返すところ
-- こいつらが返されたタイミングでは処理は実行されない
putStrLn :: String -> Teletype Unit
putStrLn s = liftF (PutStrLn s unit)

getLine :: Teletype String
getLine = liftF (GetLine identity)

-- 呼ばれたときの処理
run :: Teletype ~> Effect
run = foldFree go
  where
  go :: TeletypeF ~> Effect
  go (PutStrLn s a) = const a <$> log s
  go (GetLine k) = pure (k "fake input")

-- 実際使うところ
echo :: Teletype String
echo = do
  a <- getLine
  putStrLn a
  putStrLn "Finished"
  pure $ a <> a

main :: Effect Unit
main = do
  a <- run $ echo
  log a
