module TaglessFinal.Definition where

import Prelude

import Control.Monad.Reader (ReaderT(..))
import Effect.Aff (Aff)
import Type.Equality as TE
import Type.Row (type (+))

type User = { id :: String, name :: String, optionId :: String }

type UserOption = { optionId :: String, isTrial :: Boolean }

-- ユーザー情報のリポジトリ
class UserRepository m where
  findUserById :: String -> m User
  findOptionById :: String -> m UserOption

-- ユーザーIDに紐づくユーザーのオプションを返す関数
findUserOptionByUserId :: forall m. Monad m => UserRepository m => String -> m UserOption
findUserOptionByUserId userId = do
  user <- findUserById userId  -- ユーザーIDをもとにユーザーを取得
  findOptionById user.optionId -- オプションIDをもとにオプションを取得

-- 合成を考えて拡張可能にしておく
type UserRepositoryFunctions r = (
  findUserById :: String -> User,
  findOptionById :: String -> UserOption
  | r
)

instance instanceRepository
  :: TE.TypeEquals f (Record (UserRepositoryFunctions + r))
  => UserRepository (ReaderT f Aff) where
  findUserById userId = ReaderT $ TE.to >>> \f ->
    pure $ f.findUserById userId
  findOptionById optionId = ReaderT $ TE.to >>> \f ->
    pure $ f.findOptionById optionId

-- 何かを表示するプレゼンター
class Presenter m where
  display :: String -> m Unit

-- ユーザーオプションを受け取り、ユーザーがトライアルユーザーかどうかを出力する関数
displayTrialStatus :: forall m. Monad m => Presenter m => UserOption -> m Unit
displayTrialStatus option = display $ "This User is " <> if option.isTrial then "Trial Account." else "Premium Account."

-- 合成を考えて拡張可能にしておく
type PresenterFunctions r = (
  display :: String -> Unit
  | r
)

instance presenterMockT
  :: TE.TypeEquals f (Record (PresenterFunctions + r))
  => Presenter (ReaderT f Aff) where
  display message = ReaderT $ TE.to >>> \f ->
    pure $ f.display message


-- 2つのTagless Finalの合成
findUserWithDisplayTrialStatus :: forall m. Monad m => UserRepository m => Presenter m => String -> m Unit
findUserWithDisplayTrialStatus userId = do
  option <- findUserOptionByUserId userId
  displayTrialStatus option
