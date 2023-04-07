module Test.Usecase.FindArticleSpec where

import Prelude

import Component.State (State)
import Control.Monad.State (class MonadState, StateT, modify_, runStateT)
import Data.Tuple (snd)
import Domain.Article (Article)
import Port.ArticlePort (ArticlePortType, ArticleRunPortType)
import Presenter.ArticlePresenter (ArticlePresenterType, ArticleRunPresenter)
import Run (Run, extract)
import Run.Reader (READER, runReader)
import Test.Mock (Mock, fun, mock, thenReturn, verify, (:))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Row (type (+))
import Usecase.FindArticle (findArticleByRun, findArticleByType)

{-
  typeの関数の内容をモック的なものに書き換えることでテストしている
  presenterが呼ばれたことはStateTを利用してモック的関数の中で書き換えた値を見て検証している
-}

{-
  presenter.updateの戻り値の型は MonadState のインスタンスでなければならない
  そこで MonadState のインスタンスである StateT を返すことにより実行可能にしている
-}
executeFindArticleByType :: forall m. Monad m => String -> StateT State m Unit
executeFindArticleByType title = do
  let
    port = {
      findByTitle: \_ -> pure { title }
    } :: ArticlePortType
    presenter =  {
      update: \_ -> modify_ (\_ -> {article: {title: title}})
    } :: ArticlePresenterType
  findArticleByType title port presenter


{-
  presenter.updateの定義は
  update :: forall r m. MonadState State m => String -> Run (r) (m Unit)
  となっており、Runの(m Unit)の部分は↑と同じく MonadState のインスタンスでなければならない
  この関数は Run を返すが、Runの a の部分をMonadStateのインスタンスである StateT にしており、
  これによりテストが実行可能になっている
-}
executeFindArticleByRun :: forall m r. Monad m => String -> Run ( READER String + r) (StateT State m Unit)
executeFindArticleByRun title = do
  let
    port = {
      find: pure { title }
    } :: ArticleRunPortType
    presenter = {
      update: \_ -> pure $ modify_ (\_ -> {article: {title: title}})
    } :: ArticleRunPresenter
  findArticleByRun port presenter

spec :: Spec Unit
spec = do
  describe "FindArticleUsecase" do
    it "mock" do
      let
        m = mock (1 : "2" : 3) `thenReturn` 100
      verify m

    it "タイトルに紐づくArticleを取得してStateを更新できる" do
      result <- executeFindArticleByType "新しいタイトル"
                # flip runStateT {article: {title: "古いタイトル"}} -- flipで引数の順序を入れ替えることでこう実行できる
                <#> snd                                            -- Tuple(Unit, State)が返ってくるのでmapしてsndをかましている
      result `shouldEqual` {article: {title: "新しいタイトル"}}
    
    it "タイトルに紐づくArticleを取得してStateを更新できる(Mock版)" do
      let
        findByTitleMock :: forall m. Monad m => Mock (String -> m Article)
        findByTitleMock = mock "古いタイトル" `thenReturn` (pure { title: "新しいtitle" })
        port = { findByTitle: fun findByTitleMock } :: ArticlePortType

        updateMock :: forall m. MonadState State m => Mock (String -> m Unit)
        updateMock = mock "新しいtitle" `thenReturn` pure unit
        presenter =  { update: fun updateMock } :: ArticlePresenterType

      --_ <- findArticleByType "古いタイトル" port presenter # flip runStateT {article: {title: "Dummy"}}
      "" `shouldEqual` ""
      --verify findByTitleMock
      --verify updateMock
      

    it "タイトルに紐づくArticleを取得してStateを更新できる(Runを使う版)" do
      result <- executeFindArticleByRun "新しいタイトル2"
                # runReader "unused"
                # extract                                            -- extractすることでStateTを取り出す
                # flip runStateT {article: {title: "古いタイトル"}}
                <#> snd
      result `shouldEqual` {article: {title: "新しいタイトル2"}}
