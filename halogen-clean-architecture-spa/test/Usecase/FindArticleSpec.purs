module Test.Usecase.FindArticleSpec where

import Prelude

import Component.State (State)
import Control.Monad.State (StateT, modify_, runStateT)
import Data.Tuple (snd)
import Effect.Aff (Aff)
import Port.ArticlePort (ArticlePortType, ArticleRunPortType)
import Presenter.ArticlePresenter (ArticlePresenterType, ArticleRunPresenter)
import Run (Run, extract)
import Run.Reader (runReader)
import Test.Mock (Mock, fun, mock, thenReturn, verify)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Usecase.FindArticle (findArticleByRun, findArticleByType)

spec :: Spec Unit
spec = do
  {-
    typeの関数の内容をモック的なものに書き換えることでテストする。
    presenterの関数が呼ばれたことはStateTを利用してモック的関数の中で書き換えた値を後から参照して検証する。
    portおよびpresenterの関数が期待する引数で呼ばれたことは検証できない。
  -}
  describe "FindArticleUsecase Test" do
    it "タイトルに紐づくArticleを取得してStateを更新できる" do
      let
        -- port.findByTitleやpresenter.updateの戻り値の型は m Article や m Unit となっており、
        -- 何らかのモナドで包まれている必要がある(定義上はモナドという制約はないけど)。
        -- このモナドは、これらの port や presenter をテスト対象である findArticleByType に渡すことで定まる。
        -- なぜならば findArticleByType には forall m. MonadState State m という制約がついているから。
        -- そしてこのテストでは、値が書き換わったか検証するため MonadState のインスタンスである StateT を利用したいので
        -- m の型は (StateT State Aff) となる。
        port = {
          findByTitle: \_ -> pure { title: "新しいタイトル" }
        } :: ArticlePortType (StateT State Aff)
        presenter =  {
          update: \_ -> modify_ (\_ -> {article: {title: "新しいタイトル"}})
        } :: ArticlePresenterType (StateT State Aff)

      result <- findArticleByType "Dummy" port presenter
                # flip runStateT {article: {title: "古いタイトル"}} -- flipで引数の順序を入れ替えることでこう実行できる
                <#> snd                                            -- Tuple(Unit, State)が返ってくるのでmapしてsndをかましている(書き換えた結果がほしい)

      -- 書き換えた結果を使って検証
      result `shouldEqual` {article: {title: "新しいタイトル"}}
    
    it "タイトルに紐づくArticleを取得してStateを更新できる(Runを使う版)" do
      let
        port = {
          find: pure { title: "新しいタイトル2" }
        } :: ArticleRunPortType
        {-
          presenter.updateの定義は
            update :: forall r. String -> Run (r) (m Unit)
          となっており、Runの(m Unit)の部分は↑のテストの場合と同じく MonadState のインスタンスでなければならない。
          この関数は Run を返すが、Runの a の部分をMonadStateのインスタンスである StateT にしており、
          これによりテストが実行可能になっている
        -}
        presenter = {
          update: \_ -> pure $ modify_ (\_ -> {article: {title: "新しいタイトル2"}})
        } :: ArticleRunPresenter (StateT State Aff)

      result <- findArticleByRun port presenter
                # runReader "unused"
                # extract                                            -- extractすることでStateTを取り出す
                # flip runStateT {article: {title: "古いタイトル"}}
                <#> snd

      result `shouldEqual` {article: {title: "新しいタイトル2"}}


  describe "FindArticleUsecase Test With Mock" do
    {-
      Mockを使ってテストする
      こちらのアプローチでは、期待する引数で呼び出されたかを検証することができる
    -}
    it "タイトルに紐づくArticleを取得してStateを更新できる" do
      let
        findByTitleMock = mock "古いタイトル" `thenReturn` pure { title: "新しいtitle" }
        port = { findByTitle: fun findByTitleMock } :: ArticlePortType (StateT State Aff)
        
        updateMock = mock "新しいtitle" `thenReturn` pure unit
        presenter =  { update: fun updateMock } :: ArticlePresenterType (StateT State Aff)

      -- 結果は不要
      _ <- findArticleByType "古いタイトル" port presenter
            # flip runStateT {article: {title: "Dummy"}} 

      -- 期待する引数で呼ばれたか検証
      verify findByTitleMock
      verify updateMock      

    it "タイトルに紐づくArticleを取得してStateを更新できる(Runを使う版)" do
      let
        -- findは定数を返すのでMockは不要
        port = { find: pure { title: "新しいタイトル" } } :: ArticleRunPortType

        -- こいつは型を明示してやる必要がある
        updateMock :: forall r. Mock (String -> Run (r) (StateT State Aff Unit))
        updateMock = mock "新しいタイトル" `thenReturn` pure (pure unit :: StateT State Aff Unit)

        presenter = { update: fun updateMock } :: ArticleRunPresenter (StateT State Aff)

      _ <- findArticleByRun port presenter
                # runReader ""
                # extract
                # flip runStateT {article: {title: ""}}

      verify updateMock
