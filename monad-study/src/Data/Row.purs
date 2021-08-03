module Data.Row where

import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Prim.Row as Row

-- https://github.com/purescript/purescript/blob/ee0b3d3911/tests/purs/passing/PolyLabels.purs
get ::
  forall row tail symbol a
   . IsSymbol symbol -- これがないと reflectSymbol が呼べない
  => Row.Cons symbol a tail row -- ここでrowに対して、symbolとその値aが存在する制約をかける。これがあると存在するSymbolしか指定できなくなる。
  => SProxy symbol -- SProxyにsymbolを渡すことでTypeに変換
  -> Record row -- { | row } でもOK. これもRecordにrowを渡すことでTypeに変換している
  -> a
get l = unsafeGet (reflectSymbol l)

-- 補足: aはどこからも使われていない。それは更新前の値が何かをここでは意識していないから。同じsymbolさえあればよい。
set
  :: forall r1 r2 tail symbol a b
   . IsSymbol symbol
  => Row.Cons symbol a tail r1 -- 更新前のRowの定義
  => Row.Cons symbol b tail r2 -- 更新後のRowの定義(↑とどちらにも同じsymbolが指定されている)
  => SProxy symbol
  -> b -- symbolに対して新しく紐付ける値。↑でr2のRowにはこれが存在することが示されている
  -> Record r1
  -> Record r2
set l = unsafeSet (reflectSymbol l)

-- 実体はRowSpec.jsにある
foreign import unsafeSet
  :: forall r1 r2 a
   . String
  -> a
  -> Record r1
  -> Record r2

foreign import unsafeGet
  :: forall r a
   . String
  -> Record r
  -> a
