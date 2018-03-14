module Fields where

import Prelude

import Data.Record as R
import Data.Symbol (class IsSymbol)
import Data.Variant (SProxy(..), Variant)
import Data.Variant as V
import Type.Row (class RowLacks, class RowToList, Cons, Nil, RLProxy(..), kind RowList)

class GetCommonFieldRL (rl :: RowList) (row :: # Type) (label :: Symbol) (output :: Type) | rl -> row, row label -> output where
  getCommonFieldRL :: RLProxy rl -> SProxy label -> Variant row -> output

instance more ::
  ( IsSymbol sym1
  , IsSymbol label
  , GetCommonFieldRL (Cons sym2 rec rl) row label output
  , RowCons sym1 (Record row1) row roww'
  , RowCons label output row1' row1
  , RowLacks label row1'
  ) => GetCommonFieldRL (Cons sym1 (Record row1) (Cons sym2 rec rl)) roww' label output where
    getCommonFieldRL _ label =
      (getCommonFieldRL (RLProxy :: RLProxy (Cons sym2 rec rl)) label :: Variant row -> output)
      # V.on (SProxy :: SProxy sym1) (R.get label :: Record row1 -> output)

instance one ::
  ( IsSymbol sym
  , IsSymbol label
  , RowCons sym (Record row1) () row'
  , RowCons label output row2 row1
  , RowLacks label row2
  ) => GetCommonFieldRL (Cons sym (Record row1) Nil) row' label output where
    getCommonFieldRL _ label =
      V.case_
      # V.on (SProxy :: SProxy sym) (R.get label)

-- | If all cases of a (non-empty) variant have the form { label :: type | ... }
-- | (for some label and type), this allows us to extract out that common field.
getCommonField ::
  forall row rl label output.
    RowToList row rl =>
    GetCommonFieldRL rl row label output =>
  SProxy label -> Variant row -> output
getCommonField = getCommonFieldRL (RLProxy :: RLProxy rl)
