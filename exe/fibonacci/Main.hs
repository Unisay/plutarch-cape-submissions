module Main (main) where

import Compile (compileToUplc)
import Plutarch.Internal.Term (punsafeCoerce)
import Plutarch.Prelude

-- this is faster than `Plutarch.Internal.Fix.pfix` but generates more bloat uplc.
pfix'' :: (Term s (a :--> b) -> Term s (a :--> b)) -> Term s (a :--> b)
pfix'' f =
  plam (\r -> f (punsafeCoerce r # r))
    # plam (\r -> f (punsafeCoerce r # r))

pfibo :: Term s (PInteger :--> PInteger)
pfibo =
  pfix'' $ \r -> plam $ \x ->
    pif (x #<= 1) x (r # (x - 1) + r # (x - 2))

main :: IO ()
main = compileToUplc pfibo "fibonacci.uplc"
