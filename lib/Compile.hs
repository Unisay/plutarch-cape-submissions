module Compile (compileToUplc) where

import Control.Lens (traverseOf)
import Plutarch.Internal.Term (ClosedTerm, compile)
import Plutarch.Script (Script (Script))
import PlutusCore (FreeVariableError, runQuoteT)
import PlutusCore.Pretty (prettyPlcClassic)
import UntypedPlutusCore (
  fakeNameDeBruijn,
  progTerm,
  programMapNames,
  unDeBruijnTerm,
 )

{- | Compile a closed Plutarch term to a UPLC file.

This function handles the full compilation pipeline:
1. Compile Plutarch term to Plutus Script
2. Convert to DeBruijn notation
3. Pretty-print to text format
4. Write to specified file
-}
compileToUplc :: FilePath -> ClosedTerm a -> IO ()
compileToUplc outputPath term =
  case compile mempty term of
    Left _ -> error $ "Compilation failed for " <> outputPath
    Right (Script s) ->
      case runQuoteT $
        traverseOf progTerm unDeBruijnTerm $
          programMapNames fakeNameDeBruijn s of
        Left (_ :: FreeVariableError) ->
          error $ "DeBruijn conversion failed for " <> outputPath
        Right s' -> do
          let uplcText = show $ prettyPlcClassic s'
          writeFile outputPath uplcText
          putStrLn $ "Successfully compiled to " <> outputPath
