module QueryBuilder where

import Agda.Syntax.Parser
import Agda.Syntax.Parser.Parser
import Agda.Syntax.Parser.Monad
import Agda.Utils.Pretty
import Control.Monad.IO.Class
import System.FilePath
import Parser


maxRef :: PairLeft -> Int
maxRef U = -1
maxRef (Ref i) = i
maxRef (Func l r) = max (maxRef l) (maxRef r)
maxRef (Product (x : xs)) = foldl max (maxRef x) (map maxRef xs)
maxRef (Escape _) = -2

runQueryBuilder :: FilePath -> FilePath -> String -> IO ()
runQueryBuilder searchfile outputfile typesig =
  runPMIO (typeThingsFromFile searchfile >>= \tts ->
    let pmpl = typeSigToPairLeft tts typesig
    in  runPMIO (pmpl >>= \pl ->
        liftIO ((appendFile outputfile ("\n\n-- " ++ typesig ++ "\n"))) >>
        liftIO ((appendFile outputfile ("tc1 : TypeCode " ++ (show (maxRef pl)) ++ "\n"))) >>
        liftIO ((appendFile outputfile ("tc1 = " ++ (stringifyPairLeft pl) ++ "\n\n"))) >>
        liftIO ((appendFile outputfile "search1 : HomogeneousList\n")) >>
        liftIO ((appendFile outputfile "search1 = search tc1 searchModule")) >>
        return ()
    )) >>
    return ()

buildQuery :: FilePath -> String -> IO ()
buildQuery sourceFile typesig =
  let outputFile = replaceBaseName sourceFile (takeBaseName sourceFile ++ "Index")
  in  runQueryBuilder sourceFile outputFile typesig

typeSigToPairLeft :: [ TypeThing ] -> String -> PM PairLeft
typeSigToPairLeft existing typesig =
  let toparse = typesig
      pmexpr = Agda.Syntax.Parser.parse Agda.Syntax.Parser.exprParser toparse
  in  pmexpr >>= \f -> return (exprToPairLeft existing f)
