module Parser where

-- import Agda.Compiler.CallCompiler
-- import Agda.TypeChecking.Monad.Base
import Agda.Syntax.Parser
import Agda.Syntax.Concrete
import Agda.Syntax.Common
import Agda.Utils.FileName
import Agda.Utils.Pretty
import Data.Text.Lazy as T
import Data.Text.Internal.Lazy
import Data.List
import Control.Monad.IO.Class
import System.FilePath
import System.FilePath.Posix

data PairLeft = U | Ref Int | Func PairLeft PairLeft | Product [ PairLeft ] | Escape String deriving (Show)

data TypeThing = Pair PairLeft String deriving (Show)

-- turns a series of declarations (that we know how to deal with) into outputtable "pairs"
declsToPairs :: [ Declaration ] -> [ TypeThing ]
declsToPairs decls = declsToPairs' [] decls

declsToPairs' :: [ TypeThing ] -> [ Declaration ] -> [ TypeThing ]
declsToPairs' tts [] = tts
declsToPairs' tts ((Data _ name _ _ subdecls) : xs) =
  declsToPairs' (tts ++
                 [Pair U (show name)]
                 --(declsToPairs' (tts ++ [Pair U (show name)]) subdecls)
                )
                (subdecls ++ xs)
declsToPairs' tts ((TypeSig _ _ name expr) : xs) =
  declsToPairs' (tts ++
                 [Pair (exprToPairLeft tts expr) (show name)]
                )
                xs
declsToPairs' tts (_ : xs) = declsToPairs' tts xs

exprToPairLeft :: [ TypeThing ] -> Expr -> PairLeft
exprToPairLeft existing (Generalized expr) = exprToPairLeft existing expr
exprToPairLeft existing (RawApp _ ((Set _): exprs)) = U
  -- use this one instead for relative indexing
-- exprToPairLeft existing (RawApp _ ((Ident (QName name)) : exprs)) = Ref (offsetFromDef existing (show name))
exprToPairLeft existing (RawApp _ ((Ident (QName name)) : exprs)) = Ref (absoluteIndexOfRef existing (show name))
exprToPairLeft existing (Fun _ arg expr) = Func (argToRef existing arg) (exprToPairLeft existing expr)

multiIdentToRef :: [ TypeThing ] -> [ Expr ] -> PairLeft
multiIdentToRef existing [] = Ref (-10)
multiIdentToRef existing (Ident (QName name) : []) = Ref (absoluteIndexOfRef existing (show name))
multiIdentToRef existing ids@(Ident (QName namel) : Ident (QName namem) : Ident (QName namer) : xs) =
  if   show namem == "×"
  then Product (Prelude.map (\name -> Ref (absoluteIndexOfRef existing name))
                            (Prelude.filter (\name -> name /= "×")
                                            (Prelude.map (\e -> case e of
                                                             Ident (QName name) -> show name)
                                                         ids)))
  --then Product (Ref (absoluteIndexOfRef existing (show namer))) (Ref (absoluteIndexOfRef existing (show namel)))
  else Ref (-12)
multiIdentToRef existing _ = Ref (-13)

argToRef :: [ TypeThing ] -> Arg Expr -> PairLeft
argToRef existing arg = case unArg arg of
  -- use this one instead for relative indexing
  -- Ident (QName name) -> offsetFromDef existing (show name)
  Ident (QName name) -> Ref (absoluteIndexOfRef existing (show name))
  --Ident qu@(Qual name qn) -> multiNamesToRef existing (namesFromQual qu)
  RawApp _ idents -> multiIdentToRef existing idents
  _ -> Ref (-2)

absoluteIndexOfRef :: [ TypeThing ] -> String -> Int
absoluteIndexOfRef [] name = -1
absoluteIndexOfRef ((Pair u s) : xs) name
  | s == name = 1
absoluteIndexOfRef (_ : xs) name = 1 + (absoluteIndexOfRef xs name)

stringifyProduct :: [ PairLeft ] -> String
stringifyProduct [] = ""
stringifyProduct (x : []) = stringifyPairLeft x
stringifyProduct (x : xs) = (stringifyPairLeft x) ++ " ×× " ++ stringifyProduct xs

stringifyPairLeft :: PairLeft -> String
stringifyPairLeft U = "U"
stringifyPairLeft (Ref i) = "⟨ " ++ (show i) ++ " ⟩"
stringifyPairLeft (Func l r) = (stringifyPairLeft l) ++ " ⇒ " ++ (stringifyPairLeft r)
stringifyPairLeft (Product xs) = stringifyProduct xs
stringifyPairLeft (Escape s) = s

stringifyTypeThing :: TypeThing -> String
stringifyTypeThing (Pair pl s) = (stringifyPairLeft pl) ++ " , " ++ s

typeThingToStringListElement :: TypeThing -> String
typeThingToStringListElement tt = "∷ " ++ (stringifyTypeThing tt)

outputTypeThings :: FilePath -> String -> PM [ TypeThing ] -> IO ()
outputTypeThings filepath modulename pmtt =
  (writeFile filepath "{-# OPTIONS --type-in-type #-}\n") >>
  (appendFile filepath ("module " ++ modulename ++ "Index where\n")) >>
  (appendFile filepath ("open import " ++ modulename ++ "\n")) >>
  (appendFile filepath "open import TypingUniverse\n\n") >>
  (appendFile filepath "searchModule : Module _\n") >>
  (appendFile filepath "searchModule = [] ") >>
  runPMIO (pmtt >>= \f ->
              liftIO (appendFile filepath (typeThingToStringListElement (Data.List.head f) ++ "\n")) >>
              liftIO (mapM_
                      (\tt -> appendFile filepath ("        " ++ typeThingToStringListElement tt ++ "\n"))
                      (Data.List.tail f))
          ) >>
  return ()

typeThingsFromFile :: FilePath -> PM [ TypeThing ]
typeThingsFromFile sourceFile =
  let decls      = withModule (mkAbsolute sourceFile) (\(ps, ds) -> topLevelModule ds)
      filtered   = decls >>= \f -> return (extractImportant f)
  in  pmdeclstopairs filtered

runParser :: FilePath -> IO ()
runParser sourceFile =
  let outputFile = replaceBaseName sourceFile (takeBaseName sourceFile ++ "Index")
      moduleName = takeBaseName sourceFile
      pairs      = typeThingsFromFile sourceFile
  in  outputTypeThings outputFile moduleName pairs

parseModule :: AbsolutePath -> PM Module
parseModule path = do f <- readFilePM path
                      fmap fst $ parseFile moduleParser path (T.unpack f)

withModule :: AbsolutePath -> (Module -> a) -> PM a
withModule path f = do mod <- parseModule path
                       return (f mod)

allTypeSigs :: Module -> [ Declaration ]
allTypeSigs (ps, ds) = Prelude.filter isTypeSig ds

topLevelModule :: [ Declaration ] -> [ Declaration ]
topLevelModule ((Module _ _ _ decls) : xs) = decls
topLevelModule _ = []

-- filter for just type sigs
isTypeSig :: Declaration -> Bool
isTypeSig (TypeSig _ _ _ _) = True
isTypeSig _ = False

-- filter for just things we care about
isImportant :: Declaration -> Bool
isImportant (TypeSig _ _ _ _) = True
isImportant (DataSig _ _ _ _) = True
--isImportant (Data _ _ _ _ _) = True
isImportant (Record _ _ _ _ _ _ _ _) = True
isImportant (Postulate _ _) = True
isImportant _ = False


extractTypeSigs :: [ Declaration ] -> [ Declaration ]
extractTypeSigs [] = []
extractTypeSigs ((TypeSig a b c d) : xs) = (TypeSig a b c d) : (extractTypeSigs xs)
extractTypeSigs ((Postulate _ decls) : xs ) = (extractTypeSigs decls) ++ (extractTypeSigs xs)
extractTypeSigs (_ : xs) = extractTypeSigs xs


extractImportant :: [ Declaration ] -> [ Declaration ]
extractImportant [] = []
extractImportant ((TypeSig a b c d) : xs) = (TypeSig a b c d) : (extractImportant xs)
extractImportant ((DataSig a b c d) : xs) = (DataSig a b c d) : (extractImportant xs)
extractImportant ((Data a b c d e) : xs) = (Data a b c d e) : (extractImportant xs)
extractImportant ((Record a b c d e f g h) : xs) = (Record a b c d e f g h) : (extractImportant xs)
--extractImportant ((Postulate _ decls) : xs ) = (extractImportant decls) ++ (extractImportant xs)
extractImportant (_ : xs) = extractImportant xs

pmdeclstopairs :: PM [ Declaration ] -> PM [ TypeThing ]
pmdeclstopairs pm = pm >>= \f -> return (declsToPairs f)
