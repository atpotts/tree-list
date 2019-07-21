{-# LANGUAGE DeriveGeneric, FlexibleInstances, FlexibleContexts, RecordWildCards #-}

module Main where

import Data.TreeList (TreeListMap, TreeList(children, path, payload), TreeListData(..))

import ClassyPrelude hiding (sort)
import Control.Arrow ((>>>))
import Data.MonoTraversable (Element)
import Data.Monoid (All(..))
import Data.Map.Ordered (OMap)
import Options.Generic

data Options f w = Options
  { delim :: w ::: f Char <?> "Delimiter To Split input on - default '/'"
  , sort :: w ::: Bool <?> "Should the input be sorted"
  , compact :: w ::: Bool <?> "Compact Display"
  } deriving (Generic)

defaultOptions :: Options Maybe Unwrapped -> Options Identity Unwrapped
defaultOptions opts@Options{..} = opts {
  delim = Identity . fromMaybe '/' $ delim
}

instance ParseRecord (Options Maybe Wrapped) where
  parseRecord = parseRecordWithModifiers $
    defaultModifiers { shortNameModifier = firstLetter }

deriving instance Show (Options Maybe Unwrapped)

main :: IO ()
main = do
  x <- defaultOptions <$> unwrapRecord "Tree Formatting Programme"
  let delimiter = runIdentity . delim $ x
      compacted = compact x
      formatter :: [LText] -> LText
      formatter = case sort x of
        True -> textParse @Map delimiter >>> showPaths compacted
        False -> textParse @OMap delimiter >>> showPaths compacted
  interact $ lines >>> formatter

textParse :: forall m. (TreeList (TreeListMap m LText All))
  => Char -> [LText] -> TreeListMap m LText All
textParse = parseTrie

parseTrie ::
     ( Eq x
     , IsSequence xs, Element xs ~ x , Ord xs
     , MonoFoldable fxs, Element fxs ~ xs
     , TreeList t, TreeListPayload t ~ All, TreeListKey t ~ xs
     ) => x -> fxs -> t
parseTrie sep rows = concatMap (flip path (All True) . splitElem sep) rows

type Textish txt = (Ord txt, IsString txt, Textual txt)

type Prefixer x = [x] -> [x]

horiz, verti, elbow, branc, blanc, teeee :: Textish txt => txt
horiz = "───"
verti = " │ "
elbow = " └─"
branc = " ├─"
blanc = "   "
teeee = "─┬─"

indentWith :: Textish txt =>  txt -> Prefixer txt
indentWith name [] = [name]
indentWith name (x:xs) =
  (name ++ x) : map (foldr (\_ acc -> (" "<>) acc) "" name ++) xs

applyPrefixes :: Textish txt => txt -> txt -> Prefixer txt
applyPrefixes first rest (x:xs) = (first++x) : map (rest++) xs
applyPrefixes _ _ [] = []

firstBlock, midBlocks, lastBlock, onlyBlock :: Textish txt => Prefixer txt
firstBlock = applyPrefixes teeee verti
onlyBlock = applyPrefixes horiz blanc
midBlocks = applyPrefixes branc verti
lastBlock = applyPrefixes elbow blanc

prefixFirstBlocks, prefixBlocks :: Textish txt => Prefixer [txt]

prefixFirstBlocks [] = []
prefixFirstBlocks [x] = [onlyBlock x]
prefixFirstBlocks (x:xs) = firstBlock x : prefixBlocks xs

prefixBlocks [] = []
prefixBlocks [x] = [lastBlock x]
prefixBlocks (x:xs) = midBlocks x : prefixBlocks xs


showPaths :: (TreeList t, TreeListKey t ~ txt, Textish txt) => Bool -> t -> txt
showPaths compact = unlines . concat . sp
  where
    sp t = map (uncurry indent) $ children t
    indent name value
      | compact = indentWith name (concat . prefixFirstBlocks $ sp value)
      | otherwise = name : (concat . prefixBlocks $ sp value)
