{-# LANGUAGE ViewPatterns, GeneralizedNewtypeDeriving #-}

module General.HTML(
    -- * Library
    url_,
    HTML, HTML_, renderHTML, valueHTML, str_, raw_,
    Attribute, attribute_,
    tag_, tag__,
    (<>),
    -- * Tags
    br_, style__, link__, hr_,
    pre_, b_, html_, head_, title_, body_, h1_, h2_, ul_, ol_, li_, p_, table_, thead_, tr_, td_, tbody_, i_,
    a__, span__, p__, h2__, tr__, ol__,
    href_, class_, name_, rel_, type_, style_, id_,
    -- * Functions
    unlines_, commas_, commasLimit_, header_
    ) where

import Control.Applicative
import Data.Monoid
import Data.List
import Control.Monad
import Control.Monad.Trans.Writer
import Control.DeepSeq
import Data.Char
import Numeric
import Prelude


---------------------------------------------------------------------
-- LIBRARY

data Rope = Branch [Rope] | Leaf String

instance Eq Rope where a == b = renderRope a == renderRope b
instance Ord Rope where compare a b = compare (renderRope a) (renderRope b)

instance NFData Rope where
    rnf (Branch x) = rnf x
    rnf (Leaf x) = rnf x

renderRope :: Rope -> String
renderRope x = f x ""
    where f (Branch []) k = k
          f (Branch (x:xs)) k = f x $ f (Branch xs) k
          f (Leaf x) k = x ++ k

nullRope :: Rope -> Bool
nullRope (Branch xs) = all nullRope xs
nullRope (Leaf x) = null x

instance Monoid Rope where
    mempty = Branch []
    mappend a b = Branch [a,b]
    mconcat = Branch


url_ :: String -> String
url_ = concatMap f
    where
        f x | (x >= 'A' && x <= 'Z') || (x >= 'a' && x <= 'z') || (x >= '0' && x <= '9') || x `elem` "-_.~" = [x]
        f (ord -> x) = "%" ++ ['0' | x < 16] ++ showHex x ""


newtype HTML_ a = HTML_ {fromHTML_ :: Writer Rope a}
    deriving (Eq,Ord,Functor,Applicative,Monad)

type HTML = HTML_ ()

valueHTML :: HTML_ a -> a
valueHTML = fst . runWriter . fromHTML_

renderHTML :: HTML -> String
renderHTML = renderRope . execWriter . fromHTML_

nullHTML :: HTML -> Bool
nullHTML = nullRope . execWriter . fromHTML_

instance Monoid a => Monoid (HTML_ a) where
    mempty = return mempty
    mappend = liftM2 mappend

instance NFData a => NFData (HTML_ a) where
    rnf = rnf . runWriter . fromHTML_


str_ :: String -> HTML
str_ = raw_ . escapeHTML

raw_ :: String -> HTML
raw_ = HTML_ . tell . Leaf

escapeHTML :: String -> String
escapeHTML = concatMap $ \c -> case c of
    '<'  -> "&lt;"
    '>'  -> "&gt;"
    '&'  -> "&amp;"
    '\"' -> "&quot;"
    '\'' -> "&#39;"
    x    -> [x]


data Attribute = Attribute {fromAttribute :: String}

valid (x:xs) | isAlpha x && all isAlphaNum xs = True
valid x = error $ "Not a valid HTML name, " ++ show x

attribute_ :: String -> String -> Attribute
attribute_ a b | valid a = Attribute $ a ++ "=\"" ++ escapeHTML b ++ "\""
               | otherwise = error $ "Invalid attribute name, " ++ a


tag__ :: String -> [Attribute] -> HTML -> HTML
tag__ name at inner | not $ valid name = error $ "Invalid tag name, " ++ name
                    | otherwise = do
    -- if you collapse an "a", it goes wrong
    -- if you don't collapse a "br", it goes wrong
    let zero = nullHTML inner && name `elem` ["br","link"]
    raw_ $
        "<" ++
        unwords (name : map fromAttribute at) ++
        (if zero then " /" else "") ++ ">"
    unless zero $ do
        inner
        raw_ $ "</" ++ name ++ ">"

tag_ :: String -> HTML -> HTML
tag_ name = tag__ name []


---------------------------------------------------------------------
-- TAGS

br_ = tag_ "br" mempty
hr_ = tag_ "hr" mempty
link__ at = tag__ "link" at mempty
style__ at body = tag__ "style" at $ raw_ body

pre_ = tag_ "pre"
b_ = tag_ "b"
i_ = tag_ "i"
html_ = tag_ "html"
head_ = tag_ "head"
title_ = tag_ "title"
body_ = tag_ "body"
h1_ = tag_ "h1"
h2_ = tag_ "h2"
ul_ = tag_ "ul"
ol_ = tag_ "ul"
li_ = tag_ "li"
p_ = tag_ "p"
table_ = tag_ "table"
thead_ = tag_ "thead"
tr_ = tag_ "tr"
td_ = tag_ "td"
tbody_ = tag_ "tbody"

a__ = tag__ "a"
span__ = tag__ "span"
p__ = tag__ "p"
h2__ = tag__ "h2"
tr__ = tag__ "tr"
ol__ = tag__ "tr"

href_ = attribute_ "href"
class_ = attribute_ "class"
name_ = attribute_ "name"
rel_ = attribute_ "rel"
type_ = attribute_ "type"
style_ = attribute_ "style"
id_ = attribute_ "id"

unlines_ = mconcat . map (<> str_ "\n")
commas_ = mconcat . intersperse (str_ ", ")
commasLimit_ = limit_ commas_

limit_ :: ([HTML] -> HTML) -> Int -> [HTML] -> HTML
limit_ rejoin i xs = rejoin a <> str_ (if null b then "" else "...")
    where (a,b) = splitAt i xs


-- FIXME: hack, very much app-specific
header_ :: String -> String -> HTML
header_ tag x = a__ [id_ tag,href_ $ "#" ++ tag,class_ "self"] $ h2_ $ str_ x
