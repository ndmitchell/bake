{-# LANGUAGE ViewPatterns, GeneralizedNewtypeDeriving #-}

-- | Library for defining HTML fragments.
--   The tags will be properly nested, and all strings will be HTML escaped.
--   As an example:
--
-- > renderHTML $ html_ $
-- >    ol__ [style_ "color:darkgreen"] $
-- >        forM_ [1..10] $ \i -> li_ $ str_ $ "item number: " & show i
module General.HTML(
    -- * HTML data type
    HTML, HTML_, renderHTML, valueHTML,
    -- * Constructing pieces
    Attribute, attribute_, tag_, tag__, str_, raw_,
    -- * Tags
    br_, style__, link__, hr_,
    pre_, b_, html_, head_, title_, body_, h1_, h2_, ul_, ol_, li_, p_, table_, thead_, tr_, td_, tbody_, i_,
    a__, span__, p__, h2__, tr__, ol__,
    -- * Attributes
    href_, class_, name_, rel_, type_, style_, id_,
    -- * Functions
    (<>),
    url_,
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


-- | Escape a URL using % encoding.
url_ :: String -> String
url_ = concatMap f
    where
        f x | (x >= 'A' && x <= 'Z') || (x >= 'a' && x <= 'z') || (x >= '0' && x <= '9') || x `elem` "-_.~" = [x]
        f (ord -> x) = "%" ++ ['0' | x < 16] ++ showHex x ""

-- | The type for constructing HTML. It is a 'Monad' and 'Monoid'.
--   Typically the value paramter is '()', in which case use 'HTML'.
newtype HTML_ a = HTML_ {fromHTML_ :: Writer Rope a}
    deriving (Eq,Ord,Functor,Applicative,Monad)

-- | An alias for 'HTML_' with no interesting type.
type HTML = HTML_ ()

-- | Get the value out of an 'HTML_'.
valueHTML :: HTML_ a -> a
valueHTML = fst . runWriter . fromHTML_

-- | Render some 'HTML'.
renderHTML :: HTML -> String
renderHTML = renderRope . execWriter . fromHTML_

nullHTML :: HTML -> Bool
nullHTML = nullRope . execWriter . fromHTML_

instance Monoid a => Monoid (HTML_ a) where
    mempty = return mempty
    mappend = liftM2 mappend

instance NFData a => NFData (HTML_ a) where
    rnf = rnf . runWriter . fromHTML_

-- | Turn a string into a text fragment of HTML, escaping any characters which mean something in HTML.
str_ :: String -> HTML
str_ = raw_ . escapeHTML

-- | Turn a string into an HTML fragment, applying no escaping. Use this function carefully.
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


-- | An attribute for a tag.
data Attribute = Attribute {fromAttribute :: String}

valid (x:xs) | isAlpha x && all isAlphaNum xs = True
valid x = error $ "Not a valid HTML name, " ++ show x

-- | Construct an Attribute from a key and value string. The value will be escaped.
attribute_ :: String -> String -> Attribute
attribute_ a b | valid a = Attribute $ a ++ "=\"" ++ escapeHTML b ++ "\""
               | otherwise = error $ "Invalid attribute name, " ++ a


-- | Given a tag name, a list of attributes, and some content HTML, produce some new HTML.
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

-- | Like 'tag__' but with no attributes.
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
ol_ = tag_ "ol"
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
ol__ = tag__ "ol"

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
