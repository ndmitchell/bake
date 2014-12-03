
module General.HTML(
    -- * Library
    HTML, HTML_, renderHTML, str_, raw_,
    Attribute, attribute_,
    tag_, tag__,
    (<>),
    -- * Tags
    br_, style__, link__,
    pre_, b_, html_, head_, title_, body_,
    a__, span__, p__,
    href_, class_, name_, rel_, type_, style_, id_,
    unlines_,
    ) where

import Control.Applicative
import Data.Monoid
import Control.Monad
import Data.Char


---------------------------------------------------------------------
-- LIBRARY

data Rope = Branch [Rope] | Leaf String

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


data HTML_ a = HTML_ Rope a

type HTML = HTML_ ()

renderHTML :: HTML -> String
renderHTML (HTML_ x _) = renderRope x

nullHTML :: HTML -> Bool
nullHTML (HTML_ x _) = nullRope x

instance Monoid a => Monoid (HTML_ a) where
    mempty = HTML_ mempty mempty
    mappend (HTML_ x1 x2) (HTML_ y1 y2) = HTML_ (x1 `mappend` y1) (x2 `mappend` y2)

instance Functor HTML_ where
    fmap f (HTML_ a b) = HTML_ a $ f b

instance Applicative HTML_ where
    pure = HTML_ mempty
    HTML_ x1 x2 <*> HTML_ y1 y2 = HTML_ (x1 `mappend` y1) (x2 y2)

instance Monad HTML_ where
    return = pure
    HTML_ x1 x2 >>= f = let HTML_ y1 y2 = f x2 in HTML_ (x1 `mappend` y1) y2


str_ :: String -> HTML
str_ = raw_ . escapeHTML

raw_ :: String -> HTML
raw_ x = HTML_ (Leaf x) ()

escapeHTML :: String -> String
escapeHTML = concatMap $ \c -> case c of
    '<'  -> "&lt;"
    '>'  -> "&gt;"
    '&'  -> "&amp;"
    '\"' -> "&quot;"
    '\'' -> "&#39;"
    x    -> [x]


data Attribute = Attribute {fromAttribute :: String}

valid (x:xs) | isAlpha x && all isAlpha xs = True
valid x = error $ "Not a valid HTML name, " ++ show x

attribute_ :: String -> String -> Attribute
attribute_ a b | valid a = Attribute $ a ++ "=\"" ++ escapeHTML b ++ "\""


tag__ :: String -> [Attribute] -> HTML -> HTML
tag__ name at inner | valid name = do
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
link__ at = tag__ "link" at mempty
style__ at body = tag__ "style" at $ raw_ body

pre_ = tag_ "pre"
b_ = tag_ "b"
html_ = tag_ "html"
head_ = tag_ "head"
title_ = tag_ "title"
body_ = tag_ "body"

a__ = tag__ "a"
span__ = tag__ "span"
p__ = tag__ "p"

href_ = attribute_ "href"
class_ = attribute_ "class"
name_ = attribute_ "name"
rel_ = attribute_ "rel"
type_ = attribute_ "type"
style_ = attribute_ "style"
id_ = attribute_ "id"

unlines_ = mconcat . map (<> str_ "\n")
