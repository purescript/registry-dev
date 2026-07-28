module Registry.Docgen.HTML where

import Prelude hiding (div)

import Data.Array (fold)
import Data.Foldable (class Foldable, foldMap)
import Data.Newtype (class Newtype, un)
import Data.String (Pattern(..), Replacement(..), replaceAll)

newtype HTML = HTML String

derive newtype instance Semigroup HTML
derive newtype instance Monoid HTML
derive instance Newtype HTML _

newtype Attribute = Attribute String

instance Semigroup Attribute where
  append (Attribute x) (Attribute y) = case x, y of
    "", _ -> Attribute y
    _, "" -> Attribute x
    _, _ -> Attribute (x <> " " <> y)

instance Monoid Attribute where
  mempty = Attribute ""

escapeHtml :: String -> String
escapeHtml =
  replaceAll (Pattern "&") (Replacement "&amp;")
    >>> replaceAll (Pattern "<") (Replacement "&lt;")
    >>> replaceAll (Pattern ">") (Replacement "&gt;")
    >>> replaceAll (Pattern "\"") (Replacement "&quot;")
    >>> replaceAll (Pattern "'") (Replacement "&#x27;")

attr :: String -> String -> Attribute
attr attrName attrValue = Attribute $ attrName <> "=\"" <> escapeHtml attrValue <> "\""

text :: String -> HTML
text = HTML <<< escapeHtml

renderTagAttributes :: Array Attribute -> String
renderTagAttributes attrs = case fold attrs of
  Attribute "" -> ""
  Attribute all -> " " <> all

renderChildren :: Array HTML -> String
renderChildren = un HTML <<< fold

element :: String -> Array Attribute -> Array HTML -> HTML
element tagName attrs children =
  HTML $ "<" <> tagName <> renderTagAttributes attrs <> ">" <> renderChildren children <> "</" <> tagName <> ">"

leafElement :: String -> Array Attribute -> HTML
leafElement tagName attrs =
  HTML $ "<" <> tagName <> renderTagAttributes attrs <> " />"

div :: Array Attribute -> Array HTML -> HTML
div = element "div"

span :: Array Attribute -> Array HTML -> HTML
span = element "span"

p :: Array Attribute -> Array HTML -> HTML
p = element "p"

pre :: Array Attribute -> Array HTML -> HTML
pre = element "pre"

code :: Array Attribute -> Array HTML -> HTML
code = element "code"

section :: Array Attribute -> Array HTML -> HTML
section = element "section"

article :: Array Attribute -> Array HTML -> HTML
article = element "article"

main :: Array Attribute -> Array HTML -> HTML
main = element "main"

h1 :: Array Attribute -> Array HTML -> HTML
h1 = element "h1"

h2 :: Array Attribute -> Array HTML -> HTML
h2 = element "h2"

h3 :: Array Attribute -> Array HTML -> HTML
h3 = element "h3"

h4 :: Array Attribute -> Array HTML -> HTML
h4 = element "h4"

h5 :: Array Attribute -> Array HTML -> HTML
h5 = element "h5"

h6 :: Array Attribute -> Array HTML -> HTML
h6 = element "h6"

ul :: Array Attribute -> Array HTML -> HTML
ul = element "ul"

ol :: Array Attribute -> Array HTML -> HTML
ol = element "ol"

li :: Array Attribute -> Array HTML -> HTML
li = element "li"

a :: Array Attribute -> Array HTML -> HTML
a = element "a"

button :: Array Attribute -> Array HTML -> HTML
button = element "button"

form :: Array Attribute -> Array HTML -> HTML
form = element "form"

img :: Array Attribute -> HTML
img = leafElement "img"

input :: Array Attribute -> HTML
input = leafElement "input"

meta :: Array Attribute -> HTML
meta = leafElement "meta"

link :: Array Attribute -> HTML
link = leafElement "link"

head :: Array HTML -> HTML
head = element "head" []

body :: Array Attribute -> Array HTML -> HTML
body = element "body"

htmlTitle :: Array HTML -> HTML
htmlTitle = element "title" []

wbr :: HTML
wbr = leafElement "wbr" []

dl :: Array Attribute -> Array HTML -> HTML
dl = element "dl"

dt :: Array Attribute -> Array HTML -> HTML
dt = element "dt"

dd :: Array Attribute -> Array HTML -> HTML
dd = element "dd"

class_ :: String -> Attribute
class_ = attr "class"

id :: String -> Attribute
id = attr "id"

href :: String -> Attribute
href = attr "href"

lang :: String -> Attribute
lang = attr "lang"

src :: String -> Attribute
src = attr "src"

alt :: String -> Attribute
alt = attr "alt"

type_ :: String -> Attribute
type_ = attr "type"

name :: String -> Attribute
name = attr "name"

title :: String -> Attribute
title = attr "title"

value :: String -> Attribute
value = attr "value"

rel :: String -> Attribute
rel = attr "rel"

placeholder :: String -> Attribute
placeholder = attr "placeholder"

html :: Array Attribute -> Array HTML -> HTML
html = element "html"

doctype :: HTML
doctype = HTML "<!DOCTYPE html>\n"

when :: Boolean -> (Unit -> HTML) -> HTML
when b k = if b then k unit else mempty

forEach :: forall f a. Foldable f => f a -> (a -> HTML) -> HTML
forEach = flip foldMap
