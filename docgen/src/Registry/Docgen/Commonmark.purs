module Registry.Docgen.Commonmark
  ( MarkdownOptions
  , renderMarkdownHTML
  ) where

import Data.Function.Uncurried (Fn2, runFn2)
import Registry.Docgen.HTML (HTML)

type MarkdownOptions = { safe :: Boolean }

foreign import _renderMarkdownHTML :: Fn2 MarkdownOptions String HTML

renderMarkdownHTML :: MarkdownOptions -> String -> HTML
renderMarkdownHTML = runFn2 _renderMarkdownHTML
