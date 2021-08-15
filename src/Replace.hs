module Replace ( mkReplacement
               , replaceOne
               , replaceMany
               ) where

import Data.Text (Text, replace)

{-
  Poor man's templating -- until I find a library.

  Intended usage:
  > replaceOne (mkReplacement "{user}" "foo") "{user}@example.org"
  "foo@example.org"

-}

data Replace = Replace { replaceFrom :: Text
                       , replaceTo :: Text
                       }

mkReplacement :: Text -> Text -> Replace
mkReplacement = Replace

replaceOne :: Replace -> Text -> Text
replaceOne r = replace (replaceFrom r) (replaceTo r)

replaceMany :: Foldable f => f Replace -> Text -> Text
replaceMany rs source = foldr replaceOne source rs
