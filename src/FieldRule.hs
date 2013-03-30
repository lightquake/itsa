-- | Small helper module for making fields the way I like them.
module FieldRule (underscoredCamelCaseFields) where

import           Control.Applicative
import           Control.Lens

-- | This is like the normal CamelCase rule, except it prefixes
-- everything with _.
underscoredCamelCaseFields :: FieldRules
underscoredCamelCaseFields = FieldRules prefix rawLens niceLens' classNaming
  where niceLens' x = maybeUnderscore <$> niceLens x
        maybeUnderscore ('_':xs) = '_' : xs
        maybeUnderscore xs = '_' : xs
        FieldRules prefix rawLens niceLens classNaming = camelCaseFields
