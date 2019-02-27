module Engine.TH where

import Relude
import Control.Lens
import Data.Char (toUpper)
import Language.Haskell.TH

makeFieldsCustom :: Name -> DecsQ
makeFieldsCustom = makeLensesWith $ defaultFieldRules
    & lensField .~ custom
    where
    custom _ _ x = [MethodName (mkName className) (mkName methodName)]
        where
        name       = drop 1 $ dropWhile (/= '_') $ nameBase x
        methodName = name
        className  = "Has" <> overHead toUpper name

    overHead _ []     = []
    overHead f (a:as) = f a : as

