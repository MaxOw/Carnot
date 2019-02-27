module Engine.HasPattern where


class HasPatternTopLeft s where _PatternTopLeft :: s
pattern TopLeft :: HasPatternTopLeft s => s
pattern TopLeft <- _PatternTopLeft where TopLeft = _PatternTopLeft

class HasPatternTopCenter s where _PatternTopCenter :: s
pattern TopCenter :: HasPatternTopCenter s => s
pattern TopCenter <- _PatternTopCenter where TopCenter = _PatternTopCenter

class HasPatternTopRight s where _PatternTopRight :: s
pattern TopRight :: HasPatternTopRight s => s
pattern TopRight <- _PatternTopRight where TopRight = _PatternTopRight


class HasPatternMiddleLeft s where _PatternMiddleLeft :: s
pattern MiddleLeft :: HasPatternMiddleLeft s => s
pattern MiddleLeft <- _PatternMiddleLeft where MiddleLeft = _PatternMiddleLeft

class HasPatternMiddleCenter s where _PatternMiddleCenter :: s
pattern MiddleCenter :: HasPatternMiddleCenter s => s
pattern MiddleCenter <- _PatternMiddleCenter where MiddleCenter = _PatternMiddleCenter

class HasPatternMiddleRight s where _PatternMiddleRight :: s
pattern MiddleRight :: HasPatternMiddleRight s => s
pattern MiddleRight <- _PatternMiddleRight where MiddleRight = _PatternMiddleRight


class HasPatternBottomLeft s where _PatternBottomLeft :: s
pattern BottomLeft :: HasPatternBottomLeft s => s
pattern BottomLeft <- _PatternBottomLeft where BottomLeft = _PatternBottomLeft

class HasPatternBottomCenter s where _PatternBottomCenter :: s
pattern BottomCenter :: HasPatternBottomCenter s => s
pattern BottomCenter <- _PatternBottomCenter where BottomCenter = _PatternBottomCenter

class HasPatternBottomRight s where _PatternBottomRight :: s
pattern BottomRight :: HasPatternBottomRight s => s
pattern BottomRight <- _PatternBottomRight where BottomRight = _PatternBottomRight

class HasPatternCenter s where _PatternCenter :: s
pattern Center :: HasPatternCenter s => s
pattern Center <- _PatternCenter where Center = _PatternCenter

{-
class HasPatternLeft s where _PatternLeft :: s
pattern Left :: HasPatternLeft s => s
pattern Left <- _PatternLeft where Left = _PatternLeft

class HasPatternRight s where _PatternRight :: s
pattern Right :: HasPatternRight s => s
pattern Right <- _PatternRight where Right = _PatternRight
-}

class HasPatternVertical s where _PatternVertical :: s
pattern Vertical :: HasPatternVertical s => s
pattern Vertical <- _PatternVertical where Vertical = _PatternVertical

class HasPatternHorizontal s where _PatternHorizontal :: s
pattern Horizontal :: HasPatternHorizontal s => s
pattern Horizontal <- _PatternHorizontal where Horizontal = _PatternHorizontal

class HasPatternStart s where _PatternStart :: s
pattern Start :: HasPatternStart s => s
pattern Start <- _PatternStart where Start = _PatternStart

class HasPatternEnd s where _PatternEnd :: s
pattern End :: HasPatternEnd s => s
pattern End <- _PatternEnd where End = _PatternEnd

class HasPatternSpaceBetween s where _PatternSpaceBetween :: s
pattern SpaceBetween :: HasPatternSpaceBetween s => s
pattern SpaceBetween <- _PatternSpaceBetween where
        SpaceBetween =  _PatternSpaceBetween
