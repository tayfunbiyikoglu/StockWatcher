module Symbol exposing (isValid)

import Regex exposing (contains, regex)


isValid : String -> Bool
isValid symbol =
    contains (regex "^[0-9A-Z\\.]{1,10}$") symbol
