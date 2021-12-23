module Lib.Url.Parser exposing (percentDecodedString)

import Url
import Url.Parser as UP


percentDecodedString : UP.Parser (String -> a) a
percentDecodedString =
    UP.custom "PERCENT_DECODED_STRING" Url.percentDecode
