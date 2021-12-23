module Lib.Browser.History exposing
    ( Entry
    , History
    , empty
    , fromList
    , nextEntryId
    , pop
    , push
    , setRoute
    , setScrollPosition
    )

import Lib.Stack as Stack exposing (Stack)



-- ENTRY


type alias Entry a r =
    { a
        | route : r
        , scrollPosition :
            { x : Float
            , y : Float
            }
    }


setRoute : r -> Entry a r -> Entry a r
setRoute route entry =
    { entry | route = route }


setScrollPosition : Float -> Float -> Entry a r -> Entry a r
setScrollPosition x y entry =
    { entry | scrollPosition = { x = x, y = y } }



-- HISTORY


type alias History a r =
    Stack (Entry a r)


empty : History a r
empty =
    Stack.empty


nextEntryId : History a r -> Int
nextEntryId =
    Stack.size


fromList : List (Entry a r) -> History a r
fromList =
    Stack.fromList


push : Entry a r -> History a r -> History a r
push =
    Stack.push


pop : History a r -> ( Maybe (Entry a r), History a r )
pop =
    Stack.pop
