module Lib.Browser exposing
    ( Document
    , Key
    , UrlRequest(..)
    , Viewport
    , application
    , document
    , element
    , focus
    , getViewport
    , getViewportOf
    , load
    , onResize
    , pushUrl
    , sandbox
    , scrollToTop
    , scrollToTopOf
    , setViewportOf
    , toUnstyledDocument
    )

import Browser as B
import Browser.Dom as BD
import Browser.Events as BE
import Browser.Navigation as BN
import Html.Styled as H
import Lib.Task as Task
import Task
import Url exposing (Url)



-- SANDBOX


sandbox :
    { init : model
    , view : model -> H.Html msg
    , update : msg -> model -> model
    }
    -> Program () model msg
sandbox options =
    B.sandbox
        { init = options.init
        , view = options.view >> H.toUnstyled
        , update = options.update
        }



-- ELEMENT


element :
    { init : flags -> ( model, Cmd msg )
    , view : model -> H.Html msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    }
    -> Program flags model msg
element options =
    B.element
        { init = options.init
        , view = options.view >> H.toUnstyled
        , update = options.update
        , subscriptions = options.subscriptions
        }



-- DOCUMENT


document :
    { init : flags -> ( model, Cmd msg )
    , view : model -> Document msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    }
    -> Program flags model msg
document options =
    B.document
        { init = options.init
        , view = options.view >> toUnstyledDocument
        , update = options.update
        , subscriptions = options.subscriptions
        }



-- APPLICATION


application :
    { init : flags -> Url -> Key -> ( model, Cmd msg )
    , view : model -> Document msg
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , onUrlRequest : UrlRequest -> msg
    , onUrlChange : Url -> msg
    }
    -> Program flags model msg
application options =
    B.application
        { init = options.init
        , view = options.view >> toUnstyledDocument
        , update = options.update
        , subscriptions = options.subscriptions
        , onUrlRequest = fromExternalUrlRequest >> options.onUrlRequest
        , onUrlChange = options.onUrlChange
        }



-- STYLED DOCUMENT


type alias Document msg =
    { title : String
    , body : List (H.Html msg)
    }


toUnstyledDocument : Document msg -> B.Document msg
toUnstyledDocument { title, body } =
    { title = title
    , body = List.map H.toUnstyled body
    }



-- URL REQUEST


type UrlRequest
    = Internal Url
    | External String


fromExternalUrlRequest : B.UrlRequest -> UrlRequest
fromExternalUrlRequest request =
    case request of
        B.Internal url ->
            Internal url

        B.External url ->
            External url



-- DOM


focus : String -> msg -> Cmd msg
focus id msg =
    BD.focus id
        |> Task.attempt (always msg)



-- VIEWPORT


type alias Viewport =
    BD.Viewport


getViewport : (Viewport -> msg) -> Cmd msg
getViewport toMsg =
    Task.perform toMsg BD.getViewport


getViewportOf : String -> msg -> (Viewport -> msg) -> Cmd msg
getViewportOf id defaultMsg toMsg =
    Task.safely defaultMsg toMsg <| BD.getViewportOf id


setViewportOf : String -> Float -> Float -> msg -> msg -> Cmd msg
setViewportOf id x y defaultMsg toMsg =
    Task.safely defaultMsg (always toMsg) <| BD.setViewportOf id x y


scrollToTop : msg -> Cmd msg
scrollToTop msg =
    BD.setViewport 0 0
        |> Task.perform (always msg)


scrollToTopOf : String -> msg -> Cmd msg
scrollToTopOf id msg =
    BD.setViewportOf id 0 0
        |> Task.attempt (always msg)



-- NAVIGATION


type alias Key =
    BN.Key


pushUrl : Key -> String -> Cmd msg
pushUrl =
    BN.pushUrl


load : String -> Cmd msg
load =
    BN.load



-- EVENTS


onResize : (Int -> Int -> msg) -> Sub msg
onResize =
    BE.onResize
