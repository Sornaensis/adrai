port module BrowserFixture exposing (main)

import Browser
import Json.Decode as Decode
import Main


port fromJs : (Decode.Value -> msg) -> Sub msg


main : Program { hasCredential : Bool } Main.Model Main.Msg
main =
    Browser.element
        { init = Main.init
        , update = Main.update
        , view = Main.view
        , subscriptions = \_ -> fromJs Main.FromJs
        }
