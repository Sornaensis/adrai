module Main exposing (main)

import Api
import Json.Decode as Decode
import Platform
import Route
import Socket
import Types
import View.Compare
import View.Conflict
import View.Decision
import View.Forms
import View.History
import View.Relevant
import View.Search


main : Program Decode.Value Types.Model Types.Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


init : Decode.Value -> ( Types.Model, Cmd Types.Msg )
init _ =
    ( Types.initialModel scaffoldModules, Cmd.none )


update : Types.Msg -> Types.Model -> ( Types.Model, Cmd Types.Msg )
update message model =
    case message of
        Types.NoOp ->
            ( model, Cmd.none )


subscriptions : Types.Model -> Sub Types.Msg
subscriptions _ =
    Sub.none


scaffoldModules : List String
scaffoldModules =
    [ Api.scaffoldName
    , Route.scaffoldName
    , Socket.scaffoldName
    , View.Search.scaffoldName
    , View.Relevant.scaffoldName
    , View.Decision.scaffoldName
    , View.History.scaffoldName
    , View.Compare.scaffoldName
    , View.Conflict.scaffoldName
    , View.Forms.scaffoldName
    ]
