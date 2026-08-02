module Types exposing (Model, Msg(..), initialModel)


type alias Model =
    { scaffoldModules : List String
    }


type Msg
    = NoOp


initialModel : List String -> Model
initialModel scaffoldModules =
    { scaffoldModules = scaffoldModules
    }
