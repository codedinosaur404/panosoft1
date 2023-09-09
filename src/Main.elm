module Main exposing (main)

import Accessibility exposing (..)
import Browser
import Dict exposing (Dict)
import Http
import Json.Decode as Decode exposing (Decoder, dict, list, string)
import Json.Decode.Pipeline as D
import RemoteData exposing (RemoteData(..), WebData)



-- MODEL


type alias Payload =
    { dogBreedsApiResponse : Dict String (List String)
    }


type alias Model =
    { dogBreeds : Dict String (List String)
    , dogBreedResponse : WebData Payload
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, getDogBreeds )


initialModel : Model
initialModel =
    { dogBreeds = Dict.empty
    , dogBreedResponse = RemoteData.NotAsked
    }



-- UPDATE


type Msg
    = GotDogBreeds (WebData Payload)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotDogBreeds response ->
            case response of
                RemoteData.Success result ->
                    ( { model
                        | dogBreedResponse = response
                        , dogBreeds = result.dogBreedsApiResponse
                    }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


getDogBreeds : Cmd Msg
getDogBreeds =
    Http.get
        { url = "https://dog.ceo/api/breeds/list/all"
        , expect = payloadDecoder |> Http.expectJson (RemoteData.fromResult >> GotDogBreeds)
        }


payloadDecoder : Decoder Payload
payloadDecoder =
    Decode.succeed Payload
        |> D.required "message" (dict (list string))



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ ul []
            [ li [] []
            ]
        ]



-- PROGRAM


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
