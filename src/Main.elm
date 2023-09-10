module Main exposing (main)

import Accessibility exposing (..)
import Browser
import Dict exposing (Dict)
import Html as CoreHtml
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
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
    , currentBreed : Maybe String
    , currentPage : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, getDogBreeds )


initialModel : Model
initialModel =
    { dogBreeds = Dict.empty
    , dogBreedResponse = RemoteData.NotAsked
    , currentBreed = Nothing
    , currentPage = 1
    }



-- UPDATE


type Msg
    = GotDogBreeds (WebData Payload)
    | ChangeBreed String


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

        ChangeBreed breed ->
            ( { model | currentBreed = Just breed, currentPage = 1 }, Cmd.none )


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


keysList : Dict String (List String) -> List String
keysList dict =
    Dict.keys dict



-- get keys is sorted lowest to highest


getSubBreeds : String -> Dict String (List String) -> Maybe (List String)
getSubBreeds breed collection =
    Dict.get breed collection


view : Model -> Html Msg
view model =
    div []
        [ model.dogBreeds
            |> keysList
            |> List.sort
            |> List.map dogBreedItemView
            |> ul []
        ]



-- using core html to add onClick. The notes say to exclude the complexity


dogBreedItemView : String -> Html Msg
dogBreedItemView breed =
    li [] [ CoreHtml.a [ onClick <| ChangeBreed breed ] [ text breed ] ]



{--
    this should probably be a decorative image, information in the adjacent text
    https://www.w3.org/WAI/tutorials/images/decorative/
--}


dogDetailsView : String -> Html msg
dogDetailsView dogBreed =
    img "" [ src dogBreed ]



-- PROGRAM


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
