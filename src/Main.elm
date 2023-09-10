module Main exposing (main)

import Accessibility exposing (Html, div, img, li, text, ul)
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


type alias DogBreedDetail =
    { breeds : List String
    , imageUrls : List String
    , currentPage : Int
    , breedDetailResponse : WebData ()
    }


initialDogBreedDetail : List String -> DogBreedDetail
initialDogBreedDetail subBreeds =
    DogBreedDetail subBreeds [] 1 RemoteData.NotAsked


type alias Model =
    { dogBreeds : Dict String DogBreedDetail
    , dogBreedResponse : WebData Payload
    , currentBreed : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, getDogBreeds )


initialModel : Model
initialModel =
    { dogBreeds = Dict.empty
    , dogBreedResponse = RemoteData.NotAsked
    , currentBreed = Nothing
    }



-- UPDATE


type Msg
    = GotDogBreeds (WebData Payload)
    | GotSpecificBreed (WebData ())
    | ChangeBreed String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotDogBreeds response ->
            case response of
                RemoteData.Success result ->
                    ( { model
                        | dogBreedResponse = response
                        , dogBreeds = transformDictionary result.dogBreedsApiResponse
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ChangeBreed breed ->
            ( { model | currentBreed = Just breed }, Cmd.none )

        GotSpecificBreed _ ->
            ( model, Cmd.none )


transformDictionary : Dict String (List String) -> Dict String DogBreedDetail
transformDictionary originalDictionary =
    Dict.foldl
        (\key values acc ->
            Dict.insert key (initialDogBreedDetail values) acc
        )
        Dict.empty
        originalDictionary


getDogBreeds : Cmd Msg
getDogBreeds =
    Http.get
        { url = "https://dog.ceo/api/breeds/list/all"
        , expect = payloadDecoder |> Http.expectJson (RemoteData.fromResult >> GotDogBreeds)
        }



{--
getSpecificBreed : String -> Cmd Msg
getSpecificBreed breed =
    Http.get 
    { url = ""
    , expect = breedDecoder |> Http.expectJson (RemoteData.fromResult >> GotSpecificBreed)
    }
--}


payloadDecoder : Decoder Payload
payloadDecoder =
    Decode.succeed Payload
        |> D.required "message" (dict (list string))


breedDetailsDecoder : Decoder DogBreedDetail
breedDetailsDecoder =
    Decode.succeed DogBreedDetail
        |> D.hardcoded []
        |> D.hardcoded []
        |> D.hardcoded 1
        |> D.hardcoded RemoteData.NotAsked


keysList : Dict String DogBreedDetail -> List String
keysList dict =
    Dict.keys dict


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
