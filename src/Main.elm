module Main exposing (main)

import Accessibility exposing (Html, div, h2, img, li, text, ul)
import Browser
import Dict exposing (Dict)
import Html as CoreHtml
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Html.Extra as Html exposing (viewMaybe)
import Http
import Json.Decode as Decode exposing (Decoder, dict, list, string)
import Json.Decode.Pipeline as D
import RemoteData exposing (RemoteData(..), WebData)



-- MODEL


type alias DogBreedApiRespnse =
    { dogBreedsApiResponse : Dict String (List String)
    }


type alias ImageUrlResponse =
    { imageUrls : List String
    }


type alias DogBreedDetail =
    { subBreeds : List String
    , imageUrls : List String
    , currentPage : Int
    , breedDetailResponse : WebData ImageUrlResponse
    }


initialDogBreedDetail : List String -> DogBreedDetail
initialDogBreedDetail subBreeds =
    DogBreedDetail subBreeds [] 1 RemoteData.NotAsked


type alias Model =
    { dogBreeds : Dict String DogBreedDetail
    , dogBreedResponse : WebData DogBreedApiRespnse
    , currentBreed : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, fetchDogBreeds )


initialModel : Model
initialModel =
    { dogBreeds = Dict.empty
    , dogBreedResponse = RemoteData.NotAsked
    , currentBreed = Nothing
    }



-- UPDATE


type Msg
    = GotDogBreeds (WebData DogBreedApiRespnse)
    | GotBreedImageUrls (WebData ImageUrlResponse)
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
            let
                exactyOneApiRequestPerBreed =
                    Dict.get breed model.dogBreeds
                        |> Maybe.map (\detail -> detail.breedDetailResponse == RemoteData.NotAsked)
                        |> Maybe.withDefault True
            in
            if exactyOneApiRequestPerBreed then
                ( { model | currentBreed = Just breed }, fetchDogBreedImages breed )

            else
                ( { model | currentBreed = Just breed }, Cmd.none )

        GotBreedImageUrls response ->
            case model.currentBreed of
                Just breed ->
                    ( { model | dogBreeds = insertDogBreedDetail ( breed, response ) model.dogBreeds }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


updateImageUrls : WebData ImageUrlResponse -> DogBreedDetail -> DogBreedDetail
updateImageUrls response record =
    case response of
        RemoteData.Success result ->
            { record | imageUrls = result.imageUrls, breedDetailResponse = response }

        _ ->
            record


insertDogBreedDetail : ( String, WebData ImageUrlResponse ) -> Dict String DogBreedDetail -> Dict String DogBreedDetail
insertDogBreedDetail ( breed, imageReponse ) dogBreeds =
    dogBreeds
        |> Dict.update breed
            (\maybeDetails ->
                maybeDetails
                    |> Maybe.map (\dogDetails -> updateImageUrls imageReponse dogDetails)
            )


transformDictionary : Dict String (List String) -> Dict String DogBreedDetail
transformDictionary dogBreeds =
    Dict.foldl
        (\key values acc ->
            Dict.insert key (initialDogBreedDetail values) acc
        )
        Dict.empty
        dogBreeds


fetchDogBreeds : Cmd Msg
fetchDogBreeds =
    Http.get
        { url = "https://dog.ceo/api/breeds/list/all"
        , expect = dogBreedApiDecoder |> Http.expectJson (RemoteData.fromResult >> GotDogBreeds)
        }


fetchDogBreedImages : String -> Cmd Msg
fetchDogBreedImages breed =
    Http.get
        { url = "https://dog.ceo/api/breed/" ++ breed ++ "/images"
        , expect = imageUrlDecoder |> Http.expectJson (RemoteData.fromResult >> GotBreedImageUrls)
        }


dogBreedApiDecoder : Decoder DogBreedApiRespnse
dogBreedApiDecoder =
    Decode.succeed DogBreedApiRespnse
        |> D.required "message" (dict (list string))


imageUrlDecoder : Decoder ImageUrlResponse
imageUrlDecoder =
    Decode.succeed ImageUrlResponse
        |> D.required "message" (list string)


keysList : Dict String DogBreedDetail -> List String
keysList dict =
    Dict.keys dict


view : Model -> Html Msg
view model =
    div []
        [ h2 [ class "text-xl" ] [ text "Dog Breeds" ]
        , model.dogBreeds
            |> keysList
            |> List.sort
            |> List.map (\x -> dogBreedItemView x (Dict.get x model.dogBreeds))
            |> ul []
        ]


dogBreedItemView : String -> Maybe DogBreedDetail -> Html Msg
dogBreedItemView breed breedDetails =
    li [ class "hover:cursor-pointer" ]
        [ CoreHtml.a
            [ onClick <| ChangeBreed breed ]
            [ text breed ]
        , div []
            [ viewMaybe (breedDetailsView breed) breedDetails
            ]
        ]


breedDetailsView : String -> DogBreedDetail -> Html Msg
breedDetailsView breed breedDetail =
    breedDetail.subBreeds
        |> List.sort
        |> List.map (subBreedItemView breed)
        |> ul []


subBreedItemView : String -> String -> Html Msg
subBreedItemView breed subBreed =
    li [ class "ml-4" ]
        [ CoreHtml.a
            [ onClick <| ChangeBreed breed ]
            [ text subBreed ]
        ]



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
    Browser.document
        { init = init
        , update = update
        , view =
            \model ->
                { title = "Dog Breeds"
                , body = [ view model ]
                }
        , subscriptions = \_ -> Sub.none
        }
