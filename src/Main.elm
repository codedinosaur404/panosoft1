module Main exposing (main)

import Accessibility exposing (Html, button, div, h2, img, li, span, text, ul)
import Browser
import Dict exposing (Dict)
import Html as CoreHtml exposing (aside, main_)
import Html.Attributes exposing (class, disabled, src)
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


itemsPerPage : Int
itemsPerPage =
    20


type alias DogBreedDetail =
    { subBreeds : List String
    , imageUrls : List String
    , currentPage : Int
    , totalPages : Int
    , breedDetailResponse : WebData ImageUrlResponse
    }


initialDogBreedDetail : List String -> DogBreedDetail
initialDogBreedDetail subBreeds =
    DogBreedDetail subBreeds [] 1 1 RemoteData.NotAsked


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

                --ToDo develop Error case messagin views
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
            { record
                | imageUrls = result.imageUrls
                , breedDetailResponse = response
                , totalPages = calculateTotalPages result.imageUrls
            }

        _ ->
            record


calculateTotalPages : List String -> Int
calculateTotalPages imageUrls =
    imageUrls
        |> List.length
        |> toFloat
        |> (\v -> v / toFloat itemsPerPage)
        |> ceiling


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
    main_ []
        [ h2 [ class "text-4xl text-center" ] [ text "Dog Breeds" ]
        , div [ class "flex flex-row items-start justify-center" ]
            [ aside [ class "w-64" ]
                [ div [ class "p-4" ]
                    [ model.dogBreeds
                        |> keysList
                        |> List.sort
                        |> List.map (\x -> dogBreedItemView x (Dict.get x model.dogBreeds))
                        |> ul []
                    ]
                ]
            , viewMaybe dogBreedDetailView (getDogBreedDetail model.currentBreed model.dogBreeds)
            ]
        ]


getDogBreedDetail : Maybe String -> Dict String DogBreedDetail -> Maybe DogBreedDetail
getDogBreedDetail currentBreed dogBreeds =
    currentBreed
        |> Maybe.andThen (\breed -> Dict.get breed dogBreeds)


dogBreedItemView : String -> Maybe DogBreedDetail -> Html Msg
dogBreedItemView breed breedDetails =
    li [ class "bg-gray-50" ]
        [ CoreHtml.a
            [ class "flex items-center p-2 class hover:cursor-pointer"
            , onClick <| ChangeBreed breed
            ]
            [ span [ class "ml-3" ] [ text breed ] ]
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
    li [ class "ml-10" ]
        [ CoreHtml.a
            [ onClick <| ChangeBreed breed ]
            [ text subBreed ]
        ]


exampleBreedUrls : List String
exampleBreedUrls =
    [ "https://images.dog.ceo/breeds/akita/An_Akita_Inu_resting.jpg"
    , "https://images.dog.ceo/breeds/akita/Japaneseakita.jpg"
    ]


dogBreedDetailView : DogBreedDetail -> Html Msg
dogBreedDetailView detail =
    div [ class "flex flex-col flex-wrap items-center" ]
        [ div [ class "flex-auto flex-wrap" ]
            [ exampleBreedUrls
                |> List.map subBreedImageView
                |> ul []
            ]
        , div []
            [ span [ class "mr-2" ] [ text <| "Total Image Count:" ]
            , span [] [ text <| (String.fromInt <| List.length detail.imageUrls) ]
            ]
        , div []
            [ button [ disabled <| detail.currentPage == 1 || detail.breedDetailResponse == RemoteData.Loading ] [ text "back" ]
            , span [ class "mx-4" ] [ text <| "Current Page: " ++ String.fromInt detail.currentPage ]
            , span [ class "mx-4" ] [ text <| "Total Pages: " ++ String.fromInt detail.totalPages ]
            , button [ disabled <| detail.currentPage == detail.totalPages || detail.breedDetailResponse == RemoteData.Loading ] [ text "forward" ]
            ]
        ]


subBreedImageView : String -> Html msg
subBreedImageView imageUrl =
    li []
        [ div [] [ img "" [ src imageUrl ] ]
        ]



{--
    this should probably be a decorative image, information in the adjacent text
    https://www.w3.org/WAI/tutorials/images/decorative/
--}
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
