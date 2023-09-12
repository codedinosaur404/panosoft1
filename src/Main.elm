module Main exposing (getIndicesFromPage, main, slice)

import Accessibility exposing (Html, button, div, h1, img, li, span, text, ul)
import Browser
import Dict exposing (Dict)
import Html as CoreHtml exposing (aside, main_)
import Html.Attributes exposing (class, disabled, id, src)
import Html.Events exposing (onClick)
import Html.Extra as Html exposing (viewIf, viewMaybe)
import Http
import Json.Decode as Decode exposing (Decoder, dict, list, string)
import Json.Decode.Pipeline as D
import List.Extra exposing (getAt)
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
    , isLoading : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, fetchDogBreeds )


initialModel : Model
initialModel =
    { dogBreeds = Dict.empty
    , dogBreedResponse = RemoteData.NotAsked
    , currentBreed = Nothing
    , isLoading = True
    }


type Direction
    = Forward
    | Backward



-- UPDATE


type Msg
    = GotDogBreeds (WebData DogBreedApiRespnse)
    | GotBreedImageUrls (WebData ImageUrlResponse)
    | ChangeBreed String
    | Navigate Direction


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotDogBreeds response ->
            case response of
                RemoteData.Success result ->
                    ( { model
                        | dogBreedResponse = response
                        , dogBreeds = transformDictionary result.dogBreedsApiResponse
                        , isLoading = False
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model | isLoading = False }, Cmd.none )

        ChangeBreed breed ->
            let
                exactyOneApiRequestPerBreed =
                    Dict.get breed model.dogBreeds
                        |> Maybe.map (\detail -> detail.breedDetailResponse == RemoteData.NotAsked)
                        |> Maybe.withDefault True
            in
            if exactyOneApiRequestPerBreed then
                ( { model | currentBreed = Just breed, isLoading = True }, fetchDogBreedImages breed )

            else
                ( { model | currentBreed = Just breed, isLoading = False }, Cmd.none )

        GotBreedImageUrls response ->
            case model.currentBreed of
                Just breed ->
                    ( { model | dogBreeds = insertDogBreedDetail ( breed, response ) model.dogBreeds, isLoading = False }, Cmd.none )

                Nothing ->
                    ( { model | isLoading = False }, Cmd.none )

        Navigate direction ->
            case direction of
                Forward ->
                    case model.currentBreed of
                        Just breed ->
                            ( { model | dogBreeds = updatePageNumber (\x -> x + 1) breed model.dogBreeds }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                Backward ->
                    case model.currentBreed of
                        Just breed ->
                            ( { model | dogBreeds = updatePageNumber (\x -> x - 1) breed model.dogBreeds }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )


adjustPageNumber : (Int -> Int) -> { r | currentPage : Int } -> { r | currentPage : Int }
adjustPageNumber func record =
    { record | currentPage = func record.currentPage }


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
        |> (\v2 -> v2 / toFloat itemsPerPage)
        |> ceiling


insertDogBreedDetail : ( String, WebData ImageUrlResponse ) -> Dict String DogBreedDetail -> Dict String DogBreedDetail
insertDogBreedDetail ( breed, imageReponse ) dogBreeds =
    dogBreeds
        |> Dict.update breed
            (\maybeDetails ->
                maybeDetails
                    |> Maybe.map (\dogDetails -> updateImageUrls imageReponse dogDetails)
            )


updatePageNumber : (Int -> Int) -> String -> Dict String DogBreedDetail -> Dict String DogBreedDetail
updatePageNumber func breed dogBreeds =
    dogBreeds
        |> Dict.update breed
            (\maybeDetails ->
                maybeDetails |> Maybe.map (\dogDetails -> adjustPageNumber func dogDetails)
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
    main_ [ class "content" ]
        [ viewIf model.isLoading
            (div [ class "overlay" ]
                [ div [ class "flex justify-center items-center h-screen" ] [ text "Loading..." ] ]
            )
        , div []
            [ h1 [ class "text-4xl text-center" ] [ text "Dog Breeds" ]
            , viewMaybe totalCountView (getDogBreedDetail model.currentBreed model.dogBreeds)
            ]
        , div [ class "flex" ]
            [ aside [ class "w-64 flex flex-row" ]
                [ model.dogBreeds
                    |> keysList
                    |> List.sort
                    |> List.map (\x -> dogBreedItemView x (Dict.get x model.dogBreeds))
                    |> ul []
                ]
            , div [ class "flex flex-col items-center gap-8" ]
                [ viewMaybe dogBreedDetailView (getDogBreedDetail model.currentBreed model.dogBreeds)
                , viewMaybe paginationButtonsView (getDogBreedDetail model.currentBreed model.dogBreeds)
                ]
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


dogBreedDetailView : DogBreedDetail -> Html Msg
dogBreedDetailView detail =
    detail
        |> getImageSlice
        |> List.map subBreedImageView
        |> ul [ class "grid grid-cols-5 gap-4" ]


totalCountView : DogBreedDetail -> Html msg
totalCountView detail =
    div []
        [ span [ class "mr-2" ] [ text <| "Total Image Count:" ]
        , span [] [ text <| (String.fromInt <| List.length detail.imageUrls) ]
        ]


paginationButtonsView : DogBreedDetail -> Html Msg
paginationButtonsView detail =
    span [ class "" ]
        [ button
            [ disabled <| detail.currentPage == 1 || detail.breedDetailResponse == RemoteData.Loading
            , onClick <| Navigate Backward
            , class "bg-gray-300 hover:bg-gray-400 text-gray-800 font-bold py-2 px-4 rounded-l disabled:bg-gray-100"
            ]
            [ text "back" ]
        , span [ class "mx-4" ] [ text <| "Current Page: " ++ String.fromInt detail.currentPage ]
        , button
            [ disabled <| detail.currentPage == detail.totalPages || detail.breedDetailResponse == RemoteData.Loading
            , onClick <| Navigate Forward
            , class "bg-gray-300 hover:bg-gray-400 text-gray-800 font-bold py-2 px-4 rounded-l disabled:bg-gray-100"
            ]
            [ text "next" ]
        ]


getImageSlice : DogBreedDetail -> List String
getImageSlice detail =
    let
        ( start, end ) =
            getIndicesFromPage detail.currentPage
    in
    detail.imageUrls
        |> slice start end


getIndicesFromPage : Int -> ( Int, Int )
getIndicesFromPage pageNumber =
    if pageNumber == 1 then
        ( pageNumber - 1, itemsPerPage - 1 )

    else
        ( (pageNumber - 1) * itemsPerPage, (pageNumber - 1) * itemsPerPage + itemsPerPage - 1 )


slice : Int -> Int -> List String -> List String
slice start end list =
    let
        startIndex =
            max 0 start

        endIndex =
            min (List.length list) end

        rangeSlice =
            List.range startIndex endIndex
    in
    List.filterMap (\index -> getAt index list) rangeSlice


subBreedImageView : String -> Html msg
subBreedImageView imageUrl =
    li [ class "" ] [ img "" [ class "w-full h-52 object-cover", src imageUrl ] ]



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
