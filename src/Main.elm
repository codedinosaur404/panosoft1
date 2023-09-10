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


type alias DogBreeApiRespnse =
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
    , dogBreedResponse : WebData DogBreeApiRespnse
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
    = GotDogBreeds (WebData DogBreeApiRespnse)
    | GotBreedImageUrls (WebData (List String))
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
               breedDetails = Dict.get breed model.dogBreeds 
            in
            
            ( { model | currentBreed = Just breed }, fetchDogBreedImages breed)

        GotBreedImageUrls response ->
            case response of
                RemoteData.Success result ->
                    ( model, Cmd.none )

                _ -> 
                    ( model, Cmd.none )

updateImageUrls : List String -> DogBreedDetail -> DogBreedDetail
updateImageUrls urls record =
    { record | imageUrls = urls}

insertDogBreedDetail : (String, List String) -> Dict String DogBreedDetail -> Dict String DogBreedDetail 
insertDogBreedDetail (breed, imageUrls) dogBreeds =
    case Dict.get breed dogBreeds of
        Just details ->
            let
                updatedDetails = updateImageUrls imageUrls details
            in
            Dict.insert breed updatedDetails dogBreeds
            
        Nothing ->
            dogBreeds


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
        , expect = payloadDecoder |> Http.expectJson (RemoteData.fromResult >> GotDogBreeds)
        }


fetchDogBreedImages : String -> Cmd Msg
fetchDogBreedImages breed =
    Http.get 
    { url = "https://dog.ceo/api/breed/" ++ breed ++ "/images"
    , expect = (list string) |> Http.expectJson (RemoteData.fromResult >> GotBreedImageUrls)
    }


payloadDecoder : Decoder DogBreeApiRespnse
payloadDecoder =
    Decode.succeed DogBreeApiRespnse
        |> D.required "message" (dict (list string))


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



-- using core html to add onClick. The notes say to exclude the complexity


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
    if List.isEmpty breedDetail.breeds then
        Html.nothing

    else
        breedDetail.breeds
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
