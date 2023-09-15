module ValidatedPerson exposing (..)

import Regex
import Validate exposing (..)


type PersonFormField
    = FirstNameField
    | ZipCodeField


type alias EditablePerson =
    { firstName : String
    , zipCode : String
    }


type FirstName
    = FirstName String


type ZipCode
    = ZipCode String


type alias ValidPerson =
    { firstName : FirstName
    , zipCode : ZipCode
    }


type alias Error =
    ( PersonFormField, String )


validateEditablePerson : Validator Error EditablePerson
validateEditablePerson =
    Validate.all
        [ ifBlank .firstName ( FirstNameField, "First Name can't be blank" )
        , Validate.firstError
            [ ifBlank .zipCode ( ZipCodeField, "Last Name can't be blank" )
            , someCustomLogicValidator .zipCode ( ZipCodeField, "Some custom failure" )
            ]
        ]


fromEditable : EditablePerson -> Result (List String) ValidPerson
fromEditable edit =
    case validate validateEditablePerson edit of
        Ok valid2 ->
            Ok (valid2 |> fromValid |> (\e -> ValidPerson (FirstName e.firstName) (ZipCode e.zipCode)))

        Err errors ->
            Err (List.map (\( _, str ) -> str) errors)


someCustomLogicValidator : (subject -> String) -> error -> Validator error subject
someCustomLogicValidator subjectToString error =
    ifTrue (\subject -> isInvalidZipCode <| subjectToString subject) error


isInvalidZipCode : String -> Bool
isInvalidZipCode zipCode =
    let
        zipCodePattern =
            Regex.fromString "^[0-9]{5}(?:-[0-9]{4})?$"

        zipCodeRegex =
            Maybe.withDefault Regex.never zipCodePattern
    in
    not <| Regex.contains zipCodeRegex zipCode
