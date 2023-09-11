module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (getIndicesFromPage, slice)
import Test exposing (..)


indicesTest : Test
indicesTest =
    describe "Are my indices correct"
        [ test "given page 1 returns 0-19" <|
            \_ ->
                getIndicesFromPage 1
                    |> Expect.equal ( 0, 19 )
        , test "given page 2 returns 20-40" <|
            \_ ->
                getIndicesFromPage 2
                    |> Expect.equal ( 20, 40 )
        , test "given page 3 return 40-60" <|
            \_ ->
                getIndicesFromPage 3
                    |> Expect.equal ( 40, 60 )
        ]


paginationTest : Test
paginationTest =
    describe "is my custom slice working"
        [ test "page 2" <|
            \_ ->
                (slice 20 40 listOfThirtyFive |> List.length)
                    == 15
                    |> Expect.equal True
        , test "page 1" <|
            \_ ->
                (slice 0 19 listOfThirtyFive |> List.length)
                    == 20
                    |> Expect.equal True
        ]


listOfThirtyFive : List String
listOfThirtyFive =
    List.repeat 35 "imageUrl"
