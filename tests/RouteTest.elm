module RouteTest exposing (suite)

import Expect exposing (Expectation)
import Maybe
import Route
import Test exposing (..)
import Url


suite : Test
suite =
    describe "Route"
        [ test "should parse URL" <|
            \_ ->
                Url.fromString "http://localhost:8000/#reception/bcbae52a-6843-11ea-8e5a-aa9250e6485e"
                    |> Maybe.andThen Route.urlToRoute
                    |> Expect.equal (Just (Route.Appointment (Just "reception/bcbae52a-6843-11ea-8e5a-aa9250e6485e")))
        , test "should get id" <|
            \_ ->
                Url.fromString "http://localhost:8080/#reception/bcbae52a-6843-11ea-8e5a-aa9250e6485e"
                    |> Maybe.andThen Route.getIdFromUrl
                    |> Expect.equal (Just "bcbae52a-6843-11ea-8e5a-aa9250e6485e")
        , test "should get id2" <|
            \_ ->
                Url.fromString "http://localhost:8080/#reception/hogehoge"
                    |> Maybe.andThen Route.getIdFromUrl
                    |> Expect.equal (Just "hogehoge")
        , test "should get id3" <|
            \_ ->
                Url.fromString "http://localhost:8080/#reception/12345"
                    |> Maybe.andThen Route.getIdFromUrl
                    |> Expect.equal (Just "12345")
        , test "should fail getId" <|
            \_ ->
                Url.fromString "http://localhost:8080/#reception/"
                    |> Maybe.andThen Route.getIdFromUrl
                    |> Expect.equal Nothing
        , test "should fail getId2" <|
            \_ ->
                Url.fromString "http://localhost:8080/#rece/bcbae52a-6843-11ea-8e5a-aa9250e6485e"
                    |> Maybe.andThen Route.getIdFromUrl
                    |> Expect.equal Nothing
        , test "should fail getId3" <|
            \_ ->
                Url.fromString "http://localhost:8080/"
                    |> Maybe.andThen Route.getIdFromUrl
                    |> Expect.equal Nothing
        ]
