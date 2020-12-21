module Route exposing (Route(..), getIdFromUrl, urlToRoute)

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>))
import Url.Parser.Query as Query


type Route
    = Appointment (Maybe String)


routeParser : Parser.Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map Appointment (Parser.fragment identity)
        ]


urlToRoute : Url -> Maybe Route
urlToRoute url =
    Parser.parse routeParser url


getIdFromUrl : Url -> Maybe String
getIdFromUrl url =
    case urlToRoute url of
        Nothing ->
            Nothing

        Just (Appointment receptionId) ->
            let
                id =
                    Maybe.withDefault "" receptionId
            in
            if urlValidation id then
                Just (String.replace "reception/" "" id)

            else
                Nothing



--validation getIdFromUrl


urlValidation : String -> Bool
urlValidation url =
    case String.contains "reception/" url of
        False ->
            False

        True ->
            case String.replace "reception/" "" url |> String.isEmpty of
                True ->
                    False

                False ->
                    True
