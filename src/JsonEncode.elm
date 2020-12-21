module JsonEncode exposing (encodeGetEvents, encodeRegistEvent)

import Json.Encode as Encode


encodeGetEvents : String -> Encode.Value
encodeGetEvents receptionId =
    Encode.object
        [ ( "method", Encode.string "get_visitor_events" )
        , ( "params", getEventValue receptionId )
        ]


getEventValue : String -> Encode.Value
getEventValue id =
    Encode.object [ ( "receptionId", Encode.string id ) ]


encodeRegistEvent : String -> String -> String -> String -> Encode.Value
encodeRegistEvent eventId receptionId familyName givenName =
    Encode.object
        [ ( "method", Encode.string "regist_visitor_event" )
        , ( "params", registEventValue eventId receptionId familyName givenName )
        ]


registEventValue : String -> String -> String -> String -> Encode.Value
registEventValue eventId receptionId familyName givenName =
    Encode.object
        [ ( "eventId", Encode.string eventId )
        , ( "receptionId", Encode.string receptionId )
        , ( "guest", registNameValue familyName givenName )
        ]


registNameValue : String -> String -> Encode.Value
registNameValue familyName givenName =
    Encode.object
        [ ( "given_name", Encode.string givenName )
        , ( "family_name", Encode.string familyName )
        ]
