module JsonDecodeTest exposing (suite)

import Expect
import JsonDecode exposing (Event, decodeEvents)
import Test exposing (..)


validJson =
    """{
           "events": [
               {
                   "id": "visitorEvent1",
                   "start": "2020-03-17T22:00",
                   "end": "2020-03-17T22:59"
               },
               {
                   "id": "visitorEvent2",
                   "start": "2020-03-17T23:00",
                   "end": "2020-03-17T23:59"
               }
           ]
    }"""


inValidJson =
    """{
           "schedules": [
               {
                   "id": "visitorEvent1",
                   "start": "2020-03-17T22:00",
                   "end": "2020-03-17T22:59"
               },
               {
                   "id": "visitorEvent2",
                   "start": "2020-03-17T23:00",
                   "end": "2020-03-17T23:59"
               }
           ]
    }"""


decoded : List Event
decoded =
    [ Event "visitorEvent1" "2020-03-17T22:00" "2020-03-17T22:59", Event "visitorEvent2" "2020-03-17T23:00" "2020-03-17T23:59" ]


suite : Test
suite =
    concat
        [ test "decodeEvents Ok" <|
            \_ -> Expect.equal (decodeEvents validJson) (Ok decoded)
        , test "decodeEvents Err" <|
            \_ -> Expect.err (decodeEvents inValidJson)
        ]
