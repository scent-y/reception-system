module JsonEncodeTest exposing (suite)

import Expect exposing (Expectation)
import Json.Encode exposing (encode, string)
import JsonEncode exposing (encodeGetEvents, encodeRegistEvent)
import Test exposing (..)


getEventJson =
    """{"method":"get_visitor_events","params":{"receptionId":"reception-id"}}"""


registEventJson =
    """{"method":"regist_visitor_event","params":{"eventId":"event-id","receptionId":"reception-id","guest":{"given_name":"山田","family_name":"太郎"}}}"""


suite : Test
suite =
    concat
        [ test "encodeGetEvents Ok" <|
            \_ -> Expect.equal (encode 0 (encodeGetEvents "reception-id")) getEventJson
        , test "encodeGetEvents notEqual" <|
            \_ -> Expect.notEqual (encode 0 (encodeGetEvents "event-id")) getEventJson
        , test "encodeRegistEvent Ok" <|
            \_ -> Expect.equal (encode 0 (encodeRegistEvent "event-id" "reception-id" "山田" "太郎")) registEventJson
        , test "encodeRegistEvent notEqual" <|
            \_ -> Expect.notEqual (encode 0 (encodeRegistEvent "reception-id" "event-id" "山田" "太郎")) registEventJson
        ]
