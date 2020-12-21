module JsonDecode exposing (Event, decodeEvents, eventListDecoder)

import Json.Decode as Decode


type alias Event =
    { id : String, start : String, end : String }


eventDecoder : Decode.Decoder Event
eventDecoder =
    Decode.map3 Event
        (Decode.field "id" Decode.string)
        (Decode.field "start" Decode.string)
        (Decode.field "end" Decode.string)


eventListDecoder : Decode.Decoder (List Event)
eventListDecoder =
    Decode.field "events" (Decode.list eventDecoder)


decodeEvents : String -> Result Decode.Error (List Event)
decodeEvents json =
    Decode.decodeString eventListDecoder json
