module Main exposing (Event, createNewEvent, main)

import Browser
import Browser.Navigation as Nav
import EventTime exposing (toPosixFromIso)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http exposing (Error(..))
import Json.Decode exposing (errorToString)
import JsonDecode
import JsonEncode
import Route
import Task
import Time exposing (Month(..))
import Url exposing (Url)



--MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



--MODEL


type alias Model =
    { page : Page
    , events : List Event
    , selectedEvent : String
    , receptionId : String
    , givenName : String
    , familyName : String
    , zone : Time.Zone
    , time : Time.Posix
    }


type alias Event =
    { id : String
    , start : Time.Posix
    , end : Time.Posix
    , checked : Bool
    , zone : Time.Zone
    }


type Page
    = HttpError Http.Error
    | DecodeError Json.Decode.Error
    | AppointmentPage
    | SuccessPage
    | LoadingPage
    | RouteError


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    case Route.getIdFromUrl url of
        Nothing ->
            ( Model RouteError [] "" "" "" "" Time.utc (Time.millisToPosix 0), Cmd.none )

        Just receptionId ->
            ( Model LoadingPage [] "" receptionId "" "" Time.utc (Time.millisToPosix 0)
            , Task.perform AdjustTimeZone Time.here
            )



--UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | Loaded (Result Http.Error Page)
    | Checked String
    | GivenName String
    | FamilyName String
    | Click
    | RegistEvent (Result Http.Error String)
    | GetEvents (Result Http.Error (List JsonDecode.Event))
    | AdjustTimeZone Time.Zone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            ( model, Cmd.none )

        UrlChanged url ->
            ( model, Cmd.none )

        Loaded result ->
            ( model, Cmd.none )

        Checked eventId ->
            let
                newEvents =
                    List.map
                        (\event ->
                            if event.id == eventId && event.checked == False then
                                { event | checked = True }

                            else
                                { event | checked = False }
                        )
                        model.events
            in
            ( { model | events = newEvents, selectedEvent = eventId }, Cmd.none )

        Click ->
            ( { model | page = LoadingPage }
            , Http.request
                { method = "POST"
                , headers = []
                --TODO
                , url = ""
                , body = Http.jsonBody (JsonEncode.encodeRegistEvent model.selectedEvent model.receptionId model.familyName model.givenName)
                , expect = Http.expectString RegistEvent
                , timeout = Nothing
                , tracker = Nothing
                }
            )

        RegistEvent (Ok _) ->
            ( { model | page = SuccessPage }, Cmd.none )

        RegistEvent (Err error) ->
            ( { model | page = HttpError error }, Cmd.none )

        GetEvents (Ok eventList) ->
            let
                newEventList =
                    createNewEvent eventList model.zone
            in
            ( { model | events = newEventList, page = AppointmentPage }, Cmd.none )

        GetEvents (Err error) ->
            ( { model | page = HttpError error }, Cmd.none )

        GivenName name ->
            ( { model | givenName = name }, Cmd.none )

        FamilyName name ->
            ( { model | familyName = name }, Cmd.none )

        AdjustTimeZone zone ->
            ( { model | zone = zone }
            , Http.request
                { method = "POST"
                , headers = []
                --TODO
                , url = ""
                , body = Http.jsonBody (JsonEncode.encodeGetEvents model.receptionId)
                , expect = Http.expectJson GetEvents JsonDecode.eventListDecoder
                , timeout = Nothing
                , tracker = Nothing
                }
            )


createNewEvent : List JsonDecode.Event -> Time.Zone -> List Event
createNewEvent list zone =
    List.map (\event -> Event event.id (EventTime.toPosixFromIso event.start) (EventTime.toPosixFromIso event.end) False zone) list



--SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



--VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Appointment"
    , body =
        [ header ]
            ++ (case model.page of
                    HttpError error ->
                        viewErrorPage error

                    AppointmentPage ->
                        appointmentPage model

                    SuccessPage ->
                        successPage model

                    LoadingPage ->
                        loadingPage

                    DecodeError error ->
                        decodeErrorPage error

                    RouteError ->
                        routeErrorPage
               )
            ++ [ footer ]
    }


header : Html msg
header =
    section [ class "hero is-primary", style "background-color" "#01DC50" ]
        [ div [ class "hero-body" ]
            [ div
                [ class "container" ]
                [ h1 [ class "title" ] [ text "[訪問者様向け] 日程調整ページ" ] ]
            ]
        ]


footer : Html msg
footer =
    section []
        [ div [ class "content has-text-centered" ]
            [ div
                []
                [ p [] [ text "scent-y" ]
                ]
            ]
        ]


commonErrorMessage : String
commonErrorMessage =
    "システム内部でエラーが発生したため、恐れ入りますが担当者までご連絡ください。"


httpErrorToString : Http.Error -> String
httpErrorToString httpError =
    case httpError of
        BadUrl string ->
            "BadUrlError: " ++ string

        Timeout ->
            "TimeoutError"

        NetworkError ->
            "NetworkError"

        BadStatus int ->
            "BadStatusCode: " ++ String.fromInt int

        BadBody string ->
            "BadBodyError: " ++ string


viewErrorPage : Http.Error -> List (Html msg)
viewErrorPage error =
    [ div [ class "box" ]
        [ text commonErrorMessage ]
    , div [ class "box notification is-danger is-light" ]
        [ text <| httpErrorToString error ]
    ]


decodeErrorPage : Json.Decode.Error -> List (Html msg)
decodeErrorPage error =
    [ div [ class "box" ]
        [ text commonErrorMessage ]
    , div [ class "box notification is-danger is-light" ]
        [ text <| errorToString error ]
    ]


routeErrorPage : List (Html msg)
routeErrorPage =
    [ div [ class "box" ]
        [ text "URLが無効です" ]
    ]


appointmentPage : Model -> List (Html Msg)
appointmentPage model =
    div [ class "notification", style "background-color" "#FFFFFF" ]
        [ text "下記の日時候補から都合の良いスケジュールをひとつ選択し氏名を入力のうえ、アポイントメントを確定してください。"
        , br [] []
        , text "※全ての候補日時の都合が悪い場合は、恐れ入りますが担当者までご連絡ください。"
        ]
        :: List.map checkbox model.events
        ++ [ div [ class "box" ]
                [ label [ class "label", for "familyName" ]
                    [ text "姓"
                    , input [ id "familyName", class "input", type_ "text", placeholder "苗字を漢字で入力してください", value model.familyName, onInput FamilyName ] []
                    ]
                ]
           ]
        ++ [ div [ class "box" ]
                [ label [ class "label", for "givenName" ]
                    [ text "名"
                    , input [ id "givenName", class "input", type_ "text", placeholder "名前を漢字で入力してください", value model.givenName, onInput GivenName ] []
                    ]
                ]
           ]
        ++ [ div [ class "container" ]
                [ button
                    [ Html.Events.onClick Click
                    , disabled
                        (if model.selectedEvent == "" || model.givenName == "" || model.familyName == "" then
                            True

                         else
                            False
                        )
                    , class "button is-medium is-fullwidth"
                    , style "background-color" "#01DC50"
                    ]
                    [ text "アポイントメントを確定する" ]
                ]
           ]


successPage : Model -> List (Html Msg)
successPage model =
    [ section [ class "section" ]
        [ div [ class "container" ]
            [ article [ class "message is-primary" ]
                [ div [ class "message-header", style "background-color" "#01DC50" ]
                    [ p [] [ text "アポイントメントが確定しました" ]
                    ]
                , div [ class "message-body" ]
                    [ text "日時："
                    , text (getSelectedEvent model.selectedEvent model.events model.zone)
                    ]
                ]
            ]
        ]
    ]


getSelectedEvent : String -> List Event -> Time.Zone -> String
getSelectedEvent eventId eventList zone =
    let
        selectedEvent =
            List.filter (\event -> event.id == eventId) eventList

        selectedDate =
            List.map (\event -> makeDateFormat event zone) selectedEvent
    in
    String.concat selectedDate


loadingPage : List (Html Msg)
loadingPage =
    [ div [ class "box" ]
        [ text "loading...画面を閉じずに少々お待ちください。"
        ]
    ]


checkbox : Event -> Html Msg
checkbox event =
    div [ class "box" ]
        [ label [ class "checkbox" ] []
        , input
            [ type_ "checkbox"
            , checked event.checked
            , Html.Events.onClick (Checked event.id)
            ]
            []
        , text (makeDateFormat event event.zone)
        ]


toStartDateFromIso : Time.Posix -> Time.Zone -> String
toStartDateFromIso posix zone =
    let
        year =
            Time.toYear zone posix |> String.fromInt

        month =
            Time.toMonth zone posix |> toIntFromMonth |> String.fromInt

        day =
            Time.toDay zone posix |> String.fromInt

        hour =
            Time.toHour zone posix |> String.fromInt

        minute =
            Time.toMinute zone posix |> String.fromInt

        newMinute =
            if minute == "0" then
                "00"

            else
                minute
    in
    year ++ "年" ++ month ++ "月" ++ day ++ "日" ++ hour ++ "時" ++ newMinute ++ "分"


toEndDateFromIso : Time.Posix -> Time.Zone -> String
toEndDateFromIso posix zone =
    let
        year =
            Time.toYear zone posix |> String.fromInt

        month =
            Time.toMonth zone posix |> toIntFromMonth |> String.fromInt

        day =
            Time.toDay zone posix |> String.fromInt

        hour =
            Time.toHour zone posix

        minute =
            Time.toMinute zone posix

        --endDateのみ,1分足して表示する
        newMinute =
            (if (minute + 1) == 60 then
                0

             else
                minute + 1
            )
                |> String.fromInt

        formatNewMinute =
            if newMinute == "0" then
                "00"

            else
                newMinute

        newHour =
            (if (minute + 1) == 60 then
                hour + 1

             else
                hour
            )
                |> String.fromInt
    in
    year ++ "年" ++ month ++ "月" ++ day ++ "日" ++ newHour ++ "時" ++ formatNewMinute ++ "分"


toIntFromMonth : Time.Month -> Int
toIntFromMonth month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


makeDateFormat : Event -> Time.Zone -> String
makeDateFormat event zone =
    toStartDateFromIso event.start zone ++ "  ~  " ++ toEndDateFromIso event.end zone
