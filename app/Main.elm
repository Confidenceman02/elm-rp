module Main exposing (Model, Msg(..), init, main, update, view)

import Animation as Animation exposing (percent, px)
import Animation.Messenger
import Browser
import Browser.Dom as Dom
import Html exposing (Html, button, div, h3, li, text, ul)
import Html.Attributes exposing (id, style)
import Html.Events exposing (onClick)
import Task


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Section =
    { style : Animation.Messenger.State Msg
    , expanded : Bool
    , id : String
    , content : List Content
    }


type alias Content =
    { name : String
    }


type alias Model =
    { sections : List Section
    }


defaultStyleState : Animation.Messenger.State Msg
defaultStyleState =
    Animation.style [ Animation.height (px defaultHeight) ]


sections : List Section
sections =
    [ { style = defaultStyleState
      , expanded = False
      , id = "section01"
      , content = [ { name = "text" } ]
      }
    , { style = defaultStyleState
      , expanded = False
      , id = "section02"
      , content = [ { name = "text" } ]
      }
    ]


init : Maybe {} -> ( Model, Cmd Msg )
init _ =
    ( { sections = sections }
    , Cmd.none
    )


defaultHeight : Float
defaultHeight =
    60.0



-- UPDATE


type Msg
    = SectionExpand Section (Result Dom.Error Dom.Viewport)
    | Animate Animation.Msg
    | InitSectionHeightAnimation Section
    | SectionCollapse Section (Result Dom.Error Dom.Viewport)
    | AddContent String
    | NoOp



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        setStyles : Section -> Sub Msg
        setStyles section =
            Animation.subscription Animate [ section.style ]
    in
    Sub.batch <| List.map setStyles model.sections


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitSectionHeightAnimation section ->
            let
                resolveCmd =
                    if section.expanded then
                        Task.attempt (SectionCollapse section) (findHeight section.id)

                    else
                        Task.attempt (SectionExpand section) (findHeight section.id)
            in
            ( model
            , resolveCmd
            )

        SectionCollapse clickedSection result ->
            let
                resolveStyle : Section -> Section
                resolveStyle section =
                    if section.id == clickedSection.id then
                        case result of
                            -- ensures section snaps close to default height if node is not found
                            Err _ ->
                                { section
                                    | style =
                                        Animation.interrupt
                                            [ Animation.set [ Animation.height (px defaultHeight) ]
                                            ]
                                            section.style
                                    , expanded = False
                                }

                            -- node was found, so we can compose smooth animation
                            Ok elementInfo ->
                                { section
                                    | style =
                                        Animation.interrupt
                                            [ Animation.set [ Animation.height (px elementInfo.scene.height) ]
                                            , Animation.to [ Animation.height (px defaultHeight) ]
                                            ]
                                            section.style
                                    , expanded = False
                                }

                    else
                        section
            in
            ( { model | sections = List.map resolveStyle model.sections }, Cmd.none )

        SectionExpand clickedSection result ->
            let
                resolveStyle : Section -> Section
                resolveStyle section =
                    if section.id == clickedSection.id then
                        case result of
                            -- ensures section force opens to 100% if id is not found
                            Err _ ->
                                { section
                                    | style =
                                        Animation.interrupt
                                            [ Animation.set [ Animation.height (percent 100.0) ]
                                            ]
                                            section.style
                                    , expanded = True
                                }

                            Ok elementInfo ->
                                { section
                                    | style =
                                        Animation.interrupt
                                            [ Animation.to [ Animation.height (px elementInfo.scene.height) ]
                                            , Animation.set [ Animation.height (percent 100.0) ]
                                            ]
                                            section.style
                                    , expanded = True
                                }

                    else
                        section
            in
            ( { model
                | sections = List.map resolveStyle model.sections
              }
            , Cmd.none
            )

        Animate msgTick ->
            let
                msgCmd : Section -> ( Section, Cmd Msg )
                msgCmd section =
                    ( { section
                        | style =
                            Tuple.first <|
                                Animation.Messenger.update msgTick section.style
                      }
                    , Tuple.second <| Animation.Messenger.update msgTick section.style
                    )
            in
            ( { model
                | sections =
                    List.map (msgCmd >> Tuple.first) model.sections
              }
            , Cmd.batch
                (List.map (msgCmd >> Tuple.second) model.sections)
            )

        AddContent clickedSectionId ->
            let
                addContent : Section -> Section
                addContent section =
                    if section.id == clickedSectionId then
                        { section | content = section.content ++ [ { name = "new section" } ] }

                    else
                        section

                updatedSections : List Section
                updatedSections =
                    List.map addContent model.sections
            in
            ( { model | sections = updatedSections }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- VIEW


cardDescription : Section -> Html Msg
cardDescription section =
    div
        [ style "display" "flex"
        , style "height" "60px"
        , style "alignItems" "center"
        ]
        [ h3 [] [ text "Header title" ], button [ onClick <| InitSectionHeightAnimation section ] [ text "Toggle" ] ]


cardContents : Section -> Html Msg
cardContents section =
    let
        viewContent : Content -> Html Msg
        viewContent content =
            li [] [ text content.name ]
    in
    ul [ style "paddingBottom" "5px" ]
        (List.map viewContent section.content
            ++ [ button [ onClick (AddContent section.id) ] [ text "Add" ] ]
        )


viewSections : Section -> Html Msg
viewSections section =
    div
        (List.concat
            [ Animation.render section.style
                ++ [ id section.id
                   , style "width" "100%"
                   , style "backgroundColor" "green"
                   , style "overflow" "hidden"
                   , style "borderRadius" "4px"
                   , style "padding" "0px 5px 0px 5px"
                   , style "marginBottom" "10px"
                   ]
            ]
        )
        [ cardDescription section
        , cardContents section
        ]


view : Model -> Html Msg
view model =
    div []
        (List.map viewSections model.sections)



-- helpers


findHeight : String -> Task.Task Dom.Error Dom.Viewport
findHeight id =
    Dom.getViewportOf id
