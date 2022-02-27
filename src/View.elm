module View exposing (view)

import Browser exposing (element)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Lazy
import Element.Region as Region
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import Loading exposing (defaultConfig)
import Model exposing (Message(..), Model, UserWithRating, UsersDataState(..), defaultExpression, expressionQueryParam)
import Url.Builder


view : Model -> Browser.Document Message
view model =
    { title = "Profunctor alternative rating"
    , body = [ html model, githubLink ]
    }


html : Model -> Html.Html Message
html model =
    Element.layout defaultStyle <|
        Element.column
            [ Element.centerX
            , Element.width <| maximum style.maxWidth <| fill
            ]
            (static ++ body model)


defaultStyle : List (Attr decorative msg)
defaultStyle =
    [ Background.color style.backgroundColor
    , Font.color style.fontColor
    , Font.size style.fontSize
    ]


style =
    { fontColor = rgb255 234 234 234
    , backgroundColor = rgb255 58 63 69
    , buttonBorderColor = rgb255 186 189 182
    , buttonColor = rgba255 217 224 232 0.7
    , inputColor = rgb255 44 48 53
    , linkColor = rgb255 150 150 200
    , fontSize = 16
    , headerFontSize = 24
    , spacing = 10
    , padding = 10
    , maxWidth = 900
    , maxInputWidth = 700
    , loaderColor = "#EAEAEA" --same as font color, but no easy way to convert this
    }


static : List (Element msg)
static =
    [ Element.row [ Element.centerX ] [ header ]
    , Element.row [ Element.centerX ] [ description ]
    ]


body : Model -> List (Element Message)
body model =
    case model.usersData of
        Loading ->
            [ Element.row [ Element.centerX ] [ loadingDisplay ]
            , Element.row [ Element.centerX ] [ Element.text "Loading data from profunctior.io..." ]
            ]

        LoadingError error _ ->
            Element.row [ Element.centerX ] [ Element.Lazy.lazy errorDisplay error ] :: mainBody model

        Loaded _ ->
            mainBody model


mainBody : Model -> List (Element Message)
mainBody model =
    [ Element.row
        [ Element.centerX, Element.spacing style.spacing, Element.padding style.padding, Element.width <| maximum style.maxInputWidth <| fill ]
        [ Element.Lazy.lazy expressionInput model.expression
        , expressionButton
        ]
    , Element.row [ Element.centerX ] [ maybe errorDisplay model.expressionError ]
    , Element.row [ Element.centerX ] [ maybe mainTable model.rating ]
    ]


maybe : (a -> Element msg) -> Maybe a -> Element msg
maybe element optional =
    case optional of
        Just value ->
            Element.Lazy.lazy element value

        Nothing ->
            Element.none


header : Element msg
header =
    Element.paragraph
        [ Font.center
        , Font.bold
        , Font.size style.headerFontSize
        , Element.height Element.shrink
        , Element.width Element.fill
        , Element.padding style.padding
        , Region.heading 2
        ]
        [ Element.text "Profunctor alternative rating" ]


link : String -> String -> Element msg
link text url =
    Element.link [ Font.color style.linkColor ] { url = url, label = Element.text text }


expressionLink : String -> String
expressionLink expression =
    Url.Builder.relative [] [ Url.Builder.string expressionQueryParam expression ]


description : Element msg
description =
    Element.textColumn
        [ Font.center
        , Element.height Element.shrink
        , Element.width Element.fill
        , Element.padding style.padding
        ]
        [ Element.paragraph []
            [ Element.text "This is a "
            , link "∏ρØƒuñçτØρ Øπτµç∑" "https://t.me/profunctor_io"
            , Element.text " alternative "
            , link "rating" "https://profunctor.io/rating"
            , Element.text "."
            ]
        , Element.paragraph []
            [ Element.text "You can type in your expression based on number of posts (p), upvotes (u) and downvotes (d). " ]
        , Element.paragraph []
            [ Element.text "Supported operations: +, -, *, /, ^, sqrt, min, max, abs, ln, e, floor, round, ceil. You can also use round brackets." ]
        , Element.paragraph []
            [ Element.text "The default expression is "
            , link defaultExpression (expressionLink defaultExpression)
            , Element.text "."
            ]
        ]


expressionInput : String -> Element Message
expressionInput expression =
    formWrapper <|
        Input.text
            [ Background.color style.inputColor
            , Border.color style.inputColor
            , Element.padding style.padding
            , Element.width fill
            , Border.rounded 2
            , Border.solid
            , Border.widthXY 1 1
            , Input.focusedOnLoad
            , Element.htmlAttribute (Html.Attributes.name expressionQueryParam)
            , Element.htmlAttribute (Html.Attributes.id expressionQueryParam)
            ]
            { onChange = ExpressionUpdated
            , text = expression
            , placeholder = Just <| Input.placeholder [] (text "type an expression here")
            , label = Input.labelLeft [] (Element.text "Score expression: ")
            }


formWrapper : Element Message -> Element Message
formWrapper elem =
    Html.form
        [ Html.Events.onSubmit Calculate ]
        [ elem |> Element.layoutWith { options = [ Element.noStaticStyleSheet ] } defaultStyle ]
        |> Element.html
        |> Element.el [ Element.width fill ]


expressionButton : Element Message
expressionButton =
    Input.button
        [ Font.center
        , Font.color style.backgroundColor
        , Element.padding style.padding
        , Border.rounded 2
        , Background.color style.buttonColor
        , Border.color style.buttonColor
        , Border.solid
        , Border.widthXY 1 1
        ]
        { onPress = Just Calculate
        , label = Element.text "Update"
        }


mainTable : List UserWithRating -> Element msg
mainTable rating =
    Element.indexedTable
        [ Element.centerX
        , Element.centerY
        , Element.spacing style.spacing
        , Element.padding style.padding
        , Font.center
        ]
        { data = rating
        , columns =
            [ { header = Element.text "#"
              , width = fill
              , view = \index -> \_ -> index + 1 |> String.fromInt |> Element.text
              }
            , { header = Element.text "USERNAME"
              , width = fill
              , view = \_ -> .userData >> .username >> Element.text
              }
            , { header = Element.text "POSTS"
              , width = fill
              , view = \_ -> .userData >> .posts >> String.fromInt >> Element.text
              }
            , { header = Element.text "UPVOTES"
              , width = fill
              , view = \_ -> .userData >> .upvotes >> String.fromInt >> Element.text
              }
            , { header = Element.text "DOWNVOTES"
              , width = fill
              , view = \_ -> .userData >> .downvotes >> String.fromInt >> Element.text
              }
            , { header = Element.text "SCORE"
              , width = fill
              , view = \_ -> .rating >> round >> String.fromInt >> Element.text
              }
            ]
        }


errorDisplay : String -> Element msg
errorDisplay message =
    Element.textColumn
        [ Font.center
        , Element.height Element.shrink
        , Element.width Element.fill
        , Element.padding style.padding
        ]
        [ Element.paragraph [] [ Element.text message ] ]


loadingDisplay : Element msg
loadingDisplay =
    Element.html <|
        Loading.render
            Loading.BouncingBalls
            { defaultConfig | color = style.loaderColor, size = 100, speed = 0.7 }
            Loading.On


githubLink : Html.Html msg
githubLink =
    Html.span
        [ Html.Attributes.id "forkongithub" ]
        [ Html.node "style"
            [ Html.Attributes.type_ "text/css" ]
            -- source: https://codepo8.github.io/css-fork-on-github-ribbon/
            [ Html.text "#forkongithub a{background:#000;color:#fff;text-decoration:none;font-family:arial,sans-serif;text-align:center;font-weight:bold;padding:3.75px 30px;font-size:0.75rem;line-height:1.5rem;position:relative;transition:0.5s;}#forkongithub a:hover{background:#047700;color:#fff;}#forkongithub a::before,#forkongithub a::after{content:\"\";width:100%;display:block;position:absolute;top:1px;left:0;height:1px;background:#fff;}#forkongithub a::after{bottom:1px;top:auto;}@media screen and (min-width:800px){#forkongithub{position:absolute;display:block;top:0;right:0;width:150px;overflow:hidden;height:150px;z-index:9999;}#forkongithub a{width:150px;position:absolute;top:45px;right:-45px;transform:rotate(45deg);-webkit-transform:rotate(45deg);-ms-transform:rotate(45deg);-moz-transform:rotate(45deg);-o-transform:rotate(45deg);box-shadow:3px 3px 7.5px rgba(0,0,0,0.8);}}" ]
        , Html.a
            [ Html.Attributes.href "https://github.com/ov7a/profunctor-rating" ]
            [ Html.text "Source code on GitHub" ]
        ]
