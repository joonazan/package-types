module Main exposing (..)

import Html
import Task
import ModuleTypes


type Model s f
    = Loading
    | Success s
    | Fail f


main =
    Html.program
        { init = ( Loading, getModule )
        , update = (\msg -> always ( msg, Cmd.none ))
        , view = view
        , subscriptions = always Sub.none
        }


getModule =
    ModuleTypes.get "elm-lang/core" "Maybe"
        |> Task.attempt
            (\r ->
                case r of
                    Ok m ->
                        Success m

                    Err e ->
                        Fail e
            )


view : Model s f -> Html.Html a
view m =
    case m of
        Loading ->
            Html.text "Loading..."

        Success s ->
            Html.text <| toString s

        Fail e ->
            Html.text <| toString e
