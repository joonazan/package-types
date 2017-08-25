module Package.Version exposing (mostRecent)

import Dict exposing (Dict)
import Http
import Docs.Summary
import Docs.Version exposing (Version)
import Json.Decode as Decode
import Task exposing (Task)
import Package.Error exposing (..)


mostRecent : String -> Task Error Version
mostRecent package =
    getVersions
        |> Task.andThen
            (Dict.get package
                >> Maybe.andThen List.maximum
                >> Maybe.map Task.succeed
                >> Maybe.withDefault (Task.fail PackageNotFound)
            )


getVersions : Task Error (Dict String (List Version))
getVersions =
    Http.get "http://package.elm-lang.org/all-packages"
        (Decode.map
            (List.map (\entry -> ( entry.name, entry.versions ))
                >> Dict.fromList
            )
            Docs.Summary.decoder
        )
        |> Http.toTask
        |> Task.mapError HttpError
