module ModuleTypes exposing (get)

{-|


# Module Types

@docs get

Most of the code is directly copied from the source code of package.elm-lang.org.

-}

import Http
import Utils.Path exposing ((</>))
import Docs.Version exposing (Version, vsnToString)
import Task exposing (Task)
import Package.Version
import Docs.Package exposing (Package)
import Package.Error as Error exposing (Error)
import Dict exposing (Dict)
import Docs.Entry as Entry exposing (Model, Info(..))
import Parse.Type as Type
import Docs.Type exposing (Type)


{-| Get the definitions in a module from the most recent version of the package.
The first list contains parsed definitions and the second one has errors for
the ones that failed to parse.
-}
get : String -> String -> Task Error ( List (Model Type), List String )
get package mod =
    getModule package mod
        |> Task.map
            (\mod ->
                let
                    results =
                        mod.entries
                            |> Dict.values
                            |> List.map (resultRaisingMap Type.parse)

                    typed =
                        List.filterMap Result.toMaybe results

                    chooseErrors x =
                        case x of
                            Ok _ ->
                                Nothing

                            Err e ->
                                Just e
                in
                    ( typed, List.filterMap chooseErrors results )
            )


getModule : String -> String -> Task Error Docs.Package.Module
getModule package mod =
    Package.Version.mostRecent package
        |> Task.andThen (getPackage package)
        |> Task.andThen
            (Dict.get mod
                >> maybeToTask Error.ModuleNotFound
            )


maybeToTask : e -> Maybe a -> Task e a
maybeToTask err =
    Maybe.map Task.succeed
        >> Maybe.withDefault (Task.fail err)


getPackage : String -> Version -> Task Error Package
getPackage package version =
    Http.get
        ("http://package.elm-lang.org/packages"
            </> package
            </> vsnToString version
            </> "documentation.json"
        )
        Docs.Package.decodePackage
        |> Http.toTask
        |> Task.mapError Error.HttpError


resultRaisingMap : (a -> Result e b) -> Model a -> Result e (Model b)
resultRaisingMap func model =
    let
        newInfo =
            case model.info of
                Value tipe fixity ->
                    func tipe
                        |> Result.map (\t -> Value t fixity)

                Union { vars, tags } ->
                    List.map (tagMap func) tags
                        |> combineResult
                        |> Result.map
                            (\tags ->
                                Union { vars = vars, tags = tags }
                            )

                Alias { vars, tipe } ->
                    func tipe
                        |> Result.map (\t -> Alias { vars = vars, tipe = t })

        tagMap f tag =
            List.map f tag.args
                |> combineResult
                |> Result.map (\args -> { tag | args = args })
    in
        Result.map (\i -> { model | info = i }) newInfo


combineResult : List (Result e a) -> Result e (List a)
combineResult =
    List.foldr (Result.map2 (::)) (Ok [])
