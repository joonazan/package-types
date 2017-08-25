module Package.Error exposing (..)

{-| Just contains the error type
@docs Error
-}

import Http


{-| Encodes different errors that may occur fetching a module
-}
type Error
    = HttpError Http.Error
    | PackageNotFound
    | ModuleNotFound
