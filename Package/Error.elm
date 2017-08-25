module Package.Error exposing (..)

import Http


type Error
    = HttpError Http.Error
    | PackageNotFound
    | ModuleNotFound
