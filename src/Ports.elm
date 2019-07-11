port module Ports exposing (saveConfig)

import Types exposing (..)


port saveConfig : Config -> Cmd msg
