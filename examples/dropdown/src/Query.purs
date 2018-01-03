module Dropdown.Query where

import Select.Primitive.Container as C

data Query a
  = Log String a
  | HandleContainer (C.Message String Query) a
