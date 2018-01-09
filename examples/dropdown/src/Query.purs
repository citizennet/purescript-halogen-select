module Dropdown.Query where

import Select.Primitive.Container as C

data Query e a
  = Log String a
  | HandleContainer (C.Message String (Query e) e) a
