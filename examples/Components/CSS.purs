module Docs.CSS where

import Prelude
import Halogen.HTML as HH

baseContainer :: Array HH.ClassName
baseContainer = HH.ClassName <$>
  [ "bg-white"
  , "border-grey-80"
  , "border-l-2"
  , "border-r-2"
  , "w-full"
  ]

selectionContainer :: Array HH.ClassName
selectionContainer = baseContainer <>
  ( HH.ClassName <$>
    [ "border-t-2"
    ]
  )

itemContainer :: Array HH.ClassName
itemContainer = baseContainer <>
  ( HH.ClassName <$>
    [ "absolute"
    , "shadow"
    , "max-h-80"
    , "overflow-y-scroll"
    , "pin-t"
    , "pin-l"
    , "z-50"
    , "border-b-2"
    ]
  )

menu :: Array HH.ClassName
menu = HH.ClassName <$> [ "relative z-50" ]

ul :: Array HH.ClassName
ul = HH.ClassName <$> [ "list-reset" ]

li :: Array HH.ClassName
li = HH.ClassName <$>
  [ "px-3"
  , "rounded-sm"
  , "text-black-20"
  , "group"
  , "hover:bg-grey-97"
  , "cursor-pointer"
]

button :: Array HH.ClassName
button = HH.ClassName <$>
  [ "no-outline"
  , "px-4"
  , "py-2"
  , "!active:border-b"
  , "active:border-t"
  , "disabled:opacity-50"
  , "disabled:cursor-default"
  , "bg-blue-88"
  , "border-blue-88"
  , "hover:!disabled:bg-blue-82"
  , "focus:bg-blue-82"
  , "text-white"
  , "rounded"
  ]

input :: Array HH.ClassName
input = HH.ClassName <$>
  [ "bg-white"
  , "border-t-2"
  , "border-b-2"
  , "font-light"
  , "cc-blue-88"
  , "border-grey-80"
  , "disabled:bg-grey-95"
  , "disabled:text-grey-70"
  , "focus:no-outline"
  , "py-2"
  , "transition-1/4-bounce"
  , "border-l-2"
  , "border-r-2"
  , "w-full"
  , "px-3"
  , "focus:border-blue-88"
  , "!focus:!disabled:hover:border-grey-70"
  ]
