module Ui.VtyUtil (
    defStr
) where

import Graphics.Vty

defStr :: String -> Image
defStr = string defAttr
