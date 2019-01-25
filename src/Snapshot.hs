module Snapshot where

import 

snapshot pic = do
  jpg <- toJuicyRGBA <$> pic
  let bs = imageToJpg 98 (ImageRGBA8 jpg)
  t <- getCurrentTime
  writeFile (show t ++ ".jpg") bs
  -- Lazy.writeFile (show t ++ ".jpg") bs
  return st
