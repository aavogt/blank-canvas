{-# LANGUAGE OverloadedStrings #-}
module Image_Size where

import Graphics.Blank
import Wiki -- (578,200)

main :: IO ()
main = do
    blankCanvas 3000 $ \ context -> do
        url <- staticURL context "type/jpeg" "images/Haskell.jpg"
        send context $ do
            img <- newImage url
            drawImage(img,[69,50,97,129])
        
        
        wiki $ snapShot context "images/Image_Size.png"
        wiki $ close context
