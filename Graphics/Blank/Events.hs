{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Graphics.Blank.Events where

import Control.Applicative
import Control.Concurrent.STM

import Data.Aeson (FromJSON(..), Value(..), ToJSON(..))
import Data.Aeson.Types ((.:), (.=), object)
import Data.Text.Lazy (Text)

import TextShow.TH (deriveTextShow)

-- | 'EventName' mirrors event names from jQuery, and uses lowercase.
-- Possible named events
-- 
-- * @keypress@, @keydown@, @keyup@
-- * @mousedown@, @mouseenter@, @mousemove@, @mouseout@, @mouseover@, @mouseup@
-- * @touchstart@ @touchmove@ @touchend@
type EventName = Text

-- | 'EventQueue' is an STM channel ('TChan') of 'Event's.
-- Intentionally, 'EventQueue' is not abstract.
type EventQueue = TChan Event

-- | <https://developer.mozilla.org/en-US/docs/Web/API/Touch>
data Touch = Touch
  { touchPageXY   :: (Double, Double)
  , touchID       :: Int
  , touchForce    :: Double -- ^ [0,1]
  , touchRadiusX  :: Double
  , touchRadiusY  :: Double
  , touchRotation :: Double -- ^ [0,90] clockwise rotation of ellipse in degrees
  }
  deriving (Eq, Ord, Show)


instance FromJSON Touch where
     parseJSON (Object v) = Touch <$> v .: "pageXY"
                                  <*> v .: "ID"
                                  <*> v .: "force"
                                  <*> v .: "radiusX"
                                  <*> v .: "radiusY"
                                  <*> v .: "rotation"
     parseJSON _ = fail "no parse of Touch"

instance ToJSON Touch where
     toJSON t = object
          [ "pageXY"   .= touchPageXY t
          , "ID"       .= touchID t
          , "force"    .= touchForce t
          , "radiusX"  .= touchRadiusX t
          , "radiusY"  .= touchRadiusY t
          , "rotation" .= touchRotation t
          ]

-- | Basic event from browser. See <http://api.jquery.com/category/events/> for details.
data Event = Event
        { eMetaKey :: Bool
        , ePageXY  :: Maybe (Double, Double) -- ^ coordinates for the `mouseup` `mousemove` `mousedown`
        , eTouches :: [Touch]            -- ^ changed `touchstart`, `touchmove`, `touchend`  
        , eType    :: EventName          -- ^ "Describes the nature of the event." jquery
        , eWhich   :: Maybe Int          -- ^ magic code for key presses
        }
        deriving (Eq, Ord, Show)
$(deriveTextShow ''Event)
$(deriveTextShow ''Touch)

instance FromJSON Event where
   parseJSON (Object v) = Event <$> ((v .: "metaKey")              <|> return False)
                                <*> ((v .: "pageXY")               <|> return Nothing)
                                <*> ((v .: "touches")              <|> return [])
                                <*> (v .: "type")
                                <*> (Just <$> (v .: "which")       <|> return Nothing)
   parseJSON _ = fail "no parse of Event"    

instance ToJSON Event where
   toJSON e = object
            $ ((:) ("metaKey" .=  eMetaKey e))
            $ (case ePageXY e of
                 Nothing -> id
                 xys -> (:) ("pageXY" .= xys))
            $ ((:) ("touches" .= eTouches e))
            $ ((:) ("type" .= eType e))
            $ (case eWhich e of
                 Nothing -> id
                 Just w -> (:) ("which" .= w))
            $ []
