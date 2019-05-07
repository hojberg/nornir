{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}

module UI where

import qualified Brick.Widgets.Border          as B
import qualified Brick.Widgets.Border.Style    as BS
import qualified Brick.Widgets.Core            as C
import qualified Graphics.Vty                  as V
import           Brick.Types                    ( Widget )
import           Brick.Util                     ( on
                                                , fg
                                                , bg
                                                )
import           Brick.AttrMap                  ( AttrMap
                                                , attrMap
                                                )
import qualified Brick.AttrMap                 as A
import qualified Brick.Widgets.Edit            as E

dashedBorder :: BS.BorderStyle
dashedBorder = BS.BorderStyle
  { BS.bsCornerTL      = '╭'
  , BS.bsCornerTR      = '╮'
  , BS.bsCornerBR      = '╯'
  , BS.bsCornerBL      = '╰'
  , BS.bsIntersectFull = '┼'
  , BS.bsIntersectL    = '├'
  , BS.bsIntersectR    = '┤'
  , BS.bsIntersectT    = '┬'
  , BS.bsIntersectB    = '┴'
  , BS.bsHorizontal    = '╴'
  , BS.bsVertical      = '┆'
  }


styles :: A.AttrMap
styles = A.attrMap
  V.defAttr
  [ (B.borderAttr     , fg (V.Color240 43))
  , (E.editFocusedAttr, V.black `on` V.green)
  , ("Selected"       , fg (V.Color240 59))
  , ("Started"        , fg (V.Color240 164))
  , ("Complete"       , fg V.green)
  , ("Incomplete"     , fg V.white)
  ]


fill :: Widget a
fill = C.fill ' '


unchecked :: String
unchecked = "☐ "


dashed :: String
dashed = "☒ "


checked :: String
checked = "☑ "
