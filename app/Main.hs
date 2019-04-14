{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Control.Monad                 (void)
import           Data.Functor                  (($>))
import           Data.Text                     (Text)
import qualified Data.Text                     as Text

import qualified GI.Gtk as G
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple
import           Data.Time.LocalTime
import           Data.Time.Calendar -- introduces fromGregorian

-- for plotting and cairo:
-- https://github.com/haskell-gi/haskell-gi/blob/master/examples/advanced/Cairo.hs
import Control.Lens
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Graphics.Rendering.Chart
import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Cairo.Internal (Render(runRender))
import Graphics.Rendering.Cairo.Types (Cairo(Cairo))
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Renderable
import Control.Monad.Trans.Reader (runReaderT)
import Foreign.Ptr (castPtr)


import qualified GI.Cairo as GI.Cairo


data ButtonEvent = ButtonClicked

clickyButton :: Text -> Widget ButtonEvent
clickyButton label = widget G.Button [#label := label, on #clicked ButtonClicked]


type PlotData = [(LocalTime, Double)]
type ProjectionData = (LocalTime, Double)

data State = State { _chartData :: PlotData, _projectionData :: ProjectionData }
makeLenses ''State

data Event = Plotted
           | Plotting
           | Closed
           | ProjectingBalance Text.Text
--           | ProjectingDate    Text.Text

-- from https://github.com/haskell-gi/haskell-gi/wiki/Using-Cairo-with-haskell-gi-generated-bindings
-- notice that the example expects a Render (), but we actually end up with a Render (PickFn ())
-- from the plotting lib
renderWithContext :: GI.Cairo.Context -> C.Render c -> IO c
renderWithContext ct r = G.withManagedPtr ct $ \p ->
  runReaderT (runRender r) (Cairo (castPtr p))

-- see: https://github.com/timbod7/haskell-chart/wiki/How-to-use-backends
-- https://github.com/timbod7/haskell-chart/blob/e2e1b375ec812cc4385d84e4acfd2575c5227fee/chart-gtk3/Graphics/Rendering/Chart/Gtk.hs
-- from: https://github.com/owickstrom/gi-gtk-declarative/issues/15
drawPlot :: PlotData -> C.Render (PickFn ())
drawPlot cd = do
  runBackend (defaultEnv bitmapAlignmentFns) (render (makeChart cd) (500,500))


-- see: https://github.com/timbod7/haskell-chart/wiki/example-8
-- and the examples in general: https://github.com/timbod7/haskell-chart/wiki
makeChart :: PlotData -> Renderable ()
makeChart cd = toRenderable layout
  where
    layout = layout_title .~ "Balance history"
      $ layout_background .~ solidFillStyle (opaque white)
      $ layout_plots      .~ [ toPlot balanceChart ]
      $ layout_foreground .~ (opaque black)
      $ def

    balanceChart = plot_lines_style .~ lineStyle
      $ plot_lines_values .~ [[ (d, v) | (d,v) <- cd]]
      $ plot_lines_title  .~ "balance"
      $ def

    lineStyle = line_width .~ 3*0.25
      $ line_color .~ opaque blue
      $ def
  


updateChart :: PlotData -> GI.Cairo.Context -> G.DrawingArea -> IO (Bool, Event)
updateChart cd ctx canvas = do
  renderWithContext ctx (drawPlot cd)
  return (True, Plotted)



view' :: State -> AppView G.Window Event
view' s =
  bin
      G.Window
      [ #title := "Plot Example"
      , on #deleteEvent (const (True, Closed))
      , #widthRequest := 1200
      , #heightRequest := 900
      ]
    $ container
        G.Box
        [#orientation := G.OrientationVertical]
        [ expandingChild $ widget G.DrawingArea [onM #draw (updateChart (s ^. chartData))]
        , BoxChild defaultBoxChildProperties $ container
          G.Box
          [#orientation := G.OrientationHorizontal]
          [ expandingChild $ widget G.Label [#label := "Desired Balance" ]
          ,expandingChild $ widget G.Entry [onM #activate (fmap ProjectingBalance . G.entryGetText)]
          ,expandingChild $ clickyButton "Draw Plot" $> Plotting ]
        ]
 where
  expandingChild =
    BoxChild defaultBoxChildProperties { expand = True, fill = True }

initialData :: PlotData
initialData = [
   (mkDate 2019 01 01, 200)
  ,(mkDate 2019 02 01, 300)
  ,(mkDate 2019 03 01, 2000)
  ]
otherData :: PlotData
otherData = [
   (mkDate 2019 01 01, 200)
  ,(mkDate 2019 02 01, 100)
  ,(mkDate 2019 03 01, 20)
  ]

projectData (State _chartData _projectionData) =
  (State newData _projectionData)
  where
    newData = _chartData ++ [_projectionData]

-- from https://github.com/timbod7/haskell-chart/blob/master/chart-tests/tests/Prices.hs
mkDate d m y =
  LocalTime (fromGregorian (fromIntegral y) m d) midnight

update' :: State -> Event -> Transition State Event
update' s Plotting= Transition (projectData s) (return Nothing)
update' s          Plotted  = Transition s (return Nothing)
update' _          Closed   = Exit

update' s (ProjectingBalance b) =
  Transition (s & projectionData._2 .~ (read (Text.unpack b) :: Double)) (return Nothing)

main :: IO ()
main = void $ run App
  { view         = view'
  , update       = update'
  , inputs       = []
  , initialState = State initialData (mkDate 2019 04 01, 0)
  }

{-
Stack run should take care of the haskell dependencies. To install GTK, however:

export PKG_CONFIG_PATH="/usr/local/opt/libffi/lib/pkgconfig"
/usr/local/opt/libffi/lib/pkgconfig
ls /usr/local/opt/libffi
stack install --help 
brew install gtk+3

-}
