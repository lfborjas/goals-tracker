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
import Data.Maybe

import qualified GI.Gtk as G
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple
import           Data.Time.LocalTime
import           Data.Time.Calendar -- introduces fromGregorian
import           Data.Time.Format
import           Data.Word (Word32)

-- for plotting and cairo:
-- https://github.com/haskell-gi/haskell-gi/blob/master/examples/advanced/Cairo.hs
import Control.Lens
import Data.Colour
import Data.Colour.SRGB
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

-- my own modules
import Lib

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
           | ProjectingDate    (Word32, Word32, Word32)


-- from https://github.com/haskell-gi/haskell-gi/wiki/Using-Cairo-with-haskell-gi-generated-bindings
-- notice that the example expects a Render (), but we actually end up with a Render (PickFn ())
-- from the plotting lib
renderWithContext :: GI.Cairo.Context -> C.Render c -> IO c
renderWithContext ct r = G.withManagedPtr ct $ \p ->
  runReaderT (runRender r) (Cairo (castPtr p))

-- see: https://github.com/timbod7/haskell-chart/wiki/How-to-use-backends
-- https://github.com/timbod7/haskell-chart/blob/e2e1b375ec812cc4385d84e4acfd2575c5227fee/chart-gtk3/Graphics/Rendering/Chart/Gtk.hs
-- from: https://github.com/owickstrom/gi-gtk-declarative/issues/15
drawPlot :: PlotData -> (PlotData -> Renderable ()) -> C.Render (PickFn ())
drawPlot cd chartFn = do
  runBackend (defaultEnv bitmapAlignmentFns) (render (chartFn cd) (500,500))


-- renders a line graph, good for plotting projections
-- see: https://github.com/timbod7/haskell-chart/wiki/example-8
-- and the examples in general: https://github.com/timbod7/haskell-chart/wiki
plotLines :: PlotData -> Renderable ()
plotLines cd = toRenderable layout
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


-- renders a bar graph, good for plotting discrete historical events
-- see: https://github.com/timbod7/haskell-chart/wiki/example-11
plotBar :: PlotData -> Renderable ()
plotBar cd = toRenderable layout
  where
    layout = layout_title .~ "Balance History"
      $ layout_background .~ solidFillStyle (opaque white)
      $ layout_plots  .~ [ plotBars bars2 ]
      $ layout_foreground .~ (opaque black)
      $ def

    bars2 = plot_bars_values  .~ [ (d,[v])| (d, v) <- cd ]
      $ plot_bars_style       .~ BarsClustered
      $ def

-- renders an area graph, perhaps good for tracking different account growths?
-- see: https://github.com/timbod7/haskell-chart/wiki/example-3
plotArea :: PlotData -> Renderable ()
plotArea cd = toRenderable layout
  where
    layout = layout_title .~ "Balance History"
      $ layout_grid_last  .~ True
      $ layout_plots      .~ [ toPlot areaChart ]
      $ def

    areaChart = plot_fillbetween_style .~ solidFillStyle green1
      $ plot_fillbetween_values .~ [ (d, (0, v)) | (d, v) <- cd ]
      $ def

    green1 = opaque $ sRGB 0.5 1 0.5
    


updateChart :: PlotData -> GI.Cairo.Context -> G.DrawingArea -> IO (Bool, Event)
updateChart cd ctx canvas = do
  renderWithContext ctx (drawPlot cd plotArea)
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
          ,expandingChild $ widget G.Entry [onM #changed (fmap ProjectingBalance . G.entryGetText)]
          ,expandingChild $ widget G.Label [#label := "Expected Date" ]
          ,expandingChild $ widget G.Calendar [onM #daySelected (fmap ProjectingDate . G.calendarGetDate)]
          ,expandingChild $ clickyButton "Draw Plot" $> Plotting ]
        ]
 where
  expandingChild =
    BoxChild defaultBoxChildProperties { expand = True, fill = True }

parseDate d =
  fromJust $ (parseTimeM True defaultTimeLocale "%Y-%m-%d" d :: Maybe LocalTime)

initialData :: PlotData
initialData = [
   (parseDate "2019-01-01", 200)
  ,(parseDate "2019-02-01", 300)
  ,(parseDate "2019-03-01", 2000)
  ]

projectData (State _chartData _projectionData) =
  (State newData _projectionData)
  where
    newData = _chartData ++ [_projectionData]

-- from https://github.com/timbod7/haskell-chart/blob/master/chart-tests/tests/Prices.hs
mkDate y m d =
  LocalTime (fromGregorian (fromIntegral y) (fromIntegral m) (fromIntegral d)) midnight

update' :: State -> Event -> Transition State Event
update' s Plotting= Transition (projectData s) (return Nothing)
update' s          Plotted  = Transition s (return Nothing)
update' _          Closed   = Exit

update' s (ProjectingBalance b) =
  Transition (s & projectionData._2 .~ (read (Text.unpack b) :: Double)) (return Nothing)
update' s (ProjectingDate (y, m, d)) =
  Transition (s & projectionData._1 .~ (mkDate y m d)) (return Nothing)

main :: IO ()
main = void $ run App
  { view         = view'
  , update       = update'
  , inputs       = []
  , initialState = State initialData (parseDate "2019-04-01", 0)
  }

{-
Stack run should take care of the haskell dependencies. To install GTK, however:

export PKG_CONFIG_PATH="/usr/local/opt/libffi/lib/pkgconfig"
/usr/local/opt/libffi/lib/pkgconfig
ls /usr/local/opt/libffi
stack install --help 
brew install gtk+3

Next steps:
- add a points graph to show progression 


GTK references:
- https://github.com/owickstrom/gi-gtk-declarative/tree/04dceea04ba46ed854892e5a703eb95fe0952fb0/examples
- https://hackage.haskell.org/package/cairo-0.13.5.0/docs/Graphics-Rendering-Cairo.html#t:Render
- https://github.com/haskell-gi/gi-gtk-examples/blob/4c4f06dc91fbb9b9f50cdad295c8afe782e0bdec/menu/ComboDemo.hs
- http://hackage.haskell.org/package/gi-gtk-3.0.27/docs/GI-Gtk-Objects-Entry.html#g:98
http://hackage.haskell.org/package/gi-gtk-3.0.27/docs/GI-Gtk-Objects.html
http://hackage.haskell.org/package/gi-gtk-3.0.27/docs/GI-Gtk-Objects-Widget.html#v:Widget
http://hackage.haskell.org/package/gi-gtk-3.0.24/docs/GI-Gtk-Interfaces-FileChooser.html#v:fileChooserGetFilename
https://github.com/haskell-gi/gi-gtk-examples/blob/master/treelist/TreeTest.hs
http://hackage.haskell.org/package/gi-gtk-3.0.27/docs/GI-Gtk-Objects-Button.html#g:44
https://owickstrom.github.io/gi-gtk-declarative/attributes/events/
https://github.com/timbod7/haskell-chart/blob/master/chart-gtk3/Graphics/Rendering/Chart/Gtk.hs
https://hackage.haskell.org/package/time-1.6.0.1/docs/Data-Time-Format.html
http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Maybe.html
https://github.com/haskell-gi/haskell-gi/blob/master/examples/advanced/Cairo.hs
https://github.com/haskell-gi/gi-gtk-examples/blob/master/filechooser/FileChooserDemo.hs
http://hackage.haskell.org/package/gi-gtk-3.0.24/docs/GI-Gtk-Objects-Calendar.html#g:4
https://developer.gnome.org/gtk3/stable/GtkCalendar.html#gtk-calendar-get-date

Other references
http://hackage.haskell.org/package/time-1.9.2/docs/Data-Time-Calendar.html
http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Word.html



This is a very neat one:
https://haskell-at-work.com/episodes/2019-01-19-purely-functional-gtk-part-2-todo-mvc.html


Not used in the end:
https://formulae.brew.sh/formula/glade
https://wiki.gnome.org/action/show/Apps/Glade/Tutorials?action=show&redirect=Glade%2FTutorials

Next:

ask for a contribution, an end balance, an end date and an APY
and plot:
- the actual growth (with a bar graph?) based on the current balance, the end date, the contribution and the APY
- a suggested contribution graph (calculate that)
- a suggested end date graph
-}
