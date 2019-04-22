module Visualizations where

import Control.Lens
import Data.Colour
import Data.Colour.SRGB
import Data.Colour.Names
import Data.Default.Class
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Renderable
import Data.Time
import Lib (Balance, PlotData, ProjectionData)

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

-- compare the growth of two streams
comparisonAreas :: String -> PlotData -> PlotData -> Renderable ()
comparisonAreas title a b = toRenderable layout
  where
    layout = layout_title .~ title
      $ layout_grid_last  .~ True
      $ layout_plots      .~ [ toPlot areaChart1, toPlot areaChart2 ]
      $ def
      
    areaChart1 = plot_fillbetween_style .~ solidFillStyle green1
      $ plot_fillbetween_values .~ [ (d, (0, v)) | (d, v) <- a ]
      $ def

    areaChart2 = plot_fillbetween_style .~ solidFillStyle blue1
      $ plot_fillbetween_values .~ [ (d, (0, v)) | (d, v) <- b ]
      $ def

    green1 = opaque $ sRGB 0.5 1 0.5
    blue1 = opaque $ sRGB 0.5 0.5 1
      


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

-- convenience function when playing in the repl
writeChart :: FilePath -> PlotData -> IO (PickFn ())
writeChart file data' = renderableToFile def file $ plotArea data'



{-
playing with accounts and charts:

λ> import Lib
λ> a = growPrincipal 10000 (Rate 2 Monthly) (Contribution_ 1200 Monthly)
λ> b = growPrincipal 10000 (Rate 2 Monthly) (Contribution_ 1200 Monthly)
λ> p = growthInPeriod [a,b] (fromGregorian 2019 04 21) (fromGregorian 2020 04 21) Monthly
λ> chart = plotArea  p
λ> writeChart  "test1.png" chart
λ> :! open test1.png -- will open the chart in Preview
λ> 

-}
