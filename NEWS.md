# spatialplanr 0.7.7

* `splnr_plot_costOverlay()` gains five new optional parameters for controlling
  the appearance of the cost overlay: `costAlpha`, `costLowColour`,
  `costHighColour`, `costLowerPercentile` and `costUpperPercentile`. All
  default to the previous hard-coded values (`costAlpha = 0.1`,
  `costLowColour = "#fff5eb"`, `costHighColour = "#d94801"`), except that the
  colour scale limits now default to the 5th-95th percentile of the cost
  distribution instead of a fixed 0-99th percentile range. This narrower
  default range increases colour contrast for typical cost values (the
  previous 99th-percentile upper limit tended to wash out most of the map
  when the cost layer had a long right tail).

# spatialplanr 0.7.6

* See git history for changes prior to this NEWS.md file being introduced.
