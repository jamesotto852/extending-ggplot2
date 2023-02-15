library("ggplot2")
library("ggtrace")

# We'll explore how this data is plotted with `geom_point()`
df <- data.frame(
  x = 1:10,
  y = 1:10
) 

ggplot(df, aes(x, y)) +
  geom_point()

# What is going on with `coord <- coord$transform(data, panel_params)`?
### This happens in EVERY `$draw_*()` method
GeomPoint$draw_panel

# Unless an alternative coordinate system is specified, 
# `coord$transform` is `ggplot2:::CoordCartesian$transform`
ggplot2:::CoordCartesian$transform
#> ggplot2:::CoordCartesian$transform
#> <ggproto method>
#>   <Wrapper function>
#>   function (...) 
#>     transform(...)
#> 
#>   <Inner function (f)>
#>     function (data, panel_params) 
#> {
#>     data <- transform_position(data, panel_params$x$rescale, 
#>                                panel_params$y$rescale)
#>     transform_position(data, squish_infinite, squish_infinite)
#> }

# We can look at what's going on when the method is called using `ggtrace::ggdebugonce()`
ggdebugonce(ggplot2:::CoordCartesian$transform)

ggplot(df, aes(x, y)) +
  geom_point()

# `transform_position()` applies specified transformation to x and y aesthetics:
transform_position
ggplot2:::aes_to_scale


# grabbed with ggtrace::ggdebug(ggtrace::ggdebug())
# panel_params is a list 
# encoding information about the "panel"
ls(panel_params)
#> [1] "x"       "x.range" "x.sec"   "y"       "y.range" "y.sec"

panel_params$x.range
#> [1]  0.55 10.45

panel_params$y.range
#> [1]  0.55 10.45

panel_params$x
#> <ggproto object: Class ViewScale, gg>
#>     aesthetics: x xmin xmax xend xintercept xmin_final xmax_final xlower ...
#>     break_positions: function
#>     break_positions_minor: function
#>     breaks: NA 2.5 5 7.5 10
#>     continuous_range: 0.55 10.45
#>     dimension: function
#>     get_breaks: function
#>     get_breaks_minor: function
#>     get_labels: function
#>     get_limits: function
#>     guide: waiver
#>     is_discrete: function
#>     is_empty: function
#>     limits: 1 10
#>     make_title: function
#>     map: function
#>     minor_breaks: 1.25 2.5 3.75 5 6.25 7.5 8.75 10
#>     name: waiver
#>     position: bottom
#>     rescale: function
#>     scale: <ggproto object: Class ScaleContinuousPosition, ScaleContinuous, Scale, gg>
#>         aesthetics: x xmin xmax xend xintercept xmin_final xmax_final xlower ...
#>         axis_order: function
#>         break_info: function
#>         break_positions: function
#>         breaks: waiver
#>         call: call
#>         clone: function
#>         dimension: function
#>         expand: waiver
#>         get_breaks: function
#>         get_breaks_minor: function
#>         get_labels: function
#>         get_limits: function
#>         guide: waiver
#>         is_discrete: function
#>         is_empty: function
#>         labels: waiver
#>         limits: NULL
#>         make_sec_title: function
#>         make_title: function
#>         map: function
#>         map_df: function
#>         minor_breaks: waiver
#>         n.breaks: NULL
#>         na.value: NA
#>         name: waiver
#>         oob: function
#>         palette: function
#>         position: bottom
#>         print: function
#>         range: <ggproto object: Class RangeContinuous, Range, gg>
#>             range: 1 10
#>             reset: function
#>             train: function
#>             super:  <ggproto object: Class RangeContinuous, Range, gg>
#>         rescale: function
#>         rescaler: function
#>         reset: function
#>         scale_name: position_c
#>         sec_name: function
#>         secondary.axis: waiver
#>         train: function
#>         train_df: function
#>         trans: trans
#>         transform: function
#>         transform_df: function
#>         super:  <ggproto object: Class ScaleContinuousPosition, ScaleContinuous, Scale, gg>
#>     scale_is_discrete: FALSE
#>     super:  <ggproto object: Class ViewScale, gg>

# `panel_params_x` and `panel_params_y` being used by `transform_position()`
# Let's dig in to what's happening 

panel_params$x$rescale
#> <ggproto method>
#>   <Wrapper function>
#>   function (...) 
#>     rescale(..., self = self)
#> 
#>   <Inner function (f)>
#>     function (self, x) 
#> {
#>     self$scale$rescale(x, self$limits, self$continuous_range)
#> }

# Here, `self` is `panel_params$x`

panel_params$x$scale$rescale
#> <ggproto method>
#>   <Wrapper function>
#>   function (...) 
#>     rescale(..., self = self)
#> 
#>   <Inner function (f)>
#>     function (self, x, limits = self$get_limits(), range = limits) 
#> {
#>     self$rescaler(x, from = range)
#> }

# self$rescaler() is just scales::rescale()
# rescales x to have min of 0, max of 1, mapping linearly from `range`,
# which is `self$continuous_range`:

panel_params$x$continuous_range
#> [1]  0.55 10.45

# This is from `scale_x_continuous()`, by default it`s the expanded range!

# Note, in this case `self$rescaler()` doesn't use the provided `limits` argument
# `limits` is the range of the data:

panel_params$x$get_limits()
#> [1]  1 10


# The last thing that happens is a quick fixing of values of `Inf`:
scales:::squish_infinite

  
# Now our data has been rescaled to the unit square and is ready to be plotted
data
#>             x          y PANEL group shape colour size fill alpha stroke
#> 1  0.04545455 0.04545455     1    -1    19  black  1.5   NA    NA    0.5
#> 2  0.14646465 0.14646465     1    -1    19  black  1.5   NA    NA    0.5
#> 3  0.24747475 0.24747475     1    -1    19  black  1.5   NA    NA    0.5
#> 4  0.34848485 0.34848485     1    -1    19  black  1.5   NA    NA    0.5
#> 5  0.44949495 0.44949495     1    -1    19  black  1.5   NA    NA    0.5
#> 6  0.55050505 0.55050505     1    -1    19  black  1.5   NA    NA    0.5
#> 7  0.65151515 0.65151515     1    -1    19  black  1.5   NA    NA    0.5
#> 8  0.75252525 0.75252525     1    -1    19  black  1.5   NA    NA    0.5
#> 9  0.85353535 0.85353535     1    -1    19  black  1.5   NA    NA    0.5
#> 10 0.95454545 0.95454545     1    -1    19  black  1.5   NA    NA    0.5






# w/o output --------------------------------------------------------------

# We'll explore how this data is plotted with `geom_point()`
df <- data.frame(
  x = 1:10,
  y = 1:10
) 

ggplot(df, aes(x, y)) +
  geom_point()

# What is going on with `coord <- coord$transform(data, panel_params)`?
### This happens in EVERY `$draw_*()` method
GeomPoint$draw_panel

# Unless an alternative coordinate system is specified, 
# `coord$transform` is `ggplot2:::CoordCartesian$transform`
ggplot2:::CoordCartesian$transform

# We can look at what's going on when the method is called using `ggtrace::ggdebugonce()`
ggdebugonce(ggplot2:::CoordCartesian$transform)

ggplot(df, aes(x, y)) +
  geom_point()

# `transform_position()` applies specified transformation to x and y aesthetics:
transform_position
ggplot2:::aes_to_scale

# grabbed with ggtrace::ggdebug(ggtrace::ggdebug())
# panel_params is a list 
# encoding information about the "panel"
ls(panel_params)
#> [1] "x"       "x.range" "x.sec"   "y"       "y.range" "y.sec"

panel_params$x.range
panel_params$x

# `panel_params_x` and `panel_params_y` being used by `transform_position()`
# Let's dig in to what's happening 

panel_params$x$rescale
# Here, `self` is `panel_params$x`

panel_params$x$scale$rescale

# self$rescaler() is just scales::rescale()
# rescales x to have min of 0, max of 1, mapping linearly from `range`,
# which is `self$continuous_range`:

# This is from `scale_x_continuous()`, by default it`s the expanded range:
panel_params$x$continuous_range

# Note, in this case `self$rescaler()` doesn't use the provided `limits` argument
# `limits` is the range of the data:
panel_params$x$get_limits()


# The last thing that happens is a quick fixing of values of `Inf`:
scales:::squish_infinite

  
# Now our data has been rescaled to the unit square and is ready to be plotted
data



