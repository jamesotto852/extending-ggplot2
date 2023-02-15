# A hand-wavy explanation of `coord <- coord$transform(data, panel_params)`

df <- data.frame(
  x = 1:10,
  y = 1:10
) 

ggplot(df, aes(x, y)) +
  geom_point()

# What is the range of this plot along each axis?
# Range of the data is [1, 10].
apply(df, 2, range)

ggplot(df, aes(x, y)) +
  geom_point() +
  geom_rect(xmin = 1, xmax = 10, ymin = 1, ymax = 10, color = "red", fill = NA)

# Axis are expanded past that, default behaviour of `scale_x/y_continuous()`
# is to expand w/ multiplicative factor of 5% in each direction
scales::expand_range(c(1, 10), mul = .05)

ggplot(df, aes(x, y)) +
  geom_point() +
  geom_rect(xmin = .55, xmax = 10.45, ymin = .55, ymax = 10.45,, color = "red", fill = NA)


# Let's try to add points manually!
ggplot(df, aes(x, y)) +
  geom_point()

grid::grid.points(x = 1, y = 1)
grid::grid.points(x = 10, y = 10)
grid::grid.points(x = 100, y = 100)


# The easiest way to parameterize a plot is with "npc" units,
# the plotting region is mapped to the unit square:
ggplot(df, aes(x, y)) +
  geom_point()

grid::grid.points(x = grid::unit(1/2, "npc"), y = grid::unit(1/2, "npc"))
grid::grid.points(x = grid::unit(c(0, 0, 1, 1), "npc"), y = grid::unit(c(0, 1, 0, 1), "npc"))

# The margins complicate things, for now just consider the plotting region
ggplot(df, aes(x, y)) +
  geom_point()

ggplot(df, aes(x, y)) +
  geom_point() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks.length = unit(0, "npc"),
    plot.margin = margin()
  )
  
grid::grid.points(x = grid::unit(1/2, "npc"), y = grid::unit(1/2, "npc"))
grid::grid.points(x = grid::unit(c(0, 0, 1, 1), "npc"), y = grid::unit(c(0, 1, 0, 1), "npc"))

# This is what `ggplot2` does -- data is mapped to the unit square

# Let's look at `GeomPoint` again
GeomPoint$draw_panel

# `coord$transform()` uses the `$transform()` method of the specified coordinate system:
#   - `... + coord_cartesian()` -> `ggplot2:::CoordCartesian$transform()`
#   - `... + coord_fixed()` -> `ggplot2:::CoordFixed$transform()`
#   - `... + coord_polar()` -> `ggplot2:::CoordPolar$transform()`

# `panel_params` encodes information about the "panel"
# Most important is panel_params$x$continuous_range and panel_params$y$continuous_range
# These come from the plot's scale

# `ggplot2:::CoordCartesian$transform()` boils down to the following rescaling:
df_rescaled <- df |>
  mutate(
    x = scales::rescale(x, to = c(0, 1), from = c(.55, 10.45)),
    y = scales::rescale(y, to = c(0, 1), from = c(.55, 10.45))
  )

grid::grid.points(x = grid::unit(df_rescaled$x, "npc"), y = grid::unit(df_rescaled$y, "npc"))


ggplot(df, aes(x, y)) +
  geom_point() +
  theme_void()

grid::grid.points(x = grid::unit(df_rescaled$x, "npc"), y = grid::unit(df_rescaled$y, "npc"))






