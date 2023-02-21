library("ggplot2")

# Basic GeomCdf + StatCdf pair --------
# note: GeomCdf also works well w/ StatIdentity

GeomCdf <- ggproto("GeomCdf", Geom, 
                   
  required_aes = c("x", "y"),
  
  default_aes = aes(
    # all --
    colour = "black",
    alpha = NA,
    # lines --
    linewidth = 0.5,
    linetype = 1,
    # points --
    shape = 19,
    size = 3,
    fill = NA,
    stroke = 0.5
  ),
                   
  draw_group = function(data, panel_params, coord, open_fill, vert_type) {
    
    n <- nrow(data)
    
    ## Need two dfs:
    ## 1: encoding horizontal segments (n + 1_)
    ## 2: encoding vertical segments (n)
    
    if (panel_params$y.range[1] > 0) {
      warning("Plotting range does not include y = 0, CDF truncated")
    }
     
    if (panel_params$y.range[2] < 1) {
      warning("Plotting range does not include y = 1, CDF truncated")
    }
    
    data_hori <- data[c(1, 1:n),]
    data_hori$y <- c(0, data$y)
    data_hori$yend <- c(0, data$y)
    data_hori$x <- c(panel_params$x.range[1], data$x)
    data_hori$xend <- c(data$x, panel_params$x.range[2])
    
    data_vert <- data
    data_vert$xend <- data$x
    data_vert$y <- c(0, data$y[-n])
    data_vert$yend <- data$y
    
    coord_hori <- coord$transform(data_hori, panel_params)
    coord_vert <- coord$transform(data_vert, panel_params)
    
    grobs <- list()
    
    grobs$hori <- grid::segmentsGrob(
      coord_hori$x, coord_hori$y, coord_hori$xend, coord_hori$yend,
      default.units = "native",
      gp = grid::gpar(
        col = alpha(coord_hori$colour, coord_hori$alpha),
        fill = alpha(coord_hori$colour, coord_hori$alpha),
        lwd = coord_hori$linewidth * .pt,
        lty = coord_vert$linetype
      )
    )
     
    grobs$vert <- grid::segmentsGrob(
      coord_vert$x, coord_vert$y, coord_vert$xend, coord_vert$yend,
      default.units = "native",
      gp = grid::gpar(
        col = alpha(coord_vert$colour, coord_vert$alpha),
        fill = alpha(coord_vert$colour, coord_vert$alpha),
        lwd = coord_vert$linewidth * .pt,
        lty = vert_type
      )
    )
     
    grobs$open <- grid::pointsGrob(
      coord_vert$x, coord_vert$y, 
      default.units = "native",
      pch = 21, 
      gp = grid::gpar(
        col = coord_vert$colour, 
        fill = open_fill, # expose as a parameter
        fontsize = coord_vert$size * .pt + coord_vert$stroke * .stroke/2,
        lwd = coord_vert$stroke * .stroke/2
      )
    )
    
    grobs$closed <- grid::pointsGrob(
      coord_vert$xend, coord_vert$yend,
      pch = coord_vert$shape,
      default.units = "native",
      gp = grid::gpar(
        col = alpha(coord_vert$colour, coord_vert$alpha),
        fill = alpha(coord_vert$colour, coord_vert$alpha),
        fontsize = coord_vert$size * .pt + coord_vert$stroke * .stroke/2,
        lwd = coord_vert$stroke * .stroke/2
      )
    )
    
     
   grid::gTree(children = do.call(grid::gList, grobs))
    
  },
  
  draw_key = draw_key_path
                     
)


StatCdf <- ggproto("StatCdf", Stat, 
                   
  required_aes = c("x"),
  
  default_aes = aes(y = after_stat(Fhat)),
  
  compute_group = function(data, scales) {
    
    # this is the easiest way:
    ## data <- StatCount$compute_group(data, scales)
    ## data$fhat <- cumsum(data$prop)
    ## data
    
    ecdf <- data.frame(
      x = sort(unique(data$x)),
      count = as.numeric(table(data$x))
    )
    
    ecdf$fhat <- ecdf$count / sum(ecdf$count)
    ecdf$Fhat <- cumsum(ecdf$fhat)
    
    ecdf
    
  }
  
)

geom_cdf <- function(mapping = NULL, data = NULL, 
                     stat = StatCdf, position = "identity", 
                     ...,
                     open_fill = "white",
                     vert_type = "dashed",
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE) {
  
  
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomCdf,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      open_fill = open_fill,
      vert_type = vert_type,
      na.rm = na.rm,
      ...
    )
  )
}

stat_cdf <- function(mapping = NULL, data = NULL, 
                     geom = GeomCdf, position = "identity", 
                     ...,
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE) {
  
  layer(
    data = data,
    mapping = mapping,
    stat = StatCdf,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}



# StatCdfFun -----------

GeomCdfFun <- ggproto("GeomCdfFun", GeomCdf)

StatCdfFun <- ggproto("StatCdfFun", Stat, 
                   
  required_aes = character(0),
  
  default_aes = aes(x = after_stat(x), y = after_stat(F)),
  
  compute_group = function(data, scales, support = NULL, xlim = NULL, fun, args = list()) {
    
    if (is.null(support)) {
      rangex <- xlim %||% scales$x$dimension()
      support <- seq(rangex[1], rangex[2], by = 1)
    } 
    
    data <- data.frame(
      x = support,
      f = do.call(fun, c(quote(support), args))
    )
    
    data$F <- cumsum(data$f)
    
    data
    
  }
  
)



geom_cdf_fun <- function(mapping = NULL, data = NULL, 
                         stat = StatCdfFun, position = "identity", 
                         ...,
                         open_fill = "white",
                         vert_type = "dashed",
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  
  if (is.null(data)) data <- ggplot2:::ensure_nonempty_data
  
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomCdfFun,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      open_fill = open_fill,
      vert_type = vert_type,
      na.rm = na.rm,
      ...
    )
  )
}

stat_cdf_fun <- function(mapping = NULL, data = NULL, 
                         geom = GeomCdfFun, position = "identity", 
                         ...,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  
  if (is.null(data)) data <- ggplot2:::ensure_nonempty_data
  
  layer(
    data = data,
    mapping = mapping,
    stat = StatCdfFun,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}




## Examples: ----------


# Basic eCDF

# Small sample size
tibble(x = rbinom(15, size = 10, prob = .5)) |>
  ggplot(aes(x)) +
  geom_cdf() +
  ylim(c(0, 1))

# Large sample size 
tibble(x = rbinom(1e5, size = 10, prob = .5)) |>
  ggplot(aes(x)) +
  geom_cdf() +
  ylim(c(0, 1))

# Plotting several eCDFs together, different `prop`:
tibble(
  prob = rep(c(.25, .5, .75), each = 25),
  x = rbinom(75, size = 10, prob = prob)
) |>
  mutate(
    prob = as_factor(prob)
  ) |>
  ggplot(aes(x, color = prob)) +
  geom_cdf() +
  ylim(c(0, 1))

# Plot theoretical CDF w/ `StatIdentity`
tibble(
  x = 1:10,
  F = pbinom(x, size = 10, prob = .5)
) |>
  ggplot(aes(x, F)) +
  geom_cdf(stat = StatIdentity)

# Multiple CDFs for values of p
tibble(
  x = 1:10,
  Fx25 = pbinom(x, size = 10, prob = .25),
  Fx50 = pbinom(x, size = 10, prob = .5),
  Fx75 = pbinom(x, size = 10, prob = .75)
) |>
  pivot_longer(
    cols = starts_with("F"),
    values_to = "F",
    names_to = "prob",
    names_prefix = "Fx"
  ) |>
  mutate(
    prob = as.numeric(prob),
    prob = prob / 100,
    prob = scales::label_percent()(prob)
  ) |>
  ggplot(aes(x, y = F, color = prob)) +
  geom_cdf(stat = "Identity")

# Alternatively, can plot known CDF w/ geom_cdf_fun
ggplot() +
  geom_cdf_fun(fun = dbinom, support = 0:10, args = list(size = 10, prob = .5))

ggplot() +
  geom_cdf_fun(aes(color = "25%"), fun = dbinom, support = 0:10, args = list(size = 10, prob = .25)) +
  geom_cdf_fun(aes(color = "50%"), fun = dbinom, support = 0:10, args = list(size = 10, prob = .50)) +
  geom_cdf_fun(aes(color = "75%"), fun = dbinom, support = 0:10, args = list(size = 10, prob = .75)) +
  scale_color_discrete(name = "prob")

# eCDF for continuous data (std. normal):

# Small n
tibble(x = rnorm(10)) |>
  ggplot(aes(x)) +
  geom_cdf() +
  ylim(c(0, 1))

# Hide vertical lines 
tibble(x = rnorm(10)) |>
  ggplot(aes(x)) +
  geom_cdf(vert_type = "blank") +
  ylim(c(0, 1))

# Large n data
tibble(x = rnorm(100)) |>
  ggplot(aes(x)) +
  geom_cdf() + 
  ylim(c(0, 1))

# Hide points
tibble(x = rnorm(100)) |>
  ggplot(aes(x)) +
  geom_cdf(size = 0) + 
  ylim(c(0, 1))

# Showcase convergence (eCDF -> CDF)
groups <- 10
n <- c(10, 100, 1000)

tibble(sample_size = rep(n, times = groups)) |>
  group_by(sample_size) |>
  mutate(id = 1:n()) |>
  slice(rep(id, sample_size[1])) |>
  ungroup() |>
  mutate(
    id = as_factor(id),
    x = rnorm(n())
  ) |>
  ggplot(aes(x, color = id)) +
  geom_cdf(size = 0, show.legend = FALSE) +
  geom_function(fun = pnorm, inherit.aes = FALSE) +
  facet_wrap(vars(sample_size), ncol = 1)






















