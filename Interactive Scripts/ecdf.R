library("tidyverse"); theme_set(theme_minimal())

# in the below, df contains two cols: x (the obs) and Fx (the CDF)
ggcdf <- function(df) {
  
  # set graphical parameter (color of empty holes)
  hole_color <- ggplot2:::ggplot_global$theme_current$panel.background$fill
  if (is.null(hole_color)) hole_color <- ggplot2:::ggplot_global$theme_current$rect$fill
  
  # make ranges for data
  xrng <- scales::expand_range(range(df$x), mul = .10)
  xmin <- xrng[1]; xmax <- xrng[2]
  n <- nrow(df)
  
  # make horizontal and vertical line dfs
  stat_df_horiz <- tibble(
    x = c(xmin, df$x), xend = c(df$x, xmax),
    y = c(0, df$Fx), yend = c(0, df$Fx)
  )
  stat_df_vertical <- tibble(
    x = df$x,            xend = df$x,
    y = c(0, df$Fx[-n]), yend = df$Fx
  )
  
  # make open and closed points dfs
  stat_df_open <- tibble(x = df$x, y = c(0, df$Fx[-n]))
  stat_df_closed <- tibble(x = df$x, y = df$Fx)
  
  # make plot
  ggplot(df, aes(x)) + 
    geom_segment(aes(x = x, xend = xend, y = y, yend = yend), data = stat_df_horiz) +
    geom_segment(aes(x = x, xend = xend, y = y, yend = yend), data = stat_df_vertical, linetype = 2) +
    geom_point(aes(x, y), data = stat_df_open, shape = 21, fill = hole_color, size = 3) +
    geom_point(aes(x, y), data = stat_df_closed, shape = 21, fill = "black", size = 3) +
    labs(x = "x", y = "p")
  
}



# with a known binomial distribution
N <- 3
df <- tibble(x = 0:N) %>% 
  mutate(Fx = pbinom(x, size = N, p = .50))

p <- ggcdf(df)

layer_data(p)

N <- 20
df <- tibble(x = 0:N) %>% 
  mutate(Fx = pbinom(x, size = N, p = .50))
ggcdf(df)



# with an empirical
data <- tibble(x = rnorm(10))
df <- data %>% count(x) %>% arrange(x) %>% mutate(Fx = cumsum(n/sum(n)))
ggcdf(df)
ggcdf(df) + geom_rug(aes(x), data = data, inherit.aes = FALSE)




### My stuff ----

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
                   
  draw_group = function(data, panel_params, coord) {
    
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
        lty = "dashed" # expose as a parameter
      )
    )
     
    grobs$open <- grid::pointsGrob(
      coord_vert$x, coord_vert$y, 
      default.units = "native",
      pch = 21, # expose as a parameter
      gp = grid::gpar(
        col = coord_vert$colour, 
        fill = "white", # expose as a parameter
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
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE) {
  
  # For StatCdfFun
  if (is.null(data)) data <- ggplot2:::ensure_nonempty_data
  
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomCdf,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}




tibble(
  x = 1:10,
  Fx1 = pbinom(x, size = 10, prob = .25),
  Fx2 = pbinom(x, size = 10, prob = .5),
  Fx3 = pbinom(x, size = 10, prob = .75)
) |>
  pivot_longer(cols = starts_with("F")) |>
  ggplot(aes(x, y = value, color = name)) +
  geom_cdf(stat = "Identity")


tibble(x = rbinom(15, size = 10, prob = .25)) |>
  ggplot(aes(x)) +
  geom_cdf() +
  ylim(c(0, 1))

tibble(
  prob = rep(c(.25, .5, .75), each = 25),
  x = rbinom(75, size = 10, prob = prob)
) |>
  ggplot(aes(x, color = as_factor(prob))) +
  geom_cdf() +
  ylim(c(0, 1))

tibble(
  id = rep(1:10, each = 5),
  x = rbinom(10 * 5, size = 10, prob = .5)
) |>
  ggplot(aes(x, color = as.factor(id))) +
  geom_cdf(linewidth = 1, alpha = .5, show.legend = FALSE) +
  geom_cdf(stat = StatCdfFun, fun = dbinom, args = list(size = 10, prob = .5), xlim = c(0, 10), color = "black", linewidth = 1.5) +
  ylim(c(0, 1))

tibble(
  id = rep(1:10, each = 15),
  x = rbinom(10 * 15, size = 10, prob = .5)
) |>
  ggplot(aes(x, color = as.factor(id))) +
  geom_cdf(linewidth = 1, alpha = .5, show.legend = FALSE) +
  geom_cdf(stat = StatCdfFun, fun = dbinom, args = list(size = 10, prob = .5), color = "black", linewidth = 1.5) +
  ylim(c(0, 1))
  
tibble(
  id = rep(1:10, each = 30),
  x = rbinom(10 * 30, size = 10, prob = .5)
) |>
  ggplot(aes(x, color = as.factor(id))) +
  geom_cdf(linewidth = 1, alpha = .5, show.legend = FALSE) +
  geom_cdf(stat = StatCdfFun, fun = dbinom, args = list(size = 10, prob = .5), color = "black", linewidth = 1.5) +
  ylim(c(0, 1)) +
  xlim(c(0, 10))
  
tibble(
  id = rep(1:10, each = 100),
  x = rbinom(10 * 100, size = 10, prob = .5)
) |>
  ggplot(aes(x, color = as.factor(id))) +
  geom_cdf(linewidth = 1, alpha = .5, show.legend = FALSE) +
  geom_cdf(stat = StatCdfFun, fun = dbinom, args = list(size = 10, prob = .5), support = 0:10, color = "black", linewidth = 1.5) +
  ylim(c(0, 1))
  
sample_sizes <- c(5, 10, 250)
tibble(
  n = rep(sample_sizes, sample_sizes),
  x = rbinom(sum(sample_sizes), size = 10, prob = .5)
) |>
  ggplot(aes(x, color = as.factor(n))) +
  geom_cdf(linewidth = 1) +
  scale_color_viridis_d(name = "sample size", option = "C", begin = .6, end = 0)
  


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

ggplot() +
  geom_cdf(stat = StatCdfFun, support = 0:10, fun = dbinom, args = list(size = 10, prob = .5))

ggplot() +
  geom_cdf(stat = StatCdfFun, xlim = c(0, 8), fun = dpois, args = list(lambda = 1)) +
  ylim(c(0, 1))

tibble(x = 0:10) |>
  ggplot(aes(x)) +
  geom_cdf(stat = StatCdfFun, fun = dbinom, args = list(size = 10, prob = .5))


# Empirical CDFs from std. normal data
# with different sample sizes
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





