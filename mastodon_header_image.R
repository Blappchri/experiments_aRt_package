#pick a mastodon header 500x1500px

library(dplyr)
library(ggplot2)
library(aRt)

wander_alt<-function (
    n_lines = 100, 
    n_points = 350, 
    r_outer = 8, 
    r_inner = 3, 
    line_var = 0.01, 
    deg_jitter = 0.1, 
    linewidth = 0.1, 
    bg_col = "black", 
    col_palette = c("#a000a0", "white", "#c00020", "#10c000"), 
    n_cols = 20, 
    s = 1) 
{
  set.seed(s)
  theta <- runif(n_lines,0, 2 * pi)
  x_inner <- r_inner * cos(theta)
  y_inner <- r_inner * sin(theta)
  x_outer <- 3 * r_outer * cos(theta)
  y_outer <- r_outer * sin(theta)
  all_cols <- (grDevices::colorRampPalette(col_palette))(n_cols)
  line_data <- dplyr::bind_rows(
    purrr::map(
      .x = seq_len(length(theta)), 
      .f = ~data.frame(
        x0 = seq(x_inner[.x], x_outer[.x], length.out = n_points), 
        y0 = seq(y_inner[.x], y_outer[.x], length.out = n_points), 
        grp = .x, col = sample(all_cols, size = 1)
        )
      )
    )
  line_data$noise_x <- as.vector(
    replicate(
      length(theta), 
      cumsum(c(
        0, 
        stats::rnorm(n = n_points - 1, mean = 0, sd = sqrt(line_var))))
      )
    ) + line_var
  line_data$noise_y <- as.vector(
    replicate(
      length(theta), 
      cumsum(c(
        0, 
        stats::rnorm(n = n_points - 1, mean = 0, sd = sqrt(line_var))))
      )
    )
  plot_data <- dplyr::mutate(
    tibble::as_tibble(line_data), 
    x = (.data$x0 + .data$noise_x), 
    y = .data$y0 + .data$noise_y
    )
  lims <- ceiling(max(abs(c(plot_data$x, plot_data$y))))
  
  p <- ggplot2::ggplot() + 
    ggplot2::coord_fixed(xlim = c(-30,30),ylim=c(-10,10),expand = FALSE) + 
    ggfx::with_blur(
      ggplot2::geom_path(
        data = plot_data, 
        mapping = ggplot2::aes(
          x = .data$x, 
          y = .data$y, 
          group = .data$grp, 
          colour = .data$col
          ), 
        position = ggplot2::position_jitter(
          width = deg_jitter * 2, 
          height = deg_jitter * 2
          ), 
        linewidth = linewidth * 1.5, 
        alpha = 0.3
        ), 
      sigma = 0.5
      ) + 
    ggplot2::geom_path(
      data = plot_data, 
      mapping = ggplot2::aes(
        x = .data$x, 
        y = .data$y, 
        group = .data$grp, 
        colour = .data$col
        ), 
      position = ggplot2::position_jitter(
        width = deg_jitter, 
        height = deg_jitter
        ), 
      alpha = 0.5, 
      linewidth = linewidth
      ) + 
    ggplot2::geom_path(
      data = plot_data, 
      mapping = ggplot2::aes(
        x = .data$x, 
        y = .data$y, 
        group = .data$grp, 
        colour = .data$col
        ), 
      alpha = 0.7, 
      linewidth = linewidth
      ) + 
    ggplot2::scale_x_continuous(
      limits = c(-lims, lims)
      ) + 
    ggplot2::scale_y_continuous(
      limits = c(-lims, lims)
      ) + 
    ggplot2::scale_colour_identity() + 
    ggplot2::theme_void() + 
    ggplot2::theme(
      plot.background = ggplot2::element_rect(
        fill = bg_col, 
        colour = bg_col
        ), 
      panel.background = ggplot2::element_rect(
        fill = bg_col, 
        colour = bg_col
        ), 
      plot.margin = ggplot2::margin(0, 0, 0, 0))
  return(p)
}
wander_alt(r_inner = 1.5,n_lines = 120,line_var = 0.03,n_points = 300,n_cols = 8)

wander_alt(
  r_inner = 1.5,n_lines = 120,line_var = 0.03,n_points = 300,n_cols = 24,
  bg_col = "darkred",col_palette = c("white","lightblue2","white","deeppink2"),s=2)

