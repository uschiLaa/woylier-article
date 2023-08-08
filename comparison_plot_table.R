library(cowplot)
library(tourr)
library(woylier)
library(tidyverse)

# this is tailored for this specific matrix format!
draw_text_matrix <- function(m){
  # first build vector with the lines
  v <- character(nrow(m))
  for(i in 1:nrow(m)){
    v[i] <- paste0(round(m[i,1],3), "     ", round(m[i,2],3))
  }
  # now arrange them in draw_text
  y_vals <- c(0.75, 0.65, 0.55, 0.45)
  draw_text(v, x = 0.5, y = y_vals, size = 10)
}

set.seed(2022)
p <- 4
base1 <- tourr::basis_random(p, d=2)
base2 <- matrix(c(0, 0, 1, 0, 0, 0, 0, 1), ncol = 2)
d <- as.matrix(sine_curve[,3:6])

# we can get givens path directly
path_givens <- givens_full_path(base1, base2, nsteps = 5)

# no such function for geodesic path, so need to use workaround
pt_geo <- save_history(d, planned_tour(list(base1, base2)))
path_geo <- interpolate(pt_geo, angle = proj_dist(base1, base2)/4)[,,c(1:4,6)]
# why is this behaving so weird? both entry 4 and 5 are repeated (in 4,5 and 6,7)

all_plots <- NULL

for(i in 1:5){
  a_givens <- path_givens[,,i]
  a_geo <- drop(path_geo[,,i])
  # need to get rid of tour attributes
  attributes(a_geo) <- attributes(a_givens)
  p_givens <- d %*% a_givens
  p_geo <- d %*% a_geo
  gg_givens <- ggplot(as.tibble(p_givens), aes(V1, V2)) + geom_point() + theme_void()
  mat_givens <- ggdraw() + draw_text_matrix(a_givens)
  gg_geo <- ggplot(as.tibble(p_geo), aes(V1, V2)) + geom_point() + theme_void()
  mat_geo <- ggdraw() + draw_text_matrix(a_geo)
  if(is.null(all_plots)){
    all_plots <- list(mat_givens, gg_givens, mat_geo, gg_geo)
    next
  }
  all_plots <- append(all_plots, list(mat_givens, gg_givens, mat_geo, gg_geo))
}


plot_grid(plotlist = all_plots, ncol = 4)


