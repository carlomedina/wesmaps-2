library(tidyverse)
library(magrittr)
library(lubridate)
library(migest)
library(network)
library(ggraph)
library(igraph)
library(circlize)
library(cowplot)

# load helper functions
source("02-management.R")

flowplot <- function(mat) {
  
  {# a little convoluted. needs refactoring
  g <- graph_from_adjacency_matrix(mat, mode = "directed", weighted = T)
  
  edgelist <- get.edgelist(g) %>%
    as.tibble() %>%
    rename(from = V1, to = V2) %>%
    mutate(from = factor(from),
           to = factor(to),
           fromindex = as.numeric(from),
           toindex = as.numeric(to)) %>%
    mutate(weight = E(g)$weight) %>%
    arrange(from, to) %>%
    group_by(from) %>%
    mutate(curvature = seq(from = 0.5, by = 0.2, length.out = n()))
  
  g <- graph_from_data_frame(edgelist)
  E(g)$curvature = edgelist$curvature
  E(g)$weight = edgelist$weight
  }
  # flow plot
  p1 <- ggraph(g, layout = 'lgl') +
    geom_edge_arc(aes(color = ..index.., width = weight), 
                  curvature = 1) +
    scale_edge_width(range = c(0.5, 5)) +
    geom_node_text(aes(label = name)) +
    theme_graph() +
    theme(legend.position = "none") +
    labs(edge_width = "Volume")
 
  mat %>%
    melt() %>%
    rename(from = Var1, to = Var2) -> heatmap_dat
  fourthroot_trans = function() trans_new("fourthroot", function(x) x^(0.25), function(x) x^(0.25))
  p2 <- ggplot(heatmap_dat) +
    geom_tile(aes(to, from, fill = value), alpha = 0.8, color = "white", size = 0.5) +
    theme_classic() +
    theme(legend.position = "right",
          axis.text.x = element_text(angle = 45, hjust = 0, colour = "grey50"),
          axis.title.x = element_text(hjust = 1, margin = margin(t = -15, unit = "pt"), size = 12),
          axis.title.y = element_text(hjust = 1, margin = margin(b = 50, unit = "pt"), size = 12)) +
    scale_fill_viridis(trans = fourthroot_trans()) +
    scale_x_discrete(position = "top") +
    scale_color_discrete(na.value = "white") +
    guides(alpha = FALSE) +
    coord_fixed() +
    labs(fill = "Relative counts",
         x = "Destination",
         y = "Origin")
  title <- cowplot::ggdraw() +
    cowplot::draw_label(sprintf("Student movement between %s and %s", NULL, NULL), fontface='bold', hjust = 1, size = 16)
  cowplot::plot_grid(p1, p2, ncol = 2, rel_widths = c(0.3, 0.8), scale = c(1,1)) %>%
  {cowplot::plot_grid(title, ., ncol = 1, rel_heights = c(0.1, 1), rel_widths = c(1, 1))}
}

{

data <-  read_csv("./data/wesmaps_1169.csv")

m <- data %>%
  clean_data_semester("Fall") %>%
  count_volume_by_day_time(top_n = 10) %>%
  standardize_counts() %>%
  get_entry_exit_counts() %>%
  get_entry_exit_counts_window("10:15am") %>%
  estimate_flow() 
}
m %>%
  flowplot()
  
