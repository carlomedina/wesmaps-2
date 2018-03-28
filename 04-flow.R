library(tidyverse)
library(magrittr)
library(lubridate)
library(migest)
library(network)
library(ggraph)
library(igraph)
library(circlize)
library(cowplot)
library(reshape2)
library(scales)

# load helper functions
source("02-management.R")

flowplot <- function(mat, node_data = NULL) {
  {# a little convoluted. needs refactoring
  g <- graph_from_adjacency_matrix(mat, mode = "directed", weighted = T)
  
  edgelist <- get.edgelist(g) %>%
    as.tibble() %>%
    rename(from = V1, to = V2) %>%
    mutate(weight = E(g)$weight) %>%
    arrange(from, to) %>%
    group_by(from) %>%
    mutate(curvature = seq(from = 0.5, by = 0.2, length.out = n()))
  
  node_data_filtered <- node_data %>%
    filter(name %in% c(edgelist$from, edgelist$to))
  
  g <- graph_from_data_frame(edgelist, vertices = node_data_filtered)
  E(g)$curvature = edgelist$curvature
  E(g)$weight = edgelist$weight
  }

  # flow plot
  # , node.positions = data.frame(x = V(g)$lon, y = V(g)$lat)
  p1 <- ggraph(g, layout = 'linear', circular = TRUE) +
    geom_edge_arc(aes(color = ..index.., width = weight, alpha = weight), 
                  curvature = 1) +
    scale_edge_width(range = c(0.5, 5)) +
    geom_node_text(aes(label = name)) +
    theme_graph() +
    theme(legend.position = "none") +
    labs(edge_width = "Volume") 
  # +
  #   scale_x_continuous(limits = c(-72.66220, -72.65420)) +
  #   scale_y_continuous(limits = c(41.55050, 41.56100))
 
  mat %>%
    melt() %>%
    rename(from = Var1, to = Var2) %>%
    mutate(value = ifelse(value == 0, NA, value)) -> heatmap_dat # convert 0's to NA's for easier color control
  fourthroot_trans = function() trans_new("fourthroot", function(x) x^(0.25), function(x) x^(0.25))
  p2 <- ggplot(heatmap_dat) +
    geom_tile(aes(to, from, fill = value), alpha = 0.8, color = "white", size = 0.5) +
    theme_classic() +
    theme(legend.position = "right",
          axis.text.x = element_text(angle = 45, hjust = 0, colour = "grey50"),
          axis.title.x = element_text(hjust = 1, margin = margin(t = -15, unit = "pt"), size = 12),
          axis.title.y = element_text(hjust = 1, margin = margin(b = 50, unit = "pt"), size = 12)) +
    scale_fill_viridis(trans = fourthroot_trans(), na.value = "grey80") +
    scale_x_discrete(position = "top") +
    scale_color_discrete(na.value = "white") +
    guides(alpha = FALSE) +
    coord_fixed() +
    labs(fill = "Relative counts",
         x = "Destination",
         y = "Origin")
  title <- cowplot::ggdraw() +
    cowplot::draw_label(sprintf("Student movement between %s and %s", NULL, NULL), fontface='bold', hjust = 1, size = 16)
  cowplot::plot_grid(p1, p2, ncol = 2, rel_widths = c(0.6, 0.4), scale = c(1,1)) %>%
  {cowplot::plot_grid(title, ., ncol = 1, rel_heights = c(0.1, 1), rel_widths = c(1, 1))}
}

{

data <-  read_csv("./data/wesmaps_1169.csv")
buildings <- read_csv("./data/building_location.csv")

m <- data %>%
  clean_data_semester("Fall") %>%
  count_volume_by_day_time(top_n = 10) %>%
  standardize_counts() %>% 
  get_entry_exit_counts() %>%
  get_entry_exit_counts_window("10:15am", "m") %>%
  estimate_flow() 


m %>%
  flowplot()

### force name matches
# temporary workarounds
buildings %>%
  mutate(code = ifelse(grepl("RSC", code), "RSCSEM", code),
         code = ifelse(grepl("RLAN", code), "RLANB", code),

         code = ifelse(grepl("CFA HALL", code), "CFAHALL", code),
         code = ifelse(grepl("USDAN", code), "sink_source", code), # make usdan the source_sink
         code = ifelse(grepl("CHPL", code), "TBA", code)  # make usdan the source_sink
  ) %>%
  rename(fullname = name, name = code) -> node_data

m %>%
  flowplot(node_data)
}


# temporary temporal viz
ct <- data %>%
  clean_data_semester("Fall") %>%
  count_volume_by_day_time(top_n = NULL) %>%
  standardize_counts() %>% 
  get_entry_exit_counts() 
pdf("./temp/monday_flow.pdf", width = 10)
for (time in c("8:30am", "9:50am", "10:50am", "11:40am", "12:10pm", "1:20pm", "2:40pm", "4:00pm")) {
  ct %>%
    get_entry_exit_counts_window(time, "m") -> x

  x %>%
    estimate_flow() %>%
    flowplot(node_data) -> plot
  print(plot)
}
dev.off()
