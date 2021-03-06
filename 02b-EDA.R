library(tidyverse)
library(magrittr)
library(lubridate)
library(ggridges)
library(gganimate)


#############
#
#   used to be the "flow" script. 
#   now the functions are migrated to 02-management.R
#   "flow" script is now a standalone script
#   EDA needs to refactoring
#
#############

# load all functions
source("02-management.R")

data <-  read_csv("./data/wesmaps_1169.csv")
buildings <- read_csv("./data/building_location.csv")

buildings %>%
  mutate(code = ifelse(grepl("RSC", code), "RSCSEM", code),
         code = ifelse(grepl("RLAN", code), "RLANB", code),
         
         code = ifelse(grepl("CFA HALL", code), "CFAHALL", code),
         code = ifelse(grepl("USDAN", code), "sink_source", code), # make usdan the source_sink
         code = ifelse(grepl("CHPL", code), "TBA", code)  # make usdan the source_sink
  ) %>%
  rename(fullname = name, name = code) -> node_data


### create animation 
data %>%
  clean_data_semester("Fall") %>%
  count_volume_by_day_time(top_n = NULL) %>%
  standardize_counts(by = "building", start_time = "8:00am", end_time = "6:00pm", window_size = "20 min") %>%
  merge(node_data, by.x = "building", by.y = "name") %>%
  mutate(counts = ifelse(counts == 0, NA, counts)) -> cts
  
  ggplot(cts) +
    geom_point(aes(lon, lat, size = counts, frame = time)) +
    facet_wrap(~day, ncol = 5) +
    theme_classic() +
    coord_map() -> p1
  
  gganimate(p1, "output.html")
 




#### COUNTING ####

# by building
counts_day_time_loc <- data_fall %>%
  mutate(building = str_replace(location, "[0-9]+$", "") %>% str_trim(),
         day = factor(day, 
                      c("m", "t", "w", "r", "f"),
                      c("m", "t", "w", "r", "f"),
                      ordered = T)) %>%
  group_by(day, start, end, building) %>%
  summarise(count = sum(enrolled)) %>%
  arrange(day, start, building) %>%
  ungroup() %>%
  mutate(start = paste(Sys.Date(), start) %>% ymd_hm(),
         end = paste(Sys.Date(), end) %>% ymd_hm()) 

### total_counts 
top_building_list <- counts_day_time_loc %>%
  group_by(building) %>%
  summarise(count = sum(count)) %>%
  arrange(desc(count)) %$%
  head(building, 10)

counts_day_time_loc %>%
  filter(building %in% top_building_list) %>%
  mutate(building = factor(building, rev(top_building_list), rev(top_building_list), ordered = T),
         day = factor(day, c("m", "t", "w", "r", "f"), c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), ordered = T)) %>%
  
  ggplot() + 
  geom_segment(aes(x = start, xend = end, y = building, yend = building, size = count), color = "#ba0c2f", alpha = 0.3) + 
  scale_size_continuous(range = c(1, 15) ) +
  scale_x_datetime() +
  facet_wrap(~day, ncol = 5) +
  theme_classic() +
  labs(title = "Top 10 buildings with the greatest traffic",
       subtitle = "Source: WesMaps",
       y = "Building",
       x = "Time",
       size = "Volume") +
  theme(plot.title = element_text(size = 20, face = "bold", family = "Source Sans Pro"),
        axis.title = element_text(size = 14, face = "bold", family = "Source Sans Pro"),
        axis.title.x = element_text(margin = margin(t = 20, l = 20)),
        legend.title = element_text(size = 14, face = "bold", hjust = 1, family = "Source Sans Pro"),
        legend.position = "bottom")

# CREATE A WINDOW FUNCTION #
window_aggregator <- function(data, start_time = "8:00am", end_time = "10:00pm", window_size = "5 min") {
  init <- 0
  standard_time <- seq(ymd_hm(paste(Sys.Date(), start_time)),
                       ymd_hm(paste(Sys.Date(), end_time)), by = window_size)
  counts <- lapply(1:length(standard_time), function (index) {
    point <- standard_time[index]
    window <- data %>%
      dplyr::filter(start <= point & end > point)
    if (nrow(window) > 0) {
      return(sum(window$count))
    } else {
      return(0)
    }
  }) %>%
    unlist()
  return(tibble(time = standard_time, counts = counts))
}

standardized <- counts_day_time_loc %>%
  group_by(building, day) %>%
  nest() %>%
  mutate(standardized_time = map(data, window_aggregator)) %>%
  unnest(standardized_time, .drop = T)

standardized %>%
  filter(building %in% top_building_list) %>%
  mutate(building = factor(building, (top_building_list), (top_building_list), ordered = T),
         day = factor(day, 
                      rev(c("m", "t", "w", "r", "f")), 
                      rev(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")), 
                      ordered = T)) %>%
  
  ggplot() +
  geom_ridgeline(aes(x = time, height = counts, y = day, fill = day, color = building), size = 0.01, scale = 0.001, stat = "identity", alpha = 0.3) +
  # geom_line(aes(x = time, y = counts, color = building)) +
  scale_fill_cyclical(values = c("#ba0c2f", "black")) +
  scale_color_cyclical(values = c("blue", "green")) +
  facet_wrap(~building, ncol = 10) + 
  theme_classic() + 
  theme(plot.title = element_text(size = 20, face = "bold", family = "Source Sans Pro"),
        axis.title = element_text(size = 14, face = "bold", family = "Source Sans Pro"),
        axis.title.x = element_text(margin = margin(t = 20, l = 20)),
        legend.title = element_text(size = 14, face = "bold", hjust = 1, family = "Source Sans Pro"),
        legend.position = "bottom")

#### by major classes ####
counts_day_time_major <- data_fall %>%
  mutate(major = str_replace(id, "[0-9-]+$", "") %>% str_trim(),
         day = factor(day, 
                      c("m", "t", "w", "r", "f"),
                      c("m", "t", "w", "r", "f"),
                      ordered = T)) %>%
  group_by(day, start, end, major) %>%
  summarise(count = sum(enrolled)) %>%
  arrange(day, start, major) %>%
  ungroup() %>%
  mutate(start = paste(Sys.Date(), start) %>% ymd_hm(),
         end = paste(Sys.Date(), end) %>% ymd_hm()) 

### total_counts 
top_major_list <- counts_day_time_major %>%
  group_by(major) %>%
  summarise(count = sum(count)) %>%
  arrange(desc(count)) %$%
  head(major, 10)

counts_day_time_major %>%
  filter(major %in% top_major_list) %>%
  mutate(major = factor(major, rev(top_major_list), rev(top_major_list), ordered = T),
         day = factor(day, c("m", "t", "w", "r", "f"), c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), ordered = T)) %>%
  
  ggplot() + 
  geom_segment(aes(x = start, xend = end, y = major, yend = major, size = count), color = "#ba0c2f", alpha = 0.3) + 
  scale_size_continuous(range = c(1, 15) ) +
  scale_x_datetime() +
  facet_wrap(~day, ncol = 5) +
  theme_classic() +
  labs(title = "Top 10 major classes with the greatest traffic",
       subtitle = "Source: WesMaps",
       y = "Major",
       x = "Time",
       size = "Volume") +
  theme(plot.title = element_text(size = 20, face = "bold", family = "Source Sans Pro"),
        axis.title = element_text(size = 14, face = "bold", family = "Source Sans Pro"),
        axis.title.x = element_text(margin = margin(t = 20, l = 20)),
        legend.title = element_text(size = 14, face = "bold", hjust = 1, family = "Source Sans Pro"),
        legend.position = "bottom")



standardized_major <- counts_day_time_major %>%
  group_by(major, day) %>%
  nest() %>%
  mutate(standardized_time = map(data, window_aggregator)) %>%
  unnest(standardized_time, .drop = T)

standardized_major %>%
  filter(major %in% top_major_list) %>%
  mutate(major = factor(major, (top_major_list), (top_major_list), ordered = T),
         day = factor(day, 
                      rev(c("m", "t", "w", "r", "f")), 
                      rev(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")), 
                      ordered = T)) %>%
  
  ggplot() +
  geom_ridgeline(aes(x = time, height = counts, y = day, fill = day, color = major), size = 0.01, scale = 0.001, stat = "identity", alpha = 0.3) +
  # geom_line(aes(x = time, y = counts, color = building)) +
  scale_fill_cyclical(values = c("#ba0c2f", "black")) +
  scale_color_cyclical(values = c("blue", "green")) +
  facet_wrap(~major, ncol = 10) + 
  theme_classic() + 
  theme(plot.title = element_text(size = 20, face = "bold", family = "Source Sans Pro"),
        axis.title = element_text(size = 14, face = "bold", family = "Source Sans Pro"),
        axis.title.x = element_text(margin = margin(t = 20, l = 20)),
        legend.title = element_text(size = 14, face = "bold", hjust = 1, family = "Source Sans Pro"),
        legend.position = "bottom")


## create function to get entry and exit counts
entry_exit_counts <- function(data) {
  time <- data$time
  counts <- data$counts
  entry_exit <- lapply(2:(length(time)-1), function (index) {
    # just entered and about to leave
    # if just entered and about to leave is negative, maket them zero
    # the movement has been taken into account in time index-1
    exit <- (counts[index+1] - counts[index]) %>% 
    {ifelse(. > 0, 0, abs(.))}
    entry <- (counts[index] - counts[index-1]) %>% 
    {ifelse(. < 0, 0, .)}
    return(list(entry = entry, exit = exit))
  }) %>%
    do.call('rbind.data.frame', .)
  return(cbind.data.frame(time = time[2:(length(time)-1)], entry_exit))
}

entry_exit_loc <- standardized %>%
  group_by(building, day) %>%
  nest() %>%
  mutate(entry_exit = map(data, entry_exit_counts)) %>%
  unnest(entry_exit, .drop = T)


# take some time window and see the number entry / exit data
time <- "10:15am"
astime <- ymd_hm(paste(Sys.Date(), time))
window <- 1200

temp <- entry_exit_loc %>%
  filter(time < (astime + window) & time > (astime - window)) %>%
  group_by(building) %>%
  summarise(entry = sum(entry),
            exit = sum(exit))



#### OD MATRIX ANALYSIS ####

## HELPER FUNCTIONS ##
# adapted from my mrt-eda exploration



flowplots <- function(data) {
  data %$%
    equalize_sum(entry, exit) -> net_counts
  
  mat <- net_counts %>%
  {cm2(.[[1]], .[[2]])}
  
  mat <- mat[[1]] 
  rownames(mat) <- data$building
  colnames(mat) <- data$building
  # # # mat <- scale(mat, center = F)
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
  # plot(g,
  #      vertex.size = 0.1,
  #      vertex.label = NA,
  #      edge.width = E(g)$weight*2,
  #      edge.arrow.size = 0.01,
  #      edge.color = E(g)$color,
  #      layout = matrix(c(rep(0, 13), (1:13)*10), byrow = F, ncol = 2),
  #      edge.curved=E(g)$curvature)
  
  # flow plot
  p1 <- ggraph(g, layout = 'nicely') +
    geom_edge_arc(aes(alpha = weight), 
                  curvature = 1, 
                  arrow = arrow(length = unit(2, 'mm'), 
                                type = "closed")) +
    scale_edge_width(range = c(0.5, 5)) +
    scale_edge_alpha(guide = F) +
    geom_node_text(aes(label = name)) +
    scale_edge_color_discrete(guide = F) + 
    # geom_node_text(aes(label = name)) +
    theme_graph() +
    scale_x_reverse() +
    coord_flip() +
    theme(legend.position = "none") +
    labs(edge_width = "Volume")
  print(p1)
  # # heatmap
  # rownames(mat) <- station_labels
  # colnames(mat) <- station_labels
  # heatmap_dat <- mat %>%
  #   apply(1, function(x) {x/sum(x)}) %>%
  #   t() %>%
  #   melt() %>%
  #   rename(from = Var1, to = Var2) %>%
  #   mutate(indexto = rep(1:13, each = 13),
  #          indexfrom = rep(1:13, 13),
  #          from = factor(from, levels = rev(station_labels), ordered = T)) %>%
  #   mutate(isSB = ifelse(indexfrom == indexto, NA, indexfrom < indexto),
  #          value = ifelse(value == 0, NA, value))
  # 
  # p2 <- ggplot(heatmap_dat) +
  #   geom_tile(aes(to, from, fill = value), alpha = 0.8, color = "white", size = 2) +
  #   theme_classic() +
  #   theme(legend.position = "right",
  #         axis.text.x = element_text(angle = 45, hjust = 0, colour = "grey50"),
  #         axis.title.x = element_text(hjust = 1, margin = margin(t = -15, unit = "pt"), size = 12),
  #         axis.title.y = element_text(hjust = 1, margin = margin(b = 50, unit = "pt"), size = 12)) +
  #   scale_x_discrete(position = "top") +
  #   scale_fill_continuous(low = "grey90", high = "steelblue", na.value = "black", breaks = 0.05*0:5, labels = percent, limits = c(0, 0.25)) +
  #   scale_color_discrete(na.value = "white") +
  #   guides(alpha = FALSE) +
  #   coord_fixed() +
  #   labs(fill = "Percent traffic \nfrom origin \nto destination",
  #        x = "Destination",
  #        y = "Origin")
  # 
  # mintime_label <- paste0(str_pad(mintime, width = 2, side = "left", pad = "0"), "00")
  # maxtime_label <- paste0(str_pad(maxtime, width = 2, side = "left", pad = "0"), "00")
  # title <- cowplot::ggdraw() + 
  #   cowplot::draw_label(sprintf("Passenger flow between %s and %s", mintime_label, maxtime_label), fontface='bold', hjust = 1, size = 16)
  # cowplot::plot_grid(p1, p2, ncol = 2, rel_widths = c(0.3, 0.8), scale = c(1,1)) %>%
  # {cowplot::plot_grid(title, ., ncol = 1, rel_heights = c(0.1, 1), rel_widths = c(1, 1))}
}

pdf("./output/checkflows.pdf")
check_times <- c("8:45am", "2:45pm")
for (time in check_times) {
  # take some time window and see the number entry / exit data
  astime <- ymd_hm(paste(Sys.Date(), time))
  window <- 1200
  asday <- "m"
  
  temp <- entry_exit_loc %>%
    filter(time < (astime + window) & time > (astime - window) & day == asday) %>%
    group_by(building) %>%
    summarise(entry = sum(entry),
              exit = sum(exit)) %>% 
    arrange(entry)
  flowplots(temp)
}
dev.off()


temp %$%
  equalize_sum(entry, exit) -> net_counts

mat <- net_counts %>%
{cm2(.[[1]], .[[2]])}

mat <- mat[[1]] 
rownames(mat) <- temp$building
colnames(mat) <- temp$building
chordDiagram(mat, 
             transparency = 0.4, 
             annotationTrack = c("grid"),
             preAllocateTracks=list(track.height = 0.1))
circos.trackPlotRegion(track.index=1, 
                       panel.fun=function(x,y) {
                         xlim = get.cell.meta.data("xlim") 
                         ylim = get.cell.meta.data("ylim")
                         sector.name=get.cell.meta.data("sector.index")
                         
                         circos.text(mean(xlim), 
                                     ylim[1], 
                                     sector.name,
                                     facing="clockwise",
                                     niceFacing=TRUE,
                                     adj=c(0,0.5),
                                     cex = 0.5)
                       },
                       bg.border=NA)



#### STUDENTS ON ACADEMIC BUILDINGS AT A GIVEN TIME ####
students_in_acad <- standardized %>%
  group_by(time, day) %>%
  summarise(counts = sum(counts))

students_in_acad %>%  
  ggplot() +
  geom_line(aes(x = time, y = counts)) +
  facet_wrap(~day, ncol = 5) + 
  theme_classic()


### can model the difference as coming from some "sink"

