library(tidyverse)
library(magrittr)
library(lubridate)
library(ggridges)
library(migest)

data <-  read_csv("./data/wesmaps_1169.csv")

#### find a way to disentangle the time/location element

disentangle_time_loc <- function(data) {
  # get instructor, code, time, loc vars
  
  loc_time <- data %>%
    distinct() %>%
    mutate(id = paste0(code, "-", section)) %>%
    select(id, instructor, time, location)
  
  # split time and loc vars by ";", 
  # create a tibble output and rename the vars
  time_split <- loc_time %$%
    str_split(time, ";", simplify = T) %>%
    as.tibble()
  
  loc_split <- loc_time %$%
    str_split(location, ";", simplify = T) %>%
    as.tibble()
  
  names(time_split) <- paste0("xtime", 1:ncol(time_split))
  names(loc_split) <- paste0("xloc", 1:ncol(loc_split))
  
  # combine the outputs
  # use cbind since there is no good way to merge
  loc_time_split <- cbind.data.frame(loc_time, time_split, loc_split, stringsAsFactors = F) %>%
    select(-c(time, location))
  
  # convert wide to long
  time_long <- loc_time_split %>%
    select_if(!grepl("loc", names(.))) %>%
    gather(key = "var", value = "sched", -(id:instructor)) %>%
    select(-var)
  loc_long <- loc_time_split %>%
    select_if(!grepl("time", names(.))) %>%
    gather(key = "var", value = "location", -(id:instructor)) %>%
    select(-var)
  
  # tie things together
  # disentangle further by parsing the mtwrf times
  mutate(time_long, 
         location = loc_long$location) %>%   # cbind is appropriate here
    arrange(instructor) %>%
    filter(sched != "") %>%
    mutate(time = str_extract(sched, "[0-9].*"),
           day = str_replace(sched, "[0-9].*", ""),
           day = ifelse(day == "TBA", "na", day)) %>%   
    mutate(m = grepl("M", day),
           t = grepl("T", day),
           w = grepl("W", day),
           r = grepl("R", day),
           f = grepl("F", day)
    ) %>%
    select(id, instructor, time, location, m, t, w, r, f)  %>%
    gather(key = "day", value = "isTrue", -(c(id:location))) %>%
    filter(isTrue) %>%
    select(-isTrue) %>%
    arrange(instructor, id) %>%
    mutate(start = str_extract(time, ".*?(?=-)"),
           end = str_extract(time, "(?<=-).*") ) %>%
    select(-time) -> loc_time_clean
  
  return(loc_time_clean)
}

  aaaa <- disentangle_time_loc(data)
data_clean <- merge(disentangle_time_loc(data),
                        data %>%
                          mutate(id = paste0(code, "-", section)) %>%
                          select(id, ispoi, maxcap, available, sem),
                    by = "id"
              )

data_fall <- data_clean %>%
  filter(grepl("Fall", sem)) %>%
  mutate(enrolled = maxcap - available, 
         location = ifelse(grepl("PHED", id), "FREEM", location),
         location = ifelse(grepl("FILM", id), "CFS", location)# for PE classes, change location to Freeman
         ) %>%    
  select(-c(maxcap, available, sem)) %>%
  mutate(location = ifelse(location == "", last(location), location)) %>%
  filter(!is.na(start) & !ispoi) # remove pois and those with no start time



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

colSums(temp[2:3])
