library(tidyverse)
library(magrittr)
library(lubridate)

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

# by building
counts_day_time_loc <- data_fall %>%
  mutate(building = str_replace(location, "[0-9]+$", ""),
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


ggplot(counts_day_time_loc) + 
  geom_segment(aes(x = start, xend = end, y = building, yend = building, color = count, group = building), size = 2, alpha = 0.3) + 
  facet_wrap(~day) +
  theme_classic()




