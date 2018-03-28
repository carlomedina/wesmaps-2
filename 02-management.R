library(tidyverse)
library(magrittr)
library(lubridate)
library(ggridges)

# # test data
# data <-  read_csv("./data/wesmaps_1169.csv")

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
  mutate(time_long, 
         location = loc_long$location) %>%              # cbind is appropriate here (combine the "long" format of time and loc)
    arrange(instructor) %>%
    filter(sched != "") %>%
    mutate(time = str_extract(sched, "[0-9].*"),
           day = str_replace(sched, "[0-9].*", ""),
           day = ifelse(day == "TBA", "na", day)) %>%   # parse the sched var into time and day
    mutate(m = grepl("M", day),
           t = grepl("T", day),
           w = grepl("W", day),
           r = grepl("R", day),
           f = grepl("F", day)) %>%                     # determine if the class meets at the following day
    select(id, instructor, time, location, m, t, w, r, f)  %>%
    gather(key = "day", value = "isTrue", -(c(id:location))) %>%    # turn wide to long
    filter(isTrue) %>%                                  # select only the valid observations
    select(-isTrue) %>%
    arrange(instructor, id) %>%
    mutate(start = str_extract(time, ".*?(?=-)"),
           end = str_extract(time, "(?<=-).*") ) %>%
    select(-time) -> loc_time_clean
  
  return(loc_time_clean)
}
# clean_data_semester
# input: raw table data
# return: data with time and loc variables cleaned, for a given semester, calculated enrolled numbers
#         cleans location for PHED and FILM classes
clean_data_semester <- function(data, semester) {
  # add id
  data_with_id <- data %>%
    mutate(id = paste0(code, "-", section))
  
  merge(disentangle_time_loc(data),
        data_with_id %>% 
          select(id, ispoi, maxcap, available, sem),
        by = "id") %>%
    filter(grepl(semester, sem)) %>%
    mutate(enrolled = maxcap - available, 
           location = gsub(" .*", "", location), # remove room names after spaces
           location = ifelse(grepl("PHED", id), "FAC", location), # for PE classes, change location to Freeman
           location = ifelse(grepl("FILM", id), "CFS", location)  # for FILM classes, change location to CFS
    ) %>%    
    select(-c(maxcap, available, sem)) %>%
    mutate(location = ifelse(location == "", last(location), location)) %>%
    filter(!is.na(start) & !ispoi) # remove pois and those with no start time
  
}

# input: data - the one that is outputed by clean_data_semester
#         by - either major or building
#         top_n - top n major or building based on total counts. if NULL return all
# return: data frame of counts by day and time, either by building or major 
#         (showing the top_n of building or major)
count_volume_by_day_time <- function(data, by = "building", top_n = 10) {
  # TODO: refactor by finding a way to call in the by 
  # variable in the group_by & arrange  statement
  if (by == "building") {
    mutate(data, 
           building = str_replace(location, "[0-9]+.*$", "") %>% str_trim(),
           day = factor(day, 
                        c("m", "t", "w", "r", "f"),
                        c("m", "t", "w", "r", "f"),
                        ordered = T)) %>%
      group_by(day, start, end, building) %>%
      summarise(count = sum(enrolled)) %>%
      arrange(day, start, building) %>%
      ungroup() %>%
      mutate(start = paste(Sys.Date(), start) %>% ymd_hm(),
             end = paste(Sys.Date(), end) %>% ymd_hm()) -> intermediate

    if (!is.null(top_n)) {
      top_list <- intermediate %>%
        group_by(building) %>%
        summarise(count = sum(count)) %>%
        arrange(desc(count)) %$%
        head(building, top_n)
      
      intermediate %>%
        filter(building %in% top_list) %>%
        return()
    } else {
      return(intermediate)
    }
    
    
  } else if (by == "major") {
    mutate(data, 
           major = str_replace(id, "[0-9-]+$", "") %>% str_trim(),
           day = factor(day, 
                        c("m", "t", "w", "r", "f"),
                        c("m", "t", "w", "r", "f"),
                        ordered = T)) %>%
      group_by(day, start, end, major) %>%
      summarise(count = sum(enrolled)) %>%
      arrange(day, start, major) %>%
      ungroup() %>%
      mutate(start = paste(Sys.Date(), start) %>% ymd_hm(),
             end = paste(Sys.Date(), end) %>% ymd_hm()) -> intermediate
    if (!is.null(top_n)) {
      top_list <- intermediate %>%
        group_by(major) %>%
        summarise(count = sum(count)) %>%
        arrange(desc(count)) %$%
        head(major, top_n)
      
      intermediate %>%
        filter(major %in% top_list) %>%
        return()
    } else {
      return(intermediate)
    }
    
  }
  
}

# Disaggregate data by finding number of students in each building at a regular interval
window_disaggregator <- function(data, start_time = "8:00am", end_time = "10:00pm", window_size = "5 min") {
  standard_time <- seq(ymd_hm(paste(Sys.Date(), start_time)),
                       ymd_hm(paste(Sys.Date(), end_time)), by = window_size)
  # find the number of students at a given time point
  counts <- lapply(1:length(standard_time), function (index) {
    
    # get standardized time point
    point <- standard_time[index]
    
    # select observations where the standardized timepoint is between the start and 
    # end time of the class period
    window <- data %>%
      dplyr::filter(start <= point & end > point)
    
    # return the sum of all counts in the given period
    if (nrow(window) > 0) {
      return(sum(window$count))
    } else {
      return(0)
    }
  }) %>%
    unlist()
  
  # return a tibble of the standardized time and the corresponding counts
  return(tibble(time = standard_time, counts = counts))
}

standardize_counts <- function(data_counts_day_time_loc, by = "building") {
  if (by == "building") {
    data_counts_day_time_loc %>%
      group_by(building, day) %>%
      nest() %>%
      mutate(standardized_counts = map(data, window_disaggregator)) %>%
      unnest(standardized_counts, .drop = T) %>%
      return()
  }
  else if (by == "major") {
    data_counts_day_time_loc %>%
      group_by(major, day) %>%
      nest() %>%
      mutate(standardized_counts = map(data, window_disaggregator)) %>%
      unnest(standardized_counts, .drop = T) %>%
      return()
  }
}

entry_exit_counts <- function(data) {
  time <- data$time
  counts <- data$counts
  entry_exit <- lapply(2:(length(time)-1), function (index) {
    # just entered and about to leave
    # if just entered and about to leave is negative, make them zero
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

get_entry_exit_counts <- function(data_standardized_counts, by = "building") {
  if (by == "building") {
    data_standardized_counts %>%
      group_by(building, day) %>%
      nest() %>%
      mutate(entry_exit = map(data, entry_exit_counts)) %>%
      unnest(entry_exit, .drop = T) %>%
      return()
  }
  else if (by == "major") {
    data_standardized_counts %>%
      group_by(major, day) %>%
      nest() %>%
      mutate(entry_exit = map(data, entry_exit_counts)) %>%
      unnest(entry_exit, .drop = T) %>%
      return()
  }
}

# get the entry exit counts at a particular time window
# input: data - vector of 
get_entry_exit_counts_window <- function(data_entry_exit, time, day, window = 1200) {
  astime <- ymd_hm(paste(Sys.Date(), time))
  window <- 1200
  
  temp <- data_entry_exit %>%
    filter(time < (astime + window) & time > (astime - window) & day == day) %>%
    group_by(building) %>%
    summarise(entry = sum(entry),
              exit = sum(exit))
  
  # to do: add "major" as a by variable
  return(temp)
}


add_sink_source <- function(vector1, vector2) {
  is_vector1_greater <- sum(vector1) >= sum(vector2) 
  
  diff <- ifelse(
    is_vector1_greater, 
    sum(vector1) - sum(vector2),
    sum(vector2) - sum(vector1)
  )
  
  # and the sink/source counts
  if (is_vector1_greater) {
    vector1 <- c(vector1, 0)
    vector2 <- c(vector2, diff)
  } else {
    vector1 <- c(vector1, diff)
    vector2 <- c(vector2, 0)
  }
  return(list(vector1, vector2))
}


estimate_flow <- function(data) {
  data %$%
    add_sink_source(entry, exit) -> net_counts
  
  mat <- net_counts %>%
  {cm2(.[[1]], .[[2]])}
  
  mat <- mat[[1]] 
  rownames(mat) <- c(data$building, "sink_source")
  colnames(mat) <- c(data$building, "sink_source")
  return(mat)
}


