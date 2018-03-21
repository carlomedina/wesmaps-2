library(rvest)
library(stringr)
library(dplyr)
library(readr)
library(tibble)

# returns all department loading pages from main page
# in standard query format (e.g. subj_page=QAC&term=1179)
# change subj_page to crse_list AND
# add the query "&offered=Y" to jump straight to course listings
get_subj_pages <- function(baseurl, term) {
  url <- sprintf("%sterm=%s", baseurl, term)
  
  read_html(url) %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    {subset(., grepl("subj_page", .))} %>% 
    unlist()  %>%
    str_extract("(?<=\\?).*") %>%
    str_replace("subj_page", "crse_list") %>%
    paste0("&offered=Y") %>%
    return()
}

get_crse_list <- function(baseurl, crse_list_page) {
  url <- paste0(baseurl, crse_list_page)
  
  read_html(url) %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    {subset(., grepl("crse=", .))} %>% 
    unlist()  %>%
    str_extract("(?<=\\?).*") %>%
    return()
}

# takes a string containing the details of enrollment numbers
# returns a numeric vector containing the counts
parse_enrollment_cap <- function(enrollmentcap) {
  bins <- c(
    "Total Enrollment Limit:", "SR major:", "JR major:",  
    "Seats Available:", "GRAD:", "SR non-major:", 
    "JR non-major:", "SO:", "FR:"
  )
  
  splitpattern <- paste(bins, collapse="|")
  
  str_split(enrollmentcap, splitpattern) %>%
    unlist() %>%
    str_trim() %>%
    .[-1] %>%
    as.integer() %>%
    as.list() %>%
    return()
}

parse_itl <- function(itl) {
  bins <- c("Instructor\\(s\\):", "Times:", "Location:")
  splitpattern <- paste(bins, collapse = "|")
  str_split(itl, splitpattern) %>%
    unlist %>%
    str_trim() %>%
    .[-1] %>% 
    as.list()
}

parse_section_box <- function(sectionbox) {
  sectiontext <- sectionbox %>%
    html_nodes("tr") %>%
    html_text() 
  
  section <- sectiontext[1] %>%
    str_extract("[0-9]{2}")
  
  # ITL row (Instructor, Times, Location)
  itl <- sectiontext %>%
  {subset(., grepl("Instructor\\(s\\)", .))}
  itl_parsed <- parse_itl(itl)
  
  # check if POI
  sectiontext %>%
  {subset(., grepl("Permission of Instructor", .))} %>%
    length(.) != 0 -> isPOI
  
  # if POI
  if (isPOI) {
    sectiontext %>%
    {subset(., grepl("Enrollment capacity", .))} %>%
      str_extract("(?<=Enrollment capacity: )[0-9]+") %>%
      as.integer() %>%
      as.list() -> numseats
    
    seats_parsed <- c(numseats, as.list(rep(NA, 8)))
  }
  # else
  else {
    sectiontext %>%
    {subset(., grepl("Total Enrollment|Seats Available", .))} %>%
      paste(collapse = " ") %>%
      str_replace_all("&nbsp", "") -> enrollmentcap
    
    seats_parsed <- parse_enrollment_cap(enrollmentcap)
  }
  
  # tying the info together
  row <- c(itl_parsed, seats_parsed, list(isPOI), list(section))
  names(row) <- c("instructor", "time", "location", "maxcap",
                  "srmajor", "jrmajor", "available", "grad",
                  "srnon", "jrnon", "so", "fr", "ispoi", "section")
  row %>%
    as.tibble %>%
    return()
}

parse_code_sem <- function(codesem) {
  codesem %>%
    str_replace("Section.*", "") %>%
    str_trim() %>%
    str_split(" ") %>%
    unlist() -> codesem_clean
  return(c(paste(codesem_clean[1:2], collapse = ""), 
           paste(codesem_clean[3:4], collapse = "")))
}

get_crse_page <- function(baseurl, crse_page) {
  url <-paste0(baseurl, crse_page)
  html <- read_html(url)
  
  # get class info
  html %>%
    html_node("span.title") %>%
    html_text() -> course_title
  
  code_sem_cross <- html %>%
    html_node("table") %>% 
    html_children() %>%
    html_node("table") %>%
    html_children() %>%
    html_text() %>%
    str_replace_all("\n", " ") %>%
    {subset(., grepl("Crosslisting|[A-Z&]{3,4} [0-9]{3} (?:Fall|Spring) [0-9]{4}", .))}  # & needed for MB&B, NS&B
  
  is_cross <- ifelse(length(code_sem_cross) > 1, T, F)
  code_sem_clean <- parse_code_sem(code_sem_cross[1])
  
  # get section info
  sections <- html %>%
    html_node(css = "#print_sect_info") %>%
    html_nodes("table")
  
  # check if multiple sections
  lapply(sections, FUN = function(section) parse_section_box(section)) %>%
    do.call('rbind', .) %>%
    as.tibble() -> section_data
  
  course_data <- section_data %>%
    mutate(code = code_sem_clean[1],
           sem = code_sem_clean[2],
           title = course_title,
           iscross = is_cross)
  
}
