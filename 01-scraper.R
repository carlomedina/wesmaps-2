source("00-scraper-functions.R")

#### MAIN ####
end_point <- "https://iasext.wesleyan.edu/regprod/!wesmaps_page.html?"
term <- 1159
list_subj_pages <- get_subj_pages(end_point, term)
list_crse_pages <- lapply(list_subj_pages, FUN = function(subj_page) {
                              # skip student-forum
                              if (grepl("FORM", subj_page)) {
                                return(NULL)
                              }
                              print(subj_page)
                              get_crse_list(end_point, subj_page)
                            }) %>%
                    unlist() %>%
                    unique()


data_list <- lapply(list_crse_pages, FUN = function(crse) {
  print(crse)
  get_crse_page(end_point, crse)
})

data <- data_list %>%
  do.call('rbind', .)

outfile = sprintf("./data/wesmaps_%s.csv", term)
write_csv(data, path = outfile, col_names = T)


#### TO-DO ####
# add section details!!!