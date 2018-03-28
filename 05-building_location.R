library(rvest)
library(ggmap)

building_url <- "http://www.wesleyan.edu/registrar/general_information/building_codes.html"

read_html(building_url) %>%
  html_table() %>%
  .[[1]] %>% 
  as.matrix() %>%
  {rbind(.[,1:2], .[,3:4])} %>%
  as.data.frame(stringsAsFactors = F) %>%
  {tibble(code = .$X1[.$X1 != ""],
         name = .$X2[seq(1, nrow(.), 2)],
         address = .$X2[seq(2, nrow(.), 2)])} -> buildings

# correct the mistakes
buildings$address[buildings$code == "CRT"] <- "285 Court St"
buildings$address[buildings$code == "92TH"] <- "207 High St"
buildings$address[buildings$code == "VVO"] <- "96 Foss Hill"
buildings$address[buildings$code == "SHAN"] <- "237 Church St"
buildings$address[buildings$code == "TST"] <- "271 Washington Terrace"
buildings$address[buildings$code == "ZLKA"] <- "283 Washington Terrace"
buildings$address[buildings$code == "HALL"] <- "52 Lawn Avenue"

# add geocoding
buildings %>%
  mutate(location = sprintf("%s, Middletown, CT", address)) %>%
  mutate_geocode(location = location, source = "dsk", force = T, override_limit = T) -> buildings_geocoded
  
write_csv(buildings_geocoded, "./data/building_location.csv")


