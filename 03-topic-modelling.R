library(tidyverse)
library(readr)
library(tm)
library(topicmodels)
library(tidytext)
library(Rtsne)
library(magrittr)
library(plotly)
library(ggrepel)
library(ggalt)
library(entropy)  # for entropy
data <- read_csv("./data/wesmaps_1159.csv")
division <- read_csv("./data/division.csv")

code_description <- data %>%
  select(code, description) %>%
  distinct() %>%
  rename(doc_id = code, text = description) %>%
  as.data.frame()     # note that tibble gives a buggy value for Corpus. it returns one document only

attr(code_description, "spec") <- NULL

# find top 50 terms to remove in the corpus
code_description %>%
  unnest_tokens(word, text) %>%
  count(word) %>%
  arrange(desc(n)) %>%
  top_n(50) %$%
  word -> top_50_words

# transform data to a corpus
corpus <- DataframeSource(code_description) %>%
  Corpus() %>%
  tm_map(removePunctuation) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(removeWords, c("students", "student", "course", 
                        "will", "also", "examine", "examines",
                        "develop", "learn", "develops", "learns", "read",
                        "reads", "theory", "theories")) %>%  # remove common wesmaps details
  tm_map(removeWords, top_50_words) %>%
  tm_map(stemDocument)

dtm <- DocumentTermMatrix(corpus)

# remove documents with no words
rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ] # remove docs with no words remaining


# from tidytext tutorial: https://www.tidytextmining.com/topicmodeling.html#library-heist

# # k = 10
groups = 35
if (!file.exists(sprintf("./output/class_LDA-%s.RDS", groups))) {
  class_lda <- LDA(dtm.new, k = groups, control = list(seed = 12345))
  saveRDS(class_lda, file = sprintf("./output/class_LDA-%s.RDS", groups))
}
class_lda <- readRDS(sprintf("./output/class_LDA-%s.RDS", groups))

class_topics <- tidy(class_lda, matrix = "beta")

top_terms <- class_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)



# get document classification table
class_gamma <- tidy(class_lda, matrix = "gamma")

class_gamma_wide <- class_gamma %>%
  mutate(topic = paste0("topic", topic),
         document = as.numeric(document)) %>%
  spread(key = topic, value = gamma)

# get classified topic (topic with the highest probability)
classification <- class_gamma %>%
  group_by(document) %>%
  summarise(topic = topic[which.max(gamma)],
            highest = max(gamma)) %>%
  mutate(document = as.numeric(document)) %>%
  arrange(document)

# pre-process code_description so that we can merge
code_description_copy <- code_description %>%
  mutate(document = 1:n())

# merge
matrix <- inner_join(code_description_copy, classification) %>%
  inner_join(class_gamma_wide) 

# visualizing probabilities in 2D
mat_to_reduce <- matrix %>%
  select_if(grepl("topic[0-9]+", names(.))) %>%
  as.matrix() 

if (!file.exists(sprintf("./output/rtsne_results-%s.RDS", groups))) {
  rtsne_results <- Rtsne(mat_to_reduce, check_duplicates = F)
  saveRDS(rtsne_results, sprintf("./output/rtsne_results-%s.RDS", groups))
}
rtsne_results <- readRDS(sprintf("./output/rtsne_results-%s.RDS", groups))

tsne_mat <- rtsne_results$Y %>%
  cbind.data.frame(mat_to_reduce) %>%
  left_join(matrix) %>%
  left_join(data %>% rename(doc_id = code)) %>%
  mutate(department = str_extract(doc_id, "^[A-Z&]+"),
         division = str_extract(division, "^[A-Za-z]+"))

# pca_results <- prcomp(mat_to_reduce)$x %>%
#   .[,1:2] %>%
#   cbind.data.frame(mat_to_reduce) %>%
#   left_join(matrix) %>%
#   mutate(department = str_extract(doc_id, "^[A-Z&]+")) %>%
#   left_join(division)

# top departments 
tsne_mat %>%
  count(department) %>%
  arrange(desc(n)) %>%
  top_n(10) %$%
  department -> top_department

# tsne_data must be in the following format: x,y,topic
get_midpoints <- function(tsne_data) {
  names(tsne_data) <- c("x", "y", "topic")
  tsne_data %>%
    group_by(topic) %>%
    summarise(center.x = mean(x), 
              center.y = mean(y))
}

# by department
pdf(sprintf("./output/topicmodels-dept-%s.pdf", groups), width = 20, height = 20)
  ggplot() +
  geom_point(data = tsne_mat %>% 
               select(-division), 
             aes(`1`, `2`),
             col = "grey80", 
             size = 1, 
             alpha = 0.3) +
  geom_point(data = tsne_mat  %>%
               filter(department %in% top_department & highest > 0.3 & division != "None"), 
             aes(`1`, `2`, col = factor(department)), size = 2) +
  geom_text(data = tsne_mat %>%
              select(`1`, `2`, topic) %>%
              get_midpoints(),
            aes(center.x, center.y, label = topic), size = 4) -> p
  
  # # add circles
  # for (i in 1:groups) {
  #   p <- p + geom_encircle(data = tsne_mat %>%
  #                            filter(topic == i & highest > 0.3 & division != "None"),
  #                          aes(`1`, `2`))
  # }
  
  p <- p +
    facet_wrap(~division) +
    theme_classic() +
    coord_fixed() + 
    theme(legend.position = "bottom",
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
    scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", 
                                  "#E7298A", "#66A61E", "#E6AB02", 
                                  "#A6761D", "#ff9000", "#d10a0a",
                                  "#070099")) +
    labs(col = "Department",
         x = "",
         y = "")
p
dev.off()



# by division
pdf(sprintf("./output/topicmodels-div-%s.pdf", groups), width = 20, height = 20)
ggplot() +
  geom_point(data = tsne_mat %>% 
               select(-division), 
             aes(`1`, `2`),
             col = "grey80", 
             size = 1, 
             alpha = 0.3) +
  geom_point(data = tsne_mat  %>%
               filter(highest > 0.3 & division != "None"), 
             aes(`1`, `2`, col = factor(division)), size = 2) +
  facet_wrap(~division) +
  theme_classic() +
  coord_fixed() + 
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_color_brewer(palette = "Set1") +
  labs(col = "Department",
       x = "",
       y = "") -> p
p
dev.off()

# to do 
# calculate topic entropy (i.e. based on the diversity of divisions in a given topic)
# label clusters of topic in the plot by taking the midpoint of x's and y's

# get entropy of topics
entropies <- tsne_mat %>%
  select(topic, division) %>%
  filter(division != "None") %>%
  count(topic, division) %>%
  group_by(topic) %>%
  summarise(entropy = entropy(n) %>% round(2))


tsne_mat %>%
  select(topic, division) %>%
  filter(division != "None") %>%
  # group_by(topic, division) %>%
  left_join(entropies) %>%
  ggplot() +
    geom_bar(aes(factor(topic), fill = division), position = "fill") +
    # geom_point(aes(factor(topic), 1.5, size = entropy), alpha = 0.3, color = "red") +
    theme_classic() +
  coord_flip() +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) -> entropy1
tsne_mat %>%
  select(topic, division) %>%
  filter(division != "None") %>%
  # group_by(topic, division) %>%
  left_join(entropies) %>%
  ggplot() +
  geom_point(aes(factor(topic), entropy)) +
  geom_segment(aes(factor(topic), 0, xend = factor(topic), yend = entropy)) +
  theme_classic() + 
  coord_flip() -> entropy2

cowplot::plot_grid(entropy1, entropy2, align = "hv", ncol = 2)  

