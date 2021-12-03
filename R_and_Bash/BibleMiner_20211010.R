# + ------------------------------------------------------------------- +
# Word Input ----
# + ------------------------------------------------------------------- +
WordConcept <- "pain"

# + ------------------------------------------------------------------- +
# libraries ----
# + ------------------------------------------------------------------- +
library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidytext)
library(forcats)
library(igraph)
library(ggraph)
library(widyr)
library(tidygraph)
data("stop_words")

# + ------------------------------------------------------------------- +
# Read Data ----
# + ------------------------------------------------------------------- +
setwd("D:/BibleMiner/2021-BibleStudies/2021-10_BibleMiner")
lb_Books   <- read_csv('Books.csv', col_names = TRUE)
lb_Terms   <- read_csv('Terms.csv', col_names = TRUE)
lb_Text    <- read_csv('Text.csv',  col_names = TRUE)
lb_pos     <- read_csv('POS.csv',   col_names = TRUE)

# + ------------------------------------------------------------------- +
# Build Terms & ID Lists ----
# + ------------------------------------------------------------------- +
i_Terms <- lb_Terms %>% 
  filter(grepl(WordConcept, Term2)) %>%
  select(bID, Strongs, Term2) %>%
  unnest_tokens(word, Term2)

# View Terms List
i_Terms %>% anti_join(stop_words) %>%  count(word, sort = TRUE) %>%
  ggplot(aes(n, factor(word, rev(levels(factor(word)))))) +
  geom_col() +
  labs(y = NULL)

# Make Terms list to subset Bible
x_Terms <- i_Terms %>% anti_join(stop_words) %>%  pull(word) %>% sort() %>% unique() 
i_Strongs <- i_Terms %>% pull(Strongs) %>% sort() %>% unique()
write.table(x_Terms, paste0(WordConcept, "/", "Terms.txt"))
# subset term list by txt file row numbers
x_Terms <- x_Terms[c(3,4,5,6,7,11,12,16)]
x_Terms <- x_Terms %>% str_c(collapse = "|")

# Determine parts of speech to use to iterate study
lb_pos$p2 # display list
# adj = 1:4   adv = 5:6   name = 16:20    noun = 21:26    verb = 43:47
x_pos <- lb_pos$p2[c(1:6,16:26,43:47)] %>% str_c(collapse = "|")

# View How Terms and POS subset The Bible
o_Terms <- lb_Terms %>% 
  filter(grepl(x_Terms, Term2)) %>% select(iBk, Term2, POS) %>%
  left_join(lb_pos) %>% filter(grepl(x_pos, p2)) %>%
  left_join(lb_Books[,c(8,11)])
o_Terms$Bk <- with(o_Terms, paste0(iBk, "_" ,Bk_short))

o_Terms %>% pull(Term2) %>% sort() %>% unique()
o_Terms %>% pull(p2) %>% sort() %>% unique()

o_Terms %>% count(Term2, sort = TRUE) %>% 
  left_join(o_Terms) %>%
  ggplot(aes(fill=Term2, y=n, x=Bk)) + 
  geom_bar(position = "stack", stat = "identity") +
  coord_flip()
    
# Subset strings Across the Bible with x_Terms & x_Pos
lb_Terms_x <- lb_Terms %>%
  filter(grepl(x_Terms, Term2)) %>% 
  left_join(lb_pos) %>% filter(grepl(x_pos, p2))
aID_x <- lb_Terms_x %>% select(aID)     #pull(aID) %>% unique() %>% str_c(collapse = "|")
bID_x <- lb_Terms_x %>% select(bID)     #pull(bID) %>% unique() %>% str_c(collapse = "|")
Str_x <- lb_Terms_x %>% select(Strongs) #pull(Strongs) %>% unique() %>% sort() %>% str_c(collapse = "|")

# + ------------------------------------------------------------------- +
# Calculate TF-IDF Per Verse ----
# + ------------------------------------------------------------------- +
tfidf_aID <- lb_Terms %>% semi_join(aID_x) %>% group_by(iBk)
tfidf_aID_n <- tfidf_aID %>% count(iBk, Term2, sort = TRUE)
tfidf_aID_N <- tfidf_aID_n %>% summarize(total = sum(n))
tfidf_aID_j <- left_join(tfidf_aID_n, tfidf_aID_N)
tfidf_aID_x <- tfidf_aID_j %>% bind_tf_idf(Term2, iBk, n) %>% select(-total) %>% arrange(desc(tf_idf))

# check the pos used
tfidf_aID_o <- unique(tfidf_aID_x %>% left_join(lb_Books[,c(8,11)]))

tfidf_aID_o %>% ggplot(aes(tf_idf)) + geom_histogram()
summary(tfidf_aID_o[,6]); nrow(tfidf_aID_o[,6])
quantile(tfidf_aID_o$tf_idf, 0.999, na.rm = TRUE)

tfidf_trm <- tfidf_aID_o %>% 
                   filter(tf_idf >= quantile(tf_idf, 0.999, na.rm = TRUE)) %>%
                   select(Term2)

# + ------------------------------------------------------------------- +
# Plot sequential relationships ----
# + ------------------------------------------------------------------- +
set.seed(2021)

# display bigrams before term
bgX <- lb_Terms_x %>% 
  select(iLit, biGramX, Term2) %>%
  left_join(lb_Books[,c(2,5)])

for (iLt in sort(unique(bgX$iLit))) {
  print(iLt)
  sLit <- bgX %>% filter(iLit == iLt) %>%
    pull(sLit) %>%unique()
  
  bgXx <- bgX %>% 
    filter(iLit == iLt) %>%
    count(biGramX, Term2, sort = TRUE) 
  
  bgXx %>% graph_from_data_frame()
  print(
    ggraph(bgXx, layout = "fr") +
      geom_edge_link() + geom_node_point() +
      geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
      labs(title = paste0(iLt, " - ", sLit))
  );Sys.sleep(5)
}

# Display bigrams after term
bgY <- lb_Terms_x %>% 
  select(iLit, Term2, biGramY) %>%
  left_join(lb_Books[,c(2,5)])
  #filter(grepl(tfidf_trm_aID, biGramY)) %>% 
  
for (iLt in sort(unique(bgY$iLit))) {
  print(iLt)
  sLit <- bgY %>% filter(iLit == iLt) %>%
    pull(sLit) %>% unique()
  
  bgYy <- bgY %>%
    filter(iLit == iLt) %>%
    count(Term2, biGramY, sort = TRUE)

  bgYy %>% graph_from_data_frame()
  print(
    ggraph(bgYy, layout = "fr") +
      geom_edge_link() + geom_node_point() +
      geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
      labs(title = paste0(iLt, " - ", sLit))
  );Sys.sleep(5)
}

# + ------------------------------------------------------------------- +
# Calculate Word Correlation ----
# + ------------------------------------------------------------------- +
wdpr_aID <- lb_Terms %>%
  semi_join(aID_x) %>%
  #filter(grepl(aID_x, aID)) %>% 
  pairwise_count(Term2, aID, sort = TRUE) %>% 
  filter(grepl(x_Terms, item1))
wdcr_aID <- lb_Terms %>% 
  semi_join(aID_x) %>%
  #filter(grepl(aID_x, aID)) %>% 
  pairwise_cor(Term2, aID, sort = TRUE) %>% 
  filter(grepl(x_Terms, item1))
  
#wdcr_aID$Pvalue <- cor.test(wdcr_aID$correlation, wdcr_aID$item2)$p.value

write.csv(wdpr_aID, "pain/wdPair_Lit.csv")
write.csv(wdcr_aID, "pain/wdCorr_Lit.csv")

wdcr_aID %>% filter(item1 == "pain")


# + ------------------------------------------------------------------- +
# Write Out Data ----
# + ------------------------------------------------------------------- +
write.table(x_Terms, "joy/Terms_x.txt")
write.csv(tfidf_aID_o,"joy/tfidf_aID.csv")