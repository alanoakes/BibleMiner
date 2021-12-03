# + ------------------------------------------------------------------- +
# Word Input ----
# + ------------------------------------------------------------------- +
WordConcept <- "joy"

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
data("stop_words")

# + ------------------------------------------------------------------- +
# Read Data ----
# + ------------------------------------------------------------------- +
setwd("D:/BibleMiner/2021-BibleStudies/2021-10_BibleMiner")
lb_Books   <- read_csv('Books.csv', col_names = TRUE)
lb_Terms   <- read_csv('Terms.csv', col_names = TRUE)
lb_Text    <- read_csv('Text.csv', col_names = TRUE)
lb_pos     <- read_csv('POS.csv', col_names = TRUE)

# + ------------------------------------------------------------------- +
# Build Terms & ID Lists ----
# + ------------------------------------------------------------------- +
# Determine Terms List By search word
i_Terms <- lb_Terms %>% 
  filter(grepl(WordConcept, Term2)) %>%
  select(bID, Term2) %>%
  unnest_tokens(word, Term2)

# View Terms List
i_Terms %>% anti_join(stop_words) %>%  count(word, sort = TRUE) %>%
  ggplot(aes(n, factor(word, rev(levels(factor(word)))))) +
  geom_col() +
  labs(y = NULL)

# Make Terms list to subset Bible
x_Terms <- i_Terms %>% anti_join(stop_words) %>%  pull(word) %>% sort() %>% unique() 
write.table(x_Terms, "joy/Terms.txt")
# subset term list by txt file row numbers
x_Terms <- x_Terms[c(3, 4, 7, 8, 9, 10, 11, 12, 13, 16)]
write.table(x_Terms, "joy/Terms_x.txt")
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
aID_x <- lb_Terms_x %>% pull(aID) %>% unique() %>% str_c(collapse = "|")
bID_x <- lb_Terms_x %>% pull(bID) %>% unique() %>% str_c(collapse = "|")
Str_x <- lb_Terms_x %>% pull(Strongs) %>% unique() %>% sort() %>% str_c(collapse = "|")

# + ------------------------------------------------------------------- +
# Calculate TF-IDF ----
# + ------------------------------------------------------------------- +
tfidf_aID <- lb_Terms %>% filter(grepl(aID_x, aID)) %>% group_by(iBk)
#tfidf_Lit <- lb_Terms %>% filter(grepl(aID_x, aID)) %>% select(iLit, Term2) %>% group_by(iLit)
#tfidf_Chr <- lb_Terms %>% filter(grepl(aID_x, aID)) %>% select(iChr, Term2) %>% group_by(iChr)

# find tf-idf
tfidf_aID_n <- tfidf_aID %>% count(iBk, Term2, sort = TRUE)
tfidf_aID_N <- tfidf_aID_n %>% summarize(total = sum(n))
tfidf_aID_j <- left_join(tfidf_aID_n, tfidf_aID_N)
tfidf_aID_x <- tfidf_aID_j %>% bind_tf_idf(Term2, iBk, n) %>% select(-total) %>% arrange(desc(tf_idf))

## Literary Context
#tfidf_Lit_n <- tfidf_Lit %>% count(iLit, Term2, sort = TRUE)
#tfidf_Lit_N <- tfidf_Lit_n %>% summarize(total = sum(n))
#tfidf_Lit_j <- left_join(tfidf_Lit_n, tfidf_Lit_N)
#tfidf_Lit_x <- tfidf_Lit_j %>% bind_tf_idf(Term2, iLit, n) %>% select(-total) %>% arrange(desc(tf_idf))
#
## Chronological Context
#tfidf_Chr_n <- tfidf_Chr %>% count(iChr, Term2, sort = TRUE)
#tfidf_Chr_N <- tfidf_Chr_n %>% summarize(total = sum(n))
#tfidf_Chr_j <- left_join(tfidf_Chr_n, tfidf_Chr_N)
#tfidf_Chr_x <- tfidf_Chr_j %>% bind_tf_idf(Term2, iChr, n) %>% select(-total) %>% arrange(desc(tf_idf))

# check the pos used
tfidf_aID_o <- unique(tfidf_aID_x %>% left_join(lb_Books[,c(8,11)]))
#tfidf_Lit_o <- unique(tfidf_Lit_x %>% left_join(lb_Books[,c(2,5)]))
#tfidf_Chr_o <- unique(tfidf_Chr_x %>% left_join(lb_Books[,c(3,7)]))
write.csv(tfidf_aID_o,"joy/tfidf_aID.csv")
#write.csv(tfidf_Lit_o,"joy/tfidf_Lit.csv")
#write.csv(tfidf_Chr_o,"joy/tfidf_Chr.csv")

## View tfidf quartiles
#tfidf_qts <- rbind(
#  #tfidf_aID_o[,c(2,6)],
#  tfidf_Lit_o[,c(2,6)],
#  tfidf_Chr_o[,c(2,6)]
#)
#tfidf_qts$Context <- c(
#  #rep("aID", nrow(tfidf_aID_o[,6])), 
#  rep("Literary", nrow(tfidf_Lit_o[,6])), 
#  rep("Chronological", nrow(tfidf_Chr_o[,6]))
#)
#ggplot(tfidf_qts, aes(Context, tf_idf)) + geom_boxplot()
#ggplot(tfidf_qts, aes(x=tf_idf, group=Context, fill=Context)) + 
#  geom_density(adjust=1.5, alpha=0.4)
#
#summary(tfidf_Lit_o[,6]); nrow(tfidf_Lit_o[,6])
#summary(tfidf_Chr_o[,6]); nrow(tfidf_Chr_o[,6])

tfidf_aID_o %>% ggplot(aes(tf_idf)) + geom_histogram()
summary(tfidf_aID_o[,6]); nrow(tfidf_aID_o[,6])
quantile(tfidf_aID_o$tf_idf, 0.999, na.rm = TRUE)

tfidf_trm_aID <- tfidf_aID_o %>% 
                   filter(tf_idf >= quantile(tf_idf, 0.999, na.rm = TRUE)) %>%
                   pull(Term2) %>% unique() %>% sort() %>% 
                   str_c(collapse = "|")

#tfidf_trm_aID <- tfidf_aID_o %>% drop_na(tf_idf) %>% filter(tf_idf > mean(tf_idf, na.rm = TRUE)) %>% 
# pull(Term2) %>% unique() %>% sort() %>% str_c(collapse = "|")
#tfidf_trm_Lit <- tfidf_Lit_o %>% filter(tf_idf > mean(tf_idf, na.rm = TRUE)) %>% 
#  pull(Term2) %>% unique() %>% sort()
#tfidf_trm_Chr <- tfidf_Chr_o %>% filter(tf_idf > mean(tf_idf, na.rm = TRUE)) %>% 
#  pull(Term2) %>% unique() %>% sort()

# + ------------------------------------------------------------------- +
# Plot sequential relationships ----
# + ------------------------------------------------------------------- +
library(igraph); library(ggraph); library(widyr); library(tidygraph)
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
wdpr_aID <- lb_Terms %>% filter(grepl(aID_x, aID)) %>% 
  pairwise_count(Term2, iLit, sort = TRUE) %>% 
  filter(grepl(x_Terms, item1))
wdcr_aID <- lb_Terms %>% 
  filter(grepl(aID_x, aID)) %>% 
  pairwise_cor(Term2, iLit, sort = TRUE) %>% 
  filter(grepl(x_Terms, item1))
write.csv(wdpr_aID, "joy/wdPair_Lit.csv")
write.csv(wdcr_aID, "joy/wdCorr_Lit.csv")

wdcr_aID %>% filter(item1 == "joy")
