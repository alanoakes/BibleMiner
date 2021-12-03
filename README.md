# BibleMiner

Fri 03 Dec 2021 02:20:53

This is a text mining software made on top of "tidytext" in R and custom
VBScript to suppliment the following Biblical methods:

1. Word/Concept Studies
2. Passage/Content Studies

This software is currently under development. For questions or concerns, email
me at alan.p.oakes@gmail.com. The text mining process is as follows:

## Word Study Process

1. word input
2. read data
3. build lexicon
4. subset bible by strongs numbers
5. unnest scriptures by all strongs numbers
6. make sankey of strongs inflection counts
7. make bar chart of strongs number locations per c(chapter,section,testament)
8. choose correct inflections and lemma list
9. subset by correct inflections from strongs numbers
10. count all distinct parts of speech
11. make sankey of parts of speech throughout scripture
12. make distribution plot of all words counts and boxplot per part of speech
13. remove stop words and inflated words
14. make relevant book, chapter and verse subsets for content analysis

## Content Study Process

1. take word study subset of c(testament, section, book, chapter, verse, text)
2. unnest by tidytext and calculate tf-idf by token per section
3. Calculate weighted stddev and mean to standardize important translations
4. calculate sentimnent
5. calculate bi-gram tf-idf
6. calculate network analysis
7. calculate topic model
