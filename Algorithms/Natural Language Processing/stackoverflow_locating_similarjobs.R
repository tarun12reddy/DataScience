library(plyr)
library(textreuse)
library(RCurl)
library(XML)

minhash <- minhash_generator(n = 240, seed = 1)
dir <- "C:/Users/Talet/Documents/SourceTree_DataScience/Algorithms/Natural Language Processing/stackoverflow_jobs"
corpus <- TextReuseCorpus(dir = dir, tokenizer = tokenize_ngrams, n = 3,
                          minhash_func = minhash, keep_tokens = TRUE,
                          progress = FALSE)

#lsh_probability(h = 240, b = 4, s = 0.99)
#lsh_threshold(n = 240, b = 60)
buckets <- lsh(corpus, bands = 4, progress = FALSE)
candidates <- lsh_candidates(buckets)
candidates_compare <- lsh_compare(candidates, corpus, jaccard_similarity, progress = FALSE)

write.table(candidates_compare, "similar_stackoverflow_jobs.csv", col.names = F, row.names = F, quote = FALSE)