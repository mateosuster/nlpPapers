library(bib2df)
library(tidyverse)

path <- system.file("extdata", "data/anthology+abstracts.bib", package = "bib2df")

df <- bib2df("data/anthology+abstracts.bib")
df <- bib2df(path)

# explore vars
glimpse(df)

# filter
dfSel <- df %>% 
  select(YEAR, TITLE, ABSTRACT) %>% 
  na.omit()

# EDA
summary(dfSel)
     
## missings   
na_count <-sapply(dfSel, function(y) sum(length(which(is.na(y)))))

table(dfSel$YEAR)

## export

