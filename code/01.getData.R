library(bib2df)
library(tidyverse)


df <- bib2df("data/anthology+abstracts.bib")

# explore vars
glimpse(df)

# filter
dfSel <- df %>% 
  select(YEAR, TITLE, ABSTRACT,  PUBLISHER, BOOKTITLE, CATEGORY) %>% 
  na.omit()

# EDA
glimpse(dfSel)
summary(dfSel)
     
## missings   
na_count <-sapply(dfSel, function(y) sum(length(which(is.na(y)))))

table(dfSel$YEAR)

## export
write.csv(dfSel, file = "data/ACL_data.csv")
