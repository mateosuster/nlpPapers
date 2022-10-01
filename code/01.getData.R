library(bib2df)
library(tidyverse)


df <- bib2df("data/anthology+abstracts.bib")

# explore vars
glimpse(df)

# filter
# subset <- dfSel[ , c("TITLE", "ABSTRACT")]

dfSel <- df %>% 
  select(YEAR, TITLE, ABSTRACT, AUTHOR, PUBLISHER, BOOKTITLE, CATEGORY)  %>% 
  filter(!is.na( TITLE ) & !is.na( ABSTRACT ) )

# EDA
glimpse(dfSel)
summary(dfSel)
     
## missings   
na_count <-sapply(dfSel, function(y) sum(length(which(is.na(y)))))

table(dfSel$YEAR)

## export
dfSel_str = data.frame(lapply(dfSel, as.character), stringsAsFactors=FALSE)
write.csv(dfSel_str, file = "data/ACL_data.csv", row.names = F)
