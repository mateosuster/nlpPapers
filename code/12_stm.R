#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

library(stm)
library(tm)


semilla = 420
set.seed(semilla)

data <- read.csv("data/data_gender_abstract_year.csv")
table(data$gender_final)

#proprocesamiento
# data$full_name =paste(data$name, data$last_name)

#checks
names(data)
head(data)
sapply(data, function(x) sum(is.na(x))) # Check for NAs

sel_col = c('YEAR'  ,   'ABSTRACT', 'gender_final'  )

#sampling
paper_sample = nrow(data[data$YEAR == 2016,] )
data_sample = data[data$YEAR < 2016, sel_col]

unique(data_sample$gender_final)

for (i in seq(2016, 2022)){
  data_i = data[data$YEAR == i, sel_col]
  data_i = data_i[sample(nrow(data_i)  , paper_sample),]
  data_sample = rbind(data_sample, data_i)
  
}

data_sample =data_sample[sample(nrow(data_sample)  , 1000),]


# Double-check format
sapply(data_sample, typeof)
data_sample[1,]

# PROCESS
## SAMPLE
#### * default parameters
processed <- textProcessor(data_sample$ABSTRACT, metadata = data_sample,
                           lowercase = TRUE, #*
                           removestopwords = TRUE, #*
                           removenumbers = TRUE, #*
                           removepunctuation = TRUE, #*
                           stem = FALSE, #*
                           wordLengths = c(3,Inf), #*
                           sparselevel = 1, #*
                           language = "en", #*
                           verbose = TRUE, #*
                           onlycharacter = FALSE, # not def
                           striphtml = FALSE, #*
                           customstopwords = NULL, #*
                           v1 = FALSE) #*

out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

## ALL DATA
processed_all <- textProcessor(data$ABSTRACT, metadata = data,
                           lowercase = TRUE, #*
                           removestopwords = TRUE, #*
                           removenumbers = TRUE, #*
                           removepunctuation = TRUE, #*
                           stem = FALSE, #*
                           wordLengths = c(3,Inf), #*
                           sparselevel = 1, #*
                           language = "en", #*
                           verbose = TRUE, #*
                           onlycharacter = FALSE, # not def
                           striphtml = FALSE, #*
                           customstopwords = NULL, #*
                           v1 = FALSE) #*

out_all <- prepDocuments(processed_all$documents, processed_all$vocab, processed_all$meta)
docs_all <- out_all$documents
vocab_all <- out_all$vocab
meta_all <- out_all$meta

plotRemoved(processed$documents, lower.thresh = seq(1, 200, by = 100))
# out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 15)

#experiments
model_prev_lda <- stm(documents = out$documents, vocab = out$vocab, 
                      K = 32,
                       prevalence = ~gender_final + s(YEAR) , 
                       # content = ~gender_final + s(YEAR) , 
                       # content = ~full_name,
                       max.em.its = 5, #75 
                       data = out$meta, init.type = "LDA",
                       seed = semilla)

start_time <- Sys.time()
model_prev_spectral <- stm(documents = out$documents, vocab = out$vocab,
                      K = 32,
                      # prevalence = ~gender_final ,
                      # prevalence = ~gender_final + s(YEAR) ,
                      # content = ~gender_final + s(YEAR) ,
                      content = ~gender_final,
                      max.em.its = 5, #75
                      data = out$meta, init.type = "Spectral",
                      seed = semilla)
end_time <- Sys.time()
time_execution_2 <- end_time - start_time

STM_2 <- stm(out$documents, out$vocab, K = 20,
             prevalence = ~gender_final + s(YEAR) ,
             content =~  ~gender_final,
             max.em.its = 75, data = out$meta,
             init.type = "Spectral")

# model selection
model_sel = model_prev_spectral

# plots
plot(model_sel)
plot(model_sel, type = "perspectives", topics = c(1,2))
plot(model_sel, type = "summary", xlim = c(0, 0.3))


plot(model_sel, type = "perspectives", topics = 10, xlim = c(0, 1)) ## Topical content


# label topics
labelTopics(model_sel, c(6, 13, 18))
labelTopics(model_sel)


#explore
findThoughts(model_sel, texts = meta$ABSTRACT,n = 3, topics = 1:15)

thoughts6 <- findThoughts(model_sel, meta$ABSTRACT, n = 2, topics = 6)$docs[[1]]
thoughts18 <- findThoughts(model_sel, meta$ABSTRACT, n = 2, topics = 18)$docs[[1]]
par(mfrow = c(1, 2), mar = c(0.5, 0.5, 1, 0.5))
plotQuote(thoughts6, width = 30, main = "Topic 6")
plotQuote(thoughts18, width = 30, main = "Topic 18")


# understand
## Estimating metadata/topic relationships
levels(meta$gender_final)
out$meta$gender_final <- as.factor(out$meta$gender_final)
prep <- estimateEffect(1:20 ~ gender_final+ s(YEAR) , model_sel,
                       metadata = out$meta, 
                       documents = docs_all,
                       uncertainty = "Global")
prep <- estimateEffect( ~ gender_final+ s(YEAR) , model_sel,
                       metadata = out$meta, uncertainty = "Global")
summary(prep, topics = 1)

# ### Metadata/topic relationship visualization
plot(prep, covariate = "gender_final", topics = c(6, 13, 18),
     model = model_sel, method = "difference",
     cov.value1 = "male", cov.value2 = "female",
     # cov.value1 = "Liberal", cov.value2 = "Conservative",
     # xlab = "More Conservative ... More Liberal",
     # main = "Effect of Liberal vs. Conservative",
     xlim = c(-0.1, 0.1)
     # , labeltype = "custom",
     #  custom.labels = c("Obama/McCain", "Sarah Palin", "Bush Presidency")
     )


plot(prep, "YEAR",
     method = "continuous", topics = 13, 
     model = model_sel, 
     printlegend = FALSE, xaxt = "n", 
     # xlab = "Time (2008)"
     )

mod.out.corr <- topicCorr(model_sel)
plot(mod.out.corr)

