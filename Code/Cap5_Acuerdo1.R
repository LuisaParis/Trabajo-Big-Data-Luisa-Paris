# Trabajo Final  Big Data
# Por: Luisa Paris y Sergio Perilla
# Tema: Análisis de texto sobre Acuerdos de paz y 
#       opinión política en el tiempo.
# Sub: Acuerdo 1 y 2 - Por capitulos

library("tm")
library("wordcloud")
library("SnowballC")
setwd("C:/Users/sergi/Dropbox/Universidad/Semestre 11M/Big Data/Trabajo Final")

# Cargar mis docs
filenames <- list.files(getwd(), pattern = "Capitulo 5, Acuerdo 1.txt") 
# Aplicar la función readLines a mis docs, para que sean un vector de chr 
files <- lapply(filenames, readLines) 
# Hacer un Corpus con los vectores
docs <-Corpus(VectorSource(files))

writeLines(as.character(docs[[1]]))

# Arreglar el Corpus
docs <- tm_map(docs, content_transformer(tolower))
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
# Limpiar chr especiales y convertirla en espacio
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, "’")
docs <- tm_map(docs, toSpace, "‘")
docs <- tm_map(docs, toSpace, "•")
docs <- tm_map(docs, toSpace, "”")
docs <- tm_map(docs, toSpace, "“")
docs <- tm_map(docs, removePunctuation) # Remover Puntuación
docs <- tm_map(docs, removeNumbers) # Remover Números
docs <- tm_map(docs, removeWords, stopwords("Spanish")) # Remover Stopwords
docs <- tm_map(docs, stripWhitespace) # Remover Espacios en blanco


docs <- tm_map(docs,stemDocument) #Stemming en los docs

writeLines(as.character(docs[[1]]))


#define and eliminate all custom stopwords
myStopwords <- c("página", "pagina", "acuerdo", "final", "si", 
                 "sí",  "artículo", "título", "capìtulo",
                 "capítulo", "titulo", "parágrafo", "dicho", "demá", 
                 "así", "acción", "i", "ii", "iii", "iv", "v", "vi",
                 "especi", "gobierno", "nacional", "firmado", "manera", 
                 "derecho", "est", "sobr", "punto", "dond", "ant", 
                 "part", "tal", "ptn", "toda", "entr", "difer", "paí", 
                 "ser", "local", "forma", "cada", "protocolo", "zona",
                 "cualquier","component","integrant", "siguient", "debera",
                 "sala", "necesario", "plane", "acceso", "bien", "buen", "enfoqu", "sosten")

docs <- tm_map(docs, removeWords, myStopwords) # Remover mis StopWords

dtm <- DocumentTermMatrix(docs) #Crear document-term matrix

rownames(dtm) <- filenames #convert rownames to filenames

freq <- colSums(as.matrix(dtm)) #collapse matrix by summing over columns

length(freq) #length should be total number of terms

ord <- order(freq,decreasing=TRUE) #create sort order (descending)

freq[ord] #List all terms in decreasing order of freq and write to disk
write.csv(freq[ord],"word_freq_Capitulo5Acuerdo1.csv")

word_freq_acuerdos <- read.csv("word_freq_Capitulo5Acuerdo1.csv", header=TRUE,stringsAsFactors = T)
wordcloud(word_freq_acuerdos$X, freq=word_freq_acuerdos$x, max.words=90,random.order = FALSE, scale=c(3, 0.5), random.color = T)

library("topicmodels")
# Parametros
burnin <- 4000 
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

k <- 6 # Topics

# LDA por Gibbs Sample
ldaOut <-LDA(dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, 
                                                 best=best, burnin = burnin, 
                                                 iter = iter, thin=thin))

# Los resultados
ldaOut.topics <- as.matrix(topics(ldaOut))
write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopicsCap5Ac1.csv"))

#top 6 terms in each topic
ldaOut.terms <- as.matrix(terms(ldaOut,6))
write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"TopicsToTermsCap5Ac1.csv"))

#probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)
write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilitiesCap5Ac1.csv"))

#Find relative importance of top 2 topics
topic1ToTopic2 <- lapply(1:nrow(dtm),function(x)
  sort(topicProbabilities[x,])[k]/sort(topicProbabilities[x,])[k-1])

#Find relative importance of second and third most important topics
topic2ToTopic3 <- lapply(1:nrow(dtm),function(x)
  sort(topicProbabilities[x,])[k-1]/sort(topicProbabilities[x,])[k-2])

#write to file
write.csv(topic1ToTopic2,file=paste("LDAGibbs",k,"Topic1ToTopic2Cap5Ac1.csv"))
write.csv(topic2ToTopic3,file=paste("LDAGibbs",k,"Topic2ToTopic3Cap5Ac1.csv"))

library("tidytext")

ap_topics <- tidy(ldaOut, matrix = "beta")
library("ggplot2")
library("dplyr")
ap_top_terms<- ap_topics %>% 
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

library(tidyr)

beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic2 > .001 | topic3 > .001) %>%
  mutate(log_ratio = log2(topic3 / topic2))

x<-as.data.frame(beta_spread)
x<-x[order(-x$log_ratio),]
y<-rbind(x[1:10,],x[399:409,])

ggplot(y, aes(y=log_ratio, x=reorder(term, -log_ratio)))+geom_bar(stat='identity')+coord_flip()
