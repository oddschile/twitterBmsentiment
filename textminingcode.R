#Librerias necesarias para realizar la extracción de los datos de twitter e iniciar el análisis

library(twitteR)
library(devtools)
if(!require(Rstem)) install_url("http://cran.r-project.org/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz")
if(!require(sentiment)) install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")
library(Rstem)
library(sentiment)
library(plotly)
library(dplyr)
library(wordcloud)

#crear las llaves y token desde la API de twitter (ingresar con la cuenta individual de twitter para crear la aplicación)

conkey <- "xxxxxxxxxxxxx"
consec <- "xxxxxxxxxxxx"
acctoken <- "xxxxxxxxxxxxx"
accsec <- "xxxxxxxxxxxxxx"

setup_twitter_oauth(conkey, consec, acctoken, accsec)

#realizar la búsqueda de tweets sobre la situación de Chile y el Banco Mundial (en inglés)

bmchile <- searchTwitter('Chile+World Bank', lang='en', n=1000)

#limpiar los datos de la búsqueda

f_clean_tweets <- function (tweets) {
  
  clean_tweets = sapply(tweets, function(x) x$getText())
  # remove retweet entities
  clean_tweets = gsub('(RT|via)((?:\\b\\W*@\\w+)+)', '', clean_tweets)
  # remove at people
  clean_tweets = gsub('@\\w+', '', clean_tweets)
  # remove punctuation
  clean_tweets = gsub('[[:punct:]]', '', clean_tweets)
  # remove numbers
  clean_tweets = gsub('[[:digit:]]', '', clean_tweets)
  # remove html links
  clean_tweets = gsub('http\\w+', '', clean_tweets)
  # remove unnecessary spaces
  clean_tweets = gsub('[ \t]{2,}', '', clean_tweets)
  clean_tweets = gsub('^\\s+|\\s+$', '', clean_tweets)
  
  
  # remover emojis o caracteres especiales
  
  clean_tweets = gsub('<.*>', '', enc2native(clean_tweets))
  
  clean_tweets = tolower(clean_tweets)
  
  clean_tweets
}

clean_tweets <- f_clean_tweets(bmchile)

clean_tweets <- clean_tweets[!duplicated(clean_tweets)]

# using sentiment package to classify emotions
emotions <- classify_emotion(clean_tweets, algorithm='bayes')

# using sentiment package to classify polarities
polarities = classify_polarity(clean_tweets, algorithm='bayes')

#crear una matriz con los datos para observar las emociones de cada tweet

df = data.frame(text=clean_tweets, emotion=emotions[,'BEST_FIT'],
                polarity=polarities[,'BEST_FIT'], stringsAsFactors=FALSE)
df[is.na(df)] <- "N.A.

#crear un grafico que muestra las emociones

plot_ly(df, x=~emotion,type="histogram",
        marker = list(color = c('grey', 'red',
'orange', 'navy',
'yellow'))) %>%
layout(yaxis = list(title='Count'), title="Sentiment Analysis: Emotions")

#crear un grafico para la polaridad (rango de valoración entre palabras negativas y positivas)

plot_ly(df, x=~polarity, type="histogram",
        marker = list(color = c('magenta', 'gold',
'lightblue'))) %>%
layout(yaxis = list(title='Count'), title="Sentiment Analysis: Polarity")


df <- df %>%
  group_by(polarity) %>%
summarise(pasted=paste(text, collapse=" "))


df$pasted = removeWords(df$pasted, stopwords('english'))


corpus = Corpus(VectorSource(df$pasted))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = df$polarity

#crear una nube de palabras de comparación entre terminos negativos, positivos y neutros

comparison.cloud(tdm, colors = brewer.pal(3, 'Dark2'),
scale = c(3,.5), random.order = FALSE, title.size = 1.5)



