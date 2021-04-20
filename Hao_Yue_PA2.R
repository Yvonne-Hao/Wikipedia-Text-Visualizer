#PA2
#Yue Hao
setwd("/Users/Yvonne.Hao/Desktop/others/JHU/Summer Session/Data Visulization/PA2")


install.packages('rsconnect')

rsconnect::setAccountInfo(name='yuehao',
                          token='F81607EC161197BEAA3DCB2195481123',
                          secret='RZaZ7aCZ05OFD7UBpkSBN+JrIGRD26uqnfWKY1Q0')
library(rsconnect)
rsconnect::deployApp('/Users/Yvonne.Hao/Desktop/Data Visulization/PA2')

# a) Required libraries:
library(tidyverse) #For reading the file from a txt file and for the %>% operator
library(tm) #For text analysis
library(RColorBrewer)
library(wordcloud) #For the wordcloud visualization. You can also use wordcloud2 package
library(shiny)
library(SnowballC)
library(xml2)
library(rvest)
library(syuzhet)


# b) Web-scraping the content of the Data Visualization Wikipedia page

URL <- "https://en.wikipedia.org/wiki/data_visualization"
texts <- URL %>% 
        read_html %>% 
        html_nodes("[id=bodyContent]") %>%
        html_text()

# Find the frequency of each word and store it on dataframe d
v <- texts %>% 
  VectorSource %>% 
  Corpus %>% 
  # Remove punctuations
  tm_map(removePunctuation) %>% 
  # Eliminate extra white spaces
  tm_map(stripWhitespace) %>%
  # Text stemming
  tm_map(stemDocument) %>%
  # Convert the text to lower case
  tm_map(content_transformer(tolower)) %>%
  # Remove numbers
  tm_map(removeNumbers) %>%
  # Remove english common stopwords
  tm_map(removeWords, stopwords("english")) %>%
  TermDocumentMatrix %>%
  as.matrix %>%
  rowSums %>%
  sort(decreasing=TRUE) 



###Visualize dataframe d with wordcloud package
num = 30
minFreq = 3
pal = brewer.pal(8, "Set1")

set.seed(5)
wordcloud(words = d$word, 
          freq = d$freq, 
          max.words=num,
          random.order=FALSE, 
          min.freq = minFreq, 
          colors=pal, 
          scale = c(1, 0.5))

wikiWebScraper <- function(key, num, minFreq, pal, method){
  key <- gsub(" ", "_", key)
  URL = paste0("https://en.wikipedia.org/wiki/", key)
  texts <- URL %>% 
    read_html %>% 
    html_nodes("div[id=bodyContent]") %>%
    html_text()
  
  v <- texts %>% 
    VectorSource %>% 
    Corpus %>% 
    # Remove punctuations
    tm_map(removePunctuation) %>% 
    # Eliminate extra white spaces
    tm_map(stripWhitespace) %>%
    # Text stemming
    tm_map(stemDocument) %>%
    # Convert the text to lower case
    tm_map(content_transformer(tolower)) %>%
    # Remove numbers
    tm_map(removeNumbers) %>%
    TermDocumentMatrix %>%
    as.matrix %>%
    rowSums %>%
    sort(decreasing=TRUE) 
  
  if(method=="none") {d <- data.frame(word = names(v),freq=v) }
  else if(method=="sqrt") { d <- data.frame(word = names(v),freq=round(sqrt(v))) }
  else if(method=="log") { d <- data.frame(word = names(v),freq=round(log(v))) }
  else if(method=="log2") { d <- data.frame(word = names(v),freq=round(log2(v))) }
  
  set.seed(5)
  wordcloud(words = d$word, freq = d$freq, max.words=num,
            random.order=FALSE, min.freq = minFreq, colors=brewer.pal(8, pal))
}

# d) Create a wikiImgScraper function to scrape all the images on a given wiki page.
wikiImgScraper <- function(key) {  
  key <- gsub(" ", "_", key)
  URL = paste0("https://en.wikipedia.org/wiki/", key)
  imgList <- URL %>% 
    read_html %>% 
    html_nodes("div[id=bodyContent]") %>%
    html_nodes("img") %>%
    html_attr("src")
  return(imgList)
}
