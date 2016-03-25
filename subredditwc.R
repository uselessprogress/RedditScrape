library(rvest)
library(magrittr)
library(wordcloud)
library(tm)
library(SnowballC)
library(RColorBrewer)

###################################################
website <-"http://www.reddit.com/r/bigbrother"
Pages <- 10
###################################################


cp <- 1

df <- data.frame("titles"=0)

while(cp < Pages+1)
{
subreddit <- read_html(website,encoding = "stri_enc_detect")

titles <- subreddit %>% html_nodes(".title") %>% html_text()
df1 <- data.frame(titles)
df <-rbind(df,df1) 
nextbuttons <- subreddit %>% html_nodes(".nextprev") %>% html_nodes("a")
website <- substr(nextbuttons, 10,74)
cp <- cp + 1

}

cleaned <- data.frame(df[4:nrow(df),1])
Nth.delete<-function(dataframe, n)dataframe[-(seq(n,to=nrow(dataframe),by=n)),]
cleaned <- data.frame(Nth.delete(cleaned,2))
cleaned <- subset(cleaned, !cleaned$Nth.delete.cleaned..2. %in% c("about","apps & tools","my subreddits"))


corpus<-Corpus(VectorSource(cleaned$Nth.delete.cleaned..2.))


corpus <- tm_map(corpus, stripWhitespace)
corpus <-tm_map(corpus,content_transformer(tolower))
corpus <-tm_map(corpus,removeWords,stopwords("english"))
corpus <-tm_map(corpus,removePunctuation)




wordcloud(corpus, max.words = 100 ,min.freq=1, rot.per = 0, colors=brewer.pal(8, "Dark2"), scale=c(10,0.5))
