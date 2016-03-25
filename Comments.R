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
linklist <- data.frame("strsplit.as.character.links.........x...5."=0)
while(cp < Pages+1)
{
  subreddit <- read_html(website,encoding = "stri_enc_detect")
  
  links <-  subreddit %>% html_nodes(".title") %>% html_nodes("a")
 
  linklen <- length(links)
  x <-1
  
  while(x < linklen+1)
  {
    df1 <- data.frame(strsplit(as.character(links), " ")[[x]][5])
    linklist <-rbind(linklist,df1)
    x <- x+1

  }
  cp <- cp + 1 
  nextbuttons <- subreddit %>% html_nodes(".nextprev") %>% html_nodes("a")
  website <- substr(nextbuttons, 10,74)
} 

  linklist <- subset(linklist, is.na(linklist$strsplit.as.character.links.........x...5.)==FALSE & linklist$strsplit.as.character.links.........x...5. !=0)
  linklist["links"] <- substr(linklist$strsplit.as.character.links.........x...5., 7,99999)
  linklist$links <- substr(linklist$links, 1, nchar(linklist$links)-2)
  linklist["iscomment"] <- ifelse(substr(linklist$links,1,2)== "/r","YES","NO")
  linklist <- subset(linklist, linklist$iscomment =="YES")
linklist$links <- paste("http://www.reddit.com",linklist$links,sep="")
  linklist <- unique(linklist)
  

  rm(df, df1, cp, linklen, links, nextbuttons,Pages,subreddit,website,x)
  
  
  
  ListLen <- nrow(linklist)
  x <- 1
while(x < ListLen+1)  
{
  CommentThread <- read_html(linklist[x,2],encoding = "stri_enc_detect")
  
  Comments <-  CommentThread %>% html_nodes(".md") %>% html_nodes("p")
  Comments
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
