library(rvest)
library(magrittr)
library(wordcloud)
library(tm)
library(SnowballC)
library(RColorBrewer)

###################################################

Pages <- 10
###################################################

RedditWC <- function(subr,Pages){
  
website <-paste("http://www.reddit.com/r/",subr,sep="")
cp <- 1

df <- data.frame("titles"=0)
linklist <- data.frame("strsplit.as.character.links.........x...2."=0)
while(cp < Pages+1)
{
  subreddit <- read_html(website,encoding = "stri_enc_detect")
  
  links <-  subreddit %>% html_nodes(".first") %>% html_nodes("a")
 
  linklen <- length(links)
  x <-1
  
  while(x < linklen+1)
  {
    
    df1 <- data.frame(strsplit(as.character(links), " ")[[x]][2])
    linklist <-rbind(linklist,df1)
    x <- x+1

  }
  cp <- cp + 1 
  nextbuttons <- subreddit %>% html_nodes(".nextprev") %>% html_nodes("a")
  website <- substr(nextbuttons, 10,74)
} 

  linklist <- subset(linklist, is.na(linklist$strsplit.as.character.links.........x...2.)==FALSE & linklist$strsplit.as.character.links.........x...2. !=0)
  linklist["links"] <- substr(linklist$strsplit.as.character.links.........x...2., 7,99999)
  linklist$links <- substr(linklist$links, 1, nchar(linklist$links)-2)
  linklist <- unique(linklist)
  

  rm(df, df1, cp, linklen, links, nextbuttons,Pages,subreddit,website,x)
  
  
  
  ListLen <- nrow(linklist)
  x <- 1

  allComments <- data.frame("as.character.Comments..c..."=0)
while(x < ListLen+1)  
{
  CommentThread <- read_html(linklist[x,2],encoding = "stri_enc_detect")
  
  Comments <-  CommentThread %>% html_nodes(".md") %>% html_nodes("p")

    commentlength = length(Comments)
    c <- 1
    while(c < commentlength+1)
    {
      df1 <- data.frame(as.character(Comments[[c]]))
      allComments <-rbind(allComments,df1)
      c <- c+1
    }
    x <- x+1
    
}
  

  allComments["Clean"] <- gsub("<p>", "", allComments$as.character.Comments..c...)
  allComments$Clean <- gsub("</p>", "", allComments$Clean)


  allComments <- subset(allComments, !substr(allComments$Clean,1,3) %in% c("<a ","\n  ","<st","<em"))


corpus<-Corpus(VectorSource(allComments$Clean))


corpus <- tm_map(corpus, stripWhitespace)
corpus <-tm_map(corpus,content_transformer(tolower))
corpus <-tm_map(corpus,removeWords,stopwords("english"))
corpus <-tm_map(corpus,removePunctuation)




wc <- wordcloud(corpus, max.words = 250 ,min.freq=1, rot.per = 0, colors=brewer.pal(8, "Dark2"), scale=c(10,0.5))

return(wc)
} 


RedditWC("survivor",2)
