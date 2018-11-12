###############################
# ADP certificate
# 2017-10-20
# Modified by Dookyung Kim
# 제4장 비정형 데이터 마이닝
###############################

#########################
# 제1절 텍스트 마이닝   (p562)
##########################

# 텍스트 마이닝 4 기능
# 1. 문서 요약(summarization)
# 2. 문서 분류(classification)
# 3. 문서 군집(clustering)
# 4. 특성 추출(feature extraction)

# install.packages("tm")
library(tm)


# 가. 데이터수집 : 

# install.packages("twitteR")
library(twitteR)
# R에서 트위터 데이터 사용을 위한 기본 setting
# 1.트위터 계정 만들기
# 2. 접속후 app 생성하기 https://apps.twitter.com/app
# 3. consuemr key, consumer seret, access token, access token secret 입력하기.
api_key <-"DBZFPLJS8TpTfCHGSm7Fn381z"#  Consumer Key (API Key)
api_secret <-"mUcxfCMF921xEbL4MjdMFdDdEtW1EF8g0tky59nyNFfp5mYQGP"#Consumer Secret (API Secret)	
access_token <-"923760643185942528-W0mWYcRNIMY6qKY3cQVH2nUtt3mmGc8" # Access Token
access_token_secret <-"xnrMwKn7OepYs2OYTHklb1tDoCz8R4EhLEB4gKn89UJBj" # Access Token Secret
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

keyword<-'tensorflow'
bigdata.tw<-searchTwitter(keyword, since='2018-01-01', n=1000, lang='en')# 최대 n=1000개 골라잡아 온다.
bigdata.tw[1:3]
tweet <-bigdata.tw[[1]]# 1번 메시지 선택
tweet$getScreenName() # 작성자 이름
tweet$getText()       # 텍스트 내용

class(bigdata.tw) #리스트 형태의 bigdata.tw
bigdata.df<-twListToDF(bigdata.tw) # data.frame 변환
head(bigdata.df)
class(bigdata.df) #확인
bigdata.text<-bigdata.df$text
head(bigdata.text,3)

# plyr로 변환
# install.packages('plyr')
library(plyr)
bigdata.text <- laply(bigdata.tw, function(i) i$getText())
head(bigdata.text)


# 나. 데이터 전처리 및 가공

# 1) Corpus 생성 
## 데이터 정제, 통합, 선택, 변환의 과정을 거친 구조화된 단계로서 더이상 추가적인 절차 없이 데이터마이닝 알고리즘을 실험에서 활용할 수 있는 상태
# 다수파일 한번에 읽기
.libPaths()
txt<-system.file('texts', 'txt', package = 'tm')# 패키지 내의 다음과 같은 단어가 포함된  full file name을 찾기.
txt# 여기 디렉토리에서 파일 불러오기 위해 DirSource를 사용한다. 사용할 언어는 라틴어로 설정
library(tm)
ovid <- Corpus(DirSource(txt), readerControl = list(language='lat'))# tm 라이브러리 받은 상태에서 사용해야함.
ovid
ovid[[1]]
getReaders()#읽어들일 수 있는 종류의 문서 형식


# 메모리에서 읽기
# bigdata.text <- iconv(bigdata.text, 'UTF-8','ASCII')
# bigdata.text <- iconv(bigdata.text, 'UTF-8')
my.corpus<-Corpus(VectorSource(bigdata.text)) # Corpus형태로 바꾸기
my.corpus
my.corpus[[1]]
inspect(my.corpus[50:53])

# 2) tm_map()함수의 적용
#tm_map()은 corpus 형식을 가지는 데이터의 변형을 위한 함수.
getTransformations()
## [1] "removeNumbers"     "removePunctuation" "removeWords"      
## [4] "stemDocument"      "stripWhitespace" 
#위와 같은 5개의 함수를 볼수 있다. 

my.corpus<-tm_map(my.corpus, stripWhitespace)
inspect(my.corpus[1:2])
# @로 시작되는 re-twitter ID제거
my.corpus <- tm_map(my.corpus, content_transformer(gsub), pattern='@\\S*', replacement='')
inspect(my.corpus[1:2])
# http로 시작되는 URL 제거
my.corpus <- tm_map(my.corpus, content_transformer(gsub), pattern='http\\S*', replacement='')
inspect(my.corpus[1:2])
#온갖 문장부호 제거
my.corpus <- tm_map(my.corpus, removePunctuation)
inspect(my.corpus[1:2])
# 소문자로   #error
my.corpus1<-tm_map(my.corpus[101:300], tolower)
#my.corpus2<-tm_map(my.corpus, content_transformer(tolower))
#my.corpus2<-tm_map(my.corpus, PlainTextDocument) # 와 같이 작성되어야 함.
inspect(my.corpus1[1:5])

stopwords('en')
my.corpus <- tm_map(my.corpus, removeWords, stopwords('en'))
my.stopword <- c(stopwords('en'), 'rt','via','even')
my.corpus <- tm_map(my.corpus, removeWords, my.stopword)
inspect(my.corpus[1:2])


#다. 자연어 처리
#1) Stemming
#팁3 : stem Doc과 stemCompletion 제대로 알기
#install.packages('SnowballC')
# test<-stemDocument(c('updated', 'update', 'updating'))
# test
# test<-stemCompletion(test, dictionary=c('updated', 'update', 'updating'))
# test

dict.corpus <-my.corpus
my.corpus<-tm_map(my.corpus, stemDocument)
inspect(my.corpus[1:4])
# 모두 NA화 방지 함수
stemCompletion_mod <- function(x, dict){
  PlainTextDocument(stripWhitespace(paste(stemCompletion(unlist(strsplit(as.character(x), " ")),
                                                         dictionary = +dict, type='first'), sep="", collapse = "")))
}
my.corpus <-lapply(my.corpus, stemCompletion_mod, dict=dict.corpus)
my.corpus <-Corpus(VectorSource(my.corpus))# first
inspect(my.corpus[1:2])

#2) 한글처리
# install.packages('KoNLP')
library(rJava)
library(KoNLP)
extractNoun('연습을 해보고자 한다. 명사가 잘 추출되는지 보자. 빨간색으로 글씨를 쓰고 있다.')
sapply('연습을 해보고자 한다. 명사가 잘 추출되는지 보자. 빨간색으로 글씨를 쓰고 있다.', extractNoun)

#라. TDM 구축
#1) 모든 단어
my.TDM <- TermDocumentMatrix(my.corpus)
dim(my.TDM)
inspect(my.TDM[55:60, 1:3])
#2) 단어 사전
myDict<-c('bigdata', 'data', 'analyst', 'cloud', 'company', 'privacy','analytics', 'business', 'hadoop', 'datascience')
my.TDM<-TermDocumentMatrix(my.corpus, control=list(dictionary=myDict))
inspect(my.TDM[, 1:3])

#마. 분석 및 시각화
#1) Association
findAssocs(my.TDM, 'warehouse', 0.5)

transaction_m <-as(terms.m, "transactions")
rules.all <-apriori(transaction_m, parameter=list(supp=0.01, conf=0.5))
inspect(rules.all)

#2) 워드 클라우드
# install.packages('wordcloud')
library(wordcloud)
my.TDM.m <- as.matrix(my.TDM)
term.freq <-sort(rowSums(my.TDM.m), decreasing=T)
head(term.freq, 15)

wordcloud(words=names(term.freq), freq=term.freq, min.freq=15, random.order=F, colors=brewer.pal(8, 'Dark2'))

#3) 감성분석
hilton.tweets <- searchTwitter('@hilton', n=50)
length(hilton.tweets)
hilton.tweets[1:2]

pos.word=scan("../Desktop/positive-words.txt", what = 'character', comment.char = ";")
neg.word=scan("../Desktop/negative-words.txt", what = 'character', comment.char = ";")
pos.word <- c(pos.word, 'upgrade')
neg.word <- c(neg.word, 'wtf', 'wait', 'waiting', 'epicfail', 'mechanical')

score.sentiment <-function(sentences, pos.words, neg.words, .progress='none')#pos.word나 neg.word가 아니다. 매개변수다.
{
  require(plyr)
  require(stringr)
  #we got a vector of sentences. plyr will handle a list or a vector as an "l"for us
  #we want a simple array of scores back, so we use "l"+"a"+"ply"=laply:
  scores<- laply(sentences, function(sentence, pos.words, neg.words){
    #clean up sentences with R's  regx-driven global substitute, gsub();
    sentence <-gsub('[[:punct:]]', '', sentence)
    sentence <-gsub('[[:cntrl:]]', '', sentence)
    sentence <-gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence <-tolower(sentence)
    # split into words. str_split is in the stringr package
    word.list <- str_split(sentence, '\\s+')
    word <- unlist(word.list)
    # compare our words to the dictionaries of positive & negative terms
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    #match() returns the position of the matched term or NA
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    #and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score<- sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  scores.df<-data.frame(socre=scores, text= sentences)
  return(scores.df)
}

sample <- c('Iloveyou', 'Ihateyou', 'What a wonderful day!','I hate you')
# install.packages("stringr")
result <- score.sentiment(sample, pos.word, neg.word)
result$score




#########################
# 제2절 사회연결망 분석  (p590)
##########################

#2. R을 이용한 SNA의 활용.
#가. 단어 간 연관성을 이용한 사회연결망 분석
dim(my.TDM.m) #텍스트 마이닝 예제에서 생성한것. 같은 변수 환경 데이터를 공유해야 사용가능
term.freq <- sort(rowSums(my.TDM.m), decreasing = T)
my.Term <- my.TDM.m[rowSums(my.TDM.m) %in% names(term.freq[term.freq>20]), ]
my.Term[1:10, 995:1000]

my.Term[my.Term >=1] <-1
my.Term[1:10, 995:1000]

termMatrix <- my.Term %*% t(my.Term)
termMatrix[1:5, 1:5]

install.packages("igraph")
library(igraph)
g<-graph.adjacency(termMatrix, weight=T, mod='undirected')
g<-simplify(g)

V(g)$label <- V(g)$name
head(V(g)$label, 5)
V(g)$degree <- degree(g)
head(V(g)$degree, 5)
layout1 <- layout.fruchterman.reingold(g)
plot(g)

V(g)$label.cex <- 2.2 * V(g)$degree / max(V(g)$degree)+0.2
V(g)$label.color <- rgb(0, 0, 0.2, 0.8)
V(g)$frame.color <-NA
egam <- (log(E(g)$weight)+0.4) / max(log(E(g)$weight)+0.4)
E(g)$width <- egam
E(g)$color <- rgb(0.5, 0.5, 0, egam)
plot(g, layout=layout1)

#나. 트위터 검색을 이용한 사용자 간 소셜 네트워크
length(bigdata.tw)
length(bigdata.tw) <- 100
tw.names <- sapply(bigdata.tw, function(i) i$getScreanName())
head(tw.names)

tw.names <- as.data.frame(table(tw.names))
colnames(tw.names) <- c('user', 'tweets')
head(tw.names, 3)
g<-graph.empty(directed = T)
g<-add.vertices(g, nrow(tw.names), name=as.character(tw.names$user), tweets=tw.names$tweets)

V(g)$followers <- 0
for(usr in V(g)){
  tuser <- getUser(V(g)$name[usr+1])
  print(paste('Getting info on', screenName(tuser)))
  V(g)$followers[usr+1] <- followersCount(tuser)
  followers.list <- getUser(V(g)$name[usr+1])$getFollowers()
  for(tflwr in followers.list){
    if(screenName(tflwr) %in% V(g)$name)
      g<- add.edges(g, c(as.vector(V(g)[name==screenName(tflwr)]), usr))
  }  
  print('Sleeping 10 min...')
  Sys.sleep(600)
}

g$layout <- layout.fruchterman.reingold(g)
V(g)$size <- log(V(g)$follower)*1.8
V(g)$label <- V(g)$name
V(g)$label.cex <- 0.6
tcolors <-rev(heat.colors(max(V(g)$tweets)))
V(g)$color <- tcolors[V(g)$tweets]
E(g)$arrow.size <- 0.3
E(g)$curved <- FALSE
E(g)$color <- 'blue'
plot(g)

#Exercise1
library(KoNLP)
library(wordcloud)
park.text<-readLines('연설문 저장된 경로', encoding='UTF-8')
head(park.text, 3)
park<-paste(park.text, collapse = ' ')
tran <- Map(extractNoun, park)
tran <- unique(tran)
tran <- sapply(tran, function(x){
  Filter(function(y){
    nchar(y) <= 4 && nchar(y) > 1 && is.hangul(y)
  }, x)
})
head(tran, 2)

tran.table <- as.data.frame(tabl(tran))
head(tran.table, 3)
wordcloud(words=tran.table$tran,freq=tran.table$freq, min.freq=3,
          random.order=F, colors=brewer.pal(5, 'Dark2'))
#Exercise2
library(igraph)
ga.data <- read.csv('./ga_edgelist.csv', header=T)
g <- graph.data.frame(ga.data, directed = F)
g$layout <- layout.fruchterman.reingold(g)
plot(g)

V(g)$label <- NA
V(g)$size <- degree(g)*2
plot(g)

clo <-closeness(g)
clo.score <- round( (clo - min(clo))*length(clo)/max(clo) )+1
clo.colors <-rev(heat.colors(max(clo.score)))
V(g)$color <- clo.colors[clo.score]
plot(g)

btw <- betweenness(g)
btw.score <- round(btw)+1
btw.colors <- rev(heat.colors(max(btw.score)))
V(g)$color <- clo.colors[btw.score]
plot(g)
install.packages('Rfacebook')
library(Rfacebook)
install.packages('Rook')
library(Rook)
fb_oauth <- fb0Auth(app_id="___________", app_secret="___________")

save(fb_oauth, file='fb_oauth')
load('fb_oauth')
me <- getUsers('me', token=fb_oauth)
me

my_friends <- getFriends(token=fb_oauth)
head(my_friends, n=5)

my_friends_info <- getUsers(my_friends$id, token=fb_oauth, private_info = TRUE)
colname(my_friends_info)
table(my_friends_info$relationship_status)

g.data <- getNetwork(token = fb_oauth, format= 'edgelist', verbose = T)
library(igraph)
g.data <- as.data.frame(g.data)
g<- graph.data.frame(g.data)
g$layout <- layout.fruchterman.reingold(g)
plot(g)