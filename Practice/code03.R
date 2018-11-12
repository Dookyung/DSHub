###############################
# ADP certificate
# 2017-10-20
# Modified by Dookyung Kim
# 제3장 정형 데이터 마이닝
###############################

#########################
# 제2절 분류 분석   (p454)
##########################

### 1.로지스틱 회귀모형

# 데이터 정리
data(iris)
a <- subset(iris, Species=='setosa'|Species=='versicolor')
a$Species <- factor(a$Species)
str(a)

# 로지스텍 회귀모형 추정
b <- glm(Species~Sepal.Length, data=a, family=binomial); summary(b)  # 이항로지스틱 모형
coef(b)      # exp(beta)
exp(coef(b)["Sepal.Length"])  # Sepal.Length 1단위 증가시 versicolor(Y=2)일 오즈가 170배 증가

confint(b, parm="Sepal.Length")  # exp(beta) 신뢰구간  
exp(confint(b, parm="Sepal.Length"))
fitted(b)[c(1:5, 96:100)]    # y=2일 확률(적합결과)
predict(b, newdata=a[c(1,50,51,100),], type='response')  # 예측
cdplot(Species~Sepal.Length, data=a)   # Sepal.Length 변화에 따른 범주형변수의 조건부 분포

#로지스틱 그래프
plot(a$Sepal.Length, a$Species, xlab="Sepal.Length")
x = seq(min(a$Sepal.Length), max(a$Sepal.Length), 0.1)
lines(x, 1+(1/(1+(1/exp(-27.831 + 5.140*x)))), type='l', col="red")

# 예제  (p459)
attach(mtcars)
str(mtcars)
glm.vs <- glm(vs~mpg+am, data=,mtcars, family=binomial); summary(glm.vs)
step.vs <- step(glm.vs, direction="backward")
summary(step.vs)

ls(glm.vs)      # 분석결과 확인
str(glm.vs)

anova(glm.vs, test="Chisq") # 각 변수 추가시 단계별 이탈도 감소량 & 유의성 검증결과 

1-pchisq(18.327, 1)    #p값
1-pchisq(4.887, 1)



### 2.신경망 모형

# 방법 1 : nnet

# install.packages('nnet')
library(nnet)
nn.iris <- nnet(Species~.,data=iris, size=2, rang=0.1, decay=5e-4, maxit=200)
# size 은닉층노드수, 
# rang 초기가중치
# decay : parameter for weight decay
# maxit : max of iteration(default 100)

summary(nn.iris)    # 연결선 방향과 가중치

# plot 1
# install.packages('devtools')
library(devtools)
source_url('https://gist.githubusercontent.com/Peque/41a9e20d6687f2f3108d/raw/85e14f3a292e126f1454864427e3a189c2fe33f3/nnet_plot_update.r')
plot.nnet(nn.iris)

# plot 2
# install.packages('clusterGeneration')
# install.packages('scales')
# install.packages('reshape')
library(clusterGeneration)
library(scales)
library(reshape)
plot(nn.iris)

# 정오분류표
table(iris$Species, predict(nn.iris, iris, type='class'))  


# 방법2 : neuralnet

# install.packages('neuralnet')
library(neuralnet)
data(infert, package = 'datasets')
str(infert)
net.infert <- neuralnet(case~age+parity+induced+spontaneous, data=infert, hidden=2,
                        err.fct="ce", linear.output=FALSE, likelihood=TRUE)
net.infert

plot(net.infert)
names(net.infert)   # 수행결과 확인
net.infert$result.matrix   # 결과행렬

# 원자료 + 적합값 출력
out <- cbind(net.infert$covariate, net.infert$net.result[[1]])
dimnames(out) <- list(NULL, c("age","parity","induced","spontaneous","nn-output"))
head(out)

# 일반화가중치 : 각 공변량들의 효과 -> 국지적 기여도, 비선형적 효과
head(net.infert$generalized.weights[[1]])

# 일반화가중치 시각화
par(mfrow=c(2,2))
gwplot(net.infert, selected.covariate = "age", min=-2.5, max=5)
gwplot(net.infert, selected.covariate = "parity", min=-2.5, max=5)
gwplot(net.infert, selected.covariate = "induced", min=-2.5, max=5)
gwplot(net.infert, selected.covariate = "spontaneous", min=-2.5, max=5)
par(mfrow=c(1,1))

# compute() : 각 뉴런의 출력값 계산 -> 새로운 공변량 조합에 대한 예측값 계산
new.output <- compute(net.infert,
                      covariate = matrix(c(22,1,0,0,
                                         22,1,1,0,
                                         22,1,0,1,
                                         22,1,1,1),
                      byrow=TRUE, ncol=4))
new.output$net.result


## 다층 신경망 모형

# data
library(neuralnet)
train.input <- as.data.frame(runif(50, min=0, max=100))
train.output <- sqrt(train.input)
train.data <- cbind(train.input, train.output)
colnames(train.data) <- c("Input","Output")
head(train.data)

# 1개 은닉층, 10개 은닉노드, treshold 오차함수의 편미분에 대한 값으로 정지규칙
net.sqrt <- neuralnet(Output~Input, train.data, hidden=10, threshold=0.01)
print(net.sqrt)
plot(net.sqrt)

# test : hidden 1
test.data <- as.data.frame((1:10)^2)
test.out <- compute(net.sqrt, test.data)   # 예측
ls(test.out)
print(test.out$net.result)

# test : hidden 2
net2.sqrt <- neuralnet(Output~Input, train.data, hidden=c(10,8), threshold=0.01)
plot(net2.sqrt)
test2.out <- compute(net2.sqrt, test.data)
print(test2.out$net.result)



### 3.의사결정나무 모형

#install.packages('rpart')
library(rpart)
c <- rpart(Species~., data=iris); c
plot(c, compress=T, margin=0.3)
text(c, cex=1.5)

head(predict(c, newdata=iris, type="class"))
tail(predict(c, newdata=iris, type="class"))

#install.packages('rpart.plot')
library(rpart.plot)
prp(c, type=4, extra=2)
ls(c)

c$cptable    # 트리의 크기에 따른 비용-복잡도 모수
opt <- which.min(c$cptable[,"xerror"])
cp <- c$cptable[opt, "CP"]
prune.c <- prune(c, cp=cp)
plot(prune.c)
text(prune.c, use.n=T)

plotcp(c)


## 예제 : 전립선 암 환자 (범주형)

#install.packages('party')
library(party)
data(stagec)
str(stagec)

# missing value
stagec1 <- subset(stagec, !is.na(g2))
stagec2 <- subset(stagec1, !is.na(gleason))
stagec3 <- subset(stagec2, !is.na(eet))
str(stagec3)

# missing value : another method
a <- stagec[complete.cases(stagec),]   # 결측치 포함된 관측치 삭제
class(a)
dim(a)
nrow(a)

# train(7) vs test(3)
set.seed(1234)
ind <- sample(2, nrow(stagec3), replace=TRUE, prob=c(0.7,0.3))
ind
trainData <- stagec3[ind==1,]   # n = 102개
testData <- stagec3[ind==2,]    # n = 32개

# ctree 
tree <- ctree(ploidy~., data=trainData); tree
plot(tree)

# predict
testPred <- predict(tree, newdata=testData)
table(testPred, testData$ploidy)


### 예제 : 반응변수가 연속형인 경우

airq <- subset(airquality, !is.na(Ozone))
head(airq)
airct <- ctree(Ozone~., data=airq); airct
plot(airct)

head(predict(airct, data=airq))
predict(airct, data=airq, type="node")     

mean((airq$Ozone - predict(airct))^2)   # 평균제곱오차


### 4. 앙상블 모형(Ensemble model)

# 1) bagging

#install.packages('adabag')
library(adabag)
data(iris)
iris.bagging <- bagging(Species~., data=iris, mfinal=10)   # 10개 표본 -> 배깅
iris.bagging$importance

plot(iris.bagging$trees[[10]])
text(iris.bagging$trees[[10]])

pred <- predict(iris.bagging, newdata=iris)
table(pred$class, iris[,5])

## 2) 부스팅 (boosting)
library(adabag)
data(iris)
boo.adabag <- boosting(Species~., data=iris, boos=TRUE, mfinal=10)
boo.adabag$importance
plot(boo.adabag$trees[[10]])
text(boo.adabag$trees[[10]])

pred <- predict(boo.adabag, newdata=iris)
tb <- table(pred$class, iris[,5]); tb

# 오류분류율 계산
error.rpart <- 1-(sum(diag(tb))/sum(tb)) ; error.rpart


## 예제 : Ada 패키지 활용

# install.packages('ada')
library(ada)
data(iris)
iris[iris$Species!='setosa',] -> iris
n <- dim(iris)[1]; n

trind <- sample(1:n, floor(.6*n), FALSE)
teind <- setdiff(1:n, trind)
iris[,5] <- as.factor((levels(iris[,5])[2:3])[as.numeric(iris[,5])-1])

gdis <- ada(Species~., data=iris[trind,], iter=20, nu=1, type="discrete")
gdis <- addtest(gdis, iris[teind, -5], iris[teind, 5])
gdis

plot(gdis, TRUE, TRUE)    # 오차와 일치도를 나타내는 카파계수
varplot(gdis)             # 변수의 중요도
pairs(gdis, iris[trind, -5], maxvar=4)


### 3) Random Forest

#install.packages('randomForest')
library(randomForest)
library(rpart)
data(stagec)
stagec3 <- stagec[complete.cases(stagec),]
set.seed(1234)
ind <- sample(2, nrow(stagec3), replace=TRUE, prob=c(0.7,0.3))
trainData <- stagec3[ind==1,]
testData <- stagec3[ind==2,]
rf <- randomForest(ploidy~., data=trainData, ntree=100, proximity=TRUE)
table(predict(rf), trainData$ploidy)

print(rf)
plot(rf)

importance(rf)    # 변수의 중요도
varImpPlot(rf)

rf.pred <- predict(rf, newdata=testData)   # 예측
table(rf.pred, testData$ploidy)
plot(margin(rf))     # 훈련용 데이터의 마진값(정분류를 수행한 비율에서 다른 클래스로 분류한 비율의 최대치를 뺀값


## 예제 : party 패키지의 cforest() 함수 이용
library(party)
set.seed(1234)
cf <- cforest(ploidy~., data=trainData)
cf.pred <- predict(cf, newdata=testData, OOB=TRUE, type="response")



### 5. 모형 평가 (p497)

## 1) 표본추출법

# hold-out
data(iris)
nrow(iris)
set.seed(1234)
idx <- sample(2, nrow(iris), replace=TRUE, prob= c(0.7, 0.3))
trainData <- iris[idx==1,]
testData <- iris[idx==2,]
nrow(trainData)
nrow(testData)

# 교차검증 : cross-validation
data(iris)
set.seed(1234)
k = 10
iris <- iris[sample(nrow(iris)),]    # randomly shuffle the data
folds <- cut(seq(1,nrow(iris)), breaks=k, labels=FALSE)
trainData = list(0)
testData = list(0)

for(i in 1:k){
  testIdx <- which(folds==i, arr.ind=TRUE)
  testData[[i]] <- iris[testIdx,]
  trainData[[i]] <- iris[-testIdx,]
}
head(trainData[[1]])
head(trainData[[2]])


## 2) 오분류표 confusion matrix

iris <- subset(iris, Species=='setosa'|Species=='versicolor')
iris$Species <- factor(iris$Species)
set.seed(1234)
iris <- iris[sample(nrow(iris)),]
trainData <- iris[1:(nrow(iris)*0.7),]
testData <- iris[((nrow(iris)*0.7)+1):nrow(iris),]
nrow(trainData)

library(nnet)
library(rpart)
nn.iris <- nnet(Species~., data=trainData, size=2, rang=0.1, decay=5e-4, maxit=200) # 신경망
dt.iris <- rpart(Species~., data=trainData)

nn_pred <- predict(nn.iris, testData, type="class")
dt_pred <- predict(dt.iris, testData, type="class")

# install.packages('e1071') # caret 설치전 설치 필요
library(caret)
nn_con = confusionMatrix(nn_pred, testData$Species)
dt_con = confusionMatrix(dt_pred, testData$Species)
nn_con$table
dt_con$table

accuracy <- c(nn_con$overall['Accuracy'], dt_con$overall['Accuracy'])
precision <- c(nn_con$byClass['Pos Pred Value'], dt_con$byClass['Pos Pred Value'])
recall <- c(nn_con$byClass['Sensitivity'], dt_con$byClass['Sensitivity'])
f1 <- 2*((precision*recall)/(precision+recall))
result <- data.frame(rbind(accuracy, precision, recall, f1))
names(result) <- c("Neural Network", "Decision Tree")
result


## 3) ROC graph

set.seed(1234)
infert <- infert[sample(nrow(infert)),] # Random suhffle
infert <- infert[,c("age","parity","induced","spontaneous","case")]
trainData <- infert[1:(nrow(infert)*0.7),]
testData <- infert[((nrow(infert)*0.7)+1):nrow(infert),]

library(neuralnet)
net.infert <- neuralnet(case~age+parity+induced+spontaneous, data=trainData, hidden=3,
                        err.fct="ce", linear.output=FALSE, likelihood=TRUE)
n_test <- subset(testData, select=-case)
nn_pred <- compute(net.infert, n_test)
testData$net_pred <- nn_pred$net.result
head(testData)

# install.packages('C50')
library(C50)
trainData$case <- factor(trainData$case)
dt.infert <- C5.0(case~age+parity+induced+spontaneous, data=trainData)
testData$dt_pred <- predict(dt.infert, testData, type="prob")[,2]
head(testData)

# install.packages('Epi')
library(Epi)
neural_ROC <- ROC(form=case~net_pred, data=testData, plot="ROC")
dtree_ROC <- ROC(form=case~dt_pred, data=testData, plot="ROC")


### 3) 이익도표

# install.packages('ROCR')
library(ROCR)
n_r <- prediction(testData$net_pred, testData$case)
d_r <- prediction(testData$dt_pred, testData$case)
n_p <- performance(n_r, "tpr", "fpr")
d_p <- performance(d_r, "tpr", "fpr")
plot(n_p, col="red")
par(new=TRUE)
plot(d_p, col="blue")
abline(a=0,b=1)

# 4) 신경망 모형의 향상도

n_lift <- performance(n_r, "lift", "rpp")
plot(n_lift, col='red')
abline(v=0.2)


### 제3절 군집분석

## 1.계층적 군집
# 병합적 방법 : hclust{stats}, {cluster} 패키지의 agnes(), mclust() 함수
# 분할적 방법 : {cluster} 패키지의 diana(), mona() 함수

# method 1 : hclust()
data("USArrests")
str(USArrests)

d <- dist(USArrests, method="euclidian")  # 거리(비유사성) 행렬 제공
   # method : euclidean, maximum, manhattan, binary, minkowski

fit <- hclust(d, method="ave")    # 계층적 군집분석
   # method : ward, single(최단), complete(최장), average, centroid(중심)
par(mfrow=c(1,2))
plot(fit)
plot(fit, hang=-1)
par(mfrow=c(1,1))

groups <- cutree(fit, k=6)   # tree의 높이(h), 그룹수(k) 옵션 지정
groups
plot(fit)
rect.hclust(fit, k=6, border="red")  # 그룹 사격형 구분

hca <- hclust(dist(USArrests))
plot(hca)
rect.hclust(hca,k=3, border="red")    
rect.hclust(hca, h=50, which=c(2,7), border=3:4)

# method 2 :{cluster} agnes() 함수 : 병합적 방법
# install.packages("cluster")
library(cluster)
agn1 <- agnes(USArrests, metric="manhattan", stand=TRUE)
agn1
par(mfrow=c(1,2))
plot(agn1)

agn2 <- agnes(daisy(USArrests), diss=TRUE, method="complete")
   # daisy() 함수는 자료형태가 수치형일 필요가 없어 dist()보다 유연
plot(agn2)

agn3 <- agnes(USArrests, method='flexible', par.meth=0.6)
plot(agn3)
par(mfrow=c(1,1))

png(filename="dendrogram.png")
plot(agn3)
dev.off()


### 2. k-means clustering

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var)) # column variance
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}  # 그룹내 자료의 평균으로부터의 오차
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}


### example 1

install.packages("rattle")
install.packages("RGtk2")
library(rattle)
data(wine, package="rattle")
   # wine : 178개 이탈리아 와인의 13가지 화학성분
head(wine)

## 최적 군집수 결정 1
df <- scale(wine[-1])   # 표준화
wssplot(df)             # 최적 군집수 plot

# # check
# wss <- (nrow(df)-1)*sum(apply(df,2,var))
# k <- kmeans(df, 2)
# k
# k$withinss
# wss[2] <- sum(k$withinss)
# 
# wss


## 최적 군집수 결정 2
# install.packages("NbClust")
# par(mfrow=c(1,1))
library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method='kmeans')
table(nc$Best.n[1,])

barplot(table(nc$Best.n[1,]),
        xlab="Number of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")  


## kmean 3군집 수행
fit.km <- kmeans(df, 3, nstart=25) 
   # nstart : 25개 초기값에 대해 kmeans 실행 -> 최적값 제시
fit.km$size
fit.km$centers  # 3개 그룹의 중심점

plot(df, col=fit.km$cluster)
points(fit.km$center, col=1:3, pch=8, cex=1.5)

# 군집별 변수의 요약값
aggregate(wine[-1], by=list(cluster=fit.km$cluster),mean)
ct.km <- table(wine$Type, fit.km$cluster)   # 정오분류표
ct.km

# 군집간 일치도 순위지표
# install.packages("flexclust")
library(flexclust)
randIndex(ct.km) # 실제 와인의 종류와 군집간의 일치도를 나타내는 수정된 순위지수


### example 2
# {flexclust} kcca()를 이용한 kmeans 수행
# 다양한 시각화 기능 제공
# install.packages("flexclust")
library(flexclust)
data("Nclus")   # 서로 다른 4개의 이변량 정규분포로 부터 생성된 난수
head(Nclus)
plot(Nclus)

cl <- kcca(Nclus, k=4, family=kccaFamily("kmeans"))   # kmeans 수행
image(cl)
points(Nclus)
barplot(cl)  # 각 군집(변수별) 중심이 전체 군집의 중심(상자안의 막대)에서 얼마나 벗어나 있나?
stripes(cl)  # 각 군집내 자료들이 해당 군집의 평균에서 얼마나 떨어져 있니?


### example 3

# install.packages("cclust")
library(cclust)
cl.1 <- cclust(Nclus, 4, 20, method="kmeans")
plot(Nclus, col=cl.1$cluster)
points(cl.1$center, col=1:4, pch=8, cex=1.5)

# install.packages("cluster")
library(cluster)
clusplot(Nclus, cl.1$cluster)



### 3. 혼합분포 군집 Mixture distribution clustering

# example 1

# install.packages("mixtools")
library(mixtools)
data(faithful)
attach(faithful)

head(faithful)
hist(waiting, main="Time between Old Faithful eruptions", xlab="Minutes", ylab="",
     cex.main=1.5, cex.lab=1.5, cex.axis=1.4)

wait1 <- normalmixEM(waiting, lambda=.9, mu=c(40,70), sigma=10)
summary(wait1)

plot(wait1, density=TRUE, cex.axis=1.4, cex.lab=1.4, cex.main=1.8,
     main2="Time between Old faithful eruptions", xlab2="Minutes")


# example 2

# install.packages("mclust")
library(mclust)
mc <- Mclust(iris[,1:4], G=3)
summary(mc, parameters=TRUE)

plot.Mclust(mc,"classification")
# 'arg' should be one of “BIC”, “classification”, “uncertainty”, “density”

str(mc)
mc$classification

# predict(mc, data=???)


######## 4. SOM(Self-Organizing Maps)

# example 1
# install.packages("kohonen")
library(kohonen)
data("wines")
str(wines)
head(wines)

wines.sc <- scale(wines)
set.seed(7)
wine.som <- som(data=wines.sc,                  ### error
                grid=somgrid(5,4,"hexagonal"),
                rlen=100, 
                alpha=c(0.05, 0.01),
                toroidal=FALSE, 
                keep.data=TRUE)



### CH4. 연관분석 Association Analysis

###
# install.packages("arules")
library(arules)
data(Adult)   # 미국 소득 센서스
Adult
as(Adult, 'data.frame')  # dataframe 형식 변환

rules<-apriori(Adult)
inspect(head(rules))

adult.rules <- apriori(Adult, parameter=list(support=0.1, confidence=0.6),
                       appearance=list(rhs=c('income=small', 'income=large'),
                                       default='lhs'),
                       control=list(verbose=F))
adult.rules.sorted <- sort(adult.rules, by='lift')
inspect(head(adult.rules.sorted))

# install.packages('arulesViz')
library(arulesViz)
plot(adult.rules.sorted, method="scatterplot")
plot(adult.rules.sorted, method='graph', control=list(type='items', alpha=0.5))


### (인터넷)분석예제
library(arules)
# 1. 데이터셋 로드
df<-read.csv("sample-data.csv")
str(df)
table(df$id)   # 몇명 id?
# 2. 데이터 변환
rioter.list<-split(df$names, df$id)
rioter.transaction<-as(rioter.list, "transactions") # transaction type으로 변경
rioter.transaction
# 3. 연관규칙 생성
rules = apriori(rioter.transaction)
summary(rules)
rule.list<-as.data.frame(inspect(rules))
rule.list<-rule.list[order(rule.list$lift, decreasing=TRUE), ]
rule.list

# parameter로 조건지정
rules = apriori(rioter.transaction, parameter = list(support=0.1))
summary(rules)
rule.list<-as.data.frame(inspect(rules))
rule.list<-rule.list[order(rule.list$lift, decreasing=TRUE), ]
rule.list


### R을 사용한 베이즈 분류/예측 모델 (Naive Bayes classification in R)
# install.packages("e1071")
library(e1071)
df<-read.csv('http://www-bcf.usc.edu/~gareth/ISL/Heart.csv')
str(df)
# Heart 데이터는 흉부외과 환자 303명을 관찰한 데이터
library(caret)
set.seed(1234) 
intrain<-createDataPartition(y=df$AHD, p=0.7, list=FALSE) 
train<-df[intrain, ]
test<-df[-intrain, ]

nb_model <- naiveBayes(AHD~.,data = train)
nb_model
nbpred <- predict(nb_model, test, type='class')
confusionMatrix(nbpred, test$AHD)
