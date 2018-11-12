###############################
# ADP certificate
# 2017-10-20
# Modified by Dookyung Kim
###############################

###############################
## 제2장 통계 분석
###############################


# 제2절 기초통계 분석 p370
##########################

## 1. 기술통계
data(iris)
head(iris)
summary(iris)

mean(iris$Sepal.Length)
median(iris$Sepal.Length)
sd(iris$Sepal.Length)
var(iris$Sepal.Length)

quantile(iris$Sepal.Length, 1/4)
quantile(iris$Sepal.Length, 3/4)
max(iris$Sepal.Length)
min(iris$Sepal.Length)

#install.packages('MASS')
library(MASS)
data(Animals)
head(Animals)
quantile(Animals$body)


## 2. 회귀분석 p373

# 가.단순회귀
set.seed(2)
x = runif(10,0,11); x
y = 2 + 3*x + rnorm(10,0,0.2)
dfrm = data.frame(x, y); dfrm
lm(y~x, data=dfrm)

# 나.다중회귀
set.seed(2)
u = runif(10,0,11); v = runif(10,11,20); w = runif(10,1,30)
y = 3 + 0.1*u + 2*v -3*w + rnorm(10,0,0.1)
dfrm = data.frame(y,u,v,w); dfrm
m <- lm(y~u+v+w); m
summary(m)

# 예제
library(MASS); head(ChickWeight)
Chick <- ChickWeight[ChickWeight$Diet==1,]; Chick
lm(weight~Time, data=Chick)
summary(lm(weight~Time, data=Chick))

# 예제
data(cars); head(cars)
speed2 <- cars$speed^2
cars <- cbind(speed2, cars); head(cars)
a <- lm(dist~speed+speed2, data=cars); summary(a)

# 예제
x <- c(1,2,3,4,5,6,7,8,9); y <- c(5,3,2,3,4,6,10,12,18)
df1 <- data.frame(x,y); df1
plot(df1)

x2 <- x^2
df2 <- cbind(x2, df1); df2
b <- lm(y~x, data=df1); summary(b)
plot(b)

c <- lm(y~x+x2, data=df2); c
summary(c)
plot(lm(y~x+x2, data=df2))


## 다. 최적회귀방정식 선택 : 설명변수 선택

X1 <- c(7,1,11,11,7,11,3,1,2,21,1,11,10)
X2 <- c(26,29,56,31,52,55,71,31,54,47,40,66,68)
X3 <- c(6,15,8,8,6,9,17,22,18,4,23,9,8)
X4 <- c(60,52,20,47,33,22,6,44,22,26,34,12,12)
Y <- c(78.5,74.3,104.3,87.6,95.9,109.2,102.7,72.5,93.1,115.9,83.8,113.3,109.4)
df <- data.frame(X1, X2,X3,X4,Y)
head(df)

a <- lm(Y~X1+X2+X3+X4, data=df); summary(a)
b <- lm(Y~X1+X2+X4, data=df); summary(b)
c <- lm(Y~X1+X2, data=df);summary(c)

step(lm(Y~1, df), scope=list(lower=~1, upper=~X1+X2+X3+X4), direction="forward") #전진선택법
step(lm(Y~1, df), scope=list(lower=~1, upper=~X1+X2+X3+X4), direction="both")  # 단계적 방법
step(lm(Y~X1+X2+X3+X4, data=df), direction="backward")  # 후진선택법



# 제3절 다변량 분석  p404
##########################

## 1.상관분석 

#install.packages("Hmisc")
library(Hmisc)
data(mtcars); head(mtcars)

drat <- mtcars$drat; disp <- mtcars$disp
plot(drat, disp)
cor(drat, disp)

# 피어슨상관계수
rcorr(as.matrix(mtcars), type='pearson')
cov(mtcars)
# 스피어만 상관계수
rcorr(as.matrix(mtcars), type="spearman")


## 2.다차원 척도법 (p412)

data(eurodist); eurodist
loc <- cmdscale(eurodist); head(loc)   # cmdscale : 각 도시의 상대적 위치 xy좌표에 도식화
x <- loc[,1]; y <- loc[,2]
plot(x,y, type='n', main="eurodist")
text(x,y, rownames(loc), cex=0.8)
abline(v=0,h=0)


## 3.주성분 분석 (p414)

library(datasets)
data(USArrests); head(USArrests)
summary(USArrests)

fit <- princomp(USArrests, cor=TRUE); summary(fit)   # 공분산이 아니라 상관계수 기준
loadings(fit)   # 주성분의 로딩벡터
plot(fit, type='lines')     # 각 주성분의 분산의 크기(Scree plot)

fit$scores       # 각 관측치를 주성분들로 표현
biplot(fit)      # 관측치를 첫번째, 두번째 주성분좌표에 그린 그림


# 제4절 시계열 분석
###########################

# 1) 시계열자료 불러오기
x <- c(3,5,2,8,4,9)
ts(x)
Nile   #나일강 연락 유입량
ldeaths #영국 월별 폐질환 사망자
plot(Nile)
plot(ldeaths)

# 2) 분해시계열
# 폐질활자
ldeaths.decompose <- decompose(ldeaths)
str(ldeaths.decompose)
ldeaths.decompose$seasonal
plot(ldeaths.decompose)
ldeaths.decompose.adj <- ldeaths - ldeaths.decompose$seasonal
plot(ldeaths.decompose.adj)

# 4) ARIMA 모형
# 차분
Nile.diff1 <- diff(Nile, differences=1)
plot(Nile.diff1)
Nile.diff2 <- diff(Nile, differences=2)
plot(Nile.diff2)

# ARIMA 모델 적합 및 결정
acf(Nile.diff2, lag.max=20)
acf(Nile.diff2, lag.max=20, plot=FALSE)
pacf(Nile.diff2, lag.max=20)
pacf(Nile.diff2, lag.max=20, plot=FALSE)

#install.packages("forecast")
library(forecast)
auto.arima(Nile)    # ARMA(p,q) 결정 -> 최적모형 결정  ARIMA(1,1,1)

# ARIMA 모형을 이용한 예측
Nile.arima <- arima(Nile, order=c(1,1,1)); Nile.arima     # ARIMA(1,1,1) 추정
Nile.forecasts <- forecast(Nile.arima, h=10); Nile.forecasts          # 예측
plot(Nile.forecasts)
