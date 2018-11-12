###############################
# ADP certificate
# 2017-10-20
# Modified by Dookyung Kim
###############################

###############################
## CH1 R 기초
###############################

### 1 데이터타입 

## 도움말
?par
help(par)
rm(list=ls())
getwd()
wd <- getwd()
setwd(wd)
# setwd("~/Dropbox/certificate/ADP/code")
# source("-")

## 벡터
x <-c(1,2,3)
y <- c("사과", "배")
z <- c(x,y); z

## matrix
mx = matrix(c(1,2,3,4,5,6), nrow=2, byrow=T);mx
mx = matrix(c(1,2,3,4,5,6), ncol=2);mx
r1 = c(10,10)
rbind(mx, r1)
c1 = c(20,20,20)
cbind(mx,c1)


## data.frame
income = c(100,200,150,300,900)
car = c("kia", "hyundai","kia", "toyota","lexus")
marriage = c(FALSE, FALSE, FALSE, TRUE, TRUE)
mydat <- data.frame(income, car, marriage)

### 2 외부데이터 불러오기

## text, csv
data <- read.table("./text.txt", sep = "|", header=T); data
data <- read.table("./text.csv", sep = ",", header=T); data
data <- read.csv('text.csv'); data

## fread (package: data.table, bit64) 
library(data.table)
fread('text.csv')

## excel reading
# 1)
library(readxl)
excel_sheets('Sample-Sales-Data.xlsx')
df <- read_excel('Sample-Sales-Data.xlsx',sheet='Sheet1')
head(df)
sum(df['Value'])
str(df)
summary(df)

# 2) read entire list:
entire_workbook <- lapply(excel_sheets("Sample-Sales-Data.xlsx"), 
                          read_excel, 
                          path = "Sample-Sales-Data.xlsx")
entire_workbook

# 3) 
# install.packages('RODBC')
# library(RODBC)            # only 32bit com
# new <- odbcConnectExcel("./text")
# yourdata <- sqlFetch(new, "Sheet1")
# close(new)


## excel writing
#install.packages('xlsx',repos="http://cran.rstudio.com/")
library(xlsx)
df <- data.frame(matrix(1:10))
write.xlsx(df, "output.xlsx")
read_excel('output.xlsx')

write.csv(df, file = "foo.csv")
fread('foo.csv')
write.csv(df, file = "foo.csv",row.names = FALSE)
fread('foo.csv')

## SQL in R : (packages : "RODBC") 
# library(RODBC)
# myconn <-odbcConnect("Database_Name", uid="User_ID", pwd="password")
# dat <- sqlFetch(myconn, "Table_Name")
# querydat <- sqlQuery(myconn, "SELECT * FROM table")
# close(myconn)

## Web Scraping
demo(package='rvest')
#install.packages('rvest')

library(rvest)
lego_movie <- read_html("http://www.imdb.com/title/tt1490017/")

lego_movie %>% 
  html_node("strong span") %>%
  html_text() %>%
  as.numeric()

lego_movie %>%
  html_nodes("#titleCast .itemprop span") %>%
  html_text()

lego_movie %>%
  html_nodes("table") %>%
  .[[2]] %>%
  html_table()


### 3 R의 기초함수

## 수열 생성하기
rep(1,3)
seq(1,3)
1:3
seq(1,11, by=2)
seq(1,11, length=6)
seq(1,11, length=8)

rep(2:5, 3)

## 기초적인 수치 계산
a=1:10
a
a+a
a-a
a*a
a/a

a=c(2,7,3)
a
t(a)
A= a%*%t(a)
A

# 역행렬
mx = matrix(c(23,41,12,35,67,1,24,7,53), nrow=3)
mx
5*mx
solve(mx)

# 기초통계량
a=1:10
a
mean(a)
var(a)
sd(a)
sum(a)
median(a)
log(a)
b=log(a)
cov(a,b)
cor(a,b)
summary(a)


### 4 R 데이터 핸들링

## 1) 벡터
b = c('a','b','c','d','e'); b
b[2]
b[-4]
b[c(2,3)]

# 2) matrix / dataframe
income = c(100,200,150,300,900)
car=c('kia','hyundai','kia','toyota','lexus')
marriage = c(FALSE, FALSE, FALSE, TRUE, TRUE)
mydat = data.frame(income, car, marriage, stringsAsFactors = FALSE)
mydat
mydat[3,2]
mydat[,2]
mydat[4,]
class(mydat[["car"]])
class(mydat["car"])


### 5 반복구문과 조건문

## 1) 반복구문 for
a = c()
for (i in 1:9){
  a[i] = i*i
}
a

isum=0
for (i in 1:100){
  isum = isum + 1
}
cat("1부터 100까지의 합=", isum, "입니다.", "\n")

## 2) 반복구문 while
x=1
while(x<5){
  x=x+1
  print(x)
}

## 3) if~else 구문
StatScore=c(39,89,20, 90)
over70 = rep(0,4)
for(i in 1:4){
  if( StatScore[i]>70) over70[i] = 1
  else over70[i] =0 
}
over70

sum(over70)


### 6 사용자 정의함수

addto = function(a){
  isum = 0
  for(i in 1:a){
    isum = isum + i
  }
  print(isum)
}

addto(100)

addto(50)


### 7 기타 유용한 도구

## 1) paste
number = 1:10
aphabet = c("a","b","c")
paste(number, aphabet)
paste(number, aphabet, sep=" to the ")

## 2) substr
substr("BigDataAnalysis",1,4)
country=c("Korea","Japan","China","Singapore", "Russia")
substr(country, 1,3)

## 3) 자료형 데이터 구조 변환
# as.data.frame(x)
# as.list(x)
# as.matrix(x)
# as.vector(x)
# as.factor(x)
as.integer(3.14)
as.numeric("foo")
as.character(101)
as.numeric(FALSE)
as.logical(0.45)

mydat
as.matrix(mydat)

## 4) 문자열을 날짜로 변환
Sys.Date()
#as.Date()
as.Date("2015-01-13")
as.Date("01/13/2015") # error
as.Date("01/13/2015", format="%m/%d/%Y")

# 5) 날짜를 문자열로 변환
as.Date("08/13/2013", format="%m/%d/%Y")
format(Sys.Date())
as.character(Sys.Date())
format(Sys.Date(), format="%m/%d/%Y")

format(Sys.Date(), "%a") # 요일
format(Sys.Date(), "%b") # 월
format(Sys.Date(), "%m") # 두자리 숫자 월
format(Sys.Date(), "%d") # 두자리 숫자 일
format(Sys.Date(), "%y") # 두자리 숫자 연도
format(Sys.Date(), "%Y") # 네자리 숫자 연도


### 8 R 그래픽 기능

height = c(150, 170, 135, 180, 189)
Weight = c(68, 78, 55, 79, 80)
plot(height, Weight)

pairs(iris[1:4], main="Anderson's Iris Data -- 3 species",
      pch=21, bg=c("red","green3","blue")[unclass(iris$Species)])

StatScore = c(88,79,55,46,98,44,34,34,56,78,55,44)
hist(StatScore, prob=T)
boxplot(StatScore)



#################################
### CH2. 데이터 마트
#################################


### 1.reshape 패키지

#install.packages('reshape')
library(reshape)

data(airquality)  # data 불러오기
head(airquality, 10)

names(airquality) = tolower(names(airquality))
names(airquality)

# melt
aqm = melt(airquality, id=c('month','day'), na.rm=TRUE); 
class(aqm)
aqm[200:length(t(aqm)),]

# cast
head(aqm)
a <- cast(aqm, day ~ month ~ variable); a
a[,,"temp"]
b <- cast(aqm, month ~ variable, mean); head(b)
c <- cast(aqm, month ~ .|variable, mean); head(c)  # 모든 변수, 산출물 분리 표시
d<- cast(aqm, month ~ variable, mean, margins=c("grand_row", "grand_col")); head(d)
d<- cast(aqm, month ~ variable, mean, margins=TRUE); head(d)    # same
e <- cast(aqm, day ~ month, mean, subset=variable=="ozone"); head(e)   # 특정변수만
f <- cast(aqm, month ~ variable, range); head(f)


### 2. sqldf를 이용한 데이터 분석

#install.packages('sqldf')
library(sqldf)

data(iris)
sqldf("select * from iris")
sqldf("select * from iris limit 10")
sqldf("select count(*) from iris where Species like 'se%' ")


### 3. plyr

# 데이터 생성
library(plyr)
set.seed(1)
d = data.frame(year=rep(2012:2014, each=6), count=round(runif(9,0,20))); d

# 연도별 cv값 계산
ddply(d, "year",function(x){
  mean.count = mean(x$count)
  sd.count = sd(x$count)
  cv = sd.count/mean.count
  data.frame(cv.count = cv)
})

# summarise
ddply(d, "year", summarise, mean.count=mean(count))

# tranfrom : 원 데이터프레임에 결과값 붙이기
ddply(d, "year", transform, total.count=sum(count))


###  4. 데이터 테이블 data.table

#install.packages('data.table')
library('data.table')
DT = data.table(x=c('b','b','b','a','a'), v=rnorm(5)); DT

data(cars) ; head(cars)
CARS <- data.table(cars) ; head(CARS)
tables()   # 기존에 생성한 table 정보 조회

sapply(CARS, class)
DT[2,]
DT[DT$x=='b',]

# key지정
setkey(DT,x)  # key지정 (indexing 기능)
DT            # x로 자동 정렬
tables()      # key지정 확인
DT['b',]      # DT['b'] 동일 
DT['b', mult='first']  # b중 첫번째
DT["b", mult='last']

# 예) data.frame vs data.table 비교
# data.frame
grpsize <- ceiling(1e7/26^2)   # 천만개의 행과 676개 그룹  ????
tt <- system.time(DF <- data.frame(
  x = rep(LETTERS, each=26*grpsize),
  y = rep(letters, each=grpsize),
  v = runif(grpsize*26^2),
  stringsAsFactors = FALSE)
)
tt
head(DF,3)
tail(DF,3)
dim(DF)
10000068/0.81  #12345763 rows per seconds

tt <- system.time(ans1 <- DF[DF$x=="R" & DF$y=="h",]); tt
head(ans1,3)
dim(ans1)

# data.table
DT <- data.table(DF)
setkey(DT,x,y)
ss <- system.time(ans2 <- DT[J("R","h")])  # binary search

head(ans2,3)
dim(ans2)
identical(ans1$v, ans2$v)
ss

system.time(ans2 <- DF[DF$x=="R" & DF$y=="h",])  # bad case for using data. table
mapply(identical, ans1, ans2)

# j and by
DT[, sum(v)]
DT[, sum(v), by=x]

ttt <- system.time(tt<- tapply(DT$v, DT$x, sum)); ttt
head(tt, 10)

sss <- system.time(ss <- DT[,sum(v), by=x]) ; sss  # 3x faster
head(tt)
head(ss)
identical(as.vector(tt), ss$V1)

SSS <- system.time(ss <- DT[,sum(v), by='x,y']); SSS;ss



###################################
### CH3. 결측값 처리와 이상값 검색
##################################

### 1.데이터 탐색

data(iris)
head(iris, 10)
str(iris)
summary(iris)
cov(iris[, 1:4])
cor(iris[, 1:4])

### 2. 결측값 처리

y <- c(1,2,3,NA)
is.na(y)

# 99 -> NA처리
car=c('kia','hyundai','kia','toyota','lexus')
income = c(1,2,99,3,9)
mydat = data.frame(car, income, stringsAsFactors = FALSE)
mydat[mydat$income==99, 'income'] <- NA ; mydat   # 99를 결측값으로 처리

# 결측값 제외
x <- c(1,2,NA,3)
mean(x)
mean(x, na.rm=T)

mydat[!complete.cases(mydat),]   # 결측치 만
mydat[complete.cases(mydat),]   # 결측치 포함된 관측치 삭제


## Amelia 패키지
#install.packages('Amelia')
library(Amelia)
data(freetrade); head(freetrade)
str(freetrade)

# imputation
a.out <- amelia(freetrade, m=5, ts='year', cs='country')
hist(a.out$imputations[[3]]$tariff, col='grey', border="white")
save(a.out, file="imputations.RData")
write.amelia(obj=a.out, file.stem='outdata')
missmap(a.out)

freetrade$tariff <- a.out$imputations[[5]]$tariff
missmap(freetrade)


### 3. 이상값 검색

# 1) boxplot 활용
x = rnorm(100)
boxplot(x)

x = c(x, 19,28,30)
outwith = boxplot(x)
outwith$out         # 이상치

# 2) outlier 패키지 활용
#install.packages('outliers')
library(outliers)
set.seed(1234)
y = rnorm(100)
outlier(y)      # 평균과 가장 차이가 많이 나는 값 출력
outlier(y, opposite=TRUE)   # 반대방향으로 가장 차이가 많이 나는 값 출력
dim(y) = c(20,5)
outlier(y)
outlier(y, opposite=TRUE)
boxplot(y)
