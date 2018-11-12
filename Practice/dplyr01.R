
###############################################
# III. R Advanced stuff 
###############################################

getwd()
library(tidyverse)


### 1. XML 파싱















### 1. 대용량 데이터 빨리 불러오기 ----------------------------

## (방법1) Base R의 data.frame 이용시

# read.csv --->  90만줄 9.79  seconds
system.time(
  df <- read.csv(file = "./Practice/apt_bs_all_df.csv", encoding="EUC-KR")
 )  							 
  							 

tail(df)
class(df)


# write csv file     21.61 seconds
system.time(
	write.csv(df, "./Practice/apt_bs_all_df.csv", fileEncoding = "UTF-8", row.names = FALSE)
)


## (방법2) readr 이용시 
library(tidyverse)
	
# read with read_csv   4.4  seconds
system.time(
	dr <- readr::read_csv(file = "./Practice/apt_bs_all_dr.csv")
)

dr

#dr <- dr %>% select(-X)

# write with write_csv  3.88  seconds
system.time(
	write_csv(dr, "./Practice/apt_bs_all.csv")
)



## (방법3) data.table의 fread 이용시 

library(data.table)

# read with fread --->   read 0.32 second
system.time(
  dt <- fread("./Practice/apt_bs_all_dt.csv", sep = ",", header= TRUE)
)
class(dt)
dt


## write with fwrite --->   read 0.21 second
system.time(
fwrite(dt, file="./Practice/apt_bs_all_dt")
)


### 2. 코드 만들기 ----------------------------



bs_code <- dt %>%
	select(G1, G2, dong, bungi, aptnm, con_year, area, floor) %>%
	distinct() 



#### 2. dplyr로 데이터 전처리 고수되기


# 1) reading data

#install.packages("dplyr")
library(dplyr)

#install.packages('nycflights13')
library(nycflights13)
head(flights)
summary(flights)
dim(flights)


# 2) filter()

# (basic)
flights[flights$month == 11 & flights$day == 3 & flights$carrier == 'AA', ]
subset(flights,month==11 & day==3 & carrier=='AA')

# (dplyr)
filter(flights,month==11,carrier=='AA')


# 3) slice()

# (basic)
flights[1:10,]

# (dplyr)
slice(flights, 1:10)


# 4) arrange()

# (basic)
flights[order(flights$year, flights$month, flights$day, flights$dep_delay),]
flights[order(flights$year, flights$month, flights$day, flights$dep_delay, decreasing=c(F,F,F,T)),]

# (dplyr)
arrange(flights,year,month,day,desc(dep_delay))

# 5) select()

# (basic)
flights[,c("year","month","day","carrier")]
subset(flights, select=c("year","month","day","carrier"))

# (dplyr)
select(flights,year, month, day, carrier)


# 6) distinct()

# (basic)
data.frame(unique(flights$carrier))

# (dplyr)
distinct(select(flights,carrier))


# 7) mutate() - dplyr
df <- mutate(flights, new_col = arr_delay-dep_delay)
head(df)
df[,c("new_col")]

# 8) transmute() - dplyr
df <- transmute(flights, new_col = arr_delay-dep_delay)
head(df)

# 9) summarise() & Group_by()  - dplyr

summarise(flights, delay = mean(dep_delay, na.rm = TRUE))

by_day <- group_by(flights, year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = TRUE))


# 10) sample_n() and sample_frac()  - dplyr

sample_n(flights,10)
sample_frac(flights,0.00005) # USE replace=TRUE for bootstrap sampling


### 3. dplyr - pipe %>% 로 보다 쉽게

# 1) reading data
library(data.table)
df <- data.table(mtcars)
head(df)

# 2) Using Nesting
arrange(sample_n(filter(df,mpg>20),size=5),desc(mpg))


# 3) Using Multiple Assignments
a <- filter(df,mpg > 20)
b <- sample_n(a,size = 5)
c <- arrange(b,desc(mpg))
c

# 4) Using the Pipe Operator
df %>% filter(mpg > 20) %>% sample_n(size = 5) %>% arrange(desc(mpg))

rnorm(100) %>%
  matrix(ncol = 2) %>%
  plot() %>%
  str()


### 3. tidyr - 추가기능

library(tidyr)
library(data.table)

# 1) reading data

comp <- c(1,1,1,2,2,2,3,3,3)
yr <- c(1998,1999,2000,1998,1999,2000,1998,1999,2000)
q1 <- runif(9, min=0, max=100)
q2 <- runif(9, min=0, max=100)
q3 <- runif(9, min=0, max=100)
q4 <- runif(9, min=0, max=100)

df <- data.frame(comp=comp,year=yr,Qtr1 = q1,Qtr2 = q2,Qtr3 = q3,Qtr4 = q4)
df

# 2) gather()

head(df %>% gather(Quarter,Revenue,Qtr1:Qtr4))

head(gather(df,Quarter,Revenue,Qtr1:Qtr4))


# 3) spread()

stocks <- data.frame(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)
stocks

stocksm <- stocks %>% gather(stock, price, -time)
stocksm
stocksm %>% spread(stock, price)
stocksm %>% spread(time, price)


# 3) separate()

df <- data.frame(x = c(NA, "a.x", "b.y", "c.z"))
df
df %>% separate(x, c("ABC", "XYZ"))


# 4) unite()
head(mtcars)
unite_(mtcars, "vs.am", c("vs","am"),sep = '.')

mtcars %>%
  unite(vs_am, vs, am) %>%
  separate(vs_am, c("vs", "am"))

