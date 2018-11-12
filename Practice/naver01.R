#########################################
# 네이버 주가정보 입수
########################################

## 1. load libraries
if (! ("rvest" %in% rownames(installed.packages()))) { install.packages("rvest") }

library(rvest)
library(tidyverse)
library(multidplyr) # parallel processing

## 2. 상장주식정보 받기

#### (option1) 수작업으로 저장한 CSV 파일 읽기

list <- read.csv("./data/상장법인목록.csv")


#### (option) 입수자동화

tar <- 'http://kind.krx.co.kr/corpgeneral/corpList.do?method=download&searchType=13'

Sys.setlocale("LC_ALL", "English")

code <- read_html(tar) %>% 
	rvest::html_nodes(css="table") %>%
	rvest::html_table() 

Sys.setlocale("LC_ALL", "Korean")

code



## 3. 상장주식코드 저장하기

str(code)
class(code)

code <- code %>% 
	as.data.frame()

str(code)
class(code)


code <- code %>%  
	select(회사명, 종목코드) %>% 
	mutate(
		종목코드 = sprintf("%06d", 종목코드)
	)

names(code)<-c("name","code")

head(code)


code <- code %>% 
	as.tibble()

code



## 4. 개별주가 데이터 한 페이지 크롤링 하기

code[1,]

cd <- code[1,]$code   #  "001040"

# 네이버 금융에서 검색하여 주가제공 화면으로 이동

base_url <- "http://finance.naver.com/item/sise_day.nhn?code="
base_url <- paste0(base_url ,cd)
base_url


price <- base_url %>% 
	read_html() %>% 
	html_nodes('table') %>% 
	.[1] %>%
	html_table() %>% 
	.[[1]] %>%
	as_tibble() %>% 
	filter(날짜 != "") %>% 
	select(-전일비)


## 5. 개별주가 여러 페이지 크롤링

cd <- code[1,]$code   #  "001040"

base_url <- "http://finance.naver.com/item/sise_day.nhn?code="
base_url <- paste0(base_url ,cd,"&page=")
base_url

all.price <- as_tibble()

for (page in 1:10){

		price <- paste(base_url,page,sep='') %>%
		read_html() %>%
		html_nodes('table') %>%
		.[1] %>%
		html_table() %>% 
		.[[1]] %>%
		as_tibble() %>% 
		filter(날짜 != "") %>% 
		select(-전일비)

	  all.price <- bind_rows(all.price,price) %>% 
		as_tibble() 
	
	print(page)
	Sys.sleep(0.2)
}

all.price



## 6. 개별주가 크롤링 함수 만들기

get_data <- function(url_base){
	all.price <- as_tibble()
	
	for (page in 1:5){
			price <- paste(url_base,page,sep='') %>%
				read_html() %>%
				html_nodes('table') %>%
				.[1] %>%
				html_table() %>% 
				.[[1]] %>%
				as_tibble() %>% 
				filter(날짜 != "") %>% 
				select(-전일비) %>%
				mutate(거래량 = as.character(거래량)) %>% 
				mutate(시가 = as.character(시가)) %>% 
				mutate(종가 = as.character(종가)) %>% 
				mutate(고가 = as.character(고가)) %>% 
				mutate(저가 = as.character(저가)) 
			# %>%
			#     mutate_all(str_replace(',',""))
			
		all.price <- bind_rows(all.price,price) %>% 
			as_tibble() 
		
		print(page)
		Sys.sleep(0.2)
	}
	
	print(url_base)
	
	return(all.price)
	
}


cd <- code[1,]$code   #  "001040"

base_url <- "http://finance.naver.com/item/sise_day.nhn?code="
base_url <- paste0(base_url ,cd,"&page=")
base_url

get_data(base_url)

df <- get_data(base_url)

df


## 7. 전체 종목 주가 크롤링하기

# 종목코드 입수함수

KRX_codes_get <- function(){
	. <- NULL
	tar <- 'http://kind.krx.co.kr/corpgeneral/corpList.do?method=download&searchType=13'
	
	Sys.setlocale("LC_ALL", "English")
	cd <- xml2::read_html(tar) %>%
		rvest::html_nodes(css="table") %>%
		rvest::html_table() 
	
	Sys.setlocale("LC_ALL", "Korean")
	
	cd <- cd %>% 
		.[[1]] %>% 
		as.tibble() %>% 
		select(회사명, 종목코드) %>% 
		mutate(
			종목코드 = sprintf("%06d", 종목코드)
		)
	
	names(cd)<-c("name","code")
	return(cd)
}

KRX_codes_get()


# 종목별 주가 입수 함수(5 pages)

get_data <- function(url_base){
	all.price <- as_tibble()
	
	for (page in 1:2){

		# error 처리
		tryCatch({            
			
			price <- paste(url_base,page,sep='') %>%
				read_html() %>%
				html_nodes('table') %>%
				.[1] %>%
				html_table() %>% 
				.[[1]] %>%
				as_tibble() %>% 
				filter(날짜 != "") %>% 
				select(-전일비) %>%
				mutate(거래량 = as.character(거래량)) %>% 
				mutate(시가 = as.character(시가)) %>% 
				mutate(종가 = as.character(종가)) %>% 
				mutate(고가 = as.character(고가)) %>% 
				mutate(저가 = as.character(저가)) 
			
		}, error = function(e) {
#			price <- NA
			return(NA) # Return NA on error
		})
		all.price <- bind_rows(all.price,price) %>% 
			as_tibble() 
		
		print(page)
		Sys.sleep(0.2)
	}
	
	print(url_base)
	
	return(all.price)
	
}

## 입수목록 정리 
codes <- KRX_codes_get() %>% 
	mutate(url_base = paste0('http://finance.naver.com/item/sise_day.nhn?code=',code,'&page=')) %>% 
	.[1:100,]

codes$code


## purrr::map 함수로 for loop문 대체 - with Time needed

start <- proc.time() # Start clock

KOSPI <- codes %>%
    mutate(
        stock.prices = map(url_base, function(x) get_data(x))
    )

time_elapsed <- proc.time() - start # End clock

time_elapsed  # 55초 소요



KOSPI

KOSPI$stock.prices
KOSPI$stock.prices[1]

KOSPI_fianl <- KOSPI %>%
    drop_na(stock.prices) %>%
    unnest()

KOSPI_fianl


## 8. parallel 코딩으로 성능향상


## STEP 0: GET NUMBER OF CORES (OPTIONAL)
library(multidplyr) # parallel processing
library(parallel)
library(rvest)
library(tidyverse)
library(multidplyr) # parallel processing

cl <- detectCores()
cl

codes <- KRX_codes_get() %>% 
	mutate(url_base = paste0('http://finance.naver.com/item/sise_day.nhn?code=',code,'&page=')) %>% 
	.[1:100,]

## STEP 1: ADD GROUPS
group <- rep(1:cl, length.out = nrow(codes))
sp_500 <- bind_cols(tibble(group), codes)
sp_500

## STEP 2: CREATE CLUSTERS
cluster <- create_cluster(cores = cl)
cluster


##  STEP 3: PARTITION BY GROUP
by_group <- sp_500 %>%
	partition(group, cluster = cluster)
by_group

## STEP 4: SETUP CLUSTERS

# Utilize pipe (%>%) to assign libraries, functions, and values to clusters
by_group %>%
	# Assign libraries
	cluster_library("tidyverse") %>%
	cluster_library("rvest") %>%
#	cluster_library("R6") %>%
#	cluster_library("stringr") %>%
#	cluster_library("lubridate") %>%
#	cluster_library("quantmod") %>%
	# Assign values (use this to load functions or data to each core)
	cluster_assign_value("get_data", get_data)

cluster_eval(by_group, search())[[1]] # results for first cluster shown only
cluster_get(by_group, "get_data")[[1]] # results for first cluster shown only

## STEP 5: RUN PARALLELIZED CODE


start <- proc.time() # Start clock
sp_500_processed_in_parallel <- by_group %>% # Use by_group party_df
	mutate(
		stock.prices = map(.x = url_base, ~get_data(url_base = .x))
	) %>%
	collect() %>%           # Special collect() function to recombine partitions
	as_tibble()             # Convert to tibble


time_elapsed_parallel <- proc.time() - start # End clock

time_elapsed_parallel


sp_500_processed_in_parallel




## 9. 최종 데이터 저장하기

# save rds
file_name <- paste0('./data/KOSPI_stocks_price_', Sys.Date(),'.rds')
saveRDS(sp_500_processed_in_parallel, file_name)

# read rds 
stocks_price <- readRDS(file_name)


stock_price_csv <- stocks_price %>%
	drop_na(stock.prices) %>% 
	unnest() %>% 
	tail(10)

# save csv
write.csv(stock_price_csv, './data/KOSPI_stocks_price.csv')

read.csv('./data/KOSPI_stocks_price.csv')
tail(sp_500_processed_in_parallel)

# save csv as a tibble
write_csv(stock_price_csv, './data/KOSPI_stocks_price_encoding.csv')
read_csv('./data/KOSPI_stocks_price_encoding.csv')

