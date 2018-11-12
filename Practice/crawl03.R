#########################################
# 국토교통부 실거래가 데이터 입수
########################################

### 1. Load Library

library(tidyverse)
library(data.table)
library(purrr)

library(rvest)
library(XML)
library(httr)
library(jsonlite)
library(methods)



### 2. 지역코드 입수 및 정리

df <- fread("./Practice/regionCode.csv")
column_names <-c("number","loc","region")
setnames(df, column_names)
df

df <- df %>% 
	select(loc, region) %>%
	separate(region, c('G1','G2')) %>% 
	mutate(G1 = str_trim(G1),
				 G2 = str_trim(G2)) %>% 
	filter(G1 == '서울특별시')

# create folders to save  
mkdirs <- function(fp) {
	if(!file.exists(fp)) {
		mkdirs(dirname(fp))
		dir.create(fp)
	}
} 

mkdirs("./data/codelist")
write_csv(df, "./data/codelist/region_code.csv")

code_list <- read_csv("./data/codelist/region_code.csv")

		 
### 2. 기초데이터 입수 (not working without SERVICEKEY)

SERVICEKEY = 'roZZFFdy8FI2tEyXyvmqrvWEDc9Vh41yDFzQjbk0nRb270GEhCKGd1MdN4%2Bx1CC2PXhmL8L3ya2xOuueUljDtg%3D%3D' 


#' Molits APT trade data crawling function
#'
#' This function allows you to download raw data.
#' @param KIND bs(매매) or js(전월세) 구분
#' @param YYMM yyyymm
#' @param CODELIST_FILTERED codelist file location
#' @param SERVICEKEY your servicekey		
#' @examples CrawlRawData('bs','201810', code_list, "xxxxxx" )
#' CrawlRawData()
CrawlRawData <- function(KIND, YYMM, CODELIST_FILTERED, SERVICEKEY){
	
	# kind <- 'bs'
	# yymm <- '201810'
	region_code = read_csv(CODELIST_FILTERED)
	
	ServiceKey = SERVICEKEY
	
	if(KIND == 'bs'){
		URL_call = 'http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade'
		dir_name <- paste0("./data/API/bs/", YYMM)
	} else if(KIND == 'js') {
		URL_call = 'http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptRent'
		dir_name <- paste0("./data/API/js/", YYMM)
	} else {
		stop("허용되지 않는 거래입니다")
	}
	
	# create URL
	code <- region_code %>% 
		mutate(yymm = YYMM) %>% 
		mutate(URLJson = paste0(URL_call,"?LAWD_CD=",loc,"&DEAL_YMD=",YYMM,"&serviceKey=",SERVICEKEY),
					 URLXML = paste0(URL_call,"?LAWD_CD=",loc,"&DEAL_YMD=",YYMM,"&serviceKey=",SERVICEKEY,"&type=xml"))
	
	# create folders to save  
	mkdirs <- function(fp) {
		if(!file.exists(fp)) {
			mkdirs(dirname(fp))
			dir.create(fp)
		}
	} 
	
	mkdirs(dir_name)
	
	
	for(i in seq_along(code$URLXML)){
		save_name <- paste0(dir_name, "/", code$loc[i],".xml")
		
		
		if(!file.exists(save_name)){
			tryCatch({     
				r <- GET(code$URLXML[i], add_headers(Accept = "application/xml"))
				df <- content(r, "text", encoding = "UTF-8")
				
				writeLines(df, save_name, useBytes=T)
				Sys.sleep(1)
				
			}, error = function(e) {
				return(NA) # Return NA on error
			}) 
			
		}            
		print(i)
	}
	
}

CrawlRawData('bs', '201502', "./data/codelist/region_code.csv", SERVICEKEY)


### 3. 기초데이터 파싱

file <- "./data/API/bs/201502/11740.xml"

dt <- xml2::read_xml(file, encoding = "UTF-8") 

df <- dt %>% 
	xml_nodes("item") %>% 
	lapply(.,
				 function(child){
				 	xml_contents(child)%>% 
				 		xml_text(.) %>% 
				 		as.list() %>% 
				 		as.data.table  }) %>% 
	rbindlist()

cols <- dt %>% 
	xml_nodes("item") %>%
	.[[1]] %>% 
	xml_children() %>% 
	xml_name()

colnames(df) <- cols

df 

df <- df %>%
	rename(price = c("거래금액"),
				 con_year = c("건축년도"),
				 year = c("년"),
				 dong = c("법정동"),
				 aptnm = c("아파트"),
				 month = c("월"),
				 date = c("일"),
				 area = c("전용면적"),
				 bungi = c("지번"),
				 loc = c("지역코드"),
				 floor = c("층")
	) %>% 
	mutate(yyyymm = paste0(str_sub(year,1,4),"-",str_pad(month, 2, pad=c("0")))) %>% 
	select(loc,dong,bungi,aptnm,area,floor,con_year,year,month,date,yyyymm,price)

df <- df %>% 
	mutate(dong = str_trim(dong),
				 area = as.numeric(area),
				 floor = as.numeric(floor),
				 month = str_pad(month, 2, pad=c("0")),
				 price = as.numeric(str_trim(str_replace(price, ",",""))))

df <- df %>% as_tibble()

df


### 4. 기초데이터 파싱 함수만들기

xmlParsing <- function(file){
	
	tryCatch({     
		dt <- xml2::read_xml(file, encoding = "UTF-8")
		df <- dt %>% 
			xml_nodes("item") %>% 
			lapply(.,
						 function(child){
						 	xml_contents(child)%>% 
						 		xml_text(.) %>% 
						 		as.list() %>% 
						 		as.data.table()  }) %>% 
			rbindlist()
		
	}, error = function(e) {
		return(NA) # Return NA on error
	})        
	
	cols <- dt %>% 
		xml_nodes("item") %>%
		.[[1]] %>% 
		xml_children() %>% 
		xml_name()
	
	colnames(df) <- cols
	
	df <- df %>%
		rename(price = c("거래금액"),
					 con_year = c("건축년도"),
					 year = c("년"),
					 dong = c("법정동"),
					 aptnm = c("아파트"),
					 month = c("월"),
					 date = c("일"),
					 area = c("전용면적"),
					 bungi = c("지번"),
					 loc = c("지역코드"),
					 floor = c("층")
		) %>% 
		mutate(yyyymm = paste0(str_sub(year,1,4),"-",str_pad(month, 2, pad=c("0")))) %>% 
		select(loc,dong,bungi,aptnm,area,floor,con_year,year,month,date,yyyymm,price)
	
	df <- df %>% 
		mutate(dong = str_trim(dong),
					 area = as.numeric(area),
					 floor = as.numeric(floor),
					 month = str_pad(month, 2, pad=c("0")),
					 price = as.numeric(str_trim(str_replace(price, ",",""))))
	
	df <- df %>% as_tibble()
	Sys.sleep(0.5)
	print(df$loc[1])
	df
}


## execute

file <- "./Practice/test_data.xml"
xmlParsing(file)


### 5. 처리기간 반호 함수 만들기  ---------------------------

## for loop 방법
Sys.Date()

DownLoadPeriods <- function(start, end){
	#    start = '201101'
	#    end = '201110'
	
	years <- seq(2006, year(Sys.Date()), by=1)
	
	months <- c("01", "02", "03","04","05","06","07","08","09","10","11","12")
	
	datelist <- c()
	
	for(y in years){
		for(m in months){
			yymm <- paste0(y, m)
			#print(yymm)
			datelist <- c(datelist, yymm)
		}
	}
	
	final_datelist <- datelist %>% 
		as_tibble() %>% 
		filter(value >= start & value <= end)
	
	return(final_datelist)
	
}

DownLoadPeriods(start= '201101', end = '201810')


## lubridate 방법 

library(lubridate)
today()

#@downloads raw file into local folder ----------------
DataDownLoadPeriod_lubridate <- function(start, end){
	dl <- seq(ymd('2006-01-01'), ymd(today()), by = '1 month')
	datelist <- paste0(year(dl), str_pad(month(dl),2, pad=c('0')))
	
	final_datelist <- datelist %>% 
		as_tibble() %>% 
		filter(value >= start & value <= end)
	
	return(final_datelist)
}

DataDownLoadPeriod_lubridate(start='201801', end='201810')


### 6. 전기간 데이터 합치기  ---------------------------

TRADE <- "bs"
START <- '200601'
END <- '201810'

datelist <- DataDownLoadPeriod_lubridate(start=START, end=END)

datelist <- datelist$value
str(datelist)


## for loop     # 2006년 12개월 처리시 329.56 seconds
system.time({
#	for(i in seq_along(datelist)){
	for(i in 1:12){		
		yymm <- datelist[i]
		
		dat <- as_tibble()
		
		file_path <- paste0("./data/bs_raw/", yymm,"/")
		files <- list.files(file_path)
		
		print(file_path)
		
		for (file in files){
			
			file_name <- paste0(file_path, file)
			print(file_name)
			parsed <- xmlParsing(file_name)
			print(parsed)
			dat <- bind_rows(dat, parsed)
		}
		write.table(dat , file = paste0("./data/bs_parsed/", yymm, ".csv"))
	}	
})


## foreach parallel ---- Error!!!

library(doParallel)
library(foreach)
registerDoParallel(cores=4)

TRADE <- "bs"
START <- '200601'
END <- '201810'

datelist <- DataDownLoadPeriod_lubridate(start=START, end=END)
datelist <- datelist$value



system.time({
foreach(i=1:12, .packages=c('tidyverse', 'rvest','XML')) %dopar% {	
		yymm <- datelist[i]
		
		dat <- as_tibble()
		
		file_path <- paste0("./data/bs_raw/", yymm,"/")
		files <- list.files(file_path)
		
		print(file_path)
		
		for (file in files){
			
			file_name <- paste0(file_path, file)
			print(file_name)
			parsed <- xmlParsing(file_name)
			print(parsed)
			dat <- bind_rows(dat, parsed)
		}
		write.table(dat , file = paste0("./data/bs_parsed/", yymm, ".csv"))
	}
})




## purrr map        # 2006년 12개월 처리시 277.85   seconds

TRADE <- "bs"
START <- '200601'
END <- '201810'

datelist <- DataDownLoadPeriod_lubridate(start=START, end=END)


file_list <- datelist[1:12,] %>% 
	mutate(path = paste0("./data/bs_raw/", value,"/")) %>% 
	mutate(files = map(path, function(x){list.files(x)})) %>% 
	unnest() %>% 
	mutate(file = paste0(path, files))

system.time({
	final_df<- file_list %>%
		mutate(dt = map(file, function(x){xmlParsing(x)}))
})



tail(final_df)
