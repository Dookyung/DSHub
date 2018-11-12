############################
# R에서 파일 다루기 
############################


### 1. 경로 다루기 --------------------------

getwd()
# setwd("/file/path")

R.home()
.libPaths()


dir.create()

if(!dir.exists("myDir")) {dir.create("myDir")}

file.path("Parent", "child")


# 현재 사용하는 운영체계에 따라 경로표시 수정
normalizePath(file.path("parent", "child"), mustWork=FALSE)

# 홈 디렉토리에 대해 ~ 표시로 
path.expand("~")

# 파일명, 경로명 구분
file_path_name <- "D:/Dropbox/DSHandbook/analysis/databs_parsed/200901.csv"
basename(file_path_name)
dirname(file_path_name)

### 2. 파일 다루는 함수 --------------------------------------

list.files(R.home())
dir(R.home(), all.files=TRUE)  # includes hidden file 

file.exists("index.html")
file.create("./data/test.txt", showWarnings = TRUE)
file.create("./data/test1.txt","./data/test2.txt" )

file.remove("./data/test1.txt","./data/test2.txt")
unlink("./data/test.txt")


### 3. 파일에 대한 정보를 읽는 함수 ---------------------------------

file_info <- file.info(dir())
head(file_info)


# mtime : 최종수정시간

### 4. 임시 디렉터리와 파일 -----------------------------------

tempdir()

tmp1 <- tempfile(fileext=".png")
file.create(tmp1)
tmp1

### 5. 기타 유용한 함수 ------------------------------

x <- 37

# dynamic assign?  assign(), get()

for(i in 1:10) {
	vs <- paste0("x",i)
	assign(vs, i)
	print(get(vs))
}


options()
par()
sessionInfo()
