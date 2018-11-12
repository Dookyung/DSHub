

## 1. Download flight dataset -----------------------------------------

#install.packages("RCurl")
library(RCurl)
library(data.table)

year <- 1987:2008

for (i in year){
	
	
	url <- paste0("http://stat-computing.org/dataexpo/2009/", i, ".csv.bz2")
	saved_file <- paste0("./data/flight/",i,".csv.bz2")
	print(i)
	
	if(!file.exists(saved_file)){
		download.file(url, destfile=saved_file, method="libcurl")	
	}
	
}

## 2. Download and extract flight dataset -----------------------------------------

for (i in year){
	
	
	url <- paste0("http://stat-computing.org/dataexpo/2009/", i, ".csv.bz2")
	saved_file <- paste0("./data/flight/",i,".csv")
	print(i)
	
	if(!file.exists(saved_file)){
		temp <- tempfile()
		download.file(url ,temp)
		bunzip2(temp, saved_file, remove = TRUE, skip = TRUE)
	}
	
}


## 3. Read data -----------------------------------------

i <- '2007'
file <- paste0("./data/flight/",i,".csv")

# base R - read.csv       98 seconds
system.time({
	dt <- read.csv(file)
})

# readr - read_csv        58 seconds
library(readr)
system.time({
	dt <- read_csv(file)
})


# data.table - fread      2.4 seconds
library(data.table)
system.time({
	dt <- fread(file)
})

# feather                 1.89 seconds
library(feather)
path <- paste0("./data/flight/",i,".feather")
write_feather(dt, path)
system.time({
	df <- read_feather(path)
})


## 4.SQL databases and R -----------------------------------------


### The portal_mammals database
# install.packages("RSQLite")
library(DBI)

db_path <- "./data/DB/"

# create folders to save  
mkdirs <- function(fp) {
	if(!file.exists(fp)) {
		mkdirs(dirname(fp))
		dir.create(fp)
	}
} 

mkdirs(db_path)

download.file(url = "https://ndownloader.figshare.com/files/2292171",
							destfile = "./data/DB/portal_mammals.sqlite", mode = "wb")




## Connecting to databases
library(dplyr)
library(dbplyr)

con <- DBI::dbConnect(RSQLite::SQLite(), "./data/DB/portal_mammals.sqlite")

# The RSQLite package allows R to interface with SQLite databases.
# This command does not load the data into the R session (as the read_csv() function did). Instead, it merely instructs R to connect to the SQLite database contained in the portal_mammals.sqlite file.


#connected to mammal db:
src_dbi(con)

# check tables(plots,species,surveys)
dbListTables(con)


# Querying the database with the SQL syntax.
# With this approach you can use any of the SQL queries we have seen in the database lesson.
tbl(con, sql("SELECT year, species_id, plot_id FROM surveys"))


# Querying the database with the dplyr syntax
surveys <- tbl(con, "surveys")
surveys %>%
	select(year, species_id, plot_id)

# In this case, the surveys object behaves like a data frame.
head(surveys, n = 10)

# However, some functions don’t work quite as expected. For instance, let’s check how many rows there are in total using nrow():
nrow(surveys)

# SQL translation
# Relational databases typically use a special-purpose language, Structured Query Language (SQL), to manage and query data.


# SELECT *
# 	FROM `surveys`
# LIMIT 10

# Behind the scenes, dplyr:
# 	
# translates your R code into SQL
# submits it to the database
# translates the database’s response into an R data frame

show_query(head(surveys, n = 10))

# we can delegate this translation to dplyr.


# Simple database queries

surveys %>%
	filter(weight < 5) %>%
	select(species_id, sex, weight)

# Laziness
# When working with databases, dplyr tries to be as lazy as possible:
# 	
# 	It never pulls data into R unless you explicitly ask for it.
# It delays doing any work until the last possible moment - it collects together everything you want to do and then sends it to the database in one step.


data_subset <- surveys %>%
	filter(weight < 5) %>%
	select(species_id, sex, weight)

data_subset %>%
	select(-sex)



#Just like the first select(species_id, sex, weight) call, the select(-sex) command is not executed by R. It is sent to the database instead. Only the final result is retrieved and displayed to you.

#To instruct R to stop being lazy, 
data_subset <- surveys %>%
	filter(weight < 5) %>%
	select(species_id, sex, weight) %>%
	collect()


data_subset


# Complex database queries

# (장점) dplyr 는 여러 DB에 동일한 문법으로 코딩


plots <- tbl(con, "plots")
plots


#The plot_id column also features in the surveys table:
	
surveys

# Because plot_id is listed in both tables, we can use it to look up matching records, and join the two tables.

plots %>%
	filter(plot_id == 1) %>%
	inner_join(surveys) %>%
	collect()

# Important Note: Without the collect() statement, only the first 10 matching rows are returned. By adding  collect(), the full set of 1,985 is retrieved.


# Challenge 1

## sql
# SELECT table.col, table.col
# FROM table1 JOIN table2
# ON table1.key = table2.key
# JOIN table3 ON table2.key = table3.key

## with dplyr syntax
species <- tbl(con, "species")

left_join(surveys, species) %>%
	filter(taxa == "Rodent") %>%
	group_by(taxa, year) %>%
	tally %>%
	collect()


## with SQL syntax
query <- paste("
SELECT a.year, b.taxa,count(*) as count
FROM surveys a
JOIN species b
ON a.species_id = b.species_id
AND b.taxa = 'Rodent'
GROUP BY a.year, b.taxa",
							 sep = "" )

tbl(con, sql(query))


# Challenge 2

#Write a query that returns the total number of rodents in each genus caught in the different plot types.
#Hint: Write a query that joins the species, plot, and survey tables together. The query should return counts of genus by plot type.


species <- tbl(con, "species")
genus_counts <- left_join(surveys, plots) %>%
	left_join(species) %>%
	group_by(plot_type, genus) %>%
	tally %>%
	collect()

species <- tbl(con, "species")
unique_genera <- left_join(surveys, plots) %>%
	left_join(species) %>%
	group_by(plot_type) %>%
	summarize(
		n_genera = n_distinct(genus)
	) %>%
	collect()


####  Creating a new SQLite database ----------------------

# species <- read_csv("data/species.csv")
# surveys <- read_csv("data/surveys.csv")
# plots <- read_csv("data/plots.csv")


# my_db_file <- "portal-database.sqlite"
# my_db <- src_sqlite(my_db_file, create = TRUE)

#Currently, our new database is empty, it doesn’t contain any tables:

# my_db

#To add tables, we copy the existing data.frames into the database one by one:

# copy_to(my_db, surveys)
# copy_to(my_db, plots)
# my_db


dbDisconnect(con)


## 5.flight data and databases -----------------------------------------
library(tidyverse)
library(DBI)
library(data.table)
con <- dbConnect(RSQLite::SQLite(), "./data/DB/flights.sqlite")
# feather                 1.89 seconds


year <- 1987:1990

# db insert        65s
system.time({
for (i in year){
	path <- paste0("./data/flight/",i,".csv")
	df <- fread(path, encoding = 'UTF-8')
	print(i)
	dbWriteTable(con, "flight", df, append=TRUE)
	}
})

# db query   			 96s
system.time({
	x <- dbReadTable(con, "flight")
})


dbListFields(con, "flight")

dbListTables(con)



dbDisconnect(con)



## 6. MonetDBLite for R -----------------------------------------


#install.packages("MonetDBLite")

library(DBI)

dbdir <- tempdir()
con <- dbConnect(MonetDBLite::MonetDBLite())

dbWriteTable(con, "mtcars", mtcars)
dbGetQuery(con, "SELECT MAX(mpg) FROM mtcars WHERE cyl = 8")

library(dplyr)
ms <- MonetDBLite::src_monetdblite(dbdir)
mt <- tbl(ms, "mtcars")
mt %>% filter(cyl == 8) %>% summarise(max(mpg))

dbDisconnect(con)
#############

library(DBI)
library(data.table)
dbdir <- "./data/DB"
con <- dbConnect(MonetDBLite::MonetDBLite(), dbdir)


year <- 1987:1990

# db insert      49.5s
system.time({
	for (i in year){
		path <- paste0("./data/flight/",i,".csv")
		df <- fread(path, encoding = 'UTF-8')
		print(i)
		dbWriteTable(con, "flight", df, append=TRUE)
	}
})

# db query      4.83s
system.time({
	x <- dbReadTable(con, "flight")
})


dbDisconnect(con, shutdown=TRUE)
MonetDBLite::monetdblite_shutdown()



## 6. data analysis with Database -----------------------------------------
