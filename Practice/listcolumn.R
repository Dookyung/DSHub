
1
c(1, 2, 3.14)

is.vector(1)

is.vector(c(1, 2, 3.14))

typeof(c(1L, 2L, 3L))

typeof(c(1, 2, 3.14))

typeof(c("a", "b", "c"))

typeof(c(TRUE, FALSE))

x <- c(1L, 2L, 3L)

x

class(x) <- "Date"

x

levels(x) <- c("Blue", "Brown", "Green")

class(x) <- "factor"

x

dim(x) <- c(3, 1)

x



(y <- list(a = c(1, 2, 3.14),
					 b = c("a", "b", "c"),
					 c = c(TRUE, FALSE, FALSE)))

typeof(y)

is.vector(y)

class(y) <- "data.frame"

rownames(y) <- c("1", "2", "3")



y

# Should this work?
y$d <- list(p = 1:3, q = TRUE, r = 0L)
y # yes

y$d



##  Data frames, Tibbles, and List Columns

data.frame(a = c(1, 2, 3.14),
					 b = c("a", "b", "c"),
					 c = c(TRUE, FALSE, FALSE))

data.frame(list(a = c(1, 2, 3.14),
								b = c("a", "b", "c"),
								c = c(TRUE, FALSE, FALSE)))



data.frame(a = c(1, 2, 3.14),
					 b = c("a", "b", "c"),
					 c = c(TRUE, FALSE, FALSE),
					 d = list(p = 1:3, q = TRUE, r = 0L))

z <- data.frame(a = c(1, 2, 3.14),
								b = c("a", "b", "c"),
								c = c(TRUE, FALSE, FALSE))


z$d <- list(p = 1:30, q = TRUE, r = 0L)
z


library(tibble)
class(z) <- c("tbl_df", "tbl", "data.frame")


z
class(z) <- c("tbl_df", "tbl", "data.frame")

as-tibble(z)


data.frame(a = c(1, 2, 3.14),
					 b = c("a", "b", "c"),
					 c = c(TRUE, FALSE, FALSE),
					 d = list(p = 1:3, q = TRUE, r = 0L))

tribble(~a, ~b, ~c, ~d,
				1, "a", TRUE, p = 1:3,
				2, "b", FALSE, q = TRUE,
				3.14, "c", FALSE, r = 0L)

babynames %>%
	select(-prop)

babynames %>%
	select(-prop) %>%
	filter(!is.na(n))

babynames %>%
	select(-prop) %>%
	filter(!is.na(n)) %>%
	group_by(year, sex)

babynames %>%
	select(-prop) %>%
	filter(!is.na(n)) %>%
	group_by(year, sex) %>%
	summarise(N = sum(n))

babynames %>%
	select(-prop) %>%
	filter(!is.na(n)) %>%
	group_by(year, sex) %>%
	summarise(N = sum(n)) %>%
	ggplot() +
	geom_line(mapping =
							aes(x = year,
									y = N,
									color = sex))

y



y %>% mutate(asq = sqrt(a))

y



# Babynames that appeared each year


everpresent <- babynames %>%
	group_by(name, sex) %>%
	summarise(years = n()) %>%
	ungroup() %>%
	filter(years == max(years))

babynames <- babynames %>%
	semi_join(everpresent)


joe <- babynames %>%
	filter(name == "Joe",
				 sex == "M")

joe


joe <- babynames %>%
	filter(name == "Joe",
				 sex == "M")
joe %>%
	ggplot(mapping =
				 	aes(x = year,
				 			y = prop)) +
	geom_line() +
	geom_smooth(method = lm)


# What is the slope?
# What is the R-squared? (fit)


joe_mod <- lm(prop ~ year, data = joe)
joe_mod

coef(joe_mod)

pluck(coef(joe_mod), "year")


library(broom)
glance(joe_mod)

babynames

babynames %>%
	group_by(name, sex)

babynames %>%
	group_by(name, sex) %>%
	nest()

# Sanity check: What is in one of these cells?

babynames %>%
	group_by(name, sex) %>%
	nest() %>%
	pluck("data") %>%
	pluck(1)

babynames %>%
	group_by(name, sex) %>%
	nest() %>%
	mutate(
		model = map(data,
								~lm(prop ~ year,
										data = .x))
	)


babynames %>%
	group_by(name, sex) %>%
	nest() %>%
	mutate(
		model = map(data,
								~lm(prop ~ year,
										data = .x)),
		slope = map_dbl(model,
										~pluck(coef(.x),
													 "year"))
	)


babymods <- babynames %>%
	group_by(name, sex) %>%
	nest() %>%
	mutate(
		model = map(data,
								~lm(prop ~ year,
										data = .x)),
		slope = map_dbl(model,
										~pluck(coef(.x),
													 "year")),
		r_squared = map_dbl(model,
												~pluck(glance(.x),"r.squared")))


# Which names increased the most?

babymods %>%
	arrange(desc(slope)) %>%
	head(5) %>%
	unnest(data) %>%
	ggplot(mapping = aes(x = year, y = prop)) +
	geom_line(mapping = aes(color = name))



# Which names were the least linear?

babymods %>%
	arrange(r_squared) %>%
	head(5) %>%
	unnest(data) %>%
	ggplot(mapping = aes(x = year, y = prop)) +
	geom_line(mapping = aes(color = name))





## list column 저장방법



## List column 만드는 방법


# nest()
n_iris <- iris %>% group_by(Species) %>% nest()
n_iris

# unnest()

n_iris %>% unnest()





## 1. Make a list column


n_iris <- iris %>%
	group_by(Species) %>%
	nest()
n_iris



tribble( ~max, ~seq,
				 3, 1:3,
				 4, 1:4,
				 5, 1:5)



## 2. Work with list columns

mod_fun <- function(df)
	lm(Sepal.Length ~ ., data = df)

m_iris <- n_iris %>%
	mutate(model = map(data, mod_fun))

m_iris


	
## 3. Simplify the list column
	

b_fun <- function(mod)
	coefficients(mod)[[1]]
m_iris %>% transmute(Species,
										 beta = map_dbl(model, b_fun))

## list column 저장방법
	
	

## WORK WITH LIST COLUMNS

n_iris %>% mutate(n = map(data, dim))


m_iris %>% mutate(n = map2(data, model, list))


m_iris %>% mutate(n = pmap(list(data, model, data), list))










