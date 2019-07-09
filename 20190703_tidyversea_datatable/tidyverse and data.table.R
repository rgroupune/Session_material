## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, dev = "CairoSVG")
pacman::p_load(tidyverse, bench, microbenchmark, broom, knitr, bench, cranlogs, data.table, kableExtra)
#' 
#' # Example dataset diamonds
#' 
## ------------------------------------------------------------------------
str(diamonds)

#' 
#' ---
#' 
#' # Tibble
#' 
## ------------------------------------------------------------------------
(diamonds <- as_tibble(diamonds))

#' 
#' ---
#' 
#' # Data Table Display
#' 
## ------------------------------------------------------------------------
(diamonds_dt <- as.data.table(diamonds))

#' 
#' 
#' ---
#' 
#' # Take a subset of columns
#' 
#' Take only the `carat` `cut` and `clarity` columns.
#' 
#' 
#' 
#' ### dplyr
#' 
## ------------------------------------------------------------------------
select(diamonds,carat, cut, clarity)

#' 
#' 
#' 
#' ### data.table
#' 
## ------------------------------------------------------------------------
diamonds_dt[, .(carat, cut, clarity)]

#' 
#' 
#' ## 
#' 
#' ---
#' 
#' # Modify a columns
#' 
#' Conver the x column to inches (why? Maybe your American)
#' 
#' ### dplyr
#' 
#' 
#' Convert the length column `x` from mm to inches and then create `a` as half `x`.
## ------------------------------------------------------------------------
mutate(diamonds, x = x / 25.4, a = x / 2)

#' 
#' ### data.table
#' 
#' NB: Inplace modify
#' 
## ------------------------------------------------------------------------
temp <- diamonds_dt
temp[, x := x / 25.4]
temp

#' 
#' 
#' ---
#' 
#' #  Sorting
#' 
#' Sort the data by highest price first
#' 
#' ### dplyr
#' 
#' 
## ------------------------------------------------------------------------
arrange(diamonds, desc(price))

#' 
#' 
#' ### data.table
#' 
## ------------------------------------------------------------------------
diamonds_dt[order(-price)]

#' 
#' 
#' ---
#' 
#' # Group By
#' 
#' * Splits the data into chunks to group together to operate on within groups.
#' * For example, the count, mean, min and max of the prices by cut.
#' 
#' 
#' ### dplyr
#' 
## ------------------------------------------------------------------------
summarise(
  group_by(diamonds, cut),
  n = n(), mean(price), min(price), max(price))

#' 
#' ### data.table
#' 
## ------------------------------------------------------------------------
diamonds_dt[
  , .(n = .N, mean = mean(price), min = min(price), max = max(price)),
  by = cut]

#' 
#' 
#' # pipe example 
#' 
#' Previous group by using a pipe before we pass the data into a formatter.
#' 
#' 
## ------------------------------------------------------------------------
diamonds %>% 
  group_by(cut,color) %>% 
  summarise(n = n(), mean = mean(price), min = min(price), max = max(price)) %>%
  DT::datatable(class = 'compact')

#' 
#' # Static
#' 
## ------------------------------------------------------------------------
diamonds %>% 
  group_by(cut,color) %>% 
  summarise(n = n(), mean = mean(price), min = min(price), max = max(price)) %>%
  kable() %>%
  kable_styling()

#' 
#' 
#' ---
#' 
#' # Window functions
#' 
#' Group by functions can also be applied per row. 
#' 
#' Say we want to retrieve the top 0.1% of diamonds by price for each cut type. 
#' 
## ------------------------------------------------------------------------
diamonds %>% group_by(cut) %>%
  filter(price > quantile(price, 0.999))

#' 
#' ---
#' 
#' # Standard normal of prices by cut
#' 
## ------------------------------------------------------------------------
diamonds %>% 
  group_by(cut) %>% 
  mutate(price_std = (price - mean(price)) / sd(price)) %>% 
  select(cut, price, price_std) %>% 
  ungroup() %>% 
  sample_n(20)
  
#' ---
#' 
#' # tidy examples
#' 
#' Convert diamonds dimensions to long format
#' 
## ------------------------------------------------------------------------
diamonds %>% select(price, x,y,z) %>% gather(dim, measure, -price) %>% head(10)

#' 
#' ---
#' 
#' # Plot
#' 
## ----fig.height = 6, warning=FALSE, dev = "CairoPNG", res = 250----------
diamonds %>% select(price, x,y,z) %>% gather(dim, measure, -price) %>% ggplot(aes(measure, price, colour = dim)) + geom_point() + scale_x_log10() + scale_y_log10()

#' ---
#' 
#' # Databases (dbplyr)
#' 
#' * `dplyr` can work with databases using the same commands as previous.
#' 
#' For example, say that we placed the diamonds data in a sqlite database
#' 
#' 
## ------------------------------------------------------------------------
library(DBI)
file.remove("diamonds_db.sqlite3")
diamonds_db <- dbConnect(RSQLite::SQLite(), "diamonds_db.sqlite3")
dbWriteTable(diamonds_db, "diamonds", diamonds)

#' 
#' then we can query this data without retreiving it into memory
## ------------------------------------------------------------------------
diamonds_db %>% 
  tbl("diamonds") %>% 
  group_by(color, cut) %>% 
  summarise(pricemean = mean(price, na.rm=TRUE))

rm(diamonds_db)
gc()

#' 
