library(rvest)
library(XML)
library(magrittr)

## Race 3 Movie Review ##

race3 <- NULL
rev <- NULL
url <- "https://www.imdb.com/title/tt7431594/reviews?ref_=tt_ql_"
murl <- read_html(as.character(paste(url,1, sep = "")))
rev <- murl %>% html_nodes(".show-more__control") %>% html_text()
race3 <- c(race3,rev)

write(race3, "race.csv")
getwd()
