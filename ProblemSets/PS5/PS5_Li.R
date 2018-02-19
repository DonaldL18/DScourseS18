library(rvest)
library(dplyr)
#3
bestmovie <- read_html("http://www.imdb.com/chart/top?ref_=nv_mv_250_6")

feature_1<- bestmovie %>% html_nodes(".titleColumn a") %>% html_text
feature_2<- bestmovie %>% html_nodes(".imdbRating") %>% html_text

dataframe <- data.frame(feature_1, feature_2)

dataframe
View(dataframe)
write.csv(dataframe, "data.csv")

#4
install.packages("RedditExtractoR")
library(RedditExtractoR)

reddit_links <- reddit_urls("https://www.reddit.com/r/news/")
  search_terms   = "Mueller",
  page_threshold = 1)

Cats_on_Reddit <- get_reddit(search_terms = "Cat", regex_filter = "", subreddit = "Funny",
           cn_threshold = 1, page_threshold = 1, sort_by = "comments",
           wait_time = 2)
View(Cats_on_Reddit)


write.csv(Cats_on_Reddit, "cats.csv")
