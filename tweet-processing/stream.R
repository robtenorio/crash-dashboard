# collect tweets

library(rtweet)

directory <- "crash-dashboard"

stream_tweets(
  "RobertTenorio",
  timeout = Inf,
  parse = FALSE,
  file_name = str_c(directory, "tweet-processing/supporting_files/rob-tweets.json", sep = "/"),
  append = TRUE
)