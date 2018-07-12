library(jsonlite)
library(lubridate)
library(parallel)
library(rtweet)
library(tidyverse)

library(keras)
library(tfdeploy)
library(tidytext)

#### supporting data
directory <- "/home/ubuntu/Documents/crash-dashboard"

if (Sys.info()[["user"]] == "roberttenorio") {
  directory <- "crash-dashboard"
}

word_index <- readRDS(str_c(directory, "tweet-processing/supporting_files/word_index.rds", sep = "/"))

all_tweets_file <- str_c(directory, "tweet-processing/supporting_files/all-tweets.json", sep = "/")

#### helper functions
# make integer sequences of tweet text
make_index_list <- function(x) {
  data_index <- c()
  
  for (i in 1:length(x)) {
    text <- x[[i]]
    x_seq <- text_to_word_sequence(text, split = " ", lower = TRUE)
    
    x_int <- c()
    for(n in x_seq) {
      int <- word_index$index[word_index$word %in% n]
      x_int <- c(x_int, int)
      x_int
    }
    data_index[[i]] <- x_int
  }
  data_index
}

# hot encode integer sequences
vectorize_sequences <- function(sequences, dimension = nrow(word_index)) {
  results <- matrix(0, nrow = length(sequences), ncol = dimension)
  
  for (i in 1:length(sequences)) {
    results[i, sequences[[i]]] <- 1
  }
  results
}

### Function for predicting new Tweets
predict_accident <- function(m) {
  tweet_index <- make_index_list(m[[1]])
  tweet_vectorized <- vectorize_sequences(tweet_index)
  
  prediction <- predict_savedmodel(tweet_vectorized, str_c(directory, 'tweet-processing/supporting_files/classify_lower_25_300_92', sep = "/"))
  probability <- prediction$predictions[[1]]$dense_64
  
  tibble(text = m[[1]], prob = round(probability*100, 0), date = m[[2]], status_id = m[[3]], geocoded = 0, day_id = m[[5]])
}

# load streamed tweets
read_json <- function(file_dir) {
  read_out <- tryCatch(tweets_lines <- readLines(file_dir),
                       error = function(e) e,
                       warning = function(w) w)
  
  tweets_lines <- readLines(file_dir)
  tweets_lines_noBlank <- tweets_lines[tweets_lines != ""]
  
  if(inherits(read_out, "warning")) {
    start_row <- 1
    end_row <- length(tweets_lines_noBlank) - 1
  }
  
  if(!inherits(read_out, "warning")) {
    start_row <- 1
    end_row <- length(tweets_lines_noBlank)
  }
  
  tweets_lines_noBlank <- tweets_lines_noBlank[start_row:end_row]
  
  tweets_json <- list()
  
  # the end tweet is sometimes not coded correctly so we get an error. I though the above code fixed that, 
  #but some messed up end tweets get through, hence the below code to make double sure
  
  for(i in seq_along(tweets_lines_noBlank)) {
    json_out <- tryCatch(tweets_json[[i]] <- fromJSON(tweets_lines_noBlank[i]),
                         error = function(e) e)
  }
  
  tweets_json
}

tweets_json <- read_json(all_tweets_file)

parse_tweet_text <- function(x) {
  tweet_names <- names(x)
  
  if(length(tweet_names) == 1) { # this is probably because the tweet was deleted
    parsed <- tibble(text = NA,
                     created_at = NA,
                     status_id = NA,
                     retweet_status = NA)
    
    return(parsed) # early return of function
  }
  
  date_string <- gsub("(^[[:upper:]]{1}[[:lower:]]{2} )|(\\+.{4} )", "", x$created_at)
  date <- strptime(date_string, format = "%b %d %H:%M:%S %Y", tz = "UTC")
  date <- lubridate::ymd_hms(date)
  
  ifelse("retweeted_status" %in% tweet_names,
         retweet <- x$retweeted_status$id_str,
         retweet <- NA)
  
  ifelse("retweeted_status" %in% tweet_names,
         tweet_text <- x$retweeted_status$text,
         tweet_text <- x$text)
  
  ifelse("extended_tweet" %in% tweet_names,
         tweet_text <- x$extended_tweet$full_text,
         tweet_text <- tweet_text)
  
  ifelse("retweeted_status" %in% tweet_names & "extended_tweet" %in% names(x$retweeted_status),
         tweet_text <- x$retweeted_status$extended_tweet$full_text,
         tweet_text <- tweet_text)
  
  parsed <- tibble(text = trimws(tweet_text, which = "both"),
                   created_at = date,
                   status_id = x$id_str,
                   retweet_status = retweet)
  
  parsed
}

tweets <- purrr::map(tweets_json, parse_tweet_text) %>% bind_rows %>%
  filter(!is.na(text)) %>%
  filter(!retweet_status %in% status_id) %>%
  filter(!duplicated(status_id, incomparables = NA)) %>%
  filter(!duplicated(retweet_status, incomparables = NA))

if(!"day_id" %in% names(tweets)) {
  tweets <- tweets %>%
    mutate(date = floor_date(created_at, unit = "days")) %>%
    group_by(date) %>%
    mutate(id = row_number()) %>%
    ungroup
}

noId_date <- unique(tweets$date[is.na(tweets$day_id)])
  
if(length(noId_date) != 0) {
  tweets_noId <- tweets %>%
    filter(date == noId_date) %>%
    mutate(day_id = row_number())
  
  tweets <- tweets %>%
    filter(!date == noId_date) %>%
    bind_rows(tweets_noId)
}

tweets <- tweets %>%
  select(-date)

### Set up clusters for parallel processing
cl <- makeCluster(detectCores()-1)
clusterEvalQ(cl, {
  library(keras)
  library(tfdeploy)
  library(tidyverse)
})

clusterExport(cl, c("directory", "word_index", "vectorize_sequences", "make_index_list", "predict_accident"))

if(file.exists(str_c(directory, "tweet-processing/supporting_files/results.rds", sep = "/"))) {
  
  results <- readRDS(str_c(directory, "tweet-processing/supporting_files/results.rds", sep = "/"))
  
  if(!is.null(tweets)) {
    
    new_tweets <- tweets %>% dplyr::filter(!status_id %in% results$status_id) %>%
      filter(!retweet_status %in% results$status_id)
    
    tweet_matrix <- as.matrix(new_tweets)
    tweet_split <- split(tweet_matrix, 1:nrow(tweet_matrix))
    
    if(nrow(new_tweets) != 0) {
      new_results <- parLapply(cl, tweet_split, predict_accident) %>% bind_rows
      results <- rbind(results, new_results)
      
      saveRDS(results, str_c(directory, "tweet-processing/supporting_files/results.rds", sep = "/"))
      stopCluster(cl)
    }
    
  }
  else {
    print("all-tweets.json is null")
  }
}


if(!file.exists(str_c(directory, "tweet-processing/supporting_files/results.rds", sep = "/"))) {
  
  if(!is.null(tweets)) {
    
    tweet_matrix <- as.matrix(tweets)
    tweet_split <- split(tweet_matrix, 1:nrow(tweet_matrix))
    
    results <- parLapply(cl, tweet_split, predict_accident) %>% bind_rows
    
    saveRDS(results, str_c(directory, "tweet-processing/supporting_files/results.rds", sep = "/"))
    
    stopCluster(cl)
  }
  else {
    print("all-tweets.json is null")
  }
}

q()