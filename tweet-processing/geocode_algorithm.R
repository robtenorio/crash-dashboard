# Crashmap Algorithm

# TO DO
# 1. For multi-word prepositions (next to), check "next" is before "to" in word number
# 2. Preposition heirarchy, but maybe later on -- towards end?
# 3. Add option: if have exact match, only use that. Just to test...

# Packages ---------------------------------------------------------------------
packages <- c('magrittr','lubridate', 'dplyr', 'tidyr', 'readr', 
              'purrr', 'ggplot2', 'tidytext', 'stringr', 'ngram', 'hunspell',
              'stringdist','doBy','raster','rgeos','parallel','jsonlite','doBy',
              'ngram')

purrr::walk(packages, library, character.only=TRUE)

counter.number <- 1

# Main Algorithm ---------------------------------------------------------------
crashmap_algorithm <- function(tweets,
                               date,
                               status_id,
                               landmark.dictionary,
                               only.use.first.landmark,
                               only.use.first.landmark.and.use.longest,
                               landmark.match.type.rank.use,
                               landmark.match.type.rank,
                               landmark.match.more.types,
                               remove.street.names,
                               prepositions.list,
                               prioritize.exact.match,
                               fuzzy_match_accident,
                               fuzzy_match_accident.dist,
                               fuzzy_match_accident.onlymisspelled,
                               fuzzy_match_accident.min.word.length,
                               multiple_landmark_distance_threshold_km,
                               fuzzy_match_landmark,
                               fuzzy_match_landmark.dist,
                               fuzzy_match_landmark.min.word.length,
                               remove.text.after.toward,
                               google_map_api,
                               supporting.files.path,
                               levenstein.word,
                               levenstein.word.min.words.fuzzy,
                               levenstein.word.max.word.distance,
                               levenstein.word.fuzzy.match,
                               levenstein.word.fuzzy.match.dist,
                               levenstein.word.fuzzy.match.min.word.length,
                               levenstein.word.contiguous,
                               mc.cores){
  
  # 1. Prep certain variable inputs --------------------------------------------
  
  ### Make dataframe of tweets
  df.tweets <- as.data.frame(tweets)
  names(df.tweets) <- c("tweet_original")
  df.tweets$tweet_original <- as.character(df.tweets$tweet_original)
  df.tweets$date <- date
  df.tweets$status_id <- status_id
  df.tweets$id <- 1:nrow(df.tweets)
  
  ### If not doing a fuzzy match, set other parameters to NA
  if(fuzzy_match_accident == FALSE){
    fuzzy_match_accident.dist <-  NA
    fuzzy_match_accident.onlymisspelled <- NA
    fuzzy_match_accident.min.word.length <- NA
  }
  
  if(fuzzy_match_landmark == FALSE){
    fuzzy_match_landmark.dist <- NA
    fuzzy_match_landmark.min.word.length <- NA
  }
  
  # 2. Load and prep support files ---------------------------------------------
  
  ### Supporting files
  categories <- read.csv(paste(supporting.files.path,"accident_words_v1.csv",sep=""))
  contractions <- read.csv(paste(supporting.files.path,"streetContractions.csv",sep=""),header = FALSE)  
  streets <- read.csv(paste(supporting.files.path,"Nairobi_Streets.csv",sep=""))
  landmarks_generic <- read.csv(paste(supporting.files.path,"Generic Landmarks/generic_landmarks_google_maps.csv",sep=""))
  
  landmarks <- import.landmarks(landmark.dictionary, supporting.files.path)
  
  # Landmark Cleaning
  # Remove punctation; replace with space
  #landmarks$Landmark <- gsub('[[:punct:] ]+',' ',landmarks$Landmark)
  landmarks <- landmarks[!landmarks$Landmark %in% "",]
  landmarks <- landmarks[!landmarks$Landmark %in% " ",]
  
  landmark_list <- as.character(landmarks$Landmark)
  landmarks_list_space <- paste0("\\b", landmark_list, "\\b")
  
  ### Stop words
  stop_words_list <- tm::stopwords()
  
  # Remove certain words from stop_words list; "and": Need for places like "Nice and lovely." Could also remove stop words from landmarks?
  stop_words_list <- stop_words_list[!(stop_words_list %in% "and")]
  
  stop_words <- paste0("\\b", stop_words_list, "\\b")
  stop_words <- paste(stop_words, collapse = "|")
  
  # 3. Clean Tweets ------------------------------------------------------------
  df.tweets <- clean_tweets(df.tweets, stop_words)
  
  # 5. Geocoding: Exact and Fuzzy Match ----------------------------------------
  
  ##### Equal distant projection centered on nairobi
  equal.distant.projection <- paste("+proj=aeqd +lat_0=",-1.283333," +lon_0=",36.816667, sep="")
  
  ##### Remove words including and after toward if toward comes after 1) accident word and 2) a landmark
  if(remove.text.after.toward){
    df.tweets$tweet_clean <- unlist(lapply(1:nrow(df.tweets), remove.text.after.toward.fun, df.tweets, paste0("\\b", "toward", "\\b")))
  }
  
  ##### Remove street Names
  # WILL FAIL with "kenyatta avenue roundabout. Need workaround / exceptions
  # [if contains word, keep...]
  if(remove.street.names){
    street.names <- read.csv(str_c(supporting.files.path, "google_street_names.csv", sep = ""))
    street.names <- street.names$street_names %>% as.character
    
    # Can't input too long of a list, so loop through
    iterate.by <- 100
    start.list <- seq(from=1, to=length(street.names), by=iterate.by)
    for(start.i in start.list){
      end.i <- start.i + iterate.by - 1
      if(end.i > length(street.names)){
        end.i <- length(street.names)
      }
      
      df.tweets$tweet_clean <- paste0("\\b",street.names[start.i:end.i],"\\b", collapse="|") %>%
        gsub("",df.tweets$tweet_clean)
    }
    df.tweets$tweet_clean <- str_squish(df.tweets$tweet_clean)
  }
  
  ##### Landmark Matching
  landmark_match_df <- mclapply(df.tweets$tweet_clean, 
                                landmark_match_FUN,
                                landmark_list, 
                                landmarks_list_space,
                                fuzzy_match_landmark,
                                fuzzy_match_landmark.dist, 
                                fuzzy_match_landmark.min.word.length, 
                                landmarks, 
                                equal.distant.projection, 
                                multiple_landmark_distance_threshold_km, 
                                only.use.first.landmark, 
                                only.use.first.landmark.and.use.longest, 
                                prepositions.list,
                                prioritize.exact.match,
                                landmark.match.type.rank.use,
                                landmark.match.type.rank,
                                landmark.match.more.types,
                                mc.cores=mc.cores) %>% bind_rows
  
  df.tweets <- cbind(df.tweets, landmark_match_df)
  
  df.tweets$tweet_landmark_lat <- as.numeric(as.character(df.tweets$tweet_landmark_lat))
  df.tweets$tweet_landmark_lon <- as.numeric(as.character(df.tweets$tweet_landmark_lon))
  df.tweets$landmarks_in_tweet <- as.character(df.tweets$landmarks_in_tweet)
  df.tweets$landmarks_used_to_geocode <- as.character(df.tweets$landmarks_used_to_geocode)
  df.tweets$landmarks_in_tweet_originaltext <- as.character(df.tweets$landmarks_in_tweet_originaltext)
  
  df.tweets$tweet_landmark_lat_final <- df.tweets$tweet_landmark_lat
  df.tweets$tweet_landmark_lon_final <- df.tweets$tweet_landmark_lon
  
  
  # 6. Geocoding: Google API ---------------------------------------------------
  
  ### Google API
  if(google_map_api){
    
    # Street Exact Match
    street_dictionary <- as.character(streets$NAME)
    street_match_df <- mclapply(df.tweets$tweet_clean, street_match, street_dictionary, mc.cores=mc.cores) %>% bind_rows
    df.tweets <- cbind(df.tweets, street_match_df)
    df.tweets$number_streets_in_tweet <- as.numeric(df.tweets$number_streets_in_tweet)
    
    # Generic Landmark Match
    landmarks_generic_dictionary <- as.character(landmarks_generic$Landmark)
    df.tweets$landmark_generic <- unlist(mclapply(df.tweets$tweet_clean, generic_landmark_match, landmarks_generic_dictionary, mc.cores=mc.cores))
    
    placeUrl <- "https://maps.googleapis.com/maps/api/place/textsearch/json?query="
    key <- "AIzaSyCUNLKgQRoRwlYR7xtZ6_nA9owjxfpFvec" 
    google_API_latlon_df <- mclapply(1:nrow(df.tweets), google_API_geocode, df.tweets, mc.cores=mc.cores) %>% bind_rows
    
    df.tweets <- cbind(df.tweets, google_API_latlon_df)
    
    df.tweets$tweet_landmark_lat_final[is.na(df.tweets$tweet_landmark_lat_final)] <- df.tweets$google_API_lat[is.na(df.tweets$tweet_landmark_lat_final)]
    df.tweets$tweet_landmark_lon_final[is.na(df.tweets$tweet_landmark_lon_final)] <- df.tweets$google_API_lon[is.na(df.tweets$tweet_landmark_lon_final)]
  }
  
  # 7. Geocoding: Levenstein Word Match ----------------------------------------
  if(levenstein.word){
    
    # 1. Clean Landmarks
    landmarks_simple <- landmarks
    
    # Remove stop words from landmarks
    stop_words_all <- tm::stopwords()
    stop_words_all <- paste0("\\b", stop_words_all, "\\b")
    stop_words_all <- paste(stop_words_all, collapse = "|")
    landmarks_simple$Landmark_noStopWords <- str_replace_all(landmarks_simple$Landmark, stop_words_all,"")
    
    # Remove landmark if only one word [after removing stop words]
    landmarks_simple <- landmarks_simple[str_count(landmarks_simple$Landmark_noStopWords, "\\S+") > 1,]
    
    # 2. Create dataframe of landmarks and words
    landmark_simple_list <- landmarks_simple$Landmark_noStopWords
    landmark_simple_list_words <- strsplit(landmark_simple_list, " ")
    landmark_word_df <- lapply(1:length(landmark_simple_list), list_to_df, landmark_simple_list, landmark_simple_list_words) %>% bind_rows
    names(landmark_word_df) <- c("landmark_word","id","landmark")
    landmark_word_df$landmark_length <- 1
    
    # 3. Create list of unique words in landmark dictionary
    landmark_words_unique <- unique(landmark_word_df$landmark_word)
    
    # 4. Implement Function 
    # Subset tweets dataframe to those that haven't been geocoded AND no landmarks in tweet (even if multiple landmarks)
    df.tweets_nogeo <- df.tweets[is.na(df.tweets$tweet_landmark_lat_final) &
                                   is.na(df.tweets$landmarks_in_tweet),]
    
    if(nrow(df.tweets_nogeo) > 0){
      
      # Levenstein Word Function
      tweet_df_leven_word <- data.frame()
      for(i in 1:nrow(df.tweets_nogeo)) {
        tweet_df_leven_word_i <- levenstein_word(tweet = df.tweets_nogeo$tweet_clean[i],
                                                 id = df.tweets_nogeo$id[i],
                                                 landmark_word_df = landmark_word_df,
                                                 landmarks_simple = landmarks_simple,
                                                 equal.distant.projection = equal.distant.projection,
                                                 landmark_words_unique = landmark_words_unique,
                                                 levenstein.word.fuzzy.match = levenstein.word.fuzzy.match,
                                                 levenstein.word.fuzzy.match.dist = levenstein.word.fuzzy.match.dist,
                                                 levenstein.word.fuzzy.match.min.word.length = levenstein.word.fuzzy.match.min.word.length,
                                                 levenstein.word.contiguous = levenstein.word.contiguous,
                                                 levenstein.word.min.words.fuzzy = levenstein.word.min.words.fuzzy,
                                                 levenstein.word.max.word.distance = levenstein.word.max.word.distance,
                                                 multiple_landmark_distance_threshold_km = multiple_landmark_distance_threshold_km)
        tweet_df_leven_word <- rbind(tweet_df_leven_word, tweet_df_leven_word_i)
        
        }
      
      # Merge results back to dataframe with all tweets
      names(tweet_df_leven_word) <- c("lat_leven_word",
                                      "lon_leven_word","word_in_tweets_matched_leven_word",
                                      "landmarks_leven_word","landmarks_close_leven_word",
                                      "multiple_landmarks_max_dist",
                                      "multiple_landmarks_mean_dist",
                                      "multiple_landmarks_min_dist",
                                      "multiple_landmarks_number",
                                      "multiple_landmarks_number_pairs",
                                      "multiple_landmarks_num_pairs_below_thresh",
                                      "id")
      
      df.tweets <- merge(df.tweets, tweet_df_leven_word, by="id",all.x=T)
      
      df.tweets$multiple_landmarks_max_dist <- as.numeric(as.character(df.tweets$multiple_landmarks_max_dist))
      df.tweets$multiple_landmarks_mean_dist <- as.numeric(as.character(df.tweets$multiple_landmarks_mean_dist))
      df.tweets$multiple_landmarks_min_dist <- as.numeric(as.character(df.tweets$multiple_landmarks_min_dist))
      
      df.tweets$lat_leven_word <- as.numeric(as.character(df.tweets$lat_leven_word))
      df.tweets$lon_leven_word <- as.numeric(as.character(df.tweets$lon_leven_word))
      
      df.tweets$tweet_landmark_lat_final[is.na(df.tweets$tweet_landmark_lat_final)] <- df.tweets$lat_leven_word[is.na(df.tweets$tweet_landmark_lat_final)]
      df.tweets$tweet_landmark_lon_final[is.na(df.tweets$tweet_landmark_lon_final)] <- df.tweets$lon_leven_word[is.na(df.tweets$tweet_landmark_lon_final)]
    }
  }
  
  # Return list of data and parameters -----------------------------------------
  return(list(data = df.tweets,
              landmark.dictionary = landmark.dictionary,
              fuzzy_match_accident = fuzzy_match_accident,
              fuzzy_match_accident.dist = fuzzy_match_accident.dist,
              fuzzy_match_accident.onlymisspelled = fuzzy_match_accident.onlymisspelled,
              fuzzy_match_accident.min.word.length=fuzzy_match_accident.min.word.length,
              multiple_landmark_distance_threshold_km = multiple_landmark_distance_threshold_km,
              fuzzy_match_landmark = fuzzy_match_landmark,
              fuzzy_match_landmark.dist = fuzzy_match_landmark.dist,
              fuzzy_match_landmark.min.word.length=fuzzy_match_landmark.min.word.length,
              google_map_api=google_map_api,
              levenstein.word.fuzzy.match=levenstein.word.fuzzy.match))
  
}
  
# Functions: Importing Data ----------------------------------------------------

# original, original.clean, coders.clean, original.with.coders, original.with.coders.clean, google.maps, original.with.coders.clean.google.maps
import.landmarks <- function(landmark.dictionary, supporting.files.path){
  
  ### Landmarks
  if(landmark.dictionary %in% "original"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/landmarksOrig.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "original.clean"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/landmarksOrig_clean.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "coders.clean"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/landmarks_coders_Cleaned_clean.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "original.with.coders"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/landmarksOrig_withCodersLandmarks.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "original.with.coders.clean"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/landmarks_original_coders_clean.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "google.maps"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/landmarks_google_maps.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "google.maps.old"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/landmarks_google_maps_old.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "google.maps.simple.0.25km"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/google_maps_ngrams_0.25_distthresh_km.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "google.maps.simple.0.5km"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/google_maps_ngrams_0.5_distthresh_km.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "google.maps.simple.0.75km"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/google_maps_ngrams_0.75_distthresh_km.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "google.maps.simple.1km"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/google_maps_ngrams_1_distthresh_km.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "google.maps.simple.1.25km"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/google_maps_ngrams_1.25_distthresh_km.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "google.maps.simple.1.5km"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/google_maps_ngrams_1.5_distthresh_km.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "google.maps.simple.1.75km"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/google_maps_ngrams_1.75_distthresh_km.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "google.maps.simple.2km"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/google_maps_ngrams_2_distthresh_km.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "adj_google.maps.bitringrams.0.25km"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/adj_google_maps_bitri_0.25_distthresh_km.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "adj_google.maps.bitringrams.0.5km"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/adj_google_maps_bitri_0.5_distthresh_km.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "adj_google.maps.bitringrams.0.75km"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/adj_google_maps_bitri_0.75_distthresh_km.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "adj_google.maps.bitringrams.1km"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/adj_google_maps_bitri_1_distthresh_km.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "adj_google.maps.bitringrams.1.25km"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/adj_google_maps_bitri_1.25_distthresh_km.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "adj_google.maps.bitringrams.1.5km"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/adj_google_maps_bitri_1.5_distthresh_km.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "adj_google.maps.bitringrams.1.75km"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/adj_google_maps_bitri_1.75_distthresh_km.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "adj_google.maps.bitringrams.2km"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/adj_google_maps_bitri_2_distthresh_km.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "google.maps.bitringrams.0.25km"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/google_maps_bitri_0.25_distthresh_km.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "google.maps.bitringrams.0.5km"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/google_maps_bitri_0.5_distthresh_km.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "google.maps.bitringrams.0.75km"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/google_maps_bitri_0.75_distthresh_km.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "google.maps.bitringrams.1km"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/google_maps_bitri_1_distthresh_km.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "google.maps.bitringrams.1.25km"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/google_maps_bitri_1.25_distthresh_km.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "google.maps.bitringrams.1.5km"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/google_maps_bitri_1.5_distthresh_km.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "google.maps.bitringrams.1.75km"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/google_maps_bitri_1.75_distthresh_km.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "google.maps.bitringrams.2km"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/google_maps_bitri_2_distthresh_km.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "original.with.coders.clean.google.maps"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/landmarks_original_coders_clean_google.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "dummy"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/landmarks_dummy.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "truth_1"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/truth_landmarks_1.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "truth_2"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/truth_landmarks_2.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "truth_3"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/truth_landmarks_3.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "truth_4"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/truth_landmarks_4.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "truth_1e"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/truth_landmarks_1e.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "truth_2e"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/truth_landmarks_2e.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "truth_3e"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/truth_landmarks_3e.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "truth_4e"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/truth_landmarks_4e.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "truth_12e"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/truth_landmarks_12e.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "truth_13e"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/truth_landmarks_13e.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "truth_14e"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/truth_landmarks_14e.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "truth_23e"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/truth_landmarks_23e.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "truth_24e"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/truth_landmarks_24e.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "truth_34e"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/truth_landmarks_34e.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "truth_123e"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/truth_landmarks_123e.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "truth_124e"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/truth_landmarks_124e.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "truth_134e"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/truth_landmarks_134e.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "truth_234e"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/truth_landmarks_234e.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "truth_1234e"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/truth_landmarks_1234e.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "osm"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/landmarks_osm.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "google_maps_bitri_cluster_0.5dist_0.9close"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/google_maps_bitri_0.5_closethresh_0.9_clusterpercent.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "adj_google_maps_bitri_cluster_0.5dist_0.9close"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/adj_google_maps_bitri_0.5_closethresh_0.9_clusterpercent.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  if(landmark.dictionary %in% "adj2_google_maps_bitri_cluster_0.5dist_0.9close"){
    landmarks <- read.csv(paste(supporting.files.path,"Landmarks/adj2_google_maps_bitri_0.5_closethresh_0.9_clusterpercent.csv",sep="")) %>% dplyr::rename(landmarkLat = Lat, landmarkLon = Lon)
  }
  
  return(landmarks)
}

# Functions: Cleaning Tweets ---------------------------------------------------
contractionClean <- function(input){
  input %>% mutate(X1 = tolower(X1), X2 = tolower(X2)) %>% 
    tidyr::fill(X1, .direction =  "down")  %>% 
    rename(label = X1, abv = X2) %>%
    mutate(abv = paste0("\\b", abv, "\\b")) -> output
  
  contractionsName <- as.character(output$label)
  names(contractionsName) <- output$abv
  return(contractionsName)
}

clean_tweets <- function(input, stop_words){
  
  input_clean <- input %>%
    mutate(tweet_clean = str_to_lower(tweet_original)) 
  
  input_clean <- input_clean %>%
    mutate_at(.vars = vars(tweet_clean), .funs = function(x) str_replace_all(x, "via @[a-z_,A-Z_,0-9_]*","")) %>% 
    mutate_at(.vars = vars(tweet_clean), .funs = function(x) str_replace_all(x, "@[a-z_,A-Z_,0-9_]* ", "")) %>% 
    mutate_at(.vars = vars(tweet_clean), .funs = function(x) str_replace_all(x, "\n", "")) %>% 
    mutate_at(.vars = vars(tweet_clean), .funs = function(x) str_replace_all(x, "~", "")) %>% 
    mutate_at(.vars = vars(tweet_clean), .funs = function(x) str_replace_all(x, "\\b(http|https)://t.co/[0-9,A-Z, a-z]*\\b", "")) %>% 
    mutate_at(.vars = vars(tweet_clean), .funs = function(x) str_replace_all(x, "\\b(http|https)://t.co/[0-9,A-Z, a-z] ", "")) %>% 
    mutate_at(.vars = vars(tweet_clean), .funs = function(x) str_replace_all(x, "\\b(http|https)://t.co\\b", "")) %>% 
    mutate_at(.vars = vars(tweet_clean), .funs = function(x) str_replace_all(x, "\\b(http|https):", "")) %>% 
    mutate_at(.vars = vars(tweet_clean), .funs = function(x) str_replace_all(x, "~more ⇢", "")) %>% 
    mutate_at(.vars = vars(tweet_clean), .funs = function(x) str_replace_all(x, "(RT|rt) @[a-z,A-Z,0-9, _]*:", "")) %>% 
    mutate_at(.vars = vars(tweet_clean), .funs = function(x) str_replace_all(x, "^[0-9][0-9]\\:[0-9][0-9]", "")) %>% 
    mutate_at(.vars = vars(tweet_clean), .funs = function(x) str_replace_all(x, "[[:punct:]]", " ")) %>% 
    mutate_at(.vars = vars(tweet_clean), .funs = function(x) trimws(x)) 
  
  # Amperstand with and
  input_clean <- input_clean %>%
    mutate_at(.vars = vars(tweet_clean), .funs=function(x) str_replace_all(x, " amp "," and "))
  
  #input_consistent_contractons <- input_clean %>%
  #  mutate_at(.vars = vars(tweet_clean), .funs = function(x) str_replace_all(x, contractionsName))
  
  input_clean <- input_clean %>%
    mutate_at(.vars = vars(tweet_clean), .funs = function(x) str_replace_all(x, stop_words,""))
  
  # Remove non-ASCII characters
  input_clean$tweet_clean <- iconv(input_clean$tweet_clean, "latin1", "ASCII", sub="")
  
  # Remove unnecessary whitespace
  input_clean$tweet_clean <- str_squish(input_clean$tweet_clean)
  
  input_clean <- input_clean %>% 
    mutate(rowid = 1:nrow(input))
  
  return(input_clean)
}

# Functions: Categorize Tweets -------------------------------------------------

### Exact Word Matches
category_match <- function(tweet, accident_words_space){
  cat.match <- str_extract(tweet, accident_words_space)
  cat.match <- cat.match[!is.na(cat.match)]
  cat.match <- paste(cat.match, collapse=";")
  if(identical(cat.match,character(0))){
    cat.match <- ""
  }
  return(cat.match)
}

### Levenstein Word Matches
category_match_fuzzy <- function(tweet, accident_words, fuzzy_match_accident.onlymisspelled, fuzzy_match_accident.min.word.length, fuzzy_match_accident.dist){
  
  # Prep list of words
  tweet_words <- strsplit(tweet, split=" ")[[1]]
  
  # Limit list to only misspelled words
  if(fuzzy_match_accident.onlymisspelled){
    tweet_words <- hunspell(tweet_words) %>% 
      unlist
  }
  
  # Limit list to words of a certain length
  tweet_words <- tweet_words[nchar(tweet_words) >= fuzzy_match_accident.min.word.length]
  
  # Only check fuzzy matches if words in word list
  if(identical(tweet_words, character(0))){
    matched_words_incorrect_spelling <- ""
    matched_words_correct_spelling <- ""
  } else{
    
    # Fuzzy Match Algorithm; Returns location in dictionary of closest matched word
    category_match_position <- amatch(tweet_words, accident_words, maxDist = fuzzy_match_accident.dist, method="lv") 
    
    # List of words in tweet with matches
    matched_words_incorrect_spelling <- tweet_words[!is.na(category_match_position)] %>%
      paste(collapse=";")
    
    # List of words in dictionary with matches
    category_match_position_unique <- category_match_position[!is.na(category_match_position)] %>% unique
    matched_words_correct_spelling <- accident_words[category_match_position_unique] %>%
      paste(collapse=";")
  }
  
  df.output <- as.data.frame(t(c(matched_words_incorrect_spelling, matched_words_correct_spelling)))
  names(df.output) <- c("matched_words_incorrect_spelling","matched_words_correct_spelling")
  
  # Variables original factors; change to character
  df.output$matched_words_incorrect_spelling <- as.character(df.output$matched_words_incorrect_spelling)
  df.output$matched_words_correct_spelling <- as.character(df.output$matched_words_correct_spelling)
  
  return(df.output)
}


# Functions: Geocoding ---------------------------------------------------------

remove.text.after.toward.fun <- function(i,tweets_clean,toward.word){
  tweets_clean.i <- tweets_clean[i,]
  
  # Determine whether any word in toward.words is in tweet  
  toward.word.in.tweet <- grepl(toward.word, tweets_clean.i$tweet_clean)
  
  # If the "toward word" is in the tweet AND the tweet is an accident, check word positions
  if(toward.word.in.tweet & tweets_clean.i$accident){
    
    # Check positions of Landmarks
    landmark.start.pos <- min(str_locate(tweets_clean.i$tweet_clean, landmarks_list_space)[,1],na.rm = TRUE)
    
    # Only go further if landmark in tweet
    if(landmark.start.pos != Inf){
      
      accident.words.combined <- tweets_clean.i$category_word_match
      
      # If fuzzy matched, include misspelled words (if the algorithm found any misspelled words)
      if(fuzzy_match_accident){
        if(tweets_clean.i$matched_words_incorrect_spelling != ""){
          accident.words.combined <- paste(accident.words.combined,tweets_clean.i$matched_words_incorrect_spelling,sep=";")
        }
      }
      
      accident.words <- strsplit(accident.words.combined, ";")[[1]]
      accident.words <- paste0("\\b", accident.words, "\\b")
      
      accident.start.pos <- min(str_locate(tweets_clean.i$tweet_clean, accident.words)[,1])
      toward.start.pos <- min(str_locate(tweets_clean.i$tweet_clean, toward.word)[,1])
      
      # If accident and landmark come before toward word, then remove rest of tweet
      if((accident.start.pos < toward.start.pos) & (landmark.start.pos < toward.start.pos)){
        tweets_clean.i$tweet_clean <- gsub("toward.*","",tweets_clean.i$tweet_clean)
      }
    }
  }
  
  return(tweets_clean.i$tweet_clean)
}

### landmark starting location in tweet
str_locate_all_MIN <- function(landmark, tweet) min(str_locate_all(pattern=landmark,tweet)[[1]])

tweet_word_start_character <- function(i, tweet_words_loc){
  if(i == 1){
    return(1)
  } else{
    return(sum(tweet_words_loc$word_length[1:(i-1)]) + i)
  }
}

landmark_match_FUN <- function(tweet, 
                               landmark_list, 
                               landmarks_list_space,
                               fuzzy_match_landmark, 
                               fuzzy_match_landmark.dist,
                               fuzzy_match_landmark.min.word.length,
                               landmarks,
                               equal.distant.projection,
                               multiple_landmark_distance_threshold_km,
                               only.use.first.landmark,
                               only.use.first.landmark.and.use.longest,
                               prepositions.list,
                               prioritize.exact.match,
                               landmark.match.type.rank.use,
                               landmark.match.type.rank,
                               landmark.match.more.types){
  
  print(tweet)
  
  ##### Exact Match
  landmark_match <- tweet %>%
    str_extract(landmarks_list_space) %>%
    na.omit() %>%
    unique %>%
    as.data.frame %>%
    dplyr::rename(matched_words_tweet_spelling = ".") %>%
    dplyr::mutate(matched_words_correct_spelling = matched_words_tweet_spelling) %>%
    dplyr::mutate(match_type = "exact match")
  
  ##### Fuzzy Match
  # (1) Fuzzy Match on uni, bi and tri grams, (2) Limit list to words of a 
  # certain length, (3) Implement fuzzy match; returns location in dictionary
  # of closest matched word, (4) if finds same word as exact match, just keep
  # the exact match version in final dataframe.
  if(fuzzy_match_landmark){
    
    if(str_count(tweet, '\\w+') > 1){
      tweet_words <- ngram::ngram_asweka(tweet, min=1, max=3) 
    } else{
      tweet_words <- tweet
    }
    tweet_words <- tweet_words[nchar(tweet_words) >= fuzzy_match_landmark.min.word.length] 
    landmark_match_position <- amatch(tweet_words, landmark_list, maxDist = fuzzy_match_landmark.dist, method="lv") 
    
    landmark_match_fuzzy <- tweet_words[!is.na(landmark_match_position)] %>%
      as.data.frame %>%
      dplyr::rename(matched_words_tweet_spelling = ".") %>%
      dplyr::mutate(match_type = "fuzzy match")
    landmark_match_fuzzy$matched_words_correct_spelling <- landmark_list[landmark_match_position] %>% na.omit %>% as.character
    landmark_match <- rbind(landmark_match, landmark_match_fuzzy) %>%
      distinct(matched_words_correct_spelling, .keep_all = TRUE)
  }
  
  if(nrow(landmark_match) > 0){
    
    #### Landmarks in tweet; to return in final dataframe
    landmarks_in_tweet <- paste(landmark_match$matched_words_correct_spelling, collapse=";")
    landmarks_in_tweet_originaltext <- paste(landmark_match$matched_words_tweet_spelling, collapse=";")
    
    #### Merge in landmark locations; prep dataframe
    landmark_match <- merge(landmark_match, landmarks, by.x="matched_words_correct_spelling", by.y="Landmark", all.x=T, all.y=F)
    
    landmark_match$matched_words_correct_spelling <- as.character(landmark_match$matched_words_correct_spelling)
    landmark_match$matched_words_tweet_spelling <- as.character(landmark_match$matched_words_tweet_spelling)
    landmark_match$match_type <- as.character(landmark_match$match_type)
    landmark_match$Type <- as.character(landmark_match$Type)
    
    ##### Tweet word location dataframe and preposition location dataframe
    tweet_words_loc <- strsplit(tweet," ") %>% 
      as.data.frame %>% 
      dplyr::rename_(word = names(.)[1])
    tweet_words_loc$word <- as.character(tweet_words_loc$word)
    tweet_words_loc$word_number <- 1:nrow(tweet_words_loc)
    tweet_words_loc$word_length <- nchar(tweet_words_loc$word)
    tweet_words_loc$word_char_start <- lapply(1:nrow(tweet_words_loc), tweet_word_start_character, tweet_words_loc) %>% unlist
    tweet_words_loc_ids <- subset(tweet_words_loc, select=c(word_number,word_char_start))
    tweet_prepositions_loc <- tweet_words_loc[tweet_words_loc$word %in% prepositions.list,]
    
    ##### Character and word in tweet where landmark starts
    landmark_match$word_char_start <- lapply(landmark_match$matched_words_tweet_spelling, str_locate_all_MIN, tweet) %>% unlist
    landmark_match <- merge(landmark_match, tweet_words_loc_ids, all.x=T, all.y=F, by="word_char_start")
    
    landmark_match$id <- 1:nrow(landmark_match)
    ##### Decisions for choosing between multiple landmarks - - - - - - - - - - 
    
    ##### If have both fuzzy and exact match, only use exact match 
    if(prioritize.exact.match & (nrow(landmark_match) >= 2) & (length(unique(landmark_match$match_type))==2)){
      landmark_match <- landmark_match[landmark_match$match_type %in% "exact match",]
    }
    
    ##### If words is both an exact match and starts a fuzzy match in bi/tri gram, remove fuzzy match
    # Only do this if both a fuzzy and exact match
    # Basically: if fuzzy and exact start at same point, choose exact
    # But failes in cases where fuzzy is longer than/ different than exact.
    # Muthiaga vs. Muthiaga Min[i] Mart
    if(F){ # DONT USE FOR NOW
      if((nrow(landmark_match) >= 2) & (length(unique(landmark_match$match_type))==2)){
        for(id in landmark_match$id[landmark_match$match_type == "fuzzy match"]){
          
          # if fuzzy word number in list of exact match word numbers, remove fuzzy word
          if(landmark_match$word_number[landmark_match$id == id] %in% 
             landmark_match$word_number[landmark_match$match_type == "exact match"]){
            
            landmark_match <- landmark_match[landmark_match$id != id,]
          }
        }
      }
    }
    
    ##### If multiple words and a preposition is in front a word, use that word 
    # Check if there's an instance of a preposition before a landmark
    # [Could also do preposition heirarchy like do for type]
    if(nrow(landmark_match) >= 2){
      if(1 %in% (landmark_match$word_number - tweet_prepositions_loc$word_number)){
        landmark_match <- landmark_match[(landmark_match$word_number - tweet_prepositions_loc$word_number) %in% 1,]
      }
    }
    
    ##### If multiple words, choose first word
    if(nrow(landmark_match) >= 2){
      landmark_match <- landmark_match[landmark_match$word_number %in% min(landmark_match$word_number),]
    }
    
    ##### If multiple words, use longer one
    if((nrow(landmark_match) >= 2) & only.use.first.landmark.and.use.longest){
      landmark_match <- landmark_match[which.max(nchar(as.character(landmark_match$matched_words_tweet_spelling))),]
    }
    
    ##### If multiple words, choose by type heirarchy
    if((nrow(landmark_match) >= 2) & landmark.match.type.rank.use){
      for(type in landmark.match.type.rank){
        if(TRUE %in% grepl(type, landmark_match$Type)){ # Only subset if type in type list (otherwise will get blank dataframe)
          landmark_match <- landmark_match[grepl(type, landmark_match$Type),] 
        }
      }
    }
    
    ##### If multiple words, choose one that has more "types"
    if((nrow(landmark_match) >= 2) & landmark.match.more.types){
      landmark_match <- landmark_match[str_count(landmark_match$Type,";") %in% max(str_count(landmark_match$Type,";")),]
    }
    
    ##### If multiple words, use average location if close together
    if(nrow(landmark_match) >= 2){
      # Spatially Project
      landmark_match.sdf <- landmark_match
      coordinates(landmark_match.sdf) <- ~landmarkLon+landmarkLat
      crs(landmark_match.sdf) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
      landmark_match.sdf <- spTransform(landmark_match.sdf, CRS(equal.distant.projection)) 
      
      # Calculate Distance
      landmark.distances <- (gDistance(landmark_match.sdf,byid=T) / 1000) %>% 
        as.list %>%
        unlist %>%
        unique
      
      # If any of the distances are above the threshold, dont geocode
      if(sum(landmark.distances > multiple_landmark_distance_threshold_km) >= 1){
        tweet_landmark_lat <- NA
        tweet_landmark_lon <- NA
        tweet_landmark_multiple <- TRUE
        tweet_landmark_mult_close <- FALSE
        landmarks_in_tweet_use_to_geocode <- NA
      } else{
        tweet_landmark_lat <- mean(landmark_match$landmarkLat)
        tweet_landmark_lon <- mean(landmark_match$landmarkLon)
        tweet_landmark_multiple <- TRUE
        tweet_landmark_mult_close <- TRUE
        landmarks_in_tweet_use_to_geocode <- paste(landmark_match$matched_words_correct_spelling, collapse=";")
      }
      # If only one landmark
    } else{
      tweet_landmark_lat <- landmark_match$landmarkLat
      tweet_landmark_lon <- landmark_match$landmarkLon
      tweet_landmark_multiple <- FALSE
      tweet_landmark_mult_close <- NA
      landmarks_in_tweet_use_to_geocode <- landmark_match$matched_words_correct_spelling
    }
    
    # If no landmarks found
  } else{
    tweet_landmark_lat <- NA
    tweet_landmark_lon <- NA
    tweet_landmark_multiple <- NA 
    tweet_landmark_mult_close <- NA 
    landmarks_in_tweet <- NA
    landmarks_in_tweet_use_to_geocode <- NA
    landmarks_in_tweet_originaltext <- NA
  }
  
  df.out <- as.data.frame(t(c(tweet_landmark_lat, 
                              tweet_landmark_lon,
                              tweet_landmark_multiple, 
                              tweet_landmark_mult_close, 
                              landmarks_in_tweet,
                              landmarks_in_tweet_use_to_geocode,
                              landmarks_in_tweet_originaltext)))
  
  names(df.out) <- c("tweet_landmark_lat", 
                     "tweet_landmark_lon",
                     "tweet_landmark_multiple", 
                     "tweet_landmark_mult_close", 
                     "landmarks_in_tweet",
                     "landmarks_used_to_geocode",
                     "landmarks_in_tweet_originaltext")
  
  df.out$landmarks_in_tweet <- as.character(df.out$landmarks_in_tweet)
  df.out$landmarks_used_to_geocode <- as.character(df.out$landmarks_used_to_geocode)
  df.out$landmarks_in_tweet_originaltext <- as.character(df.out$landmarks_in_tweet_originaltext)
  df.out$tweet_landmark_lat <- as.numeric(as.character(df.out$tweet_landmark_lat))
  df.out$tweet_landmark_lon <- as.numeric(as.character(df.out$tweet_landmark_lon))
  
  if(!exists("prev.time")) prev.time <<- Sys.time()
  print(Sys.time()-prev.time)
  prev.time <<- Sys.time()
  counter.number <<- counter.number + 1
  print(counter.number)
  print("")
  
  return(df.out)
}

# Functions: Geocoding, Google API ---------------------------------------------

### Exact Street Matches
street_match <- function(tweet, dictionary){
  dictionary.match <- str_extract(tweet, dictionary)
  dictionary.match <- dictionary.match[!is.na(dictionary.match)]
  
  dictionary.match.length <- length(dictionary.match)
  
  dictionary.match <- paste(dictionary.match, collapse=";")
  if(identical(dictionary.match,character(0))){
    dictionary.match <- ""
  }
  
  df.output <- as.data.frame(t(c(dictionary.match, dictionary.match.length)))
  names(df.output) <- c("streets_in_tweet","number_streets_in_tweet")
  
  return(df.output)
}

### Generic Landmark Matches
generic_landmark_match <- function(tweet, dictionary){
  dictionary.match <- str_extract(tweet, dictionary)
  dictionary.match <- dictionary.match[!is.na(dictionary.match)]
  
  dictionary.match <- paste(dictionary.match, collapse=";")
  if(identical(dictionary.match, character(0))){
    dictionary.match <- ""
  }
  
  return(dictionary.match)
}

google_API_geocode <- function(i, tweets_clean){
  tweets_clean.i <- tweets_clean[i,]
  
  # If more than on tweet
  if(tweets_clean.i$number_streets_in_tweet >= 2){
    # CODE HERE
  }
  
  # If a generic landmark and a street (just 1 street for now)
  if((tweets_clean.i$number_streets_in_tweet %in% 1) & (tweets_clean.i$landmark_generic != "")){
    search.term <- paste(tweets_clean.i$landmark_generic, ", ", tweets_clean.i$streets_in_tweet, ", Nairobi, Kenya", sep="")
    search.term.api <- paste(placeUrl, search.term, "&key=", key)
    
    rd2 <- fromJSON(URLencode(search.term.api))
    
    if(rd2$status != "ok"){
      output <- as.data.frame(t(c(NA,NA)))
    } else{
      output <- rd2$results$geometry$location[1,] 
    }
    
    names(output) <- c("google_API_lat","google_API_lon")
  } else{
    output <- as.data.frame(t(c(NA,NA)))
    names(output) <- c("google_API_lat","google_API_lon")
  }
  
  return(output)
}


# Functions: Levenstein Word Match ---------------------------------------------
list_to_df <- function(i,landmark_list,landmark_list_words){
  df <- as.data.frame(cbind(
    landmark_list_words[[i]],
    rep(i, length(landmark_list_words[[i]])),
    rep(landmark_list[i], length(landmark_list_words[[i]]))
  ))
  return(df)
}

landmark_words_in_tweet <- function(tweet_clean, landmark_words_unique, levenstein.word.fuzzy.match, levenstein.word.fuzzy.match.dist, levenstein.word.fuzzy.match.min.word.length){
  
  # 1. Prep tweet words
  tweet_words <- strsplit(tweet_clean, " ")[[1]] %>% unique()
  
  # 2. Exact Match
  landmark_words_match <- str_extract(tweet_clean, paste0("\\b", landmark_words_unique, "\\b"))
  landmark_words_match <- landmark_words_match[!is.na(landmark_words_match)]
  landmark_words_match <- landmark_words_match[landmark_words_match != ""]
  tweet_words_match <- landmark_words_match
  
  words_df <- as.data.frame(cbind(tweet_words_match,tweet_words_match))
  names(words_df) <- c("landmark_word","word_in_tweet")
  
  # 3. Fuzzy Match
  if(levenstein.word.fuzzy.match){
    
    # 3.1 Prep tweet words
    
    # 3.1.1 Only keep long words
    tweet_words_long <- tweet_words[nchar(tweet_words) >= levenstein.word.fuzzy.match.min.word.length]
    
    # 3.1.2 If word has an exact match, remove from tweet words to consider (only do levenstein for words w/o an exact match)
    # Fails if word is mispelled but matches another word in dictionary [juction, want to match with junction but juction is in dictionary]
    #tweet_words_long <- tweet_words_long[!(tweet_words_long %in% tweet_words_match)]
    
    # 3.1.2 [modified] If word has an exact match, remove if from landmark dictionary
    landmark_words_unique <- landmark_words_unique[!(landmark_words_unique %in% tweet_words_match)]
    
    # 3.2. Fuzzy Match Algorithm; Returns location in dictionary of closest matched word
    landmark_match_position <- amatch(tweet_words_long, landmark_words_unique, maxDist = levenstein.word.fuzzy.match.dist, method="lv") 
    
    # 3.3. List of words in tweet with matches
    tweet_words_match_fuzzy <- tweet_words_long[!is.na(landmark_match_position)]
    
    # 3.4. List of words in dictionary with matches
    landmark_match_position_unique <- landmark_match_position[!is.na(landmark_match_position)] %>% unique
    landmark_words_fuzzy_match <- landmark_words_unique[landmark_match_position_unique] 
    
    # 3.5 Landmark 
    words_df_fuzzy <- as.data.frame(cbind(landmark_words_fuzzy_match,tweet_words_match_fuzzy))
    names(words_df_fuzzy) <- c("landmark_word","word_in_tweet")
    
    # 3.6 Combine Exact and Fuzzy Matches
    landmark_words_match <- c(landmark_words_match, landmark_words_fuzzy_match) %>% unique
    tweet_words_match <- c(landmark_words_match, tweet_words_match_fuzzy) %>% unique
    
    words_df <- rbind(words_df,words_df_fuzzy)
  }
  
  # Location Landmark Words Found in Tweet
  words_df$word_loc_in_tweet <- match(words_df$word_in_tweet, tweet_words)
  words_df$word_loc_in_tweet[is.na(words_df$word_loc_in_tweet)] <- 200 # If NA
  
  if(nrow(words_df) > 0){
    words_df$tweet_contain_word <- 1
  } else{
    tweet_contain_word <- as.data.frame(matrix(NA,nrow=0,ncol=1))
    names(tweet_contain_word) <- "tweet_contain_word"
    words_df <- cbind(words_df,tweet_contain_word)
  }
  
  #if(length(landmark_words_match) > 0){
  #  landmark_words_match_df <- as.data.frame(landmark_words_match)
  #  names(landmark_words_match_df) <- "word"
  #  landmark_words_match_df$tweet_contain_word <- 1
  #  tweet_words_match <- paste(tweet_words_match, collapse=";")
  #} else{
  #  landmark_words_match_df <- as.data.frame(t(c("",NA)))
  #  names(landmark_words_match_df) <- c("word","tweet_contain_word")
  #  tweet_words_match <- ""
  #}
  
  return(list(landmark_words_match_df=words_df,
              tweet_words_match=tweet_words_match))
}

words_contiguous_check.i <- function(id,tweet_df){
  tweet_df.i <- tweet_df[tweet_df$id %in% id,]
  locations_in_tweet <- tweet_df.i$word_loc_in_tweet
  
  contiguous <- !(FALSE %in% (min(locations_in_tweet):max(locations_in_tweet) %in% locations_in_tweet))
  
  df_out <- as.data.frame(matrix(NA, nrow=1,ncol=0))
  df_out$id <- id
  df_out$contiguous <- contiguous
  
  return(df_out)
}

levenstein_word <- function(tweet, 
                            id,
                            landmark_word_df, 
                            landmarks_simple, 
                            equal.distant.projection, 
                            landmark_words_unique, 
                            levenstein.word.fuzzy.match, 
                            levenstein.word.fuzzy.match.dist, 
                            levenstein.word.fuzzy.match.min.word.length, 
                            levenstein.word.contiguous, 
                            levenstein.word.min.words.fuzzy, 
                            levenstein.word.max.word.distance,
                            multiple_landmark_distance_threshold_km){
  
  # For each tweet
  landmarks_output <- landmark_words_in_tweet(tweet, landmark_words_unique, levenstein.word.fuzzy.match, levenstein.word.fuzzy.match.dist, levenstein.word.fuzzy.match.min.word.length)
  landmarks_df <- landmarks_output$landmark_words_match_df
  tweet_words_match <- landmarks_output$tweet_words_match
  
  # Only keep landmarks where the landmarks that include a landmark word from tweet
  # 1) See which landmarks have the word
  # 2) Get list of landmark IDs with those words 
  # 3) keep landmarks with those IDS
  landmark_word_df.tweet <- landmark_word_df[landmark_word_df$id %in% 
                                               unique(landmark_word_df$id[landmark_word_df$landmark_word %in% landmarks_df$landmark_word]),]
  landmark_word_df.tweet <- merge(landmark_word_df.tweet, landmarks_df, by="landmark_word", all.x=T)
  
  if(nrow(landmark_word_df.tweet) == 0) return()
  
  # Number of landmark words in tweet
  landmark_word_df.tweet.col <- summaryBy(tweet_contain_word+landmark_length ~ landmark+id, data=landmark_word_df.tweet, FUN=sum, keep.names=T, na.rm=T)
  landmark_word_df.tweet.col <- landmark_word_df.tweet.col[landmark_word_df.tweet.col$tweet_contain_word > 0,]
  
  # Merge in lat/lon
  landmarks_simple$Landmark <- as.character(landmarks_simple$Landmark)
  landmark_word_df.tweet.col <- merge(landmark_word_df.tweet.col, landmarks_simple, by.x="landmark", by.y="Landmark_noStopWords", all.x=T, all.y=F)
  
  # Contiguous
  landmark_word_df.tweet.order <- landmark_word_df.tweet[!is.na(landmark_word_df.tweet$word_in_tweet),]
  contiguous_df <- lapply(unique(landmark_word_df.tweet.order$id), words_contiguous_check.i, landmark_word_df.tweet.order) %>% bind_rows
  if(nrow(contiguous_df) > 0){
    landmark_word_df.tweet.col <- merge(landmark_word_df.tweet.col, contiguous_df, by="id")
  }
  
  # Determine landmarks to keep
  # 1. If landmark words in tweet are contiguous
  if(levenstein.word.contiguous){
    landmark_word_df.tweet.col <- landmark_word_df.tweet.col[landmark_word_df.tweet.col$contiguous %in% TRUE,]
  }
  
  # 2. If length of landmark is exactly the number of landmark words in tweet
  landmark_word_df.tweet.col.exact <- landmark_word_df.tweet.col[landmark_word_df.tweet.col$tweet_contain_word == landmark_word_df.tweet.col$landmark_length,]
  
  # 3. If length of landmark is close to number of landmark words in tweet
  #    Only do this if there is no exact distance match.
  if(nrow(landmark_word_df.tweet.col.exact) %in% 0){
    landmark_word_df.tweet.col$distance <- landmark_word_df.tweet.col$landmark_length - landmark_word_df.tweet.col$tweet_contain_word
    
    landmark_word_df.tweet.col <- landmark_word_df.tweet.col[(landmark_word_df.tweet.col$tweet_contain_word >= levenstein.word.min.words.fuzzy) & 
                                                               (landmark_word_df.tweet.col$distance <= levenstein.word.max.word.distance),]
    
  } else{
    landmark_word_df.tweet.col <- landmark_word_df.tweet.col.exact
  }
  
  if(nrow(landmark_word_df.tweet.col) >= 2){
    
    landmark_word_df.tweet.col.sdf <- landmark_word_df.tweet.col
    coordinates(landmark_word_df.tweet.col.sdf) <- ~landmarkLon+landmarkLat
    crs(landmark_word_df.tweet.col.sdf) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    landmark_word_df.tweet.col.sdf <- spTransform(landmark_word_df.tweet.col.sdf, CRS(equal.distant.projection)) 
    
    multiple_landmarks_number <- nrow(landmark_word_df.tweet.col)
    multiple_landmarks_number_pairs <- sum((gDistance(landmark_word_df.tweet.col.sdf, byid=T) > 0)) / 2
    multiple_landmarks_num_pairs_below_thresh <- sum((gDistance(landmark_word_df.tweet.col.sdf, byid=T) > 0) & (gDistance(landmark_word_df.tweet.col.sdf, byid=T) <= multiple_landmark_distance_threshold_km*1000)) / 2
    multiple_landmarks_max_dist <- max(gDistance(landmark_word_df.tweet.col.sdf, byid=T)) / 1000
    multiple_landmarks_mean_dist <- mean(gDistance(landmark_word_df.tweet.col.sdf, byid=T)) / 1000
    multiple_landmarks_min_dist <- min(gDistance(landmark_word_df.tweet.col.sdf, byid=T)[gDistance(landmark_word_df.tweet.col.sdf, byid=T) > 0]) / 1000
    
    if(max(gDistance(landmark_word_df.tweet.col.sdf, byid=T)) < multiple_landmark_distance_threshold_km*1000){
      lat <- mean(landmark_word_df.tweet.col$landmarkLat)
      lon <- mean(landmark_word_df.tweet.col$landmarkLon)
      multiple_landmarks_close <- TRUE
    } else{
      lat <- NA
      lon <- NA
      multiple_landmarks_close <- FALSE
    }
  }
  
  if(nrow(landmark_word_df.tweet.col) == 1){
    lat <- landmark_word_df.tweet.col$landmarkLat
    lon <- landmark_word_df.tweet.col$landmarkLon
    multiple_landmarks_close <- NA
    multiple_landmarks_max_dist <- NA
    multiple_landmarks_mean_dist <- NA
    multiple_landmarks_min_dist <- NA
    multiple_landmarks_number <- 1
    multiple_landmarks_number_pairs <- NA
    multiple_landmarks_num_pairs_below_thresh <- NA
  }
  
  if(nrow(landmark_word_df.tweet.col) == 0){
    lat <- NA
    lon <- NA
    multiple_landmarks_close <- NA
    multiple_landmarks_max_dist <- NA
    multiple_landmarks_mean_dist <- NA
    multiple_landmarks_min_dist <- NA
    multiple_landmarks_number <- 0
    multiple_landmarks_number_pairs <- NA
    multiple_landmarks_num_pairs_below_thresh <- NA
  }
  
  landmarks_use <- paste(landmark_word_df.tweet.col$landmark, collapse=";")
  tweet_words_match <- paste(tweet_words_match, collapse=";")
  df_out <- as.data.frame(t(c(lat,lon,tweet_words_match,landmarks_use,
                              multiple_landmarks_close,
                              multiple_landmarks_max_dist,
                              multiple_landmarks_mean_dist,
                              multiple_landmarks_min_dist,
                              multiple_landmarks_number,
                              multiple_landmarks_number_pairs,
                              multiple_landmarks_num_pairs_below_thresh, id)))
  
  print("tweet")
  
  return(df_out)
}
  
  
  
  
  
  