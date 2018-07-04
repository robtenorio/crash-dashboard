
rm(list = ls())

library(dplyr)
library(googlesheets)
library(lubridate)
library(magrittr)
library(stringr)
library(tidyr)
library(twilio)

select <- dplyr::select

directory <- "/home/ubuntu/Documents/crash-dashboard"

source(str_c(directory, "tweet-processing/geocode_algorithm.R", sep = "/"))

results <- readRDS(str_c(directory, "tweet-processing/supporting_files/results.rds", sep = "/")) 

crash_results <- results %>%
  mutate(text = gsub("^[[:digit:]]{1,2}([[:blank:]]|:)[[:digit:]]{2}", "", text)) %>%
  mutate(text = gsub("(via @\\S*)|(@\\S*)", "", text)) %>%
  mutate(text = gsub("(https://\\S*)", "", text)) %>%
  mutate(text = trimws(text, which = "both")) %>%
  filter(!duplicated(text)) %>%
  filter(geocoded == 0) %>%
  filter(prob >= 70) %>% 
  mutate(nwords = str_count(text, pattern = "\\s")) %>%
  filter(!nwords < 5) # shorter sentences are unlikely to be about crashes

#### Twilio
twilios_phone_number <- "2407022326"

out_schedule <- tryCatch({
  # download the students' schedule from GS
  gs <- gs_title("Schedule") 
  
  schedule <- gs %>% gs_read("Schedule")
  
  # parse, making intervals of the times
  schedule_long <- schedule %>%
    gather(key = "date", value = "student", -Time) %>%
    separate(Time, c("start_time", "end_time"), "-") %>%
    mutate(start = str_c(start_time, date, sep = " ")) %>%
    mutate(end = str_c(end_time, date, sep = " ")) %>%
    mutate(start = parse_date_time(start, "HM m/d/Y", tz = "Africa/Nairobi")) %>%
    mutate(end = parse_date_time(end, "HM m/d/Y", tz = "Africa/Nairobi")) %>%
    mutate(end = end - seconds(1)) %>% # subtract one second from the end time so we don't have any overlap
    mutate(time = interval(start, end)) %>%
    select(time, student)
  
  # find which student is on duty
  student <- schedule_long %>%
    filter(with_tz(Sys.time(), tzone = "Africa/Nairobi") %within% time) %>%
    use_series(student)
  
  if(length(student != 0)) {
    # if a student is on duty, the phone number is that of the student in the contact info sheet
    contact <- gs %>% gs_read("Contact Information")
    
    phone_number <- contact %>%
      filter(tolower(Name) == tolower(student)) %>%
      mutate(`Phone Number` = str_c("+", `Phone Number`, sep = "")) %>%
      mutate(`Phone Number` = gsub("\\s+", "", `Phone Number`)) %>%
      use_series(`Phone Number`)
  }
  
  # if no student is on duty, forward to Robert's phone
  if(length(student) == 0) {
    phone_number <- "2023200815"
    student <- "DIME"
  }
  
  
},
error = function(e) e)

if(inherits(out_schedule, "error")) phone_number <- "2023200815"
if(!exists("phone_number")) phone_number <- "2023200815" # last defense in case something unforeseen went wrong

out_geocode <- tryCatch(
  {
    if(nrow(crash_results) == 0) {
      # label all results as having been geocoded
      results$geocoded <- 1
      saveRDS(results, str_c(directory, "tweet-processing/supporting_files/results.rds", sep = "/"))
      print("no new crash data")
      q()
    }
    
    if(nrow(crash_results) > 0) {
      
      geocoded_crashes <- crashmap_algorithm(tweets = crash_results$text, 
                                             date = crash_results$date,
                                             status_id = crash_results$status_id,
                                             landmark.dictionary="adj_google_maps_bitri_cluster_0.5dist_0.9close",
                                             only.use.first.landmark=TRUE,
                                             only.use.first.landmark.and.use.longest=TRUE,
                                             landmark.match.type.rank.use=TRUE,
                                             landmark.match.type.rank=c("bus_station","shopping_mall"),
                                             landmark.match.more.types=TRUE,
                                             prepositions.list <- c("at","near","opposite","opp","after","before","hapo","outside"),
                                             prioritize.exact.match=TRUE,
                                             remove.street.names=TRUE,
                                             fuzzy_match_accident=FALSE,
                                             fuzzy_match_accident.dist=2,
                                             fuzzy_match_accident.onlymisspelled=FALSE,
                                             fuzzy_match_accident.min.word.length=6,
                                             multiple_landmark_distance_threshold_km=1,
                                             fuzzy_match_landmark=TRUE,
                                             fuzzy_match_landmark.dist=1,
                                             fuzzy_match_landmark.min.word.length=6,
                                             remove.text.after.toward=FALSE,
                                             google_map_api=FALSE,
                                             supporting.files.path=str_c(directory, "tweet-processing/supporting_files/", sep = "/"),
                                             levenstein.word=FALSE,
                                             levenstein.word.min.words.fuzzy=2,
                                             levenstein.word.max.word.distance=1,
                                             levenstein.word.fuzzy.match=FALSE,
                                             levenstein.word.fuzzy.match.dist=1,
                                             levenstein.word.fuzzy.match.min.word.length=6,
                                             levenstein.word.contiguous=TRUE,
                                             mc.cores=1)
      
      new_crash_data <- geocoded_crashes$data %>% 
        dplyr::select(tweet_original, 
                      tweet_landmark_lon_final,
                      tweet_landmark_lat_final,
                      date,
                      id, status_id) %>%
        rename(long = tweet_landmark_lon_final, lat = tweet_landmark_lat_final) %>%
        filter(!duplicated(tweet_original)) 
      
      if(nrow(new_crash_data) == 0) {
        # label all results as having been geocoded
        results$geocoded <- 1
        saveRDS(results, str_c(directory, "tweet-processing/supporting_files/results.rds", sep = "/"))
        print("no new crash data")
        q()
      }
      
      new_crash_data <- new_crash_data %>%
        mutate(date = ymd_hms(date, tz = "UTC")) %>%
        mutate(date = with_tz(date, tzone = "Africa/Nairobi")) %>%
        mutate(year = year(date)) %>%
        mutate(month = month(date)) %>%
        mutate(day = wday(date, label = TRUE)) %>%
        mutate(hour = hour(date)) %>%
        mutate(time_day = case_when(hour %in% c(1,2,3,4) ~ "01:00-04:59",
                                    hour %in% c(5,6,7,8,9) ~ "05:00-09:59",
                                    hour %in% c(10,11,12,13) ~ "10:00-13:59",
                                    hour %in% c(14,15,16,17) ~ "14:00-17:59",
                                    hour %in% c(18,19,20,21) ~ "18:00-21:59",
                                    hour %in% c(22,23,0) ~ "22:00-00:59")) %>%
        mutate(time_day = factor(time_day, ordered = TRUE)) %>%
        dplyr::select(year, month, day, time_day, long, lat, date, id, status_id)
      
      non_geocoded_crash_data <- new_crash_data %>%
        filter(is.na(long) | is.na(lat))
      
      new_crash_data <- new_crash_data %>%
        na.omit
      
      if(nrow(new_crash_data) > 0) {
        ##### Cluster crashes
        truth.df <- new_crash_data
        ##### Cluster
        # Spatially Project
        truth.geo.df <- truth.df[!is.na(truth.df$lat),]
        truth.geo.df <- truth.df[!is.na(truth.df$long),]
        
        coordinates(truth.geo.df) <- ~long + lat
        crs(truth.geo.df) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
        equal.distant.projection <- paste("+proj=aeqd +lat_0=",-1.283333," +lon_0=",36.816667, sep="")
        
        # filter out points outside of Kenya
        counties <- c("Nairobi", "Kiambu", "Murang'a", "Kajiado", "Machakos")
        ke_counties <- getData("GADM", level = 1, country = "KEN")
        ke_county <- ke_counties[ke_counties@data$NAME_1 %in% counties,]
        
        ke_country <- getData("GADM", level = 0, country = "KEN")
        over_truth <- over(truth.geo.df, ke_country)
        truth.geo.df <- truth.geo.df[!is.na(over_truth$OBJECTID),]
        
        truth.geo.df <- spTransform(truth.geo.df, CRS(equal.distant.projection)) 
        truth.geo.df$longitude <- coordinates(truth.geo.df)[,1]
        truth.geo.df$latitude <- coordinates(truth.geo.df)[,2]
        truth.geo.df <- truth.geo.df@data
        
        # Implement Clustering
        truth.geo.df$cluster.id <- NA
        cluster_km <- 2
        
        for(i in seq(nrow(truth.geo.df))){
          truth.geo.df.i <- truth.geo.df[i,]
          within.time <- abs((truth.geo.df.i$date - truth.geo.df$date)) <= 30*60
          within.distance <- sqrt((truth.geo.df.i$lon - truth.geo.df$lon)^2 + (truth.geo.df.i$lat - truth.geo.df$lat)^2) <= (cluster_km*1000)
          truth.geo.df$cluster.id[within.time & within.distance] <- truth.geo.df.i$id
        }
        
        new_crash_data_clusters <- left_join(new_crash_data, truth.geo.df[,c("id", "cluster.id")], by = "id") %>%
          filter(!duplicated(cluster.id))
        
        # send SMS
        # make df for sending sms by including original tweet text
        names_geocoded_crashes <- names(geocoded_crashes$data)[names(geocoded_crashes$data) %in% c("id", "tweet_original", 
                                                                                                   "landmarks_used_to_geocode")]
        new_crash_data_sms <- left_join(new_crash_data_clusters, geocoded_crashes$data[,names_geocoded_crashes], by = "id") 
      }
      
      
      non_geocoded_crash_data_sms <- left_join(non_geocoded_crash_data, geocoded_crashes$data[,c("id", "tweet_original", 
                                                                                                 "landmarks_used_to_geocode")])
      
      if(exists("new_crash_data_sms")) {
        crash_data_sms <- new_crash_data_sms %>%
          select(-cluster.id) %>%
          rbind(non_geocoded_crash_data_sms) %>%
          mutate(landmark = case_when(!is.na(landmarks_used_to_geocode) ~ as.character(landmarks_used_to_geocode),
                                      is.na(landmarks_used_to_geocode) ~ "NO LANDMARK"))
      }
      
      if(!exists("new_crash_data_sms")) {
        crash_data_sms <- non_geocoded_crash_data_sms %>%
          mutate(landmark = case_when(!is.na(landmarks_used_to_geocode) ~ as.character(landmarks_used_to_geocode),
                                      is.na(landmarks_used_to_geocode) ~ "NO LANDMARK"))
      }
      
      for(i in seq(nrow(crash_data_sms))) {
        tw_send_message(from = twilios_phone_number, to = phone_number,
                        body = str_c("Sendy: Please fill out the survey at: http://bit.ly/2Kt7HL0",
                                     "Tweet: ", crash_data_sms$tweet_original[i],
                                     "Suggested Landmark: ", crash_data_sms$landmark[i],
                                     "Unique ID: ", crash_data_sms$status_id[i],
                                     sep = "\n"))
        tw_send_message(from = twilios_phone_number, to = phone_number,
                        body = str_c("Please direct a Sendy driver to the suggested landmark or what you identify as the nearest location.",
                                     "\nCopy and paste the entire SMS with the Tweet, Landmark, and Unique ID into the Sendy order.",
                                     "\nIf there is no Suggested Landmark, please decide where to send the driver based on the tweet.",
                                     "Please fill out the Google Docs sheet at the end of your shift.",
                                     sep = "\n"))
        
        ### Rob's phone number
        tw_send_message(from = twilios_phone_number, to = "5133793170",
                        body = str_c("nSendy: Please fill out the survey at: http://bit.ly/2Kt7HL0",
                                     "Tweet: ", crash_data_sms$tweet_original[i],
                                     "Suggested Landmark: ", crash_data_sms$landmark[i],
                                     "Unique ID: ", crash_data_sms$status_id[i],
                                     sep = "\n"))
        tw_send_message(from = twilios_phone_number, to = "5133793170",
                        body = str_c("Please direct a Sendy driver to the suggested landmark or what you identify as the nearest location.",
                                     "\nCopy and paste the entire SMS with the Tweet, Landmark, and Unique ID into the Sendy order.",
                                     "\nIf there is no Suggested Landmark, please decide where to send the driver based on the tweet.",
                                     "Please fill out the Google Docs sheet at the end of your shift.",
                                     sep = "\n"))
      }
      
      ### append data to Google Doc
      gs_out <- tryCatch({
        crash_data_google <- crash_data_sms %>%
          mutate(time = format(date, "%H:%M:%S")) %>%
          mutate(date = format(date, "%Y-%m-%d")) %>%
          mutate(recipient = student) %>%
          select(recipient, date, time, status_id, landmark, tweet_original)
        
        gs_add_row(gs, ws = "Tweet Log", input = crash_data_google)
      },
      error = function(e) e)
      
      if(inherits(gs_out, "error") || inherits(gs_out, "warning")) print(gs_out)
      
      ### append data to app data
      
      if(exists("new_crash_data_clusters")) {
        # remove ids
        new_crash_data_clusters <- new_crash_data_clusters %>%
          dplyr::select(-id, -cluster.id, -status_id)  
        
        previous_crash_data <- readRDS(str_c(directory, "data/twitter_data.rds", sep = "/"))
        
        crash_data <- rbind(previous_crash_data, new_crash_data_clusters) %>%
          na.omit
      }
      
    }
  },
  error = function(e) e)

if(inherits(out_geocode, "error")) {
  results$geocoded <- 1
  saveRDS(results, str_c(directory, "tweet-processing/supporting_files/results.rds", sep = "/"))
  print("there was an error")
  q()
}

if(!inherits(out_geocode, "error")) {
  # label all results as having been geocoded
  results$geocoded <- 1
  if(exists("crash_data")) {
    saveRDS(crash_data, str_c(directory, "data/twitter_data.rds", sep = "/"))
  }
  saveRDS(results, str_c(directory, "tweet-processing/supporting_files/results.rds", sep = "/"))
  print("twitter_data updated")
  q()
}






