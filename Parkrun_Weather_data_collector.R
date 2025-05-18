#' ---
#' title: "CS5811"
#' author: "Jeison Beraun"
#' date: "2025-03-21"
#' output: html_document
#' ---
#' 
## ----setup, include=FALSE----------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

#' 
#' ## R Markdown
#' 
#' loading packages
#' 
## ----------------------------------------------------------------------------------------------------------------------
if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
if (!require(rvest)) install.packages("rvest"); library(rvest)
if (!require(httr2)) install.packages("httr2"); library(httr2)


#' 
#' # List of all parkruns in London
#' 
#' Based on https://www.richardgower.com/blog/londone and chatGPT
#' 
## ----------------------------------------------------------------------------------------------------------------------
london_parkruns = read.csv("london_parkruns.csv")


#' 
#' # Extracting event history
#' ## df_historic
#' 
#' Creates the dataframe df_historic, which retrieve the following information for the latest Parkrun events in London:
#' date
#' EventId
#' finishers
#' volunteers
#' first_male_time
#' first_female_time
#' 
#' EventId is the latest eventId that will help latter to iterate backwards to get all parkrun events
#' 
## ----------------------------------------------------------------------------------------------------------------------

link = "https://www.parkrun.org.uk/"
errors = vector()

df_historic = data.frame()

for (i in rownames(london_parkruns)) {
  
  name <- london_parkruns[i,3]
  link = "https://www.parkrun.org.uk/"
  link = paste(link,name,sep="")
  link = paste(link,"/results/eventhistory/",sep="")
  
  print(link)
  
  skip_to_next <- FALSE

  #Creating request
  tryCatch({
    req <- request(link) |> req_user_agent("Mozilla/5.1 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/134.0.0.0 Safari/537.3") |> req_timeout(120)
    resp <- req |> req_perform()
    html_body <- resp |> resp_body_html()
  }, error = function(e) {skip_to_next <<- TRUE})
    
  if(skip_to_next) {
    message = paste("Error:",link,sep = " ")
    print(message)
    errors <- append(errors, name)
    Sys.sleep(20)
    next
  }
  
  tr <- html_body |> html_elements("tbody") |> html_elements("tr")
  date <- tr |> html_attr("data-date")
  EventId <- tr |> html_attr("data-parkrun")
  finishers <- tr |> html_attr("data-finishers")
  volunteers <- tr |> html_attr("data-volunteers")
  first_male_time <- tr |> html_attr("data-maletime") 
  first_female_time <- tr |> html_attr("data-femaletime")
  
  df <- data.frame(date,name,EventId,finishers,volunteers,first_male_time,first_female_time)
  df_historic <- rbind(df_historic, df)

  Sys.sleep(40) # Sleep pause added to avoid captcha test
  
}

#write.csv(df_historic,"df_historic.csv",row.names = FALSE)
df_errors <- as.data.frame(errors)

#write.csv(df_events, "Bushy_events.csv",row.names = FALSE)

#' 
#' # Extracting data from all london parkruns - iterating
#' ## df_parkrun
#' 
#' R code that will iterate through all latest parkrun events in london defined in 'london_parkruns'.
#' Parameters start and finish will allow to set the number and how recent events to be web scrapped, base on the latest parkrun eventId
#' These parameters allow to be flexible on when and what data to be retrieved.
#' 
#' Data scrapped will be saved and updated in df_parkrun after every loop.
#' Errors when fetching information (captcha check) are mapped in 'errors' vector
#' 
## ----------------------------------------------------------------------------------------------------------------------

london_parkruns <- read.csv("london_parkruns.csv")

base_link <- "https://www.parkrun.org.uk/"
parkrun = ""
df_parkrun = data.frame()
errors = vector()

start = 2
finish = 1

for (c in start:finish){
  for( i in rownames(london_parkruns) ) {
    
    name <- london_parkruns[i,3]
    borough <- london_parkruns[i,1]
    id <- london_parkruns[i,4] - c
    
    if (id < 1) next # skip if id < 1
    
    link = paste(base_link,name,sep = "")
    link = paste(link,"/results/",sep = "")
    link = paste(link,id,sep = "")
    print(link)
  
    skip_to_next <- FALSE
    #Creating request
    tryCatch({
      req <- request(link) |> req_user_agent("Mozilla/5.1 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/134.0.0.0 Safari/537.3") |> req_timeout(120)
      resp <- req |> req_perform()
      html_body <- resp |> resp_body_html()
    }, error = function(e) {skip_to_next <<- TRUE})
    
    if(skip_to_next) {
      message = paste("Error:",link,sep = " ")
      print(message)
      errors <- append(errors, message) 
      Sys.sleep(20)
      next
    }
    
    # Extracting geolocation data:
    head <- html_body |> html_elements("head")
    meta <- head |> html_elements("meta")
    data <- meta |> html_attr("content")
    
    #content > div.Results > div.Results-header > h3 > span:nth-child(3)
    event_name <- london_parkruns[i,2]
    parkrun_geo <- data[7]
    
    latitude <- unlist(strsplit(parkrun_geo,";"))[1]
    longitude <-unlist(strsplit(parkrun_geo,";"))[2]
    
    # Extracting date
    h3 <- html_body |> html_elements("h3")
    date <- h3 |> html_elements(".format-date") |> html_text2() |>  parse_date("%d/%m/%Y")
    
    # Extracting ID
    event_id <- h3 |> html_elements("span:nth-child(3)") |> html_text2()
    event_id <- substr(event_id,2,nchar(event_id))
    
    # Extracting results table
    table <- html_body |> html_elements("table")
    tr <- table |> html_elements("tr")
    
    # Parsing data from table
    participant_name <- tr |> html_attr("data-name")
    agegroup <- tr |> html_attr("data-agegroup")
    category <- substr(agegroup,0,1)
    agegroup <- substr(agegroup,3,7)
    club <- tr |> html_attr("data-club")
    gender <- tr |> html_attr("data-gender")
    position <- tr |> html_attr("data-position")
    runs <- tr |> html_attr("data-runs")
    vols <- tr |> html_attr("data-vols")
    agegrade <- tr |> html_attr("data-agegrade")
    achievement <- tr |> html_attr("data-achievement")
    
    #Remove first row so number of rows matches
    df <- data.frame(borough, event_name, event_id, date, latitude, longitude, participant_name,category,agegroup,club,gender,position,runs,vols,agegrade, achievement)
    df <- df[-1,]
    
    # Extracting more complex data
    time <- tr |> html_elements("td.Results-table-td.Results-table-td--time") |> html_node("div.compact") |> html_text2()
    
    pos_gender <- tr |> html_elements("td.Results-table-td.Results-table-td--gender") |> html_node("div.detailed") |> html_text2()
    
    pb_time <- tr |> html_elements("td.Results-table-td.Results-table-td--time") |> html_node("div.detailed") |> html_text2()
    
    participant_id <- tr |> html_elements("td.Results-table-td.Results-table-td--name") |> html_node("div.compact") |> html_node("a") |> html_attr("href")
    start = 14 + nchar(name)
    end = nchar(participant_id)
    participant_id <- substr(participant_id,start,end)
    
    #content > div.Results > table > tbody > tr:nth-child(4) > td.Results-table-td.Results-table-td--name > div.compact > a
    
    df$time <- time
    df$pos_gender <- pos_gender
    df$pb_time <- pb_time
    df$participant_id <- participant_id
    
    # Anonimysing full names to names and 2 characters of last name
    lastname_len <- nchar(str_split_fixed(df$participant_name, ' ', 2)[,2])
    df$participant_name <- substr(df$participant_name,0,nchar(df$participant_name)-lastname_len+2)
    
    df_parkrun <- rbind(df, df_parkrun)
    
    Sys.sleep(30)

  }
}

#write.csv(df_parkrun,"parkrun_data_1.csv",row.names = FALSE)
#write.csv(errors,"errors_1.csv",row.names = FALSE)


#' 
#' # Extracting weather historic data using API
#' 
#' The input is unique date and geolocation data from previous section df_parkrun
#' 
## ----------------------------------------------------------------------------------------------------------------------
weather_input <-  df_parkrun %>%
  select(date,latitude,longitude) %>%
  mutate(geolocation = paste(latitude, longitude, sep = ",")) %>%
  select(-latitude, -longitude)


#' 
#' ## df_weather
#' R code to extract historic data at specifically 9AM, that corresponds to 'hourly[4]' information retrieve in the XML response.
#' Data retrieved will be saved and updated in df_weather after every loop.
#' 
## ----------------------------------------------------------------------------------------------------------------------

base_link = "http://api.worldweatheronline.com/premium/v1/past-weather.ashx?key="

df_weather <- data.frame()

# Signup in https://www.worldweatheronline.com/weather-api/ to get a free key
key = "enter_key_here"

for (i in rownames(weather_input)) {
  
  geo = weather_input[i,2]
  date = weather_input[i,1]

  link = paste(base_link,key,sep = "")
  link = paste(link,"&q=",sep = "")
  link = paste(link,geo,sep = "")
  link = paste(link,"&date=",sep = "")
  link = paste(link,date,sep = "")
  link = paste(link,"&tp=3",sep = "")
  link <- paste0(link,"&format=xml")
  
  print(link)
  
  req <- request(link) |> req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/134.0.0.0 Safari/537.36") |> req_timeout(60)
  resp <- req |> req_perform()
  
  xml_body <- resp |> resp_body_xml()
  
  hourly <- xml_body |> xml_nodes("weather") |> xml_nodes("hourly")
  weatherDesc <- hourly[4] |> xml_node("weatherDesc") |> html_text2()
  
  tempC <- hourly[4] |> xml_node("tempC") |> html_text2()
  FeelsLikeC <- hourly[4] |> xml_node("FeelsLikeC") |> html_text2()
  cloud_cover <- hourly[4] |> xml_node("cloudcover") |> html_text2()
  precipMM <- hourly[4] |> xml_node("precipMM") |> html_text2()
  windspeedMiles <- hourly[4] |> xml_node("windspeedMiles") |> html_text2()
  winddirDegree <- hourly[4] |> xml_node("winddirDegree") |> html_text2()
  humidity <- hourly[4] |> xml_node("humidity") |> html_text2()
  visibility <- hourly[4] |> xml_node("visibility") |> html_text2()
  pressure <- hourly[4] |> xml_node("pressure") |> html_text2()
  uvIndex <- hourly[4] |> xml_node("uvIndex") |> html_text2()
  
  df <- data.frame(date, geo, weatherDesc, tempC, FeelsLikeC, cloud_cover, precipMM, windspeedMiles, winddirDegree, humidity, visibility, pressure, uvIndex)
  
  df_weather <- rbind(df, df_weather)
  
  Sys.sleep(10)

}

#write.csv(df_weather, "weather_5.csv",row.names = FALSE)

#' 
#' 
