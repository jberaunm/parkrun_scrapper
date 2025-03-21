#' ---
#' title: "CS5811"
#' author: "Jeison Beraun"
#' date: "2025-03-21"
#' output: html_document
#' ---
#' 
## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
#install.packages("rvest")
#install.packages("httr2")
#install.packages("tidyverse")


#' 
#' ## R Markdown
#' 
#' loading packages
#' 
## ---------------------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(rvest)
library(httr2)

#' 
#' # Extracting parkrun results
## ---------------------------------------------------------------------------------------------------------------------------------------------
link <- "https://www.parkrun.org.uk/bushy/results/latestresults/"

#Creating request
req <- request(link) |> req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/134.0.0.0 Safari/537.36")
resp <- req |> req_perform()
html_body <- resp |> resp_body_html()

# Extracting geolocation data:
head <- html_body |> html_elements("head")
meta <- head |> html_elements("meta")
data <- meta |> html_attr("content")
parkrun_name <- data[6]
parkrun_geo <- data[7]

# Extracting date
h3 <- html_body |> html_elements("h3")
date <- h3 |> html_elements(".format-date") |> html_text2() |>  parse_date("%d/%m/%Y")

# Extracting results table
table <- html_body |> html_elements("table")
tr <- table |> html_elements("tr")

# Parsing data from table
name <- tr |> html_attr("data-name")
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
df <- data.frame(parkrun_name, parkrun_geo, date, name,category,agegroup,club,gender,position,runs,vols,agegrade, achievement)
df <- df[-1,]

# Extracting more complex data
time <- tr |> html_elements("td.Results-table-td.Results-table-td--time") |> html_node("div.compact") |> html_text2()

pos_gender <- tr |> html_elements("td.Results-table-td.Results-table-td--gender") |> html_node("div.detailed") |> html_text2()

df$time <- time
df$pos_gender <- pos_gender


#' 
#' # Extracting parkrun report
## ---------------------------------------------------------------------------------------------------------------------------------------------

link <- "https://www.parkrun.org.uk/bushy/news/category/run-reports/"

#Creating request
req <- request(link) |> req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/134.0.0.0 Safari/537.36")
resp <- req |> req_perform()
html_body <- resp |> resp_body_html()


title <- html_body |> html_elements("div.entry-content") |> html_elements("h1") |> html_nodes("b") |> html_text2()
title <- title[1]

report <- html_body |> html_elements("div.entry-content") |> html_elements("p") |> html_text2()
flattened_report <- paste(report, collapse = " ")

df$title <- title
#df$report <- flattened_report

write.csv(df, "Bushy_parkruncsv")


#' 
#' # Sentiment analysis for report/weather (flattened_report)
## ---------------------------------------------------------------------------------------------------------------------------------------------


#' 
#' # Extracting weather historic data
## ---------------------------------------------------------------------------------------------------------------------------------------------


#' 
