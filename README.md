# Parkrun & Weather conditions Data Collector
Script in R that enables web scraping data of all Parkruns in London, and it's historic weather conditions.

## Parkrun data

### Parkrun historic events
List of London Parkrun were retrieved from https://www.richardgower.com/blog/londone
The script will retrieve all events history from all parkruns, including the latest event_ID and geolocation data (longitude and latitude)
For instance: https://www.parkrun.org.uk/Barking/results/eventhistory/

### Parkrun data results per event
The script will iterate from the latest event to whatever event is set in variables **start** and **finish**

For instance:
start = 1
finish = 2
Will retrieve the latest two events of each parkrun in London.

[1] "https://www.parkrun.org.uk/Barking/results/593"
[2] "https://www.parkrun.org.uk/Barking/results/592"

## Historic Weather Conditions data
The input is unique date and geolocation data from previous section
The R script extracts historic data at specifically 9AM, that corresponds to 'hourly[4]' information retrieve in the XML response.
Data retrieved will be saved and updated in df_weather after every loop.

A **key** is needed to use the API
A free key can be obtained by signing up in https://www.worldweatheronline.com/weather-api/
