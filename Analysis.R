library(readr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(leaflet)

options(scipen = 9999)
options(tibble.width = Inf)
theme_set(theme_bw())

raw <- read_csv("Data/Contribs.csv")

raw <- mutate(raw, receipt_date = mdy(receipt_date),
              amount = as.numeric(gsub("$", "", amount, fixed = TRUE)))

raw <- mutate(raw, contributor_city = if_else(contributor_city %in% 
    c("SAMAMISH", "SAMAMMISH", "SAMMAM", "SAMMAMISH,", "SAMMMAMISH", "SAMMASH",
    "SAMMAMAMISH", "SAMMAMASH", "SAMMAMICH", "SAMMAMIH", "SAMMAMISH", 
    "SAMMAMISH WA", "SAMMAMISH", "SAMMAMISH, WA", "SAMMAMISHS", "SAMMAMIXH", 
    "SAMMAMIZH", "SAMMAMMISH", "SAMMAMMMISH","SAMMAMSH", "SAMMAMSIH", 
    "SAMMAMSISH", "SAMMANISH", "SAMMANMISH", "SAMMASH ", "SAMMIMISH", "SAMMISH", 
    "SAMMMAMISH ", "SAMMMAMSIH", "SAMMMISH", "SAMMSMISH"), "SAMMAMISH", contributor_city))



#1- who are the donors living in sammamish
d <- filter(raw, election_year >= 1998, contributor_city == "SAMMAMISH")

summary(d$amount)

filter(d, !is.na(office)) %>%
  group_by(office) %>%
  summarize(median = median(amount)) %>%
  ggplot(aes(office, median)) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  labs(x = "", y = "Median Donation amount",
       title = "Donation amount by office")


unique_loc <- group_by(d, contributor_name) %>%
  summarise(contributor_location = first(contributor_location))

loc <- group_by(d, contributor_name) %>%
  summarize(Total = sum(amount), Frequency = n()) %>%
  inner_join(unique_loc, by = "contributor_name") %>%  
  separate(contributor_location, into = c("Lat", "Long"), sep = ",") %>%
  mutate(Lat = as.numeric(gsub("(", "", Lat, fixed = TRUE)), 
         Long = as.numeric(gsub(")", "", Long, fixed = TRUE)))

summary(loc$Frequency)

get_color_frequency <- function(freq) {
  if_else(freq == 1, "red", 
          if_else(freq > 1 & freq <= 5, "blue", "green"))
}

icons <- awesomeIcons(
  icon = 'ion-social-usd',
  iconColor = 'black',
  library = 'ion',
  markerColor = get_color_frequency(loc$Frequency)
)


leaflet(data = loc) %>% addTiles() %>%
  addAwesomeMarkers(~Long, ~Lat, 
                    popup = ~as.character(paste0(contributor_name, 
                                                 "<br>$", Total,
                                                 "<br>Donated ", Frequency,
                                                 " times")),
                    clusterOptions = markerClusterOptions(),
                    icon = icons)


#2- who gives to sammamish city council races.
d1 = filter(raw, election_year >= 1998, jurisdiction == "CITY OF SAMMAMISH",
            office == "CITY COUNCIL MEMBER")

filers <- group_by(d1, filer_name) %>% summarize(Total = sum(amount)) 
summary(filers$Total)

group_by(d1, election_year, filer_name) %>%
  summarize(Total = sum(amount)) %>%
  group_by(election_year) %>%
  summarize(Mean = mean(Total), Num = n()) %>%
  arrange(election_year) %>%
  ggplot(aes(as.factor(election_year), Mean)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste(Num, "candidates")), vjust = -1) +
  labs(x = "", y = "Mean amount raised by each candidate", 
       title = "Amount raised in City council elections")


loc <- group_by(d1, contributor_name, contributor_location) %>%
  summarize(Total = sum(amount), Frequency = n()) %>%
  separate(contributor_location, into = c("Lat", "Long"), sep = ",") %>%
  mutate(Lat = as.numeric(gsub("(", "", Lat, fixed = TRUE)), 
         Long = as.numeric(gsub(")", "", Long, fixed = TRUE)))

unique_loc <- group_by(d1, contributor_name) %>%
  summarise(contributor_location = first(contributor_location))

loc <- group_by(d1, contributor_name) %>%
  summarize(Total = sum(amount), Frequency = n()) %>%
  inner_join(unique_loc, by = "contributor_name") %>%  
  separate(contributor_location, into = c("Lat", "Long"), sep = ",") %>%
  mutate(Lat = as.numeric(gsub("(", "", Lat, fixed = TRUE)), 
         Long = as.numeric(gsub(")", "", Long, fixed = TRUE)))

icons <- awesomeIcons(
  icon = 'ion-social-usd',
  iconColor = 'white',
  library = 'ion',
  markerColor = get_color_frequency(loc$Frequency)
)

leaflet(data = loc) %>% addTiles() %>%
  addAwesomeMarkers(~Long, ~Lat, 
                    popup = ~as.character(paste0(contributor_name, 
                                                 "<br>$", Total,
                                                 "<br>Donated ", Frequency,
                                                 " times")),
                    icon = icons)


arrange(d1, desc(election_year), desc(amount)) %>%
  select(contributor_name, contributor_address, amount, election_year, filer_name)
