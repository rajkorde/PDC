library(tidyverse)
library(leaflet)
library(geojsonio)
library(forcats)

precsp <- geojson_read("Data/Votes/json/wa.geojson", what = "sp")

raw2017 <- read_csv("Data/Votes/2017General.csv")
raw2018 <- read_csv("Data/Votes/2018General.csv")

raw2017 <- mutate(raw2017, Year = 2017)
raw2018 <- mutate(raw2018, Year = 2018)

d <- bind_rows(raw2018, raw2017)
d <- mutate(d, CounterType = as.factor(CounterType)) %>%
  mutate(CounterType = fct_recode(CounterType,
                                  "Rituja" = "Rituja Indapure",
                                  "Kim" = "Kim Schrier",
                                  "Manka" = "Manka Dhingra",
                                  "Voters" = "Registered Voters")) %>%
  mutate(CounterType = fct_relevel(CounterType, "Rituja"))

voter_count <- filter(d, CounterType == "Voters") %>%
  group_by(Precinct) %>%
  mutate(RN = row_number()) %>%
  ungroup() %>%
  filter(RN == 1) %>%
  select(-RN)

d <- filter(d, CounterType != "Voters") %>%
  bind_rows(voter_count)

#Rituja

x1 <- filter(d, CounterType == "Rituja") %>%
  select(PRECNAME=Precinct, CounterType, SumOfCount)
precs <- precsp[precsp$PRECNAME %in% x1$PRECNAME,]
samm_precs <- x1$PRECNAME

samm_voters <- filter(d, CounterType == "Voters") %>%
  filter(Precinct %in% samm_precs) %>%
  select(PRECNAME=Precinct, CounterType, SumOfCount)

x1 <- bind_rows(samm_voters, x1) %>%
  spread(key=CounterType, value=SumOfCount) %>%
  mutate(VotePercent = (Rituja*100)/Voters)


pal <- colorNumeric("RdYlGn", domain = x1$VotePercent)
x2 <- sp::merge(x=precs, y=x1)

leaflet(x2) %>%
  addTiles() %>%
  addPolygons(stroke = TRUE, weight = 1,
              fillOpacity = 1,
              fillColor = ~pal(VotePercent),
              label = ~paste0(PRECNAME, " Vote Count: ", formatC(Rituja), " Vote Percent: ", formatC(VotePercent), "%"))



plotDiff <- function(d, precs, name) {
  x1 <- filter(d, CounterType %in% c(name, "Rituja")) %>%
    select(PRECNAME=Precinct, CounterType, SumOfCount) %>%
    spread(key=CounterType, value=SumOfCount) %>%
    rename_(D1 = names(.)[2], D2 = names(.)[3]) %>%
    mutate(Diff = D1 - D2)
  
  x2 <- sp::merge(x=precs, y=x1)
  pal <- colorNumeric("RdYlGn", domain = x1$Diff)
  
  leaflet(x2) %>%
    addTiles() %>%
    addPolygons(stroke = TRUE, weight = 1,
                fillOpacity = 1,
                fillColor = ~pal(Diff),
                label = ~paste0(PRECNAME, " Vote Difference: ", formatC(Diff)))
}

# Kim
plotDiff(d, precs, "Kim")
# Manka 2017
plotDiff(filter(d, Year == 2017), precs, "Manka")
# Manka 2018
plotDiff(filter(d, (CounterType == 'Rituja' & Year == 2017) | 
                  (CounterType == 'Manka' & Year == 2018)), precs, "Manka")

d1 <- filter(d, !((CounterType == 'Manka' & Year == 2017)))

x1 <- filter(d1, CounterType %in% c("Manka", "Kim", "Rituja")) %>%
  select(PRECNAME=Precinct, CounterType, SumOfCount) %>%
  filter(PRECNAME %in% samm_precs) %>%
  spread(key=CounterType, value=SumOfCount) %>%
  mutate(Max = ifelse(is.na(Manka), Kim, pmax(Manka, Kim))) %>%
  mutate(Diff = Max - Rituja) %>%
  arrange(desc(Diff))
  

