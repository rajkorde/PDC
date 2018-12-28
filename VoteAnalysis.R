library(tidyverse)
library(leaflet)
library(geojsonio)
library(forcats)

precsp <- geojson_read("Data/Votes/json/wa.geojson", what = "sp")

raw2017 <- read_csv("Data/Votes/2017Primary.csv")
raw2016 <- read_csv("Data/Votes/2016General.csv")

d <- bind_rows(raw2017, raw2016)

d <- mutate(d, CounterType = as.factor(CounterType)) %>%
  mutate(CounterType = fct_recode(CounterType,
           "Rituja" = "Rituja Indapure",
           "Pam" = "Pam Stuart",
           "Karen" = "Karen N. Howe",
           "Ryika" = "Ryika Hooshangi",
           "Roger" = "Roger Goodman",
           "Minal" = "Minal Kode Ghassemieh",
           "Hillary" = "Hillary Clinton & Tim Kaine",
           "Trump" = "Donald J. Trump & Michael R. Pence",
           "Chris" = "Chris Ross",
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
  

#Rituja raw votes
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
              label = ~paste0(PRECNAME, " Vote Count: ", formatC(Rituja), " VotePercent: ", formatC(VotePercent)))


# Republican vs Democrat
x1 <- filter(d, CounterType %in% c("Trump", "Hillary")) %>%
  filter(Precinct %in% samm_precs) %>%
  select(PRECNAME=Precinct, CounterType, SumOfCount) %>%
  spread(key=CounterType, value=SumOfCount) %>%
  mutate(Diff = Hillary - Trump)
x2 <- sp::merge(x=precs, y=x1)
pal <- colorNumeric("RdYlBu", domain = x1$Diff)

leaflet(x2) %>%
  addTiles() %>%
  addPolygons(stroke = TRUE, weight = 1,
              fillOpacity = 1,
              fillColor = ~pal(Diff),
              label = ~paste0(PRECNAME, " Vote Difference: ", formatC(Diff)))

# Compare with 2017 votes

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


plotDiff(d, precs, "Karen")
plotDiff(d, precs, "Pam")
plotDiff(d, precs, "Ryika")
plotDiff(d, precs, "Minal")

# compare with 2016 votes
plotDiff(d, precs, "Roger")
plotDiff(d, precs, "Hillary")

# compare with Chris Ross and Pam

x1 <- filter(d, CounterType %in% c("Rituja", "Pam", "Chris")) %>%
  select(PRECNAME=Precinct, CounterType, SumOfCount)

x1 <- bind_rows(samm_voters, x1) %>%
  spread(key=CounterType, value=SumOfCount) %>%
  mutate(CPDiff = Chris - Pam) %>%
  mutate(CRDiff = Chris - Rituja)

norm <- filter(x1, PRECNAME != "SAM 45-3722") %>%
  mutate(CPDiff = if_else(CPDiff == 0, 0.5, as.numeric(CPDiff))) %>%
  mutate(temp = abs(CPDiff)) %>%
  mutate(norm = CRDiff/(11.2+temp)) %>%
  select(-temp)

library(ggrepel)
ggplot(x1, aes(x=CPDiff, y=CRDiff)) + 
  geom_point() +
  xlab("Chris-Pam Vote Diff") + ylab("Chris-Rituja Vote Diff") +
  geom_text_repel(aes(label=PRECNAME))

pal <- colorNumeric("RdYlGn", domain = norm$norm)
x2 <- sp::merge(x=precs, y=norm)


leaflet(x2) %>%
  addTiles() %>%
  addPolygons(stroke = TRUE, weight = 1,
              fillOpacity = 1,
              fillColor = ~pal(norm),
              label = ~paste0(PRECNAME, " Pam Vote Difference: ", formatC(CPDiff), " Rituja Vote Diff: ", formatC(CRDiff), " norm: ", formatC(norm)))

# Targetted list of max differences
x1 <- filter(d, CounterType %in% c("Karen", "Pam", "Rituja")) %>%
  select(PRECNAME=Precinct, CounterType, SumOfCount) %>%
  spread(key=CounterType, value=SumOfCount) %>%
  mutate(Max = pmax(Karen, Pam)) %>%
  mutate(Diff = Rituja - Max)

x2 <- sp::merge(x=precs, y=x1)
pal <- colorNumeric("RdYlGn", domain = x1$Diff)

leaflet(x2) %>%
  addTiles() %>%
  addPolygons(stroke = TRUE, weight = 1,
              fillOpacity = 1,
              fillColor = ~pal(Diff),
              label = ~paste0(PRECNAME, " Vote Difference: ", formatC(Diff)))

mutate(x1, Diff = -Diff) %>%
  arrange(desc(Diff))
