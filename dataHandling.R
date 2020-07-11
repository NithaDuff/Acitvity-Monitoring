storm_data <- read.csv("Storm_DA/repdata_data_StormData.csv.bz2",na.strings = c('',' ',NA,'?')) %>%  mutate_all(.funs=toupper)
storm_data$EVTYPE <-   str_replace(storm_data$EVTYPE,'S$','') %>% str_trim()

storm_data$INJURIES <- as.numeric(storm_data$INJURIES)
storm_data$FATALITIES <- as.numeric(storm_data$FATALITIES)
storm_data$CROPDMG <- as.numeric(storm_data$CROPDMG)
storm_data$PROPDMG <- as.numeric(storm_data$PROPDMG)

storm_health <- storm_data  %>%
  group_by(EVTYPE) %>%
  summarise(total_injury = sum(INJURIES,na.rm = T), total_fatality = sum(FATALITIES,na.rm = T))
storm_health$total_impact <- storm_health$total_injury + storm_health$total_fatality
max_health_effect <- storm_health$total_impact == max(storm_health$total_impact)
storm_health[max_health_effect,] %>% print


significant_impact <- storm_health$total_impact >= 500.0
g <- ggplot(storm_health[significant_impact,], aes(x = EVTYPE, y = total_impact)) +
  geom_bar(stat = "identity") +
  labs(title = "Impact on Health", x = "Event", y = "Total Impact(Injuries and Fatalities)") +
  theme(axis.text.x = element_text(angle = 30, hjust=1))
print(g)