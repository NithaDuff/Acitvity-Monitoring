sd <- storm_data

sd$CROPDMGEXP <- sd$CROPDMGEXP %>% str_replace_all("K","3")  %>%
  str_replace_all("M","6") %>%
  str_replace_all("B","9")
sd$CROPDMGEXP <- as.numeric(sd$CROPDMGEXP)
sd$CROPDMGEXP <- 10^sd$CROPDMGEXP
sd$CROPDMG <- as.numeric(sd$CROPDMG)
#unique(sd$CROPDMGEXP) %>% print

sd$PROPDMGEXP <- sd$PROPDMGEXP %>% str_replace_all("H","10")  %>%
  str_replace_all("\\-","-1") %>%
  str_replace_all("\\+","1") %>%
  str_replace_all("K","3")  %>%
  str_replace_all("M","6") %>%
  str_replace_all("B","9")
sd$PROPDMGEXP <- as.numeric(sd$PROPDMGEXP)
sd$PROPDMGEXP <- 10^sd$PROPDMGEXP
sd$PROPDMG <- as.numeric(sd$PROPDMG)
unique(sd$PROPDMGEXP) %>% print

sd$cropdmgtot <- sd$CROPDMG + sd$CROPDMGEXP
#unique(sd$cropdmgtot)
sd$propdmgtot <- sd$PROPDMG + sd$PROPDMGEXP
#unique(sd$propdmgtot)

storm_prop <- sd  %>%
  group_by(EVTYPE) %>%
  summarise(total_cropdmg = sum(cropdmgtot,na.rm = T), total_propdmg = sum(propdmgtot,na.rm = T)) 

storm_prop$total_impact <- storm_prop$total_cropdmg + storm_prop$total_propdmg
storm_prop %>% View
max_prop_effect <- storm_prop$total_impact == max(storm_prop$total_impact)
storm_prop[max_prop_effect,] %>% print

significant_impact <- storm_prop$total_impact >= 0.3e+10
g <- ggplot(storm_prop[significant_impact,], aes(x = EVTYPE, y = total_impact)) +
  geom_bar(stat = "identity") +
  labs(title = "Impact on property", x = "Event", y = "Total Impact(Crop and Property)") +
  theme(axis.text.x = element_text(angle = 30, hjust=1))
print(g)
