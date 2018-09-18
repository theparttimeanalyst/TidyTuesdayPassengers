airp<-read.csv(text=getURL("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-09-18/us-airports.csv"))

airp1 <- airp %>% filter(year == 2012) %>%
                      filter(passengers > 10000) %>%
                                mutate(passengers2 = passengers/1000000) %>%
                          select(airport_name, airport_classification, passengers2, hub_type, region)

colnames(airp1)[3] <- "Passengers12"

ggplot(airp1, aes(x = yearly_rank, y = passengers, col = hub_type)) + geom_point()

airp2 <- airp %>% filter(hub_type == "Large")

ggplot(airp2, aes(x = year, y = passengers, col = region)) + geom_point()

airp3 <-airp %>% filter(year == 2017) %>%
                  filter(passengers > 0) %>%
                      mutate(passengers2 = passengers/1000000) %>%
                    select(airport_name, passengers2)


colnames(airp3)[2] <- "Passengers17"

airp4 <- left_join(airp1, airp3, by = "airport_name")

airp5 <- airp4 %>% mutate(delta = Passengers17 - Passengers12) %>%
                      mutate(PerDiff = delta/Passengers12 * 100) %>%
                          mutate(rank = min_rank(PerDiff)) %>%
                                  filter(rank >371) %>%
                                          arrange(rank) %>%
                                            mutate(airport_name = factor(airport_name, levels = .$airport_name))


ggplot(airp5, aes(x = airport_name, y = PerDiff, col = airport_classification)) + 
              geom_segment(aes(x = airport_name, y = 0, xend = airport_name, yend = PerDiff), color = "grey50") +
                          geom_point(aes(size = Passengers17)) + 
                           coord_flip() +
                              labs(x = "Airport Name", y = "Percentage Difference", title = "20 Biggest Percent Increase in Passengers") +
                               guides(col = guide_legend(title = "Airport Classification"), size = guide_legend(title = "Passengers 2017 (Mill)")) +
                                  theme_minimal() +
                                    theme(legend.position = c(0.8,0.6))

airp6 <- airp4 %>% mutate(delta = Passengers17 - Passengers12) %>%
                     mutate(PerDiff = delta/Passengers12 * 100) %>%
                                filter(PerDiff > 0) %>%
                             mutate(rank = min_rank(PerDiff)) %>%
                                                 arrange(rank) %>%
                                                   mutate(airport_name = factor(airport_name, levels = .$airport_name))

ggplot(airp6, aes(x = airport_name, y = PerDiff, col = airport_classification)) + 
                 geom_segment(aes(x = airport_name, y = 0, xend = airport_name, yend = PerDiff), color = "grey50") +
                     geom_point(aes(size = Passengers17)) + 
                         coord_flip() +
                          labs(x = "Airport Name", y = "Percentage Difference", title = "20 Biggest Percent Increase in Passengers") +
  guides(col = guide_legend(title = "Airport Classification"), size = guide_legend(title = "Passengers 2017 (Mill)")) +
  theme_minimal() +
  theme(legend.position = c(0.8,0.6))
