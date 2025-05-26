setwd("D:/OneDrives/OneDrive/University/2025 Autumn/DSAA811/Assignment/DSAA811-Final-Report")

library(tidyverse)
library(sjmisc)
athletes <- read.csv('./data/athlete_events_data_dictionary.csv', header = TRUE)
events <- read.csv('./data/athlete_events.csv', header = TRUE)
countryDefdd<- read.csv('./data/country_definitions_data_dictionary.csv', header = TRUE)
countryDef <- read.csv('./data/country_definitions.csv', header = TRUE)
events$Medal <- factor(events$Medal,
                       levels = c("Gold", "Silver", "Bronze"),
                       labels = c("Gold", "Silver", "Bronze"),
                       ordered = TRUE)
head(athletes,10)
head(events,10)
head(countryDef, 10)
head(countryDefdd,10)

#This one tells you how many medals each person won
#This is important because there are athletes that win multiple medals
myMini <- events %>%  filter(Year == 1896) %>% 
  select(ID, NOC, Year, Season, Medal) %>% 
  pivot_wider(names_from = Medal,
              values_from = Medal,
              values_fill = 0,
              values_fn = list(Medal = length),
              ) 

#This is looking at weight and height of athletes in one sport dimension
#I want to focus more on the categorical data
events %>% filter(Sport == "Gymnastics") %>%
  ggplot() +
  geom_point(aes(x = Age, y = Weight)) +
  labs(title = 'Olympic gymnasts ages and weights') +
  theme_bw()


#This is not taking into consideration the number of athletes this is just the events
#per sport

events  %>% filter(Year == 2016) %>%
  ggplot() +
  geom_bar(aes(y = Sport), stat="count") +
  labs(title = 'Number of athletes per sport in 2016') +
  theme_minimal(12)

#Similar to the above. Only counting the events in 2000
events %>% filter (Year == 2000) %>%
  ggplot() +
  geom_bar(aes(y = Sport), stat="count") +
  labs(title = 'Number of athletes per sport in 2000') +
  theme_bw()

summary(events)

events$Sex <- factor(events$Sex,
                     levels = c("M","F"),
                     labels = c("M","F"))

#Within your exploratory analysis include:
#1. A visualisation of an amount or frequency over two qualitative variables.

events  %>% filter(Year == 2016) %>%
  ggplot() +
  geom_bar(aes(y = Sport), stat="count") +
  labs(title = 'Number of athletes per sport in 2016') +
  theme_minimal(12)

#Shows the change in the number of events per sport at the olympics
Summer <- events %>% filter (Season == "Summer") %>% filter (Year > 2000)
Summer %>% 
  ggplot(aes(width=60, height=100)) +
  geom_bar(aes(y = Sport), stat="count") +
  labs(title = 'Number of athletes per sport in 2000') + theme_bw() +
  facet_wrap(vars(Summer$Year))


summerCounts <- Summer %>%  
  group_by (Year) %>%
  count(Sport)

summerCounts <- Summer %>% 
  group_by (Year,Sport) %>%
  count(Sport) %>% 
  arrange(Year, desc(n)) %>%
  pivot_wider(values_from = n, names_from = Sport)

summerCounts <- rotate_df(summerCounts)
print.data.frame (summerCounts)
#2. A visualisation comparing two (or more) quantitative associations.

events %>% filter(Sport == "Gymnastics") %>%
  ggplot() +
  geom_point(aes(x = Age, y = Weight)) +
  labs(title = 'Olympic gymnasts ages and weights') +
  theme_bw()

#3. A visualisation demonstrating a trend or relationship in the data.

Year = 1896

lmdata <- Summer %>%  
  group_by (Year) %>%
  count(Sport) %>% 
  arrange(Year, desc(n)) %>%
  pivot_wider(values_from = n, names_from = Sport)

ggplot(lmdata) +
  geom_line(aes(x = lmdata[,2,], y = Year)) +
  theme_linedraw()

lmdata[,2:37,]

#4. A visualisation describing the shape of the distribution of the data.
#5. A visualisation demonstrating the difference in means 
#     (with confidence intervals) across different groups of people

summerCounts
summerCounts <- summerCounts[-1,]
names(summerCounts) <- c("2004", "2008","2012", "2016")

summerCounts <- tibble::rownames_to_column(summerCounts, "Sport")
plsummerCounts <- summerCounts %>% pivot_longer(!Sport, names_to = "year", values_to = "count")

ggplot(plsummerCounts, aes(x = year, y = count)) + 
  geom_line(aes(color = Sport, group = Sport)) +
  facet_wrap(~Sport)

summerCounts

summerCounts2 <- Summer %>%  
  group_by (Team, Year) %>%
  count(Sport)

summerCounts2 <- Summer %>%  filter(Team == "Australia" | Team == "United States") %>%
  filter( Sport >= "Swimming") %>%
  group_by (Team, Year) %>%
  count(Sport)


ggplot(summerCounts2, aes(x = Year, y = n)) + 
  geom_line(aes(color = Team, linetype = Team)) +
  facet_grid(vars(Sport)) + theme(legend.position="none")


Gold <- events %>% 
    filter(grepl("Summer",Season) & Medal == "Gold" & Year == 1896) %>% 
    select(Games, Year, Sport, Event, Medal) %>% 
    group_by(Sport) %>% 
    summarize(Medals_Given = n()) 

Silver1 <- events %>% 
  filter(grepl("Summer",Season) & !is.na(Medal) & Year == 1896) %>% 
  select(Games, Year, Sport, Event, Medal) %>% 
  group_by(Sport, Medal) %>% 
  summarize(Medals_Given = n()) %>% 
  pivot_wider(names_from = Medal, values_from = Medals_Given)


