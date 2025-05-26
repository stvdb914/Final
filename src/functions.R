#This function locates the regions on a world map depending on the olympics needed
#and the caption you want over the image
drawMap <- function(strSeason, olympicCaption){
  
  # Plot the information regarding all the athletes on the map
  
  locations_df <- events %>% 
    distinct(NOC, Season) %>% 
    full_join(locations, by = join_by(NOC == Alpha.3.code)) %>% 
    filter((grepl(strSeason,Season)) & !is.na(Country))  %>% 
    select(region = Country, lat = Latitude..average., long = Longitude..average.)
  
  
  world_map <- map_data("world")
  
  # Add locations to the plot
  ggplot() +
    geom_map(
      data = world_map,
      map = world_map,
      aes(map_id = region),
      color = "black",
      fill = "lightgrey",
      linewidth = 0.5
    ) +
    theme_void() +
    geom_point(
      data = locations_df,
      aes(x = long, y = lat, color = region),
      size = 3,
      shape = 20
    ) +
    theme(panel.background = element_rect(fill = "lightblue"),
          panel.grid = element_blank()) +
    scale_color_discrete(name = "Locations") +
    labs(title = olympicCaption) +
    theme(legend.position = "none")
}

# This function takes in the season and year of the olympics 
# and generates a table of the number
# of medals won at that olympics by the sport
# 
# eg:- in 1896 There are 37 medals awarded for Athletics
MedalWinners <- function(strSeason, nYear) {
  events %>% 
    filter(grepl(strSeason,Season) & !is.na(Medal) & Year == nYear) %>% 
    select(Games, Year, Sport, Event, Medal) %>% 
    group_by(Sport) %>% 
    summarize(Medals_Given = n()) %>% 
    ungroup()
}

# This function takes in the season and year of the olympics 
# and generates a table of the number
# of medals won at that olympics by the sport
MedalSportBreakDown <- function(strSeason, nYear) {
#The following 2 lines are used for testing only
#  strSeason <- "Summer"
#  nYear = 1896

  #Get the total medals awarded per sport
  TotalMedals <- MedalWinners(strSeason, nYear)
  
  #Break the medals down by Gold, Silver and Bronze, per sport
  MedalTypes <- events %>% 
    filter(grepl(strSeason,Season) & !is.na(Medal) & Year == nYear) %>% 
    select(Sport, Medal) %>% 
    group_by(Sport, Medal) %>% 
    summarize(Medals_Given = n(), .groups = 'drop') %>% 
    pivot_wider(names_from = Medal, values_from = Medals_Given) %>% 
    ungroup()

  #Put the two tables together
  left_join(MedalTypes, TotalMedals, join_by(Sport)) %>% 
  names(Together) = c("Sport", "Gold", "Silver", "Bronze", "Total")
}

# This function takes in the season and year of the olympics 
# and generates a table of the number
# of medals won at that olympics by team and events
MedalBreakDownByTeam <- function(strSeason, nYear, TeamsRequired = -1) {
    #strSeason <- "Summer"
    #nYear = 1952
  
    #Get the number of medals per country
    TotalEvents <- events %>% 
      filter(grepl(strSeason,Season) & Year == nYear) %>% 
      select(Team, Medal) %>% 
      group_by(Team) %>% 
      arrange(Medal) %>% 
      summarize(TotalEvents = n()) %>% 
      ungroup()
  
    #Break the medals down into types per Team/events
    MedalTypes <- events %>%  filter(grepl(strSeason,Season) & Year == nYear) %>% 
      group_by(Team, Medal) %>% 
      select(Team, Medal) %>% 
      summarize(TotalEvents = n(),.groups = "drop") %>% 
      pivot_wider(names_from = Medal,
                  values_from = TotalEvents,
                  values_fill = 0,
                  values_fn = list(Medal = length)) %>% 
      ungroup()
  
  Together <- left_join(MedalTypes, TotalEvents, join_by(Team)) %>% 
    mutate(
      Medals = Gold + Silver + Bronze) %>% 
    arrange(desc(Medals)) %>% 
    select(Team, "NA", Gold, Silver, Bronze, TotalEvents, Medals)
  names(Together) = c("Team","None", "Gold", "Silver", "Bronze", "TotalEvents", "TotalMedals") 
  if (TeamsRequired == -1) {
    return(Together)
  }
  return(head(Together,TeamsRequired))
}

# This function takes in the season and year of the olympics 
# and generates a table of the number
# of athletes that were sent from that country
AthletesPerCountry <- function(strSeason, nYear){
  #strSeason = "Summer"
  #nYear = 1952
  
  AthleteMedals(strSeason, nYear) %>% 
    group_by(Team) %>% 
    summarise(AthletesSent = n()) %>% 
    ungroup
}

# This function takes in the season and year of the olympics 
# and generates a table of the athlete, participation list and 
# number of medals or each kind that was awarded
AthleteMedals <- function(strSeason, nYear) {
  events %>%  filter(grepl(strSeason,Season) & Year == nYear) %>% 
    select(ID, Team, Year, Season, Medal) %>% 
    pivot_wider(names_from = Medal,
                values_from = Medal,
                values_fill = 0,
                values_fn = list(Medal = length),
    )
}

# This function takes in the season and year of the olympics with a 
# stacked bar using "fill" and a dodged bar using "dodge" 
# and generates a plot of the proportion of medals to events performed in
# For example 
# Australia entered 4 events in 1896 and won 2 gold medals, therefor
# 50% of the event participation earned gold and 50% earned no medals.

PropTeamWinners <- function(strSeason, nYear, myPos = "Fill", nCountries = -1) {
  #strSeason = Season of the olympics "Summer" or "Winter"
  #nYear     = The year of the olympics, eg 1896
  #myPos     = How you want the stacks to return "fill" lor "dodge"
  #nCountries= How many top countries you require, It can be left blank and it will default to all
  
  Switch <- ifelse(myPos == "Dodge", F, T)
  
  TeamMed1 <- left_join(MedalBreakDownByTeam(strSeason, nYear, nCountries), 
                        AthletesPerCountry(strSeason, nYear), 
                        join_by(Team)) %>%
  pivot_longer(cols=c(None, Gold, Silver, Bronze),
               names_to = "MedalType",
               values_to = c("Medals"))
  
  TeamMed1$MedalType <- factor(TeamMed1$MedalType, 
                                 levels = c("Gold", "Silver", "Bronze", "None"),
                                 labels = c("Gold", "Silver", "Bronze", "None"),
                                 ordered = TRUE)
  
  plotme = ggplot(TeamMed1, aes(x = Medals/TotalEvents,
                       y = reorder(Team,TotalMedals),
                       fill = MedalType,
                       group = fct_rev(MedalType))) +
    labs(title = paste0("Proportion of Medals Versus Events Participated in ",strSeason," ", nYear),
         x = "Proportion of Medals versus Events Participated in",
         y = "Olympic Team Name") +
    geom_bar(aes(x = Medals/TotalEvents,
                 y = reorder(Team,TotalMedals),
                 fill = MedalType,
                 group = fct_rev(MedalType)), stat = "identity", position = myPos) +
    scale_fill_manual(values = c("gold", "#C0C0C0", "#CD7F32", "#FFFAF0"))
    
    if(Switch) {
      plotme <- plotme + geom_text(aes(label=paste0(sprintf("%1.0f", (Medals/TotalEvents)*100),"%")),
             position=position_fill(vjust=0.5), colour="#4682B4")
    }

    plotme <- plotme + theme_classic()
    print(plotme)
}

#Calculating per Athlete
PropTeamAthleteWinners <- function(strSeason, nYear, myPos, nCountries = -1) {
  
  TeamAth1 <- left_join(MedalBreakDownByTeam(strSeason, nYear, nCountries), 
                        AthletesPerCountry(strSeason, nYear), 
                        join_by(Team)) %>% 
    arrange(desc(TotalMedals)) %>% 
    pivot_longer(cols=c(Gold, Silver, Bronze, None),
                 names_to = "MedalType",
                 values_to = c("Medals")) 
  
  TeamAth1$MedalType <- factor(TeamAth1$MedalType, 
                               levels = c("Gold", "Silver", "Bronze", "None"),
                               labels = c("Gold", "Silver", "Bronze", "None"),
                               ordered = TRUE)
  
  # adding the percentage label
  TeamAth1$prcntlabel = paste0(sprintf("%.0f",
                                       TeamAth1$Medals/TeamAth1$AthletesSent),
                                 "%")
  
  ggplot(TeamAth1, aes(x = Medals/AthletesSent,
                       y = reorder(Team,TotalMedals),
                       fill = MedalType,
                       group = fct_rev(MedalType))) +
    labs(title = paste0("Proportion of Medals Versus Team participants ",strSeason," ", nYear),
         x = "Proportion of Medals versus Team Members in",
         y = "Olympic Team Name") +
    geom_bar(aes(x = Medals/AthletesSent,
                 y = reorder(Team,TotalMedals),
                 fill = MedalType,
                 group = fct_rev(MedalType)), stat = "identity", position = myPos) +
    scale_fill_manual(values = c("gold", "#C0C0C0", "#CD7F32", "#FFFAF0"), drop = FALSE) +
    geom_text(aes(label=paste0(sprintf("%1.0f", (Medals/AthletesSent)*100),"%")),
              position=position_fill(vjust=0.5), colour="#4682B4") +
    theme_classic()
 }


#Trying to demonstrate the change in event numbers for every olympics
#
sportTotals <- function(){
Summer <- events %>% 
  filter (Season == "Summer" & Year > 1850)

summerCounts <- Summer %>% 
  group_by (Year,Sport) %>%
  count(Sport) %>% 
  arrange(Year, desc(n)) %>%
  pivot_wider(values_from = n, names_from = Sport)

summerCounts <- rotate_df(summerCounts)
#print.data.frame (summerCounts)

#names(summerCounts)
names(summerCounts) <- as.character(unlist(summerCounts[1,]))
#names(summerCounts)

summerCounts <- summerCounts[-1,]
#head(summerCounts)

summerCounts <- tibble::rownames_to_column(summerCounts, "Sport")
plsummerCounts <- head(summerCounts,16) %>% 
  pivot_longer(!Sport, names_to = "year", values_to = "count")

ggplot(plsummerCounts, aes(x = year, y = count)) + 
  geom_line(aes(color = Sport, group = Sport)) +
  facet_wrap(~Sport) +
  labs(y = "Number of Events", x = "Summer Olympic Years") +
  theme(axis.text.x = element_blank())
}

