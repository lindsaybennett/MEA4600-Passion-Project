## MEA4600 Passion Project
## by Lindsay Bennett

## Police Citations in Johns Creek, GA: Analyzing Racial Disparities in Ticketing

## Background: Addressing the Rumors
# Johns Creek, Georgia is a large suburb of Atlanta incorporated in late 2006, home to many upper-middle-class families. 
# Growing up in the city, high school students are often told cautionary tales about the Johns Creek police and their ticketing habits. 
# In particular, I remember constantly hearing that we needed to be careful towards the end of the month, because the police had "ticketing quotas" they needed to fill
# Now, the "ticketing quota" rumor is not entirely unfounded. In 2022, CBS46 (https://www.atlantanewsfirst.com/2022/04/11/its-point-system-how-policing-incentives-encourage-officers-write-more-tickets/) conducted an investigation into the Atlanta Police Department's incentivized ticketing policy. 
# The investigation found that police officers were being awarded points for different citations completed within one day, and even more compelling, that they had a minimum number of points to acquire each day. 
# Although this investigation suggests that the police department's behavior is influenced by a reward system, it does not directly address quotas by month. 
# In this section of the report, I used the Johns Creek Data Hub's (https://datahub.johnscreekga.gov) police citation dataset with records from 2008-2022 to analyze the volume of tickets given over the course of a month. 

### Loading Packages and Data
library(tidyverse)
library(lubridate)
install.packages('ggmap')
library(ggmap)
library(gridExtra)
install.packages('lutz')
library(lutz)
install.packages('sf')
library(sf)
install.packages('suncalc')
library(suncalc)

PoliceCitations <- read.csv("~/Downloads/Police_Citations_Written.csv")
View(PoliceCitations)

## Visualizing Citations Over The Month
# First, let's start by creating simple graphs depicting the number of citations given on each day of the month from 2008-2022. 
ggplot(PoliceCitations, aes(x=mday(DateTimeOccurred))) +
  geom_bar(aes(fill = ..count..)) +
  labs(x="Day of Month",
       y="Number of Citations",
       fill="Number of Citations",
       title="Number of Citations Given Each Day of The Month")

# From this graph, it appears as though the number of citations given actually decreases towards the end of the month
# This graph illustrates that the 29th, 30th, and 31st day of each month have some of the least numbers of citations given
# It is important to note, however, that not every month has a 31st day, which is most likely why the number of citations decreases so drastically
# Overall, this graph shows little variability in the number of citations given throughout the month

# This data contains both citations and warnings given, so I wondered if the pattern changes based on the type of police encounter:
## Citations Only by day of month
CitationsOnly <- PoliceCitations %>%
  filter(WarningOnly %in% "No")

ggplot(CitationsOnly, aes(x=mday(DateTimeOccurred))) +
  geom_bar(aes(fill = ..count..)) +
  labs(x="Day of Month",
       y="Number of Citations",
       fill="Number of Citations",
       title="Number of Citations Given Each Day of The Month")

## Warnings Only by day of month
WarningsOnly <- PoliceCitations %>%
  filter(WarningOnly %in% "Yes")

ggplot(WarningsOnly, aes(x=mday(DateTimeOccurred))) +
  geom_bar(aes(fill = ..count..)) +
  labs(x="Day of Month",
       y="Number of Warnings",
       fill="Number of Warnings",
       title="Number of Warnings Given Each Day of The Month")

# As seen in these two visualizations, the number of warnings given has more variation over the month than the number of citations given
# However, despite the increased variability in the number of warnings given, the total number of warnings given towards the end of the month is still less than the rest of the month
# A caveat to these visualizations is the scale, considering that there have been 95,439 citations since 2008 and only 6,947 warnings recorded since 2008.

# Well, if the police are not giving more citations or warnings at the end of the month to everyone, maybe they're specifically targeting younger people?
# Could this be why the rumor travels mainly among high school students? Let's find out:
## all citations for people ages 17-25
YoungPeople <- PoliceCitations %>%
  filter(AgeGroup %in% "17 - 25")

ggplot(YoungPeople, aes(x=mday(DateTimeOccurred))) +
  geom_bar(aes(fill = ..count..))  +
  labs(x="Day of Month",
       y="Number of Citations",
       fill="Number of Citations",
       title="Number of Citations Given Each Day of The Month")

# Interesting, once again the number of citations given to young people decreases in the last couple days of the month
# The largest number of citations are given around the 25th day of each month, which is towards the end and could be causing the rumor
# However, the variability in citations between days is still low, and it does not seem that the peak on the 25th is large enough to issue concern

# Johns Creek's population is 43% minorities, mainly Asian and Black individuals. 
# Given the police department's reputation, which Johns Creek is not exempt to, the ticketing quotas could disproportionately affect the minority populations.
# To analyze the end-of-the-month rumor further, I separated the citations by White people, Black people, and Asian people. 
WhiteJC <- PoliceCitations %>%
  filter(Race %in% "White")

WhiteGraph1 <- ggplot(WhiteJC, aes(x=mday(DateTimeOccurred))) +
  geom_bar(aes(fill = ..count..)) +
  labs(x="Day of Month",
       y="Number of Citations",
       fill="Number of Citations",
       title="Number of Citations Given to White People Each Day of The Month")

# The graph of citations given to white people is consistent with the previous visualizations with low variability and a decrease in the last few days
# Surprisingly, the number of citations given to white people is highest at the beginning of the month
# Overall, there were 56,969 citations given to white people from 2008-September 2022

BlackJC <- PoliceCitations %>%
  filter(Race %in% "Black")

BlackGraph1 <- ggplot(BlackJC, aes(x=mday(DateTimeOccurred))) +
  geom_bar(aes(fill = ..count..)) +
  labs(x="Day of Month",
       y="Number of Citations",
       fill="Number of Citations",
       title="Number of Citations Given to Black People Each Day of The Month")

# The graph of citations given to Black people has more variation across the days, but continues to show a decrease in the last few days
# Overall, there were 19,665 citations given to Black people from 2008-September 2022

AsianJC <- PoliceCitations %>%
  filter(Race %in% "Asian")

AsianGraph1 <- ggplot(AsianJC, aes(x=mday(DateTimeOccurred))) +
  geom_bar(aes(fill = ..count..)) +
  labs(x="Day of Month",
       y="Number of Citations",
       fill="Number of Citations",
       title="Number of Citations Given to Asian People Each Day of The Month")

# The graph of citations given to Asian people is consistent with the overall citation patterns, where variability in number of citations is low across the days and the number decreases in the last few days of each month
# Overall, there were 17,748 citations given to Asian people from 2008-September 2022

# To save space in the final report, I'm combining these graphs:
RaceCitationsFreq <- grid.arrange(WhiteGraph1, BlackGraph1, AsianGraph1, nrow=3)

## Wait a minute... That doesn't add up 
# 57.7% of Johns Creek is made up of white people, and they've received 56,969 citations out of 102,386 citations (55.64%)
# 25.52% of Johns Creek is made up of Asian people, and they've received 17,748 citations out of 102,386 citations (17.33%)
# 9.99% of Johns Creek is made up of Black people, and they've received 19,665 citations out of 102,386 citations (19.21%)

# These numbers highlight a glaring issue: Black people make up less than 10% of the total population, but almost 20% of the total citations given???
# The end-of-month rumor is clearly just a rumor, shown by the previous visualizations, but this disparity looks very real.
# This next section of the report will use the same Police Citation data from the Johns Creek Data Hub, along with historical Population Data from the same Data Hub

# In 2020, the Stanford Open Policing Project began an analysis into traffic citations across the United States. 
# They utilized the Benchmark Test and the Veil of Darkness Test to determine disparities in the rate at which police issue citations, accounting for external factors like population proportions and daylight. 
# To analyze the Johns Creek Police Department's disparities and bias, I followed the same approach:

### THE BENCHMARK TEST -- THE STANFORD POLICING PROJECT
# Creating a data set with full dates, years, and time of citations
# Filtering the races to only include those with a major proportion of the population: White, Asian, and Black people
# Filtering out warnings to focus only on citations
# Setting the year range from 2010-2022, data collected before 2010 was scarce and the Johns Creek police department opened in 2008
PoliceCitations_Clean <- PoliceCitations %>%
  mutate(date=DateTimeOccurred,
         year=year(DateTimeOccurred),
         times= substr(DateTimeOccurred, 12, 19),
         Date_Cleaned = as.Date(DateTimeOccurred),
         time=hms(times)) %>%
  filter(Race %in% c("Asian", "Black", "White"), WarningOnly %in% "No", year >= 2010)

PoliceCitations_Clean %>% count(Race)
# There are 53,413 citations given to White people in the clean data set
# There are 16,882 citations given to Asian people in the clean data set
# There are 18,217 citations given to Black people in the clean data set

PoliceCitations_2 <- PoliceCitations_Clean %>%
  count(Race) %>%
  mutate(prop = n/sum(n))
# White people received 60.35% of the citations in the clean data set
# Asian people received 19.07% of the citations in the clean data set
# Black people received 20.58% of the citations in the clean data set

# Adding a variable for the proportion of the population that each race occupies
PoliceCitations_3 <- PoliceCitations_Clean %>%
  count(year, Race) %>%
  mutate(popprops = case_when(Race == "Asian" ~ 0.2552,
                              Race == "Black" ~ 0.099,
                              Race == "White" ~ 0.577))

## Total Number of Citations Given to Each Race Each Year
PoliceCitations_Clean %>% 
  count(year, Race) %>% 
  ggplot(aes(x = as.factor(year), y = n, color = Race, group=Race)) +
  geom_point() +
  geom_line(aes(group=Race)) +
  labs(x="Year",
       y="Number of Citations Given",
       title="Raw Number of Citations Given to Each Race by Year")

# This visualization shows that white people receive the most number of citations in each year
# The number of citations given to white people is decreasing over time, whereas the number of citations given to Black and Asian people stays pretty consistent
# These numbers are not adjusted for the population proportions, so it makes sense that white people receive the most citations since they occupy most of the population

# We have to take into account that Black and Asian people make up a much smaller proportion of the population than white people
# To do this, we have to create a benchmark to compare the citations given
# In this case, the benchmark will be the rate at which citations are given to each race
# This rate is calculated by dividing the number of citations given to the race/race's population in Johns Creek for each year
WhiteProps <- data.frame(year = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022),
                         race = rep("White", 13),
                         cite_rate = c(4314/44256, 5401/44602, 6396/44948, 5438/45237, 4461/45583, 4971/45929, 5286/46275, 4541/46564, 3981/46910, 4845/48468, 2496/48814, 2141/47891, 2687/48237))
BlackProps <- data.frame(year = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022),
                         race = rep("Black", 13),
                         cite_rate = c(1058/7593, 1369/7653, 1624/7712, 1634/7762, 1420/7821, 1498/7880, 1935/7940, 1702/7989, 1801/8049, 2150/8316, 1087/8375, 862/8217, 1524/8276))
AsianProps <- data.frame(year = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022),
                         race = rep("Asian", 13),
                         cite_rate = c(745/19574, 1288/19727, 1656/19880, 1487/20008, 1345/20161, 1882/20314, 2003/20467, 1536/20595, 1528/20748, 1819/21437, 785/21590, 655/21182, 1019/21335))  
AllProps <- rbind(WhiteProps, BlackProps, AsianProps)

## Rate at which Citations are Given to each Race, relative to their proportion of the population
AllRaces_Graph1 <- AllProps %>%
  ggplot(aes(x=as.factor(year), y=cite_rate, color=race, group=race)) +
  geom_point() +
  geom_line(aes(group=race)) +
  labs(x="Year",
       y="Rate at Which Citations are Given",
       title="Citation Rate for Each Race by Year",
       color="Race")

# This graph reveals that Black people have a much higher citation rate than White or Asian people across all years
# Black citation rates dropped from 2019-2021, perhaps due to police criticism from the Black Lives Matter movement in 2020 and the COVID-19 shutdown of typical work procedures
# However, since 2021, the Black citation rate has nearly doubled by the end of Q3 in 2022. 

# We can calculate the average citation rate across the years to compare the likelihood of each race receiving a citation:
AvgWhiteRate <- mean(WhiteProps$cite_rate)
AvgBlackRate <- mean(BlackProps$cite_rate)
AvgAsianRate <- mean(AsianProps$cite_rate)

#Likelihood of Black People Being Cited Compared to White People Being Cited
AvgBlackRate/AvgWhiteRate
# On average, Black people in Johns Creek are 2x more likely to be given a citation than white people

#Likelihood of Asian People Being Cited Compared to White People Being Cited
AvgAsianRate/AvgWhiteRate
# On average, Asian people in Johns Creek are 1.4x less likely to be given a citation than white people

# These numbers are surprising and reveal a ticketing bias towards the Black population

## How significant is the bias?
# If we compare white citation rates to minority citation rates, how likely is it that the minority citation rates will be higher?
# For this comparison, I aggregated Black and Asian citation rates to represent the minority rates

AllProps_GraphData <- AllProps %>%
  mutate(group=case_when(race %in% "White" ~ "White",
                         race %in% c("Asian", "Black") ~ "Minority")) %>%
  group_by(year, group) %>%
  summarize(cite_rate = sum(cite_rate))

maxcite <- max(AllProps_GraphData$cite_rate)
mincite <- min(AllProps_GraphData$cite_rate)

# Creating a wide-format data set with white and minority citation rates over the years
PropsGraph_Transposed <- data.frame(minority_cite_rate = c(0.17739956, 0.24417532, 0.29388071, 0.28483303, 0.24827542, 0.28274699, 0.34156763, 0.28762414, 0.29740016, 0.34339105, 0.16615047, 0.13582695, 0.23190882),
                                    white_cite_rate = c(0.09747831, 0.12109322, 0.14229777, 0.12021133, 0.09786543, 0.10823227, 0.11423015, 0.09752169, 0.08486463, 0.09996286, 0.05113287, 0.04470569, 0.05570413),
                                    year=c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022))

# Plotting Minority Citation Rates against White Citation Rates
PropsGraph_Transposed %>% 
  ggplot(aes(
    x = white_cite_rate,
    y = minority_cite_rate
  )) +
  geom_point() +
  # This sets a diagonal reference line (line of equal hit rates)
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  # These next few lines just make the axes pretty and even
  scale_x_continuous("White Citation Rate", 
                     limits = c(0, maxcite + 0.01),
                     labels = scales::percent
  ) +
  scale_y_continuous("Minority Citation Rate", 
                     limits = c(0, maxcite + 0.01),
                     labels = scales::percent
  ) +
  # This makes sure that 1% on the x-axis is the same as 1% on the y-axis
  coord_fixed()

# In the graph above, points above the dashed, diagonal line represent times where the minority citation rate was higher than the white citation rate
# Points below the dashed, diagonal line represent time where the minority citation rate was lower than the white citation rate
# From the graph, we can tell that across 2010-2022, the rate at which citations are given to minorities is consistently higher than the rate at which citations are given to white people

# The Stanford Open Policing Project states that the Benchmark test has a few caveats, one of which being that it cannot account for how many crimes were actually committed (how many times a citation is justified).
# Do certain races just commit less crimes? It's highly unlikely, but the citation rates do not take into account how many citations were actually justified. 
# To account for proportion of crimes committed, the Stanford Open Policing Project recommends using the Veil of Darkness Test.
# The Veil of Darkness Test relies on the hypothesis that officers who engage in racial profiling are less likely to be able to identify a person's race (especially while driving) after dark than during the daylight
# Under this hypothesis, if citations given after dark have a smaller proportion of Black people cited than citations given during the daylight, this would be evidence of the presence of racial profiling

### THE VEIL OF DARKNESS TEST, invented by Grogger and Ridgeway in 2006 -- THE STANFORD POLICING PROJECT:
# In order to test citations given before and after dark accurately, we have to calculate Johns Creek's intertwilight period
# The intertwilight period is a period of time during the day that has light during one part of the year and is dark during another part of the year
# By calculating Johns Creek's intertwilight period, we can compare citations given to each race when it is light at that time to when it is dark at that time
# If more citations are given to minorities at that time when it is light outside than when it is dark, it could be evidence of racial profiling

# First, we must calculate the time zone for our citations
# Johns Creek, GA is in the Eastern Standard Timezone, which R specifies as the "America/New York" timezone
geox <- PoliceCitations_Clean$geox
geoy <- PoliceCitations_Clean$geoy
timezone <- tz_lookup_coords(geoy, geox, warn = F)

# Helper function: this code converts the citation times in my data to minutes
library(hms)
time_to_minute <- function(time) {
  hour(hms(time)) * 60 + minute(hms(time))
}

# This clean data set contains the location and time of each citation given, in the EST timezone
PoliceCitations_Dates <- PoliceCitations_Clean %>%
  mutate(DateTimeOccurred = as.POSIXct(DateTimeOccurred, tz="America/New_York")) %>%
  select(geox, geoy, DateTimeOccurred)

# Compute sunset time for each date in our dataset
# To calculate the intertwilight period, we are finding the dusk and sunset times in Johns Creek throughout the year
library(hms)
library(chron)
sunset_times <- PoliceCitations_Dates %>%
  mutate(
    lat = geoy,
    lon = geox,
    date=as.Date(DateTimeOccurred)) %>% 
  select(DateTimeOccurred, lat, lon, date) %>%
  distinct() 

sunset_times <- sunset_times %>%
  getSunlightTimes(data=.,
                   keep = c("sunset", "dusk"), 
                   tz = "America/New_York") %>%
  mutate_at(vars("sunset", "dusk"), ~format(., "%H:%M:%S", tz="America/New_York"))

library(data.table)
sunset_times <- sunset_times %>%
  mutate(sunset = as.ITime(sunset),
         dusk = as.ITime(dusk)) %>%
  filter(!is.na(dusk), !is.na(sunset))

sunset_times <- sunset_times %>%
  mutate(sunset = chron(times. = sunset, format = "hh:mm:ss"),
         dusk = chron(times.= dusk, format = "hh:mm:ss"))

sunset_times <- sunset_times %>% 
  mutate(sunset_minute = time_to_minute(sunset),
    dusk_minute = time_to_minute(dusk),
    date = ymd(substr(date, 1, 10))) %>% 
  select(date, sunset, dusk, ends_with("minute"))

# Saving the sunset and dusk times to a separate data set for safety
write.csv(sunset_times, "~/Downloads/Sunsets_And_Dusk.csv")

## Johns Creek's Inter-Twilight Period
sunset_times %>% 
  filter(dusk == min(dusk) | dusk == max(dusk))
## the earliest sunset was around 5:30pm in December 2020 (and it was fully dark by 5:55pm)
## the latest sunset was around 9:11pm in June 2011 (and it was fully dark by 9:38pm)

# Rename the date variable to be consistent in each data set
sunsets_data <- sunset_times %>%
  mutate(Date_Cleaned = date)

# Calculate the earliest time that dusk occurred
min(sunsets_data$dusk_minute)

# Create a dataset of citations given only during Johns Creek's twilight period
intertwilight_cites <- 
  PoliceCitations_Clean %>% 
  left_join(
    sunsets_data,
    by = "Date_Cleaned"
  ) 

intertwilight_cites <- intertwilight_cites %>%
  mutate(times = as.ITime(times))

intertwilight_cites <- intertwilight_cites %>%
  mutate(
    minute = ((hour(times)*60) + minute(times) + (second(times)/60)),
    minutes_after_dark = (minute - dusk_minute),
    is_dark = minute > dusk_minute,
    min_dusk_minute = 1070,
    max_dusk_minute = 1300,
    is_black = Race == "Black"
  ) %>% 
  filter(
    # Filter to get only the intertwilight period
    minute >= min_dusk_minute,
    minute <= max_dusk_minute,
    !(minute > sunset_minute & minute < dusk_minute)
  ) %>%
  distinct(OBJECTID, .keep_all = TRUE)

# Comparing Citation Rates for Black People Between 6:30-6:45pm
intertwilight_cites %>% 
  filter(time > hm("18:30"), time < hm("18:45")) %>% 
  group_by(is_dark) %>% 
  summarize(prop_black = mean(Race=="Black"))  
# 6:30-6:45 is in the intertwilight period in Johns Creek
# This table tells us the proportion of citations given to Black people when it's dark vs. when it's light
# When it's light outside, the proportion of citations given to Black people is 20.7%
# However, when it's dark outside, the proportion of citations given to Black people decreases to 18.5%

# This only gives us evidence of racial profiling between 6:30-6:45pm in Johns Creek
# Next, we have to check every time in our citations data set
# To do so, we will use a logistic regression model to analyze darkness and clock time's influence on whether a Black person will receive a citation:
BlackCitations_Model1 <- glm(
  is_black ~ is_dark + splines::ns(minute, df = 6),
  family = binomial,
  data = intertwilight_cites
)

# From this model, we can identify the "Estimate" to determine how darkness effects whether a Black person receives a citation
summary(BlackCitations_Model1)$coefficients["is_darkTRUE", c("Estimate", "Std. Error")]
# The estimate for darkness in this model is -0.0322, the negative number indicates that darkness lessens the likelihood of a Black person receiving a citation
# This result supports our hypothesis that if racial profiling is present, Black people will be more likely to receive a citation during the daylight than in darkness
# The standard error for this model is 0.05, a small number, which indicates that our model is statistically significant
# In other words, darkness significantly lessens the likelihood of a Black person receiving a citation

# To check for robustness, we can adjust our model to account for location of the Police District for each citation. 
# This attempts to account for differences in location in our model
# Adjusting the Model for Location
Model2_LocationAdj <- glm(
  is_black ~ is_dark + splines::ns(minute, df = 6) + as.factor(PDDistrict),
  family = binomial,
  data = intertwilight_cites
)

summary(Model2_LocationAdj)$coefficients["is_darkTRUE", c("Estimate", "Std. Error")]
# The estimate for darkness in this model is -0.034, which indicates that when accounting for location, darkness still lessens the likelihood of a Black people receiving a citation
# This result also supports our hypothesis that if racial profiling is present, Black people will be more likely to receive a citation during the daylight than in darkness
# The standard error for this model is also 0.05, indicating statistical significance of the model

## Conclusions
# The rumor that police give more citations towards the end of the month may be disproved, but that doesn't mean that bias and incentives don't influence behaviors at the police department
# In Johns Creek, the rate at which different races receives citations varies greatly. 
# On average in Johns Creek, white citation rates are around 9.5%, Black citation rates are around 18.99%, and Asian citation rates are around 6.66%
# All of these rates dropped in 2020, likely due to heavy police criticism and efforts by the Black Lives Matter movement
# On average in Johns Creek, Black people are twice as likely to receive a citation than white people
# When examining racial profiling, we found that Black people are more likely to receive citations in the daylight than in the darkness
# Given the proportion of Black people in Johns Creek, it is evident that the police department gives Black people citations at disproportionate rates to their white counterparts
# It is evident that disproportionate citation rates may be due to racial profiling, noted by significantly different citation rates for Black people in the daylight vs. in the darkness







