#############################################
#Programmed by Raymond Eddie Almodóvar Jr.  #
#              Vinay M.                     #
#############################################
#Descr.: Plots & Maps PPD Incident Data     #
#############################################
#Last Revised: Wednesday ; November 21, 2021#
#############################################


library(ggplot2)
library(tidyverse)
library(readxl)
library(plyr)


#Reads Excel File
#change file path to where your data is save at; will not work otherwise
PPDincidentTwentyTwenty <- read_excel("Desktop/INFO-250-001/R Directory/incidents_part1_part2.xlsx")
#View(PPDincidentTwentyTwenty) #View data table

#filtered dataset soley for violent crimes
violentPPDincidentTwentyTwenty <- filter(PPDincidentTwentyTwenty, PPDincidentTwentyTwenty$'Statutory Offense' %in% c("Aggravated Assault Firearm", "Aggravated Assault No Firearm", "Homicide - Criminal", "Other Assaults", "Rape", "Robbery Firearm", "Robbery No Firearm"))
#View(violentPPDincidentTwentyTwenty) #View data table


#Formats Date variable as a Date Type 
violentPPDincidentTwentyTwenty$Date <- as.Date(violentPPDincidentTwentyTwenty$Date)


#Creates table to show frequencys of All Crime and then All Violent Crimes
allCrimeFrequency <- table(PPDincidentTwentyTwenty$'Statutory Offense')
violentCrimeFrequency <- table(violentPPDincidentTwentyTwenty$'Statutory Offense')


#Displays the above tables of Frequency in console; used to see which catagories were too small to include
allCrimeFrequency
violentCrimeFrequency


#Graphs a Bar Chart with the count of each of the Violent Offense catagories
#Note: The definition of assault varies by jurisdiction, but is generally defined as intentionally putting another person
#      in reasonable apprehension of an imminent harmful or offensive contact. Physical injury is not required.
ggplot(data = violentPPDincidentTwentyTwenty, aes(x = violentPPDincidentTwentyTwenty$'Statutory Offense', fill = violentPPDincidentTwentyTwenty$'Statutory Offense')) +
  geom_bar(show.legend = FALSE, color="black") + #Disables Legend
  labs(y="Crime Incident Count", x="Crime Type", title="2020 Phiadelphia Violent Crime Incident Count") + #Y and X axis titles; Main Titles
  theme(plot.title = element_text(hjust = 0.5)) + #Centers main title
  geom_text(stat='count', aes(label=..count..), vjust=-1, family="Times") + #Adds count above each bar
  theme(text = element_text(family = "Times New Roman"))


#Create a new coded field using the Hour variable to distinquish the time of day a crime incident occured
dataWithTimeOfDay <- mutate(violentPPDincidentTwentyTwenty, 'Time Of Day' = ifelse(Hour %in% 5:11 ,"Morning: 5AM-12PM",
                                                                            ifelse(Hour %in% 12:16, "Afternoon: 12PM-5PM",
                                                                            ifelse(Hour %in% 17:20, "Evening: 5PM-9PM",
                                                                            ifelse(Hour %in% 21:23, "Night: 9PM-5AM", "Night: 9PM-5AM")))))
#view(dataWithTimeOfDay) #Views the new mutated table for quality assurance


#Set order of time of day by factoring
dataWithTimeOfDay$'Time Of Day' <- factor(dataWithTimeOfDay$'Time Of Day', levels = c("Morning: 5AM-12PM", "Afternoon: 12PM-5PM", "Evening: 5PM-9PM", "Night: 9PM-5AM"))


#Graphs a STACKED Bar Chart (stacked with Offense) with the count of violent crimes by time of day
ggplot(data = dataWithTimeOfDay, aes(x = dataWithTimeOfDay$'Time Of Day', fill = dataWithTimeOfDay$'Statutory Offense')) +
  geom_bar(color="black") + #Disables Legend
  labs(y="Crime Incident Count", x="Time of Day", title="2020 Philadelphia Violent Crime Incident Count By Time of Day (Offense Breakdown)") + #Y and X axis titles; Main Titles
  theme(plot.title = element_text(hjust = 0.5)) + #Centers main title
  geom_text(stat='count', aes(label=..count..), position = position_stack(vjust = .5), color = "white", family="Times") + #Adds count above each bar; centered count for each stack with position_stack(vjust = .5)
  scale_fill_discrete(name = "Offense") + #legend name
  theme(text = element_text(family = "Times New Roman"))


#Graphs a Bar Chart with the count of violent crimes by time of day
ggplot(data = dataWithTimeOfDay, aes(x = dataWithTimeOfDay$'Time Of Day', fill = dataWithTimeOfDay$'Time Of Day')) +
  geom_bar(color="black", show.legend = FALSE) + #Disables Legend
  labs(y="Crime Incident Count", x="Time of Day", title="2020 Philadelphia Violent Crime Incident Count By Time of Day") + #Y and X axis titles; Main Titles
  theme(plot.title = element_text(hjust = 0.5)) + #Centers main title
  geom_text(stat='count', aes(label=..count..), vjust=-1, family="Times") + #Adds count above each bar
  scale_fill_discrete(name = "Crime Type") +
  theme(text = element_text(family = "Times New Roman"))


dataWithCount <- mutate(violentPPDincidentTwentyTwenty, Count = 1)
#view(dataWithCount) #Views the new mutated table for quality assurance

#Aggreates data into Vector type
crimeDateCount <- aggregate(dataWithCount["Count"], by=dataWithCount["Date"], sum)
crimeDateCount


#Annotation font size adjustments
geom.text.size = 4
theme.size = (13/5) * geom.text.size


#Violent Crime Time Series Plot
ggplot(data = crimeDateCount, aes(x=Date, y=Count, color= "Red")) +
  geom_line(show.legend = FALSE) +
  scale_x_date(date_breaks = "month")+
  labs(y=" Crime Incident Count", x="Date", title="2020 Philadelphia Violent Crime Incident Count Time Series") + #Y and X axis titles; Main Titles
  theme(plot.title = element_text(hjust = 0.5)) +#Centers main title
  annotate(geom="text", x=as.Date("2020-09-10"), y=188, size=geom.text.size, family="Times",
    label="October 27, 2020 | Protest (and other demonstrations) \nafter the PPD fatally shot Walter Wallace Jr.") +
  annotate(geom="point", x=as.Date("2020-10-27"), y=191, size=2, shape=21, fill="red") +
  theme(text = element_text(family = "Times New Roman"))


#Graphs a STACKED Bar Chart (stacked with Offense) with the count of Violent Crime by Hour
ggplot(data = dataWithTimeOfDay, aes(x = dataWithTimeOfDay$Hour, fill = dataWithTimeOfDay$'Statutory Offense')) +
  geom_bar(color="black") + #Disables Legend
  labs(y="Crime Incident Count", x="Hour (24 Hour Standard)", title="2020 Philadelphia Violent Crime Incident Count By Hour (Offense Breakdown)") + #Y and X axis titles; Main Titles
  theme(plot.title = element_text(hjust = 0.5)) + #Centers main title
  geom_text(stat='count', aes(label=..count..), position = position_stack(vjust = .5), color = "white", family="Times") + #Adds count above each bar; centered count for each stack with position_stack(vjust = .5)
  scale_fill_discrete(name = "Offense") + #legend name
  theme(text = element_text(family = "Times New Roman")) +
  scale_x_continuous(breaks=c(0:23))


#Violent Crime by Hour
ggplot(data = dataWithTimeOfDay, aes(x = dataWithTimeOfDay$Hour, fill = dataWithTimeOfDay$Hour)) +
  geom_bar(color="black", show.legend = FALSE) + #Disables Legend
  labs(y="Crime Incident Count", x="Hour (24 Hour Standard)", title="2020 Philadelphia Violent Crime Incident Count By Hour") + #Y and X axis titles; Main Titles
  theme(plot.title = element_text(hjust = 0.5)) + #Centers main title
  geom_text(stat='count', aes(label=..count..), vjust=-1, family="Times") + #Adds count above each bar
  scale_fill_discrete(name = "Crime Type") +
  theme(text = element_text(family = "Times New Roman")) +
  scale_x_continuous(breaks=c(0:23))



