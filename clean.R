
# Load Data ---------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(readr)
library(stringr)
library(zoo)
library(anytime)

se_report <- as.data.frame(do.call(rbind,
                                   
                                   lapply(list.files(path = "Data", full.names = TRUE), read.csv)))

# Factor starting with a digit is not liked when using ggplot
# Converting all "1st Cycle Action" to "First Cylcle Action"

se_report$Cycle <- as.character(se_report$Cycle)
index <- se_report$Cycle == "1st Cycle Action"
se_report$Cycle[index] <- "First Cycle Action"
index <- is.na(se_report$Cycle)
se_report$Cycle <- as.factor(se_report$Cycle)

# Converting blank "Letter.Type" to "Unknown"
se_report$Letter.Type <- as.character(se_report$Letter.Type)
index <- se_report$Letter.Type == ""
se_report$Letter.Type[index] <- "Unknown"
index <- is.na(se_report$Letter.Type)
se_report$Letter.Type <- as.factor(se_report$Letter.Type)

colSums(is.na(se_report))

blank_Action.Date_count <- se_report %>% filter(Action.Date == "") %>% count()
blank_Action.Date_data <- se_report %>% filter(Action.Date == "")

# Clean Data --------------------------------------------------------------
# Four differnt formats for dates within Due Date (13 char, 14 char, 15 char and 19 char + NA's. )
se_report$Due.Date <- as.character(se_report$Due.Date)
se_report$Action.Date <- as.character(se_report$Action.Date)
se_report$Issued.Date <- as.character(se_report$Issued.Date)
se_report$Received.Date <- as.character(se_report$Received.Date)

table(nchar(se_report$Due.Date))
table(nchar(se_report$Action.Date))
table(nchar(se_report$Issued.Date))
table(nchar(se_report$Received.Date))

# Convert type to Date
# First convert blanks to NA so the conversion works correctly
se_report$Action.Date[grepl("^ *$",se_report$Action.Date)] <- NA
se_report$Received.Date[grepl("^ *$",se_report$Received.Date)] <- NA
se_report$Due.Date[grepl("^ *$",se_report$Due.Date)] <- NA

se_report$Received.Date <- as.Date(se_report$Received.Date, format = "%m/%d/%Y")
se_report$Action.Date <- as.Date(se_report$Action.Date, format = "%m/%d/%Y")
se_report$Due.Date <- as.Date(se_report$Due.Date, format = "%m/%d/%Y")

# Convert type to char
se_report$STN <- as.character(se_report$STN)

# Remove column of NA values
se_report <- se_report %>% select(-Goal.Pending)

# Create a more readable report while preserving full report (rearrange dates to make more sense too!)
se_report_short <- se_report %>% select(c("PM",
                                          "Received.Date",
                                          "Goal.Days.",
                                          "Due.Date",
                                          "Action.Date",
                                          diff_days = "Difference",
                                          "Company",
                                          "Ctgry.Name",
                                          "Cycle",
                                          "STN",
                                          "Letter.Type"))


# Create Fiscal Year based on Due Date
se_report_short$Fiscal.Year <- as.integer(as.yearmon(se_report_short$Due.Date) - 9/12 + 1)

# Create a Month Feature
se_report_short$Month.Name <- months(se_report_short$Due.Date)
se_report %>% filter(is.na(Action.Date)) %>% count()

se_report_short <- se_report_short %>%
  mutate(Status = case_when(diff_days > 0 ~ 1, diff_days <= 0 ~ 0))

tmp <- se_report_short

# Functions ---------------------------------------------------------------

se_report_filtered <- function (my_filter, start_year, end_year){
  tmp <- se_report_short %>%
    filter(Fiscal.Year >= start_year) %>%
    filter(Fiscal.Year <= end_year)
  
  
  
  if (my_filter != "") {
    tmp <- tmp  %>%
      filter(PM == my_filter)
  }
  
  # Recalculate goal counts
  
  goal_met_count <- table(tmp$Status)[1]
  goal_miss_count <- table(tmp$Status)[2]
  goal_achievement_percent <- goal_met_count / (goal_met_count + goal_miss_count)
  
  
  # Create PM List
  PM_list <- tmp %>% select(PM) %>% distinct()
  
  # Tally the monthly met/miss totals
  
  # Plots -------------------------------------------------------------------
  
  # Plot type/cycle
  
  plot_type_cycle <- ggplot(tmp, aes(Letter.Type)) +
    geom_bar(aes(fill = Cycle)) +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 70, hjust = 1),
          axis.text = element_text(size=10,face="bold")) +
    coord_flip()
  
  return(list(tmp))
}


# Create PM List

PM_list <- se_report %>% select(PM) %>% distinct()

achievement_by_year_month <- se_report_short %>%
  group_by(Fiscal.Year, Month.Name) %>%
  summarise(Achievement.Rate =
              (n()- sum(Status)) / n()
  )


achievement_by_year_month$Month.Name <- ordered(
  achievement_by_year_month$Month.Name,levels = c(
    "October",
    "November",
    "December",
    "January",
    "February",
    "March",
    "April",
    "May",
    "June",
    "July",
    "August",
    "September"))

achievement_by_year_month %>% ggplot() +
  geom_point(mapping = aes(x = Month.Name,
                           y = Achievement.Rate)) +
  geom_line(aes(x = Month.Name, y = Achievement.Rate, group = 1)) +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(rows = vars(achievement_by_year_month$Fiscal.Year))