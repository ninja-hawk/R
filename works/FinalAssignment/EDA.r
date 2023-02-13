# Put DataForTable2.1.xls in the appropriate directory
# Read data
data <- read.csv("FinalAssignment/DataForTable2.1.csv", header = TRUE)

# Load dplyr
library(dplyr)

# include Na or not

na_column <- data %>% lapply(.,anyNA) %>% unlist

print(na_column)

print(table(is.na(data)))



# get how many kinds of Rank in the data
country_labels <- data %>%
  group_by(Country.name) %>%
  summarize(total_count = n(), .groups = "drop")

year_labels <- data %>%
  group_by(year) %>%
  summarize(total_count = n(), .groups = "drop")

print(country_labels)
print(year_labels)


# erase the rows which have
data <- na.omit(data)

# get how many kinds of countries in the data
country_labels <- data %>%
  group_by(Country.name) %>%
  summarize(total_count = n(), .groups = "drop")

# get how many years in the data
year_labels <- data %>%
  group_by(year) %>%
  summarize(total_count = n(), .groups = "drop")

print(country_labels)
print(year_labels)