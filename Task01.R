##Task 01
# Task 1 using the sample dataset from Prodigy

library(ggplot2)
library(dplyr)

# Load population data
pop <- read.csv("worldpopulationdata.csv")

# Inspect
str(pop)
summary(pop)

colnames(pop)

##reshape wide format into long format

library(dplyr)
library(tidyr)

pop_long <- pop %>%
  pivot_longer(
    cols = starts_with("X"),   # all columns that start with "X"
    names_to = "Year", 
    values_to = "Population"
  )

# Remove "X" from the year names
pop_long$Year <- as.numeric(gsub("X", "", pop_long$Year))

pop_2022 <- pop_long %>% filter(Year == 2022)

library(scales)

##Histogram to visualize the distribution of Country Populations (2022)

ggplot(pop_2022, aes(x = Population)) +
  geom_histogram(binwidth = 1e7, fill="skyblue", color="black") +
  scale_x_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  labs(title="Distribution of Country Populations (2022)",
       x="Population", y="Number of Countries")


##Bar chart to visualize the top 10 Most Populous Countries (2022)

pop_top10 <- pop_2022 %>%
  arrange(desc(Population)) %>%
  head(10)

ggplot(pop_top10, aes(x = reorder(Country.Name, Population), y = Population)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  labs(title = "Top 10 Most Populous Countries (2022)",
       x = "Country",
       y = "Population")

# Line plot for some selected countries to analyze the Population Growth (2001–2022) for them
countries_selected <- c("Egypt, Arab Rep.", "India", "China", "United States")

pop_selected <- pop_long %>% 
  filter(Country.Name %in% countries_selected, Year >= 2001, Year <= 2022)

ggplot(pop_selected, aes(x=Year, y=Population, color=Country.Name)) +
  geom_line(size=1.2) +
  labs(title="Population Growth (2001–2022)",
       x="Year", y="Population") +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) 
  




