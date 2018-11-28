# Create dummy budget for attribution dataset

# Load Libraries ----
library(dplyr)
library(lubridate)
library(data.table)
library(readr)

# Load the attribution dataset
df.budget1 = fread("C:\\Users\\matcyt\\Desktop\\MarketingAttribution\\attribution_markov_dataset.csv", dec = ",")

# work laptop
df.budget1 = fread("C:\\Users\\mateusz.cytrowski\\Desktop\\Github\\attribution_markov_dataset.csv", dec = ",")

# Troubles with loading the file - decimals " , " 

# Prep ----
df.budget2 <- as.data.table(df.budget1[, -1])

str(df.budget2)


# Aggregate by impression
AggregateImpressions = function(df) {
  df %>%
    mutate(time = as_datetime(df$time),
           day = as.Date(df$time),
           impression = ifelse(df$interaction == 'impression', 1, 0)) %>%
    group_by(day, channel) %>%
    summarise(impressions = sum(impression))
}

# Create budget value ----

df.budget3 <- as.data.table(AggregateImpressions(df.budget2))

df.budget3$cost <- (df.budget3$impressions / 200) * seq(0.7, 1.3, by = 0.1)


# Total Cost for the whole period ----
final_budget <- as.data.table(
  df.budget3 %>%
    group_by(channel) %>%
    summarise(total_cost = round(sum(cost), 1))
)

final_budget

# Save the files

# Home
write_csv(df.budget3, path = "C:/Users/matcyt/Desktop/MarketingAttribution/attribution_budget_daily.csv")
write_csv(final_budget, path = "C:/Users/matcyt/Desktop/MarketingAttribution/attribution_budget_total.csv")

# Work
write.csv2(df.budget3, file = "C:/Users/matcyt/Desktop/MarketingAttribution/attribution_budget_daily.csv")
write.csv2(final_budget, file = "C:/Users/matcyt/Desktop/MarketingAttribution/attribution_budget_total.csv")
