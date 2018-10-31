# Dummy Dataset Creation for Marketing Attribution on Cookie Level using Markov Chains

# Libraries ----
library(dplyr)
library(data.table)
library(lubridate)

# Cookie
# Time
# Interaction
# Channel
# Type of message
# Value
# Type
# Device

df_initial = fread("C:\\Users\\mateu\\Desktop\\MarketingAttribution\\sample_dataset_main.csv")

df_cut = df[, c("cookie", "time", "event", "creative_name", "deviceType_name", "country_name", "conversion")]

# Cookie
df_cut$cookie2 = casefold(df_cut$cookie, upper = TRUE)
df_cut$cookie2 = chartr("1234567890", "jknofghlmi", df_cut$cookie2)
df_cut$cookie2 = chartr("glmj", "3790", df_cut$cookie2)
df_cut$cookie3 = substr(df_cut$cookie2, 4, 28)

df_cut = df_cut[, -c("cookie", "cookie2")]

# Country & City & Devide
country = "Germany"

cities = c("Berlin", "Hamburg", "Munich", "Cologne", "Frankfurt")
city = sample(cities, 1000000, replace = TRUE, prob = c(0.42, 0.2, 0.16, 0.13, 0.1)) #based on population

devices = c("PC", "Mobile", "Tablet", "SmartTV")
device = sample(devices, 1000000, replace = TRUE, prob = c(0.35, 0.6, 0.04, 0.01))

# Dataset

dataset = data.table(
  cookie = 
)

# Explore the dataset

