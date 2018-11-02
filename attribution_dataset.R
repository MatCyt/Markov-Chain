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

df.initial = fread("C:\\Users\\mateu\\Desktop\\MarketingAttribution\\sample.dataset.main.csv")

df.cut = df.initial[, c("cookie", "time", "event", "creative_name", "deviceType_name", "country_name", "conversion")]

# Cookie
df.cut$cookie2 = casefold(df.cut$cookie, upper = TRUE)
df.cut$cookie2 = chartr("1234567890", "jknofghlmi", df.cut$cookie2)
df.cut$cookie2 = chartr("glmj", "3790", df.cut$cookie2)
df.cut$cookie3 = substr(df.cut$cookie2, 4, 28)

df.cut = df.cut[, -c("cookie", "cookie2")]

# Country & City & Devide
country = "Germany"

cities = c("Berlin", "Hamburg", "Munich", "Cologne", "Frankfurt")
city = sample(cities, 1000000, replace = TRUE, prob = c(0.42, 0.2, 0.16, 0.13, 0.1)) #based on population

devices = c("PC", "Mobile", "Tablet", "SmartTV")
device = sample(devices, 1000000, replace = TRUE, prob = c(0.35, 0.6, 0.04, 0.01))

# Time
df.cut2 = 
  df.cut %>%
  mutate(time = as_datetime(time),
         date = as.Date(time)) %>%
  filter(date <= "2014-12-31") %>%
  mutate(time2 = time + years(3) + months(7),
         day2 = as.Date(time2)) %>%
  select(-time, -date) 

# Cut rows
df.cut3 = df.cut2[-sample(1:nrow(df.cut2), 750792), ]

str(df.cut3)



# Events
table(df.cut3$event3)

df.cut3$event3 = sample(c("Impression", "Conversion"), 1000000, replace = TRUE, prob = c(0.98, 0.02))

# Dataset

dataset = data.table(
  cookie = 
)

# Explore the dataset

