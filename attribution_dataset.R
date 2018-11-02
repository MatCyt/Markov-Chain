# Dummy Dataset Creation for Marketing Attribution on Cookie Level using Markov Chains

# Libraries ----

library(data.table)
library(lubridate)
library(dbplyr)

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
table(df.cut3$new_event)

df.cut3$new_event = sample(c("impression", "conversion"), 1000000, replace = TRUE, prob = c(0.98, 0.02))

str(df.cut3)

# Conversions
df.cut3$new_conversion = 0
df.cut3$new_conversion[df.cut3$new_event == "conversion"] = 1
  
table(df.cut3$new_conversion)



# Conversion Value

prices <- seq(from = 4, to = 8.5, by = 0.5)
prices

df.cut3$conversion_value = 0
df.cut3$conversion_value[df.cut3$new_conversion == 1] = df.prices

df.prices <- sample(prices, 19653, replace = TRUE,
                    prob = c(0.05, 0.05, 0.1, 0.1, 0.2, 0.2, 0.1, 0.1, 0.05, 0.05))

# Channels
df.cut3 %>%
  group_by(channel) %>%
  summarise(sum(new_conversion))

df.cut3$channel = 0

df.cut3 = 
  df.cut3 %>%
  mutate(channel = replace(channel, creative_name == "Amnet_DTH_all_creatives", "Social Media"),
         channel = replace(channel, creative_name == "Agora_doublebillboard", "Paid Search"),
         channel = replace(channel, creative_name == "Polsat_all_banners_CPC", "Online Video"),
         channel = replace(channel, creative_name == "WP_SG_doublebillboard", "Online Display"),
         channel = replace(channel, creative_name == "Interia_doublebillboard", "Paid Search"),
         channel = replace(channel, creative_name == "Polsat_Double_overlay", "Paid Search"),
         channel = replace(channel, creative_name == "Polsat_welcome_screen", "Online Display"),
         channel = replace(channel, creative_name == "WP_sport,turystyka,film_doublebillboard", "Paid Search"),
         channel = replace(channel, creative_name == "Interia_rectangle", "Paid Search"))



table(df.cut3$channel)

table(df.cut3$creative_name)

# Dataset

str(df.cut3)

final_data = 
  df.cut3 %>%
  select(cookie3, time2, new_event, new_conversion, conversion_value, channel) %>%
  mutate(device_type = device, country = country, city = city)

colnames(final_data) = c("cookie", "time", "interaction", "conversion", "conversion_value", "channel", "device_type", "country", "city")

str(final_data)

# Explore the dataset

