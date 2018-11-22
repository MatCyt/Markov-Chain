
### Load Libraries ----

if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, dplyr, ChannelAttribution, ggplot2)


### Load Datasets ----
campaign_data = fread("C:/Users/matcyt/Desktop/MarketingAttribution/attribution_markov_dataset.csv")
campaign_budget_daily = fread("C:/Users/matcyt/Desktop/MarketingAttribution/attribution_budget_daily.csv", dec = ",")

# work
campaign_data = fread("C:/Users/mateusz.cytrowski/Desktop/Github/attribution_markov_dataset.csv")
campaign_budget_daily = fread("C:/Users/mateusz.cytrowski/Desktop/Github/attribution_budget_daily.csv", dec = ",")

### Prepare the files - Split Paths ----
df_split = campaign_data %>%
  group_by(cookie) %>%
  arrange(time) %>%
  mutate(path_no = ifelse(is.na(lag(cumsum(conversion))), 0, lag(cumsum(conversion))) + 1) %>%
  ungroup() %>%
  mutate(path_id = paste0(cookie, path_no))


### Prepare the file - Create the paths ----
df_paths = df_split %>%
  group_by(path_id) %>%
  arrange(time) %>%
  summarise(path = paste(channel, collapse = ">"),
            total_conversions = sum(conversion)) %>%
  ungroup()

### Markov Chain and Heuristic Models ----
markov_attribution <- markov_model(df_paths,
                             var_path = "path",
                             var_conv = "total_conversions",
                             var_value = NULL,
                             var_null = NULL,
                             out_more = TRUE)


heuristic_attribution <- heuristic_models(df_paths,
                                     var_path = "path",
                                     var_conv = "total_conversions")


### Prepare final joint dataset ----

# Join attribution results
all_model_results = merge(markov_attribution$result, heuristic_attribution)

# Aggregate budget
campaign_budget_total = as.data.table(
  campaign_budget_daily %>%
    group_by(channel) %>%
    summarise(total_cost = round(sum(cost), 1))
)

# Join into final results
campaign_attribution = merge(all_model_results, campaign_budget_total, 
                             by.x = "channel_name", by.y = "channel")

#### Calculate ROAS
campaign_attribution = 
  campaign_attribution %>%
  mutate(chanel_weight = (total_conversions / sum(total_conversions)),
         cost_weight = (total_cost / sum(total_cost)),
         roas = chanel_weight / cost_weight,
         optimal_budget = total_cost * roas)

# Change the name of markov results column
names(campaign_attribution)[names(campaign_attribution) == "total_conversions"] = "markov_result"

# Save the outputs
write.csv2(campaign_attribution, "C:/Users/mateusz.cytrowski/Desktop/Github/campaign_attribution.csv")
