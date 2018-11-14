### Visualize the results

#libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table,ggplot2, wesanderson)

# Campaign attribution - conversions ----

# Re-order the factors for channel names - for proper order of the bars
df_g1 = campaign_attribution[order(-campaign_attribution$markov_result), ]
df_g1$channel_name = factor(df_g1$channel_name, levels = c("Facebook", "Instagram", "Paid Search", "Online Video", "Online Display"))

# Create an ordered graph showing conversions attributed to each channel
g_channel_performance <- ggplot(df_g1, aes(x = channel_name, y = markov_result, fill = channel_name)) + 
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "PuBu") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "", y = "Conversions", title = "Channel Performance") +
  guides(fill=FALSE)


g_channel_performance


# Optimal budget allocation - ROAS based ----

campaign_attribution
channel_performance <- campaign_data_markov[order(-campaign_data_markov$total_conversions),]

as.character(unique(campaign_attribution$channel_name))

channels <- c("Agora_double", "Amnet_All", "Interia_double", "Interia_rectangle", 
              "Polsat_all", "Polsat_double", "Polsat_welcome", "WP_double", "WP_other")

graph1 <- ggplot(channel_performance, aes(x = channels, y = total_conversions, fill = channels)) + 
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "RdYlBu") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "", y = "Conversions", title = "Channel Performance")

# Compare Markov Chain attribution and heuristics models ----

# Markov network graph ----

channel_performance <- campaign_data_markov[order(-campaign_data_markov$total_conversions),]
channels <- c("Agora_double", "Amnet_All", "Interia_double", "Interia_rectangle", 
              "Polsat_all", "Polsat_double", "Polsat_welcome", "WP_double", "WP_other")



optimal_budget <- channel_performance[, c("creative_name", "total_cost", "OptimalBudget")]
optimal_budget <- melt(optimal_budget, id = "creative_name")


graph1 <- ggplot(channel_performance, aes(x = channels, y = total_conversions, fill = channels)) + 
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "RdYlBu") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "", y = "Conversions", title = "Channel Performance")


graph2 <- ggplot(optimal_budget, aes(x = creative_name, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = "Spectral") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "", y = "Budget", title = "Budget Allocation")

graph2