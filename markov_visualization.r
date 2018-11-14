### Visualize the results

#libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table,ggplot2, wesanderson)

## Campaign attribution - conversions ----

# Re-order the factors for channel names - for proper order of the bars
df_g1 = campaign_attribution[order(-campaign_attribution$markov_result), ]
df_g1$channel_name = factor(df_g1$channel_name, levels = c("Facebook", "Instagram", "Paid Search", "Online Video", "Online Display"))

# Create an ordered graph showing conversions attributed to each channel
g_channel_performance <- ggplot(df_g1, aes(x = channel_name, y = markov_result, fill = channel_name)) + 
  geom_bar(stat = "identity", width = 0.8) +
  scale_fill_brewer(palette = "OrRd", direction = -1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.6)) +
  theme(panel.grid.major.x = element_blank()) +
  labs(x = "", y = "Conversions") +
  ggtitle("Channel Performance") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill=FALSE)

g_channel_performance



## Visualize optimal budget allocation - ROAS based ----
# Compare current budget allocation with the one suggested by Markov attribution

# Create melted dataset for budget comparison
df_g2 = campaign_attribution[, c("channel_name", "total_cost", "optimal_budget")]
df_g2 = melt(df_g2, id = "channel_name")

graph2 <- ggplot(df_g2, aes(x = channel_name, y = value, fill = variable)) + 
  geom_bar(stat = "identity", width = 0.6, position = position_dodge(width = 0.7)) +
  scale_fill_manual(values = c("tan1", "forestgreen")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.6)) +
  theme(panel.grid.major.x = element_blank()) +
  labs(x = "", y = "Budget $") +
  ggtitle("Budget Allocation") +
  theme(plot.title = element_text(hjust = 0.5))

graph2
scale_fill_brewer(palette = "OrRd")


# Compare Markov Chain attribution and heuristics models ----

# Markov network graph ----




graph2 <- ggplot(optimal_budget, aes(x = creative_name, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = "Spectral") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "", y = "Budget", title = "Budget Allocation")

graph2