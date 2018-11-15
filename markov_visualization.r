### Visualize the results

#libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table,ggplot2,dplyr, visNetwork)

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

# Create double bar chart
g_budget_allocation <- ggplot(df_g2, aes(x = channel_name, y = value, fill = variable)) + 
  geom_bar(stat = "identity", width = 0.6, position = position_dodge(width = 0.7)) +
  scale_fill_manual(labels = c("Current Budget", "Optimal Budget"), values = c("tomato3", "green4")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.6)) +
  theme(panel.grid.major.x = element_blank()) +
  labs(x = "", y = "Budget $") +
  ggtitle("Budget Allocation") +
  theme(plot.title = element_text(hjust = 0.5))

g_budget_allocation

## Compare Markov Chain attribution and heuristics models ----

# Create df for comparing heuristic models and markov results
df_g3 = campaign_attribution[, 1:5]
df_g3 = df_g2 = melt(df_g3, id = "channel_name")

g_model_comparison <- ggplot(df_g3, aes(x = channel_name, y = value, fill = variable)) + 
  geom_bar(stat = "identity", width = 0.6, position = position_dodge(width = 0.7)) +
  scale_fill_manual(labels = c("Markov Model", "First Touch", "Last Touch", "Linear"), 
                    values = c("#e65368",
                               "#4e74ff",
                               "#75d3fa",
                               "#5feac4")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.6)) +
  theme(panel.grid.major.x = element_blank()) +
  labs(x = "", y = "Budget $") +
  ggtitle("Budget Allocation") +
  theme(plot.title = element_text(hjust = 0.5))

g_model_comparison

## Markov network graph ----

# Calculate transition matrix from markov chain - ChannelAttribution package

trans_matrix_prob = markov_attribution$transition_matrix
trans_matrix_prob[, c(1,2)] = lapply(trans_matrix_prob[, c(1,2)], as.character)


### Visualize the matrix ----
edges <-
  data.frame(
    from = trans_matrix_prob$channel_from,
    to = trans_matrix_prob$channel_to,
    label = round(trans_matrix_prob$transition_probability, 2),
    font.size = trans_matrix_prob$transition_probability * 100,
    width = trans_matrix_prob$transition_probability * 15,
    shadow = TRUE,
    arrows = "to",
    color = list(color = "#95cbee", highlight = "red")
  )

nodes <- data_frame(id = c( c(trans_matrix_prob$channel_from), c(trans_matrix_prob$channel_to) )) %>%
  distinct(id) %>%
  arrange(id) %>%
  mutate(
    label = id,
    color = ifelse(
      label %in% c('(start)', '(conversion)'),
      '#4ab04a',
      ifelse(label == '(null)', '#ce472e', '#ffd73e')
    ),
    shadow = TRUE,
    shape = "box"
  )

visNetwork(nodes,
           edges,
           height = "2000px",
           width = "100%",
           main = "Markov Chain Visualized") %>%
  visIgraphLayout(randomSeed = 123) %>%
  visNodes(size = 5) %>%
  visOptions(highlightNearest = TRUE)
