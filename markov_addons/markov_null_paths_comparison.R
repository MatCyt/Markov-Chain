# In the markov chain approach and ChannelAttribution package we should remember about including the paths
# that did not ended in conversion. Below there is a comparison of results for both approaches with visualized output.


## Load libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, ggplot2, dplyr, ChannelAttribution)


## Markov Attribution without taking null conversion into consideration
df_paths_conv = df_split %>%
  group_by(path_id) %>%
  arrange(time) %>%
  summarise(path = paste(channel, collapse = ">"),
            total_conversions = sum(conversion)) %>%
  ungroup() 


markov_attribution_conv <- markov_model(df_paths_conv,
                                        var_path = "path",
                                        var_conv = "total_conversions",
                                        var_value = NULL,
                                        var_null = NULL,
                                        out_more = TRUE)



## Markov Attribution including null conversion
df_paths_null = df_split %>%
  group_by(path_id) %>%
  arrange(time) %>%
  summarise(path = paste(channel, collapse = ">"),
            total_conversions = sum(conversion)) %>%
  ungroup() %>% 
  mutate(null_conversion = ifelse(total_conversions == 1, 0, 1)) # adding the null column


markov_attribution_null <- markov_model(df_paths_null,
                                   var_path = "path",
                                   var_conv = "total_conversions",
                                   var_value = NULL,
                                   var_null = "null_conversion", # adding the null variable
                                   out_more = TRUE)


## Comparing results
conv_result = markov_attribution_conv$result
null_result = markov_attribution_null$result

colnames(null_result) = c("channel_name","null_included")
colnames(conv_result) = c("channel_name", "null_omitted")

compare_approach = 
  null_result %>%
  left_join(conv_result)

df_gcompare = melt(compare_approach, id = "channel_name")

g_compare_null <- ggplot(df_gcompare, aes(x = channel_name, y = value, fill = variable)) + 
  geom_bar(stat = "identity", width = 0.6, position = position_dodge(width = 0.7)) +
  expand_limits(y=10000) +
  scale_fill_manual(labels = c("Null Included", "Null Omitted"), values = c("deepskyblue", "goldenrod1")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.6)) +
  theme(panel.grid.major.x = element_blank()) +
  geom_text(aes(label = round(value, 0)), 
            fontface = "bold", size = 3.5, 
            vjust = -0.5, position = position_dodge(width = 0.75)) +
  labs(x = "", y = "Conversions") +
  ggtitle("Including vs omitting null conversion paths") +
  theme(plot.title = element_text(hjust = 0.5))


