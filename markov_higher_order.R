# Markov Chain can operate on lower on higher order - number of steps taken back when calculating the effect
# Below is a simple comparison on ChannelAttribution the effect size between different orders


# Libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, ggplot2, dplyr, knitr, kableExtra)


### Higher order markov ----

# Calculate markov chains

markov_order1 = markov_model(df_paths,
                                   var_path = "path",
                                   var_conv = "total_conversions",
                                   var_value = NULL,
                                   var_null = NULL,
                                   out_more = TRUE,
                                   order = 1)

markov_order2 = markov_model(df_paths,
                              var_path = "path",
                              var_conv = "total_conversions",
                              var_value = NULL,
                              var_null = NULL,
                              out_more = TRUE,
                              order = 2)

markov_order3 = markov_model(df_paths,
                              var_path = "path",
                              var_conv = "total_conversions",
                              var_value = NULL,
                              var_null = NULL,
                              out_more = TRUE,
                              order = 3)


### Compare results ----

# Merge results
markov_results1 = markov_order1$result
markov_results2 = markov_order2$result
markov_results3 = markov_order3$result

order_comparison = 
  markov_results1 %>% 
  left_join(markov_results2, by = 'channel_name') %>%
  left_join(markov_results3, by = 'channel_name') %>%
  arrange(desc(total_conversions.x))

# change column names
colnames(order_comparison) = c('channel_name', 'order1', 'order2', 'order3') 

# round all numeric columns
nums = vapply(order_comparison, is.numeric, FUN.VALUE = logical(1))
order_comparison[,nums] = round(order_comparison[,nums], 0)


kable(order_comparison) %>%
  kable_styling(bootstrap_options = c("bordered", "hover"), full_width = F) %>%
  column_spec(1, bold = T, background = "aliceblue")

