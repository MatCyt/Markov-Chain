# Compare one path and multipath channels

# Markov model underestimates the conversions brought by one path channels (Channel -> Conversion)
# Check if there is any channel with particular high one-path proportion?

##### one- and multi-channel paths #####
df_path_1_clean <- df_split %>%
  group_by(cookie) %>%
  mutate(uniq_channel_tag = ifelse(length(unique(channel)) == 1, TRUE, FALSE)) %>%
  ungroup()

df_path_1_clean_uniq <- df_path_1_clean %>%
  filter(uniq_channel_tag == TRUE) %>%
  select(-uniq_channel_tag)

df_path_1_clean_multi <- df_path_1_clean %>%
  filter(uniq_channel_tag == FALSE) %>%
  select(-uniq_channel_tag)

### experiment ###
# attribution model for all paths
df_all_paths <- df_path_1_clean %>%
  group_by(cookie) %>%
  summarise(path = paste(channel, collapse = ' > '),
            conversion = sum(conversion)) %>%
  ungroup() %>%
  filter(conversion == 1)

mod_attrib <- markov_model(df_all_paths,
                           var_path = 'path',
                           var_conv = 'conversion',
                           out_more = TRUE)
mod_attrib$removal_effects
mod_attrib$result
d_all <- data.frame(mod_attrib$result)

# attribution model for splitted multi and unique channel paths
df_multi_paths <- df_path_1_clean_multi %>%
  group_by(cookie) %>%
  summarise(path = paste(channel, collapse = ' > '),
            conversion = sum(conversion)) %>%
  ungroup() %>%
  filter(conversion == 1)

mod_attrib_alt <- markov_model(df_multi_paths,
                               var_path = 'path',
                               var_conv = 'conversion',
                               out_more = TRUE)
mod_attrib_alt$removal_effects
mod_attrib_alt$result

# adding unique paths
df_uniq_paths <- df_path_1_clean_uniq %>%
  filter(conversion == 1) %>%
  group_by(channel) %>%
  summarise(conversions = sum(conversion)) %>%
  ungroup()

d_multi <- data.frame(mod_attrib_alt$result)

d_split <- full_join(d_multi, df_uniq_paths, by = c('channel_name' = 'channel')) %>%
  mutate(result = total_conversions + conversions)

sum(d_all$total_conversions)
sum(d_split$result)
