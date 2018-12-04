# Building a small simulation of states after n steps on markov chain

# Code and additional background originally from:
# https://analyzecore.com/2016/08/03/attribution-model-r-part-1/

library(expm)

##### modeling states and conversions #####
# transition matrix preprocessing
trans_matrix_complete <- markov_attribution$transition_matrix
trans_matrix_complete <- rbind(trans_matrix_complete, df_dummy %>%
                                 mutate(transition_probability = perc) %>%
                                 select(channel_from, channel_to, transition_probability))
trans_matrix_complete$channel_to <- factor(trans_matrix_complete$channel_to, levels = c(levels(trans_matrix_complete$channel_from)))
trans_matrix_complete <- dcast(trans_matrix_complete, channel_from ~ channel_to, value.var = 'transition_probability')
trans_matrix_complete[is.na(trans_matrix_complete)] <- 0
rownames(trans_matrix_complete) <- trans_matrix_complete$channel_from
trans_matrix_complete <- as.matrix(trans_matrix_complete[, -1])


# creating empty matrix for modeling
model_mtrx <- matrix(data = 0,
                     nrow = nrow(trans_matrix_complete), ncol = 1,
                     dimnames = list(c(rownames(trans_matrix_complete)), '(start)'))
# adding modeling number of visits
model_mtrx['(start)', ] <- 1000

c(model_mtrx) %*% (trans_matrix_complete %^% 5) # after 5 steps
c(model_mtrx) %*% (trans_matrix_complete %^% 100000) # after 100000 steps



df_dummy <- data.frame(channel_from = c('(start)', '(conversion)', '(null)'),
                       channel_to = c('(start)', '(conversion)', '(null)'),
                       n = c(0, 0, 0),
                       tot_n = c(0, 0, 0),
                       perc = c(0, 1, 1))

