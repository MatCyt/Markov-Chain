# Marketing-Attribution_Markov-Chain
R based marketing attribution and campaign budget optimization using Markov Chains

All of the concepts described shortly below have a corresponding links in the reference section. You can find there all articles and papers that I think are doing particulary great job in introducing and explaining this topic.

### Marketing Attribution

Marketing attribution is trying to answer one of the key questions in marketing world: how precisely my campaigns and actions contributed to the results I see? In more [formal definition](https://en.wikipedia.org/wiki/Attribution_(marketing)):

> attribution is the identification of a set of user actions ("events" or "touchpoints") that contribute in some manner to a desired outcome, and then the assignment of a value to each of these events. Marketing attribution provides a level of understanding of what combination of events in what particular order influence individuals to engage in a desired behavior, typically referred to as a conversion.

Original, popular approach tries to solve this problem with set of heuristics answers: attributing all the conversions to the last or first touchpoint (last-touch or first-touch), spreading the glory equally among channels (linear attribution) or giving more credit to the recent one (time decay attribution). Below you can find a short visual summary together with a link in the last section discussing them in detail.

<p align="center">
  <img src="https://github.com/MatCyt/Markov-Chain/blob/master/img/AttributionApproach.PNG" alt="Attribution Models"
       width="600" height="270">
 </p>

While all of them are relatively easy to understand and implement and above all better than nothing they are also too simple to be true. Attribution problem can however be answered with more accurate data-driven models including:

* logistic regressions
* VAR models
* Shapley value
* Regression-based models (dominance analysis and relative weight analysis)
* multivariate time-series models
* Markov chains

Following repository answers the attribution challenge using the popular Markov Chain.

### Markov Chain - Introduction 

Markov Chain essentially translate series of events into set of states (events itself) and transition probabilities between them (chance of moving from one event to another or staying in the current event).

<p align="center">
  <img src="https://github.com/MatCyt/Markov-Chain/blob/master/img/markov_graph.png" alt="Markov Chain graph">
 </p> 

In our marketing attribution problem Markov Chain applies great to the concept of the customer journey. Each touchpoint (online ad, landing page etc.) represents the state with the conversion or no-conversion being the final outcome of the journey. Based on the cookie level data tracking the customer actions online we can calculate the transition probabilities between each touchpoint. Final outcome of this transition matrix can be represented as a markov graph.

<p align="center">
  <img src="https://github.com/MatCyt/Markov-Chain/blob/master/img/MarkovChain.PNG" alt="Campaign Graph"
       width="450" height="300">
 </p>

Attribution itself is calculated within Markov Chain by removal effect. [Explaining it simply](https://www.analyticsvidhya.com/blog/2018/01/channel-attribution-modeling-using-markov-chains-in-r/):

> Removal effect principle says that if we want to find the contribution of each channel in the customer journey, we can do so by removing each channel and see how many conversions are happening without that channel being in place

### Markov Chain - R implementation

Greatest article describing implementation of Markov Chain in R is definitely coming from Sergey's Bryl [Analyze Core](https://analyzecore.com/2017/05/31/marketing-multi-channel-attribution-model-r-part-2-practical-issues/). My main code in *markov_chain_attribution.R* is following and adjusting his main steps. You can test it out yourself using sample databases - description for them is included in their separate folder.

Markov Chain for attribution is calculated on cookie level dataset containing time information. There are two most important distinct part of the code:

Firstly after loading the campaign data we have to create a paths (touchpoints journey) for each distinct cookie that have ended with converion or not - which is important information to include. The end results will create a string of touchpoints (ex. Channel1 > Channel5 > Channel7 > Conversion).
``` R
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
  ungroup() %>% 
  mutate(null_conversion = ifelse(total_conversions == 1, 0, 1)) # path that have not led to conversion
  ```
  
Then we can calculate the actuall Markov Chain results using [Channel Attribution](https://cran.r-project.org/web/packages/ChannelAttribution/ChannelAttribution.pdf) package available in R. There are several information that we need to indicate within a function.
* *var_path* and var_conv specify the appropriate columns (path and binary conversion) from input dataframe
* *order* indicates how many steps we want to take back in calculating the current transition probability. You can compare differences between different order results in markov_higher_order.R and read more in reference links. Web users are not consider purely markovian [1](https://dl.acm.org/citation.cfm?id=2187919) therefore an order of 2 or 3 is typically applied for symilar problems
* *var_null* specifies the column containing binary values for paths that have not ended with conversion
* *out_more* returns transition probabilities if set to TRUE.

We can also calculate heuristics based results in order to compare them with Markov Chain attribution.

``` R
### Markov Chain and Heuristic Models ----
markov_attribution <- markov_model(df_paths,
                             var_path = "path",
                             var_conv = "total_conversions",
                             var_value = NULL,
                             order = 2, # higher order markov chain
                             var_null = null_conversion,
                             out_more = TRUE)


heuristic_attribution <- heuristic_models(df_paths,
                                     var_path = "path",
                                     var_conv = "total_conversions")

```


### Markov Chain - Attribution Result and Heuristics comparisons

Following graphs show results of Markov model compared with heuristic models. All visualizations code can be found in results_visualization together with dataset necessary to run them.


<p align="left">
  <img src="https://github.com/MatCyt/Markov-Chain/blob/master/img/Channel%20Performance.png" alt="Channel Attribution"
       width="425" height="330">
  <img src="https://github.com/MatCyt/Markov-Chain/blob/master/img/Markov%20vs%20Heuristics.png" alt="Channel Comparison"
       width="425" height="330">
  </p> 


### Markov Chain and budget allocation

Based on Markov Chain attribution results we can calculate not only attributed conversion but also more better allocation of the campaign budget. In order to do so we will calculate *Return on Ad Spend* using the following formula

**ROAS = Channel Conversion Weight / Channel Budget Weight**

Channel Conversion Weight and Channel Budget Weight are ration of Channel's Conversion to Total Conversions and Channel Cost to total Campaigns Cost. ROAS > 100% indicates that channel is undervalued. Having this metric in place we can simply move to campaign budget recommendation through:

**Proposed budget = Current budget x ROAS**

All of this we calculate by merging aggregated budget data with the results of markov chain modelling:

```R
# Aggregate budget
campaign_budget_total = as.data.table(
  campaign_budget_daily %>%
    group_by(channel) %>%
    summarise(total_cost = round(sum(cost), 1))
)

# Join into final results
campaign_attribution = merge(all_model_results, campaign_budget_total, 
                             by.x = "channel_name", by.y = "channel")

#### Calculate ROAS and CPA
campaign_attribution = 
  campaign_attribution %>%
  mutate(chanel_weight = (total_conversions / sum(total_conversions)),
         cost_weight = (total_cost / sum(total_cost)),
         roas = chanel_weight / cost_weight,
         optimal_budget = total_cost * roas,
         CPA = total_cost / total_conversions)
```

Graph below will show us the comparison of current versus recommended budget based on the markov-driven allocation

<p align="center">
  <img src="https://github.com/MatCyt/Markov-Chain/blob/master/img/BudgetAllocation.png" alt="Budget Allocation"
       width="480" height="370">

### Possible improvements

Customer journey not ending with conversion can last for dozens of days (limited by cookie lifetime) and have many different touchpoint. We may want to decide to break it after X touchpoints or days following specific business logic. 

You may want try to validate the Markov results through accuracy measures based on prediction results to compare it with other allocation measurement

As Sergey's mention in his post the unique channel paths are undervalued by default in current calculation method. You may want to double check this impact and calculate markov results separately for one and multi-channel path.

### Markov Chain - Links and materials
Above all - if you should read only one thing it would be two posts from Analyze Core blog by Sergey Bryl

[Part 1 - Introduction to the topic in digital marketing context](https://analyzecore.com/2016/08/03/attribution-model-r-part-1/)

[Part 2 - Great R implementation of Markov Chain](https://analyzecore.com/2017/05/31/marketing-multi-channel-attribution-model-r-part-2-practical-issues/)

*R Libraries that can be applier to this problem*
* [ChannelAttribution](https://cran.r-project.org/web/packages/ChannelAttribution/ChannelAttribution.pdf)
* [markovchain](https://cran.r-project.org/web/packages/markovchain/index.html)
* [clickstream](https://cran.r-project.org/web/packages/clickstream/clickstream.pdf)

*Additional Resources*

1) [Heuristics Models Overview 1](https://www.snapapp.com/blog/marketing-attribution-models/)
2) [Heuristics Models Overview 2](https://www.referralsaasquatch.com/marketing-attribution/)
3) [Model Based Attribution - Overview](https://www.slideshare.net/MarketingFestival/lucie-sperkova-pioneering-multichannel-attribution-for-the-lack-of-comprehensive-solutions)
4) [Graphical introduction to Markov Chain](http://setosa.io/ev/markov-chains/)
5) [Markov Chains & Google Analytics Connection with R](https://stuifbergen.com/2016/11/conversion-attribution-markov-model-r/)
6) [Validating Markov Chains](https://amunategui.github.io/markov-chains/index.html)

*Whitepapers*

1) [Mapping the Customer Journey: A Graph-Based Framework for Online Attribution Modeling](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2343077) 
2) [Are Web Users Really Markovian?](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.232.5927&rep=rep1&type=pdf)
3) [Modeling Online Browsing and Path Analysis Using Clickstream Data](https://www.andrew.cmu.edu/user/alm3/papers/purchase%20conversion.pdf)

