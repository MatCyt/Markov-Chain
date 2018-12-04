## Datasets description
Here you can find two sample datasets necessary for calculating markov attribution and budget allocation

### Campaign dataset - cookie level

The main dataset resembling a (simplified) real data coming from digital marketing campaigns on cookie tracking level.
To decrease the sample dataset size it has the 4 key variables necessary for markov chain analysis:

**cookie** - unique identifier of user/session. Cookie lifetime varies usually from 30 to 90 days in online ads.
Cookie campaign data serves as log of each user(s) action and is creating a map of his digital touchpoints and interacton with content over a period of time. Distribution of cookies in this file is based on actual campaign data.

**timestamp** - the time of particular interaction with an add

**interaction** - type of interaction between cookie and an add. Typically it will consist of impressions, clicks and conversions s but additional metrics might be defined by particular ad serving company

**conversion** - binary column containing information if particular visit ended in conversion or not. Created out of interaction variable. Contains a low conversion distribution based on actual campaign. Typically around 0.9% to 2.0% of all journeys lead to a successful conversion [[1]](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2343077).

Real campaign data will have many more variables like geography location, campaign name or information about browser, creative, device type or system. While it can bring new dimensions to analytics of the campaign it is not necessary for markov chain attribution model.


### Budget

Simulated daily cost values for whole campaign calculated as a function of number of impressions.


