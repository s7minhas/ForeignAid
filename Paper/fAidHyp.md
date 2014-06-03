Foreign Aid Hypotheses
===

Summarizing hypotheses into Idealist & Neo-Realist/Realist Camps
---

* Idealist

	* R1, R3, and maybe R5/D6

* Realist

	* R2, D2, D5 and R6/D7 (I think we're getting at the same thing) 

* Ambiguous (to me at least)

	* R4, R5/D6, D1, D3, D4

* Controls

	* S1a, S1b, S2 

Recipient Characteristics
---

* R1: The greater the humanitarian need of a recipient country the more likely it is to recieve foreign aid. Researchers have operationalizd humanitiarn need with a variety of measures, i.e. income levels, food availability, levels of education, measures of weight/height and malnutrition, mortality rates etc. Data available from the World Bank (and already collected).

	* All the above sounds good, lets forget about the HDI measures for now. Using them would require us to subset our analysis for the period after 1990. 

* R2: The more natural resources a recipient country has the more likely there is to be foreign aid. This should be interesting to consider especially in light of Kevin Morrison's (2009) work and how he lumps foreign and and natural resources together in terms of non-tax revenue. This hypotheses stems in particular from observations about China's engagement with Africa within the last decade and a half wherein foreign aid is often coupled with FDI/natural resource extraction. Data for oil and gass available from Michael Ross and the OECD. Might want to investigate data availability for other natural resources - metals and minerals etc.

	* We could measure this indirectly as well by using the World Bank's data on [total natural resource rents](http://data.worldbank.org/indicator/NY.GDP.TOTL.RT.ZS). Might be easier than trying to gather all the natural resource data. 

* R3: The more severe a natural disaster the recipient country experiences, the more more likely there is to be foreign aid (We might also want to consider interaction effects with general humanitarian need/income here as well as more developed/prepared countries are less likely to need aid in the event of a natural disaster). Data available from EMDAT (and already collected).

* R4: The more domestic conflict there is in a country, the less likely it is to receive foreign aid (Berthelemy 2006). Data available from PRIO (and already collected) and Banks. 

* R5/D6?: The better the quality of governance in a recipient country, the more likely it is to receive foreign aid. This may be more of a dyadic relationship; that is the more the donor and recipient dyad score well on the governance indicators, the more likely foreign aid flows. Data available from the WGI.

	* Maybe instead of the better quality of governance, it's the change in governance? So a government that is moving towards becoming more democratic according to Polity or "Free" according to Freedom House is more likely to receive foreign aid. Though there is an obvious endogeneity problem here.

	* We could also use human rights data from [CIRI](http://www.humanrightsdata.com/p/data-documentation.html) available from 1981 to 2011. They disaggregate human rights to a number of dimensions and also have a couple of aggregate indices that we can use. The relationship between human rights and foreign aid was a popular debate in the '80s and '90s, see [McCormick & Mitchell 1988](http://www.jstor.org/discover/10.2307/2111319?uid=3739776&uid=2&uid=4&uid=3739256&sid=21104105627587)

* R6/D7: What about diplomatic explanations? There are two primary explanations for the distribution of foreign aid. The "Idealist theory" espouses the idea that allocation results from humanitarian needs and the realist/neo-realist theory that it is a result of the foreign policy interests of the donor [(McKinlay & Little 1977)](http://journals.cambridge.org/action/displayAbstract?fromPage=online&aid=7661300). We should develop some measures that get at this second explanation, below are some ideas I found in the literature -- none very good. 

	* [Kuziemko and Werker 2006](http://www.jstor.org/discover/10.1086/507155?uid=3739776&uid=2&uid=4&uid=3739256&sid=21104105627587) show that the US increases foreign aid to countries when they rotate onto the UN security council. This can be thought of as a receiver level covariate by getting data on whether the country is a part of the UN security council, maybe? 

	* Or we could use foreign aid to track if bribery has taken place, by looking at the similarity of UN voting records at the dyadic level. Getting the lag right for this would be hard though because I can see two scenarios, both probably occur: first a poor country gets foreign aid and then votes the way the sender state wants it to, second a poor country is promised foreign aid if it votes in a certain way.

	* In addition to UN voting records we could also just look at whether or not countries are allied. In general I think it would be interesting to show that foreign aid is typically driven by diplomatic considerations. Data on alliances available from COW.

Sender Characteristics
---

* S1a: The richer the donor country, the more likely it is to give foreign aid. Data available from the World Bank.

* S1b: The richer a donor country, the more aid recipients it is likely to give to (This might be better under Network Characteristics). Data available from the World Bank (and already collected).

* S2: The greater a donor countries' growth rate, the more likely it is to give foreign aid (not a hypothesis I've seen in the previous foreign aid literature). Data available from the World Bank.

	* This could be nice to have in as a control

* Are there sender level covariates that we can use to get at the idea that donors are sending aid to maximize their own economic/political interests?

Dyad specific
---

* D1: The more cultural affinity (as measured by language or colonial history), the more likely there is to be foreign aid between dyads. Not sure where this data is, but should be relatively easy to construct.

* D2: The more trade linkages between a dyad, the more likely there is to be foreign aid (perhaps as a trade promotion tactic?). Data available from COW (and already collected)

* D3: The more political affinity (i.e. the donor and recipient are both democracies or both autocracies, the more likely there is to be foreign aid between dyads. Data can be constructed from a variety of monadic indcators, i.e. Polity IV, Geddes et al, Svolik.

* D4: The more religious affinity between dyads, the more likely there is to be foreign aid. Data available from Pew (and already collected).

* D5: The more strategically important a recipient country is to a donor country, the more likely it is to recieve aid. UN voting scores and military spending from a donor country in the recipient country are commonly used measures. Specific issues are often also singled out and measured as a dummy variable, including country dummies for Egypt and Isreal as well as a dummy for diplomatic relations with Taiwan that is specific to instances where China is a donor country. We should think about how (and if) we want to incorporate these dummies in this model. Data available from Erik Gartzke.

	* This is what I was thinking of as well when it came to R6/D7.


Network characteristics
---

* By using the Hoff & Westveld model, the network level variation will all be projected into the latent space. If we were using a random graph model approach, then we would be able to specify specific network level characteristics. 

* N1: The more foreign aid is given in the network in general, the more a country is likely to give foreign aid. That is, if foreign aid is generally employed for strategic reasons, then the political capital one can by with a unit of foreign aid money decreases as total foreign aid given in a country increases.