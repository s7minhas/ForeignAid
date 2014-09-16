ForeignAid
==========

## ToDos

* Generate strategic interest variable

	* Approaches: 

		* Run null GBME on alliance, UN votes, MIDs, IOs

			* Run GBME model on each of these variables at yearly level. From each model pull out the latent u'v term. 

			* Need to find some way, such as PCA, to find an underlying variable that explains the variation in each

			* Validation method: Compare aggregated measure to either the dyadic terms we generated or to S-scores or tau-b from Signorino and whoever. Just in case think of any other commonly used strategic interest variable in the literature to compare ours to. 

	* ToDos:

		* Data gathered: we have alliance, IOs, MIDs, UN voting data

		* Organizing and running GBME: 

			* Cindy: alliance, IOs (undirected for both)

			* Shahryar: MIDs, UN voting

* Modeling aid

	* Approaches: 

		* Dump network approach

		* Consider Hierarchical Bayesian Model