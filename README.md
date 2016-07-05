# data-wrangler-for-wordsmith

The purpose of the Data Wrangler for Wordsmith is to provide an easy-to-use interface for clients to pre-process datasets that are not yet in a Wordsmith-ready format. 

Goals of the project: 
- (1) Allow clients to select the variables of interest from a potentially larger pool of variables, to facilitate ease of Wordsmith use (Example: omit variables that do not meaningfully add to narratives.) 
- (2) Allow clients to easily add columns to their dataset, such as means, sd's, sums, mins, maxs, and ranks of a subset of variables, as these values must appear as columns if they are to be referenced in a narrative. 
- (3) Allow clients to compute the above summaries for the entire dataset, or using group by statements (Example: mean by gender). 
- (4) Allow clients to filter their dataset to generate narratives only for rows meeting particular conditions (Example: price > 100). 
- (5) Allow clients to restructure their dataset (long-to-wide or wide-to-long) if the rows of the dataset do not correspond to desired narratives (Example: rows are players, but would like to generate narratives for teams). 
- (6) Provde a "Download Wrangled Dataset" button so that users may save output and upload to Wordsmith. 
- (7) Provide a "Save state / Generate script" button so that users may save the selected commands and re-run the dplyr code on a new dataset that is formatted identically to the original one. (Example: a dataset containing new homes for sale in June could be wrangled based on the set of commands the user selected for their May dataset).) 
