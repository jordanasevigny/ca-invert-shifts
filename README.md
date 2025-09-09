# Over 120 Years of Episodic Range Extensions of Nearshore Marine Invertebrates in the Northeastern Pacific
Authors here

Corresponding aurthor here

# Where do the data come from?

We compiled a database of northward range extensions of warm-water marine invertebrates through a systematic literature review of both historical survey reports and broader peer-reviewed literature. A central source was the California Cooperative Oceanic Fisheries Investigations (CalCOFI) State of the [California Current reports]([url](https://calcofi.org/publications/calcofi-reports/)). These reports provide serendipitous records of unusual species occurrences noted by scientists during routine surveys, rather than targeted searches for range shifts. To expand beyond these historical accounts, we conducted a systematic literature review of peer-reviewed studies, integrating their observations with those from the CalCOFI reports to assemble a more comprehensive record of extension events and assess broader patterns. All records used in this analysis come from onshore or nearshore (i.e., coastal) observations of species with a historical or novel range edge in California. 

# What's in this repository?
The repository is organized as follows:
* 'scripts/' contains scripts to clean up the raw datafiles; analyze the data; and generate figures.
* 'data/' containes raw, pre-processed / pre-cleaned data files
* 'processed_data/'  contains outputs of scripts that filter, clean, summarize, or analyze data.
* 'figures/' contains the figures included in the paper.

# In what order should things be run?
To reproduce the analysis, run these scripts in order:

Extension data & analysis:
1. 'load_calcofi_review.R'; 'load_lab_review.R'; 'load_historical_distributions.R' process the raw, irregular format versions of the calcofi extension data, review data, and third-party historical range edge data respectively. Each generates a cleaned version of the input data with mergable column names and consistent format. This does not need to be rerun unless changes to the raw data are required, in which case these should be rerun in order.
2. 'merge_calcofi_lab_reviews.R' merges the cleaned output from the three load scripts ('calcofi_review_data_clean.csv', 'lab_review_with_longitudes.csv', 'historical-distributions-clean.csv') into one master dataframe ('merged_calcofi_lab_review.csv'). This does not need to be rerun unless changes to the raw data are required, in which case these should be rerun in order. 
3a. 'oni_vs_extension_distance.R' conducts a linear regression comparing ONI to extension number and to extension distance. This script also generates the preliminary figures related to the analyses. This and the following #3 scripts can be run in any order.
3b. ‘ca-invert-shits/scripts/enso_ext_timeseries.R’ generates preliminary figures for figure 2 and the final form of the supplementary barplot showcasing which extensions come from which data source. This script also finds several of the counts included in the paper methods.
3c. ‘ca-invert-shits/scripts/blob_analysis.R’ generates preliminary figure 4 and calculates counts and statistical tests whether the number of extension events during 2014-2016 exceeds expectations by chance using a chi-squared goodness-of-fit test.
3d. ‘ca-invert-shits/scripts/extension_map.R’ generates final version of figure 1 and supplementary figure 1 (extension maps).
3e. 'ca-invert-shits/scripts/Figure2Panel.R' generates final version of figure 2 and supplementary figure 2 (extension number vs ONI).
3f. 'ca-invert-shits/scripts/Figure3Panel.R' generates final version of figure 3 (extension distance vs ONI).


Systematic literature review:
1. ‘ca-invert-shits/scripts/WoRMS_taxonomy_selection.R’ generates the taxa list to fill into the keyword search for Web of Science. This does not need to be rerun unless an updated version of the list is warranted.
2. 'ca-invert-shits/scripts/collating_WOS.R' pulls all the web of science results into one dataframe (each page of results had to be downloaded separately); deduplicates; and adds R-generated DOIs.

# Table of Contents
Data: 
* Calcofi extensions raw data: ‘ca-invert-shits/data/CalCOFI Coding Form (Responses).xlsx’
* Lab review extensions raw data: ‘ca-invert-shits/data/Screening & Review Tracking - Full-text Screening.csv’
* Historical distributions raw: ‘ca-invert-shits/data/historical-distributions.xlsx’
* Calcofi extensions cleaned: ‘ca-invert-shits/processed_data/calcofi_review_data_clean.csv’
* Lab review extensions cleaned: ‘ca-invert-shits/processed_data/lab_review_with_longitudes.csv’
* Historical distributions clean: ‘ca-invert-shits/processed_data/historical-distributions-clean.csv’
* **Master Extension Dataframe**: ‘ca-invert-shits/processed_data/merged_calcofi_lab_review.csv’
* Resulting papers for lab review from WOS: ‘ca-invert-shits/processed_data/combined_search29.xlsx’
* Taxa list for WOS search keywords: ‘ca-invert-shits/processed_data/WoRMS_taxlist_20250211_processed_V2.xlsx’
* Filtered dataset of furthest north extensions for the 3+ extension species: ‘ca-invert-shits/processed_data/threeplus_ext_sp_furthest_north.csv’
* ENSO / ONI data downloaded from download_enso(climate_idx = "oni", create_csv = TRUE), r library rsoi. ‘ca-invert-shits/data/enso_data.csv’

Scripts:
* ‘ca-invert-shits/scripts/load_calcofi_review.R’
  * Input:  ‘ca-invert-shits/data/CalCOFI Coding Form (Responses).xlsx’,
  * Output: ‘ca-invert-shits/processed_data/calcofi_review_data_clean.csv’,
  * Method: Stacks the individual extensions per calcofi excerpt vertically and renames columns to match shared lab review columns.
* ‘ca-invert-shits/scripts/load_lab_review.R’
  * Input:  ‘ca-invert-shits/data/Screening & Review Tracking - Full-text Screening.csv’,
  * Output: ‘ca-invert-shits/processed_data/lab_review_with_longitudes.csv’,
  * Method: Adds longitudes of coastline to datapoints with only latitudes available. Updates column names to match shared calcofi review columns.
* ‘ca-invert-shits/scripts/load_historical_distributions.R’
  * Input: ‘ca-invert-shits/data/historical-distributions.xlsx’,
  * Output: ‘ca-invert-shits/processed_data/historical-distributions-clean.csv’,
  * Method:  Dropped notes to merge with review.
* ‘ca-invert-shits/scripts/merge_calcofi_lab_reviews.R’
  * Inputs: 'ca-invert-shits/processed_data/calcofi_review_data_clean.csv', 'ca-invert-shits/processed_data/lab_review_with_longitudes.csv', 'ca-invert-shits/processed_data/historical-distributions-clean.csv',
  * Output: "ca-invert-shits/processed_data/merged_calcofi_lab_review.csv",
  * Method: Filters calcofi and lab reviews for ‘Include’ extensions and drop the low confidence ‘Opportunistic’ extensions from the lab review. Merge by the shared columns. Convert and formalize data types. Merge the historical ranges for the extension species. Requires a minimum year of 1900 (there was some data from pre 1900 accidentally pulled but should already have been excluded). Add coastal longitudes for historical distribution latitudes. Select the northernmost observation for each species each year. Make sure extensions are only included if observation latitude is > historical range lat. Add extension event ids where consecutive yearly observations are one extension event (i.e. if pelagic red crab was found 2013, 2014, 2018, There would be two event ids assigned: 1: 2013-2014 and 2: 2018).
* ‘ca-invert-shits/scripts/oni_vs_extension_distance.R’,
  * Input: "ca-invert-shits/processed_data/merged_calcofi_lab_review.csv",
  * Output: Scatterplot for Max ONI vs extension distance; histogram of extension frequency vs Max ONI; associated statistics for both.
  * Method: Classify ENSO years by start (El Niño began), peak (Max number of months of an El Niño event was in that year), end (El Niño concluded), or some combination. Add filter for # of extensions requirement for each species. Add a column of whether the first year of an extension event was the start/peak/end of El Niño. Calculate extension distance (distHaversine from historical range edge to observation location) for each observation. Add oni values to extensions. Filter for furthest extensions of each species in each extension event and the max ONI for the extension event. Make max extension distance per event vs max ONI during event scatter plot and linear regression. Also has a version 2 where max ONI has a small change where if the first year of an extension event was the end of an El Niño event (i.e. a delayed response observation of the extension), the ONI for the year prior was used in the max ONI query. 
* ‘ca-invert-shits/scripts/enso_ext_timeseries.R’
  * Input: ‘ca-invert-shits/processed_data/merged_calcofi_lab_review.csv’,
  * Output: ENSO time-wave and extension event bubble figure and supplementary extension barplot colored by calcofi and review; calcofi vs lab rev extension figure.
  * Method: plots ENSO and tallies the number of extension events that begin each year.
* ‘ca-invert-shits/scripts/blob_analysis.R’
  * Input: ‘ca-invert-shits/processed_data/merged_calcofi_lab_review.csv’,
  * Output: Bar plot of extensions that begin in each year over time with an ENSO wave but focuses on last 3 decades to demonstrate that the blob had a crazy number of anomalous extensions.
* ‘ca-invert-shits/scripts/extension_map.R’,
  * Input: ‘ca-invert-shits/processed_data/merged_calcofi_lab_review.csv’,
  * Output: ‘ca-invert-shits/processed_data/threeplus_ext_sp_furthest_north.csv’ and 'ca-invert-shits/figures/ext_map', 'ca-invert-shits/figures/ext_map_supp'
  * Method: Filter the main dataset down to only species with 3+ extension events and within those events, only the furthest north observation. This is the dataset for the Fig 1 arrow map.
* 'ca-invert-shits/scripts/Figure2Panel.R',
  * Input: ‘ca-invert-shits/processed_data/merged_calcofi_lab_review.csv’,
  * Output: 'ca-invert-shits/figures/figure2panel', 'ca-invert-shits/figures/figure2a_supp'
* 'ca-invert-shits/scripts/Figure3Panel.R',
  * Input: ‘ca-invert-shits/processed_data/merged_calcofi_lab_review.csv’,
  * Output: 'ca-invert-shits/figures/figure3panel'
* ‘ca-invert-shits/scripts/WoRMS_taxonomy_selection.R’
  * Input: ‘ca-invert-shits/processed_data/WoRMS_taxlist_20250211_processed_V2.xlsx’,
  * Output: a formatted printout of all the order/class/phylum names to add to the Web of Science search terms.
* ‘ca-invert-shits/scripts/collating_WOS.R’
  * Input: WOS_search29 folder,
  * Output: "ca-invert-shits/processed_data/combined_search29.xlsx",
  * Method: This script pulls all the web of science results into one dataframe (each page of results had to be downloaded separately), deduplicates, and adds R-generated DOIs. 
