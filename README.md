# A century of invertebrate range extensions in the eastern North Pacific

Authors: Jordana K. Sevigny<sup>1</sup>, Emma J. Walker<sup>1</sup>, Bella G. Lipsey<sup>1</sup>, Theodore T. Tran<sup>1</sup>, and Alexa L. Fredston<sup>1</sup>

Affiliations:
1. Department of Ocean Sciences, University of California, Santa Cruz, California, United States of America

Address for correspondence: Jordana K. Sevigny, Department of Ocean Sciences, University of California, Santa Cruz, California, 1156 High Street, Santa Cruz, California 95064. jsevigny@ucsc.edu

# Where do the data come from?

We compiled a database of northward range extensions across California of marine invertebrates through a systematic literature review of both historical survey reports and broader peer-reviewed literature. A central source was the California Cooperative Oceanic Fisheries Investigations (CalCOFI) State of the [California Current reports]([url](https://calcofi.org/publications/calcofi-reports/)). These reports provide serendipitous records of unusual species occurrences noted by scientists during routine surveys, rather than targeted searches for range shifts. To expand beyond these historical accounts, we conducted a systematic literature review of peer-reviewed studies, integrating their observations with those from the CalCOFI reports to assemble a more comprehensive record of extension events and assess broader patterns. All records used in this analysis come from onshore or nearshore (i.e., coastal) observations of species with a historical or novel range edge in California. 

# What's in this repository?
The repository is organized as follows:
* 'scripts/' contains scripts to clean up the raw datafiles; analyze the data; and generate figures.
* 'data/' containes raw, pre-processed / pre-cleaned data files
* 'processed_data/'  contains outputs from scripts that filter, clean, summarize, or analyze data.
* 'figures/' contains the figures included in the paper.

# In what order should things be run?
To reproduce the analysis, run these scripts in order:

Extension data & analysis:
1. 'load_lab_review.R' adds R-generated longitudes to observation latitudes as needed for the observations from the literature review. It outputs 'lab_review_with_longitudes.csv.' This does not need to be rerun unless changes to the raw data are required, in which case these should be rerun in order.
2. 'merge_calcofi_lab_reviews.R' merges 'calcofi_review_data_clean.csv', 'lab_review_with_longitudes.csv', 'historical_distributions_clean.csv' into one master dataframe ('merged_calcofi_lab_review.csv'). This does not need to be rerun unless changes to the raw data are required, in which case these should be rerun in order. 
3. Analysis scripts (can be run in any order)
  - (a) 'oni_vs_extension_distance.R' conducts a linear regression comparing ONI to extension number and to extension distance. This script also generates the preliminary figures related to the analyses. 
  - (b) ‘ca-invert-shits/scripts/enso_ext_timeseries.R’ generates preliminary figures for figure 2 and the final form of the supplementary barplot showcasing which extensions come from which data source. This script also finds several of the counts related to # extensions occuring during El Niño included in the paper methods.
  - (c) ‘ca-invert-shits/scripts/blob_analysis.R’ generates preliminary figure 4 and calculates # extensions to occur during the 2014-2016 marine heatwave and statistically tests whether the number of extension events during 2014-2016 exceeds expectations by chance using a chi-squared goodness-of-fit test.
  - (d) ‘ca-invert-shits/scripts/extension_map.R’ generates final version of figure 1 and supplementary figure 1 (extension maps).
  - (e) 'ca-invert-shits/scripts/Figure2Panel.R' generates final version of figure 2 and supplementary figure 2 (extension number vs ONI).
  - (f) 'ca-invert-shits/scripts/Figure3Panel.R' generates final version of figure 3 (extension distance vs ONI).
  - (g) 'ca-invert-shits/scripts/summary_extension_table.R' generates the supplementary table 1.
  - (h) 'ca-invert-shits/scripts/hist_lat_supp_table.R' generates the supplementary table 2.


Systematic literature search:
1. ‘ca-invert-shits/scripts/WoRMS_taxonomy_selection.R’ generates the taxa list to fill into the keyword search for Web of Science. This does not need to be rerun unless an updated version of the list is warranted.

2. 'ca-invert-shits/scripts/collating_WOS.R' pulls all the web of science results into one dataframe (each page of results had to be downloaded separately); deduplicates; and adds R-generated DOIs.

# Table of Contents
Data: 
* Forward search articles: 'ca-invert-shits/data/forward_search_articles.csv' (the paper ID is paired with the paper ID in the master extension dataframe)
* Backward search articles: 'ca-invert-shits/data/backward_search_articles.csv' (the paper ID is paired with the paper ID in the master extension dataframe)
* Literature review extensions without R-generated longitudes: ‘ca-invert-shits/data/lab_review_clean_missing_longitudes.csv’
* Literature review extensions with R-generated longitudes: ‘ca-invert-shits/processed_data/lab_review_with_longitudes.csv’
* Calcofi extensions: ‘ca-invert-shits/processed_data/calcofi_review_data_clean.csv’
* Historical distributions: ‘ca-invert-shits/processed_data/historical_distributions_clean.csv’
* **Master Extension Dataframe**: ‘ca-invert-shits/processed_data/merged_calcofi_lab_review.csv’ ('paper_id' can be cross-referenced to obtain paper reference with forward_search_articles.csv for 1-375 or backward_search_articles.csv for 376+; 'report_year' can be used for calcofi report lookup, https://calcofi.org/publications/calcofi-reports/)
* Resulting papers for lab review from WOS: ‘ca-invert-shits/processed_data/combined_search29.xlsx’
* Taxa list for WOS search keywords: ‘ca-invert-shits/processed_data/WoRMS_taxlist_20250211_processed_V2.xlsx’
* Filtered dataset of furthest north extensions for the 3+ extension species: ‘ca-invert-shits/processed_data/threeplus_ext_sp_furthest_north.csv’
* ENSO / ONI data downloaded from download_enso(climate_idx = "oni", create_csv = TRUE), r library rsoi. ‘ca-invert-shits/data/enso_data.csv’

Scripts:
* ‘ca-invert-shits/scripts/load_lab_review.R’
  * Input:  ‘ca-invert-shits/data/lab_review_clean_missing_longitudes.csv’,
  * Output: ‘ca-invert-shits/processed_data/lab_review_with_longitudes.csv’,
  * Method: Adds longitudes of coastline to datapoints with only latitudes available.
* ‘ca-invert-shits/scripts/merge_calcofi_lab_reviews.R’
  * Inputs: 'ca-invert-shits/processed_data/calcofi_review_data_clean.csv', 'ca-invert-shits/processed_data/lab_review_with_longitudes.csv', 'ca-invert-shits/processed_data/historical-distributions-clean.csv',
  * Output: "ca-invert-shits/processed_data/merged_calcofi_lab_review.csv",
  * Method: Filters calcofi and lab reviews for ‘Include’ extensions and drop the low confidence ‘Opportunistic’ extensions from the lab review. Merge by the shared columns. Convert and formalize data types. Merge the historical ranges for the extension species. Requires a minimum year of 1900 (there was some data from pre 1900 accidentally pulled but should already have been excluded). Add coastal longitudes for historical distribution latitudes. Select the northernmost observation for each species each year. Make sure extensions are only included if observation latitude is > historical range lat. Add extension event ids where consecutive yearly observations are one extension event (i.e. if pelagic red crab was found 2013, 2014, 2018, There would be two event ids assigned: 1: 2013-2014 and 2: 2018).
* ‘ca-invert-shits/scripts/oni_vs_extension_distance.R’,
  * Input: "ca-invert-shits/processed_data/merged_calcofi_lab_review.csv",
  * Output: Scatterplot for Max ONI vs extension distance; histogram of extension frequency vs Max ONI; main and supplementary figure of species count per extension el nino frequency; associated statistics for all.
  * Method: Classify ENSO years by start (El Niño began), peak (Max number of months of an El Niño event was in that year), end (El Niño concluded), or some combination. Add filter for # of extensions requirement for each species. Add a column of whether the first year of an extension event was the start/peak/end of El Niño. Calculate extension distance (distHaversine from historical range edge to observation location) for each observation. Add oni values to extensions. Filter for furthest extensions of each species in each extension event and the max ONI for the extension event. Make max extension distance per event vs max ONI during event scatter plot and linear regression. Also has a version 2 where max ONI has a small change where if the first year of an extension event was the end of an El Niño event (i.e. a delayed response observation of the extension), the ONI for the year prior was used in the max ONI query. 
* ‘ca-invert-shits/scripts/enso_ext_timeseries.R’
  * Input: ‘ca-invert-shits/processed_data/merged_calcofi_lab_review.csv’,
  * Output: Preliminary ENSO time-wave and extension event bubble figure (final in Figure2Panel.R), supplementary extension barplot colored by calcofi and review, and extension frequency over time linear regression and related supplementary plot.
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
* 'ca-invert-shifts/scripts/summary_extension_table.R',
  * Input: 'ca-invert-shits/processed_data/merged_calcofi_lab_review.csv’,
  * Output: 'ca-invert-shits/figures/summary_extension_table.png'
* ‘ca-invert-shits/scripts/WoRMS_taxonomy_selection.R’
  * Input: ‘ca-invert-shits/processed_data/WoRMS_taxlist_20250211_processed_V2.xlsx’,
  * Output: a formatted printout of all the order/class/phylum names to add to the Web of Science search terms.
* ‘ca-invert-shits/scripts/collating_WOS.R’
  * Input: WOS_search29 folder,
  * Output: 'ca-invert-shits/processed_data/combined_search29.xlsx',
  * Method: This script pulls all the web of science results into one dataframe (each page of results had to be downloaded separately), deduplicates, and adds R-generated DOIs.
* 'ca-invert-shits/scripts/hist_lat_supp_table.R'
  * Input: 'processed_data/historical_distributions_clean.csv'
  * Output: 'figures/hist_lat_supp_table.png'
