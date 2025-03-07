
WOS_search <- read.csv('sandbox/Screening & Review Tracking - Full Search.csv')
zotero_scrape <- read.csv('sandbox/WOS_search27.csv')

WOS_search_DOI <- WOS_search$r_DOI
zotero_scrape_DOI <- zotero_scrape$DOI

shared <- intersect (WOS_search_DOI, zotero_scrape_DOI)
