# random selection of sample set CalCOFI reports
# must include at least 1 El Nino date

# years without a CalCOFI report
no_rep_yrs <- c(1951, 1954, 1957, 1959, 1962,
                1964, 1966, 1973, 1975, 1978)

# sequence of CalFOCI report years
all_yrs <- seq(from=1950, to=2019, by=1)
report_yrs <- setdiff(all_yrs, no_rep_yrs)

# El Nino years
enso <- c(1958, 1966, 1973, 1978, 1980, 1983, 1987, 
          1988, 1992, 1995, 1998, 2003, 2007, 2010, 2016)

# Repeat loop
# set seed to maintain random choice
set.seed(100)
sample_set <- sample(report_yrs, 5)
repeat {
  print(sample_set)
  enso_event_included <- length(as.numeric(intersect(sample_set, enso)))
  
  # Break statement to terminate if x > 4
  if(enso_event_included > 1) {
    break
  } 
  
  # Increment x by 1
  sample_set <- sample(report_yrs, 5)
  
}

#[1] 1965 2014 1997 2007 2010