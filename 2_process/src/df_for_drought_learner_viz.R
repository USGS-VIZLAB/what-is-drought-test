#' @description Function that combines the droughts as defined as "severe" (10%) for a give
#' site and year and for the two methods: site-specific thresholds or variable thresholds defined
#' by a 7 day floating window
#' @param StationID char, Gage identifier 
#' @param focal_year num, year of interest
#' @param df_1951_2020_drought_prop_site df, the droughts as defined by the fixed, site-specific thresholds
#' @param df_1951_2020_drought_prop_jd_7d df, the droughts as defined by the variable thresholds with 
#' a seven-day floating window (3 days on either side of a specific day)
prep_drought_df_lrnr_viz <- function(StationID, focal_year,
                                     df_1951_2020_drought_prop_site,
                                     df_1951_2020_drought_prop_jd_7d, 
                                     focal_threshold) {
  
  # Filter and simplify data to include only OH station and only 10% threshold and only 1963
  droughts_site <- df_1951_2020_drought_prop_site %>%
    filter(StaID == StationID, year(start) == focal_year, threshold == focal_threshold) %>%
    select(drought_id, duration, start, end, StaID, threshold) %>%
    mutate("method" = "fixed")
  droughts_j7 <- df_1951_2020_drought_prop_jd_7d %>%
    filter(StaID == StationID, year(start) == focal_year, threshold == focal_threshold) %>%
    select(drought_id, duration, start, end, StaID, threshold) %>% 
    mutate("method" = "variable")
  
  # Bind together
  droughts_learner_viz_df <- bind_rows(droughts_site, droughts_j7)
  
  return(droughts_learner_viz_df)
}

#' @description Function that takes a focal year and site (StaID) and cleans and combines the
#' streamflow percentiles and metadata to be ready for plotting in the drought learner viz
#' @param StationID char, Gage identifier 
#' @param focal_year num, year of interest
#' @param df_1951_2020_metadata df, contains metadata about each station/gage, including 
#' station name, etc
#' @param df_1951_2020_streamflow_perc df, contains the streamflow values by year and 
#' date for the stations, including also the thresholds used to define drought and the
#' percentiles for each day's streamflow value based on the thresholds
prep_streamflow_df_lrnr_viz <- function(StationID, focal_year,
                                        df_1951_2020_metadata,
                                        df_1951_2020_streamflow_perc) {
  
  # First, only the focal site and year to create basic df
  streamflow <- df_1951_2020_streamflow_perc %>% 
    # by site and year
    filter(StaID == StationID, year == focal_year) %>%
    select(dt, value, year, jd, thresh_10_site, thresh_10_jd_07d_wndw, StaID)
  
  # Second, calculate average streamflow by day of year
  streamflow_averages <- df_1951_2020_streamflow_perc %>%
    # By site and year
    filter(StaID == StationID) %>%
    # Summarize by day of year (julian day)
    group_by(jd) %>%
    summarise(mean_flow = mean(value))
  
  # Third extract station name from metadata
  streamflow <- streamflow %>%
    left_join(df_1951_2020_metadata %>% select(StaID, STANAME, STATE, LAT_GAGE, LNG_GAGE), by = "StaID")
  
  # Finally add in the averages to the main df
  prep_streamflow_df_lrnr_viz <- streamflow %>% 
    left_join(streamflow_averages, by = "jd")
  
  return(prep_streamflow_df_lrnr_viz)
}


prep_droughts_70year_learner_viz <- function(StationID, 
                                             df_1951_2020_drought_prop_jd_7d,
                                             df_1951_2020_drought_prop_site, 
                                             focal_threshold){
  # Select only the Ohio site to start, 10% threshold
  drought_prop_site <- df_1951_2020_drought_prop_site %>%
    filter(threshold == focal_threshold, StaID == StationID)
  drought_prop_j7 <- df_1951_2020_drought_prop_jd_7d %>%
    filter(threshold == focal_threshold, StaID == StationID)
  
  # Split each drought out by day that it occurred (longer) based on start and duration
  drought_prop_site_long <- drought_prop_site %>%
    group_by(drought_id) %>%
    slice(rep(seq_len(drought_id), each = duration)) %>%
    # mutate to add in date 
    mutate(drought_date = start + (seq_len(duration)-1),
           drought_date_noYr = format(as.Date(drought_date), "%m-%d"),
           drought_date_fakeYr = as.Date(sprintf("1999-%s", drought_date_noYr)),
           decade = year(floor_date(start, years(10))),
           method = "Fixed")
  drought_prop_j7_long <- drought_prop_j7 %>%
    group_by(drought_id) %>%
    slice(rep(seq_len(drought_id), each = duration)) %>%
    # mutate to add in date 
    mutate(drought_date = start + (seq_len(duration)-1),
           drought_date_noYr = format(as.Date(drought_date), "%m-%d"),
           drought_date_fakeYr = as.Date(sprintf("1999-%s", drought_date_noYr)),
           decade = year(floor_date(start, years(10))),
           method = "Variable")
  
  # Add in sequence value for stacking beeswarm points
  drought_prop_site_long <- drought_prop_site_long %>%
    group_by(drought_date_noYr) %>%
    mutate(y_seq = 1:n())
  drought_prop_j7_long <- drought_prop_j7_long %>%
    group_by(drought_date_noYr) %>%
    mutate(y_seq = 1:n())
  
  droughts_70year_learner_viz_df <- bind_rows(drought_prop_site_long,
                                                 drought_prop_j7_long)
}


p2_droughts_stacked_byDOY_df <- function(droughts_prop_target, StationID, example_threshold){
  # Select correct station ID and threshold
  droughts <- droughts_prop_target %>%
    filter(StaID == StationID, threshold == example_threshold) %>%
    select(drought_id, duration, start, end, StaID, threshold)
  
  
  
  # create date values for plotting
  droughts <- droughts %>% 
    mutate(start_year = year(start),
           start_noYr = format(as.Date(start), "%m-%d"),
           start_fakeYr = as.Date(sprintf("%s-%s", focal_year, start_noYr)),
           end_year = year(end),
           end_noYr = format(as.Date(end), "%m-%d"),
           end_fakeYr = as.Date(sprintf("%s-%s", focal_year, end_noYr)))
  
  # Deal with drought events that wrap year
  #   1. filter out only those 
  droughts_wrapYr <- droughts %>%
    filter(start_year != end_year)

  #   2. Duplicate records
  droughts_wrapYr_beginningOfYear <- droughts_wrapYr %>%
    mutate(start = as.Date(sprintf("%s-01-01", year(end))))
  droughts_wrapYr_endOfYear <- droughts_wrapYr %>%
    mutate(end = as.Date(sprintf("%s-12-31", year(start))))
  droughts_wrapYr <- bind_rows(droughts_wrapYr_beginningOfYear,
                                  droughts_wrapYr_endOfYear) 

  #   3. Fix metadata
  droughts_wrapYr <- droughts_wrapYr %>%
    mutate(start_year = year(start),
           start_noYr = format(as.Date(start), "%m-%d"),
           start_fakeYr = as.Date(sprintf("%s-%s", focal_year, start_noYr)),
           end_year = year(end),
           end_noYr = format(as.Date(end), "%m-%d"),
           end_fakeYr = as.Date(sprintf("%s-%s", focal_year, end_noYr)))

  #   4. Merge back in with other records
  droughts_noWrapYr <- droughts %>%
    filter(start_year == end_year)
  droughts <- bind_rows(droughts_wrapYr,
                           droughts_noWrapYr)

}