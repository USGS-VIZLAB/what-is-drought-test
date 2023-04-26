##### steps #####
[X] add variable thresholds (7d), 2% 
[ ] explore adding longest drought events first? - # of drought days doesn't = # of tiles
[ ] plot by region - # static 2%, variable 7d 2%
[ ] try using different % thresholds

# idea - take expanded table. already right# of rows. 

##### update casc #####
p2_casc_list <- list(NW = c('WA','OR','ID'),
                SW = c('CA','UT','NV','AZ'),
                SC = c('OK','TX','LA','NM'),
                NC = c('MT','ND','SD','WY','CO','NE','KS'),
                MW = c('WI','MN','IA','IN','IL','OH','MI', 'MO'),
                NE = c('ME','VT','NH','NY','NJ','PA','MA','RI','CT','WV','VA','MD','DE','KY'),
                SE = c('AR','MS','TN','NC','SC','AL','FL','GA','PR'),
                PI = c('HI','AS','GU'),
                AK = c('AK'))

# intentionally not including PI and AK?


##### test swarm ####
library(targets)
library(tidyverse)
library(data.table)
library(ggplot2)
library(lubridate)
library(scales)
library(ggforce)
library(showtext)
library(paletteer)
library(BAMMtools)
library(scico)
library(scales)
library(ggthemes)
library(usmap)
library(cowplot)
library(ggbeeswarm)
# tar_load(p2_site_prop_2)
tar_load(p2_1951_2020_drought_prop_jd_7d)
tar_load(p2_1951_2020_metadata)
p2_casc_list <- list(NW = c('WA','OR','ID'),
                     SW = c('CA','UT','NV','AZ'),
                     SC = c('OK','TX','LA','NM'),
                     NC = c('MT','ND','SD','WY','CO','NE','KS'),
                     MW = c('WI','MN','IA','IN','IL','OH','MI', 'MO'),
                     NE = c('ME','VT','NH','NY','NJ','PA','MA','RI','CT','WV','VA','MD','DE','KY'),
                     SE = c('AR','MS','TN','NC','SC','AL','FL','GA','PR'),
                     PI = c('HI','AS','GU'),
                     AK = c('AK'))
metadata <- p2_1951_2020_metadata %>%
  mutate(CASC = case_when(
    STATE %in% p2_casc_list$NW ~ 'NW',
    STATE %in% p2_casc_list$SW ~ 'SW',
    STATE %in% p2_casc_list$SC ~ 'SC',
    STATE %in% p2_casc_list$NC ~ 'NC',
    STATE %in% p2_casc_list$MW ~ 'MW',
    STATE %in% p2_casc_list$NE ~ 'NE',
    STATE %in% p2_casc_list$SE ~ 'SE'
  )) %>%
  filter(!is.na(CASC))
selected_national_prop <- p2_1951_2020_drought_prop_jd_7d %>%
  left_join(metadata %>%
              select(StaID:STATE, HCDN_2009, CASC)) %>%
  filter(threshold==2)

prop_expanded <- expand_drought_prop(selected_national_prop)
prop_chunks <- identify_drought_chunks(prop_expanded, 365/3)

n_chunks <- nrow(prop_chunks)
prop_swarm <- purrr::pmap_dfr(prop_chunks, function(...) {
  current_chunk <- tibble(...)
  swarm_data <- create_event_swarm_dt(event_data = selected_national_prop,
                                      start_period = current_chunk$start_date,
                                      end_period = current_chunk$break_date,
                                      max_droughts = current_chunk$max_single_day_droughts)
  message(sprintf('chunk %s of %s complete', current_chunk$chunk_num, n_chunks))
  return(swarm_data)
}, n_chunks)
prop_swarm_plot <- event_swarm_plot(prop_swarm)
ggsave('3_visualize/out/swarm_jd7d_2.png', 
       prop_swarm_plot,
       width = 100, height = 10, dpi = 300, limitsize = FALSE)

##### test regional ######

CASC_regions <- selected_national_prop %>% filter(!is.na(CASC)) %>% pull(CASC) %>% unique()

# source('2_process/src/prep_stripswarm.R')
# source('3_visualize/src/plot_stripswarm.R')
 
# selected_region <- CASC_regions[1]
# national_prop <- selected_national_prop
regional_duration_chart <- function(national_prop, selected_region, outfile_template) {
  # browser()
  message(sprintf('filtering to %s', selected_region))
  regional_prop <- national_prop %>%
    filter(CASC==selected_region)
  message('expanding region')
  regional_expanded <- expand_drought_prop(regional_prop)
  message(sprintf('nrow(regional_expanded): %s', nrow(regional_expanded)))
  message('identifying drought chunks')
  regional_drought_chunks <- identify_drought_chunks(regional_expanded, min_chunk_days=365/3)
  message(sprintf('sum(regional_drought_chunks$total_drought_days): %s', sum(regional_drought_chunks$total_drought_days)))
  message('generating swarm')
  n_chunks <- nrow(regional_drought_chunks)
  regional_swarm <- purrr::pmap_dfr(regional_drought_chunks, function(...) {
    current_chunk <- tibble(...)
    swarm_data <- create_event_swarm_dt(event_data = regional_prop,
                         start_period = current_chunk$start_date,
                         end_period = current_chunk$break_date,
                         max_droughts = current_chunk$max_single_day_droughts)
    message(sprintf('chunk %s of %s complete', current_chunk$chunk_num, n_chunks))
    return(swarm_data)
  }, n_chunks)
  message(sprintf('nrow(regional_swarm): %s', nrow(regional_swarm)))
  message('plotting swarm')
  regional_plot <- event_swarm_plot(swarm_data = regional_swarm)
  message('saving swarm plot')
  outfile <- sprintf(outfile_template, selected_region)
  ggsave(outfile, 
         regional_plot,
         width = 100, height = 10, dpi = 300, limitsize = FALSE)
  return(outfile)
} 
CASC_regions <- 'SW'
plot_files <- purrr::map(CASC_regions, function(region) {
  regional_plot <- regional_duration_chart(selected_national_prop, region, outfile_template='3_visualize/out/swarm_jd7d_2_%s.png')
  return(regional_plot)
})

##### 2010-2019 2% #####

### national
tar_load(p2_1951_2020_drought_prop_jd_7d)

selected_national_prop <- p2_1951_2020_drought_prop_jd_7d
prop_2010_2019 <- selected_national_prop %>%
  left_join(metadata %>%
              select(StaID:STATE, HCDN_2009, CASC)) %>%
  filter(threshold==2) %>%
  filter(start >= '2010-01-01' & end <= '2019-12-31')
prop_2010_2019_expanded <- expand_drought_prop(prop_2010_2019)
prop_2010_2019_chunks <- identify_drought_chunks(prop_2010_2019_expanded, min_chunk_days = 365/3)
n_chunks <- nrow(prop_2010_2019_chunks)
swarm_2019_2019 <- purrr::pmap_dfr(prop_2010_2019_chunks, function(...) {
  current_chunk <- tibble(...)
  swarm_data <- create_event_swarm_dt(event_data = prop_2010_2019,
                                      start_period = current_chunk$start_date,
                                      end_period = current_chunk$break_date,
                                      max_droughts = current_chunk$max_single_day_droughts)
  message(sprintf('chunk %s of %s complete', current_chunk$chunk_num, n_chunks))
  return(swarm_data)
}, n_chunks)
swarm_plot_2010_2019 <- event_swarm_plot(swarm_2019_2019)
ggsave('3_visualize/out/swarm_jd7d_2_2010_2019.png', swarm_plot_2010_2019, width = 100, height = 10, dpi = 300, limitsize = FALSE)

### regional
regions_2010_2019 <- prop_2010_2019 %>% filter(!is.na(CASC)) %>% pull(CASC) %>% unique()
regional_plots_2010_2019 <- purrr::map(regions_2010_2019, function(region) {
  regional_plot <- regional_duration_chart(prop_2010_2019, region, outfile_template='3_visualize/out/swarm_jd7d_2_2010_2019_%s.png')
  return(regional_plot)
})

### western states
western_states <- c('AZ','CA','CO','ID','MT','NV','NM','OR','UT','WA','WY')
western_prop_2010_2019 <- prop_2010_2019 %>% 
  filter(STATE %in% western_states)
western_prop_expanded <- expand_drought_prop(western_prop_2010_2019)
western_prop_chunks <- identify_drought_chunks(western_prop_expanded, 365/3)

n_chunks <- nrow(western_prop_chunks)
western_swarm <- purrr::pmap_dfr(western_prop_chunks, function(...) {
  current_chunk <- tibble(...)
  swarm_data <- create_event_swarm_dt(event_data = western_site_2010_2019,
                                      start_period = current_chunk$start_date,
                                      end_period = current_chunk$break_date,
                                      max_droughts = current_chunk$max_single_day_droughts)
  message(sprintf('chunk %s of %s complete', current_chunk$chunk_num, n_chunks))
  return(swarm_data)
}, n_chunks)
western_swarm_plot_2010_2019 <- event_swarm_plot(western_swarm)
ggsave('3_visualize/out/swarm_jd7d_2_2010_2019_western_states.png', western_swarm_plot_2010_2019, width = 100, height = 10, dpi = 300, limitsize = FALSE)


state_duration_chart <- function(national_prop, selected_state, outfile_template) {
  message(sprintf('filtering to %s', selected_state))
  state_prop <- national_prop %>%
    filter(STATE==selected_state)
  message('expanding state prop')
  state_expanded <- expand_drought_prop(state_prop)
  message(sprintf('nrow(state_expanded): %s', nrow(state_expanded)))
  message('identifying drought chunks')
  state_drought_chunks <- identify_drought_chunks(state_expanded, min_chunk_days=365/3)
  message(sprintf('sum(state_drought_chunks$total_drought_days): %s', sum(state_drought_chunks$total_drought_days)))
  message('generating swarm')
  n_chunks <- nrow(state_drought_chunks)
  message(sprintf('data will be processed in %s chunks', n_chunks))
  state_swarm <- purrr::pmap_dfr(state_drought_chunks, function(...) {
    current_chunk <- tibble(...)
    swarm_data <- create_event_swarm_dt(event_data = state_prop,
                                        start_period = current_chunk$start_date,
                                        end_period = current_chunk$break_date,
                                        max_droughts = current_chunk$max_single_day_droughts)
    message(sprintf('chunk %s of %s complete', current_chunk$chunk_num, n_chunks))
    return(swarm_data)
  }, n_chunks)
  message(sprintf('nrow(state_swarm): %s', nrow(state_swarm)))
  message('plotting swarm')
  state_plot <- event_swarm_plot(swarm_data = state_swarm)
  message('saving swarm plot')
  outfile <- sprintf(outfile_template, selected_state)
  ggsave(outfile, 
         state_plot,
         width = 100, height = 10, dpi = 300, limitsize = FALSE)
  return(outfile)
} 
western_state_plots_2010_2019 <- purrr::map(western_states, function(state) {
  state_plot <- state_duration_chart(site_2010_2019, state, outfile_template='3_visualize/out/swarm_jd7d_2_2010_2019_%s.png')
  return(state_plot)
})

##### test percentiles #####
library(targets)
library(tidyverse)
library(data.table)
library(ggplot2)
library(lubridate)
library(scales)
library(ggforce)
library(showtext)
library(paletteer)
library(BAMMtools)
library(scico)
library(scales)
library(ggthemes)
library(usmap)
library(cowplot)
library(ggbeeswarm)
tar_load(p2_1951_2020_metadata)
tar_load(p2_1951_2020_drought_prop_site)
tar_load(p2_1951_2020_drought_prop_jd_7d)


selected_national_prop <- p2_1951_2020_drought_prop_jd_7d
percentile_levels <- selected_national_prop %>% pull(threshold) %>% unique()
percentile_levels <- percentile_levels[!(percentile_levels==2)] # drop 2 b/c already plotted
min_date <- '1951-01-01' #min(selected_national_prop$start)
max_date <- '1952-12-31' # min_date %m+% years(2) 

source('2_process/src/prep_stripswarm.R')
source('3_visualize/src/plot_stripswarm.R')

# selected_percentile <- percentile_levels[1]
national_prop_filter <- selected_national_prop %>%
  filter(start >= min_date & end <= max_date)
# metadata <- p2_1951_2020_metadata
# outfile_template<-'3_visualize/out/swarm_site_%s_1951_1952.png'
percentile_duration_chart <- function(national_prop, selected_percentile, metadata, outfile_template) {
  message(sprintf('filtering to %s threshold', selected_percentile))
  percentile_prop <- national_prop %>%
    filter(threshold == selected_percentile) %>%
    left_join(metadata %>%
                select(StaID:STATE, HCDN_2009, CASC))
  message('expanding data')
  percentile_expanded <- expand_drought_prop(percentile_prop)
  message(sprintf('nrow(percentile_expanded): %s', nrow(percentile_expanded)))
  message('identifying drought chunks')
  percentile_drought_chunks <- identify_drought_chunks(percentile_expanded, min_chunk_size=365/3)
  message(sprintf('sum(percentile_drought_chunks$total_drought_days): %s', sum(percentile_drought_chunks$total_drought_days)))
  message('generating swarm')
  n_chunks <- nrow(percentile_drought_chunks)
  percentile_swarm <- purrr::pmap_dfr(percentile_drought_chunks, function(...) {
    current_chunk <- tibble(...)
    swarm_data <- create_event_swarm_dt(event_data = percentile_prop,
                                        start_period = current_chunk$start_date,
                                        end_period = current_chunk$break_date,
                                        max_droughts = current_chunk$max_single_day_droughts)
    message(sprintf('chunk %s of %s complete', current_chunk$chunk_num, n_chunks))
    return(swarm_data)
  }, n_chunks)
  message(sprintf('nrow(percentile_swarm): %s', nrow(percentile_swarm)))
  message('plotting swarm')
  percentile_plot <- event_swarm_plot(swarm_data = percentile_swarm)
  message('saving swarm plot')
  outfile <- sprintf(outfile_template, selected_percentile)
  ggsave(outfile, 
         percentile_plot,
         width = 100, height = 10, dpi = 300, limitsize = FALSE)
  return(outfile)
} 
percentile_levels <- 2
percentile_plot_files <- purrr::map(percentile_levels, function(percentile) {
  percentile_plot <- percentile_duration_chart(national_prop_filter, percentile, p2_1951_2020_metadata, outfile_template='3_visualize/out/swarm_jd7d_%s_1951_1952.png')
  return(percentile_plot)
})

##### timing #####
result_of_timing <- tibble(
  fxns = c('_dt','_dt','_dplyr','_dplyr', '_dplyr','_dt'),
  years = c(1,5,1,5, 10, 10),
  seconds = c(11.5,123.9,20.1,104.6,277.3, 337.6) #579 with targets
)

ggplot2::ggplot(result_of_timing) + 
  geom_line(aes(x=years,y=seconds,color=fxns))

##### date chunking #####
# fully expand drought dataset - every day with drought is represented
# identify dates that have no droughts - pull for chunking
# process chunks of data then merge
# also identify max # of droughts on given date

p2_site_prop_2_expanded <- p2_site_prop_2 %>%
  uncount(duration, .remove=FALSE, .id="id") %>%
  mutate(date = start+id-1, .after=end)
# pull unique drought dates
# drought_dates <- unique(p2_site_prop_2_expanded$date)
drought_dates <- p2_site_prop_2_expanded %>%
  count(date, name='n_droughts')
# get full sequence of dates
full_date_sequence <- seq.Date(from=min(drought_dates$date),to=max(drought_dates$date),by=1)
# find dates that didn't have droughts
dates_w_o_drought <- full_date_sequence[!(full_date_sequence %in% drought_dates$date)]
# for each time chunk, find max # of droughts on single day in that period

# figure out chunks if used all dates w/o drought as breaks
drought_chunks <- 
  tibble(break_date = c(dates_w_o_drought, max(full_date_sequence))) %>%
  mutate(chunk_num = row_number(),
         start_date = case_when(
           chunk_num==1 ~ min(full_date_sequence),
           TRUE ~ lag(break_date)+1
         ),
         chunk_length_days = as.numeric(break_date-start_date)) %>%
  select(chunk_num, start_date,break_date, chunk_length_days) #%>%
  # group_by(chunk_num) %>%
  #   group_modify( ~ {
  #     drought_dates_subset <- drought_dates %>%
  #       filter(date >= .x$start_date & date<=.x$break_date)
  #     .x <- mutate(.x, max_single_day_droughts = max(drought_dates_subset$n_droughts),
  #                  total_drought_days = sum(drought_dates_subset$n_droughts))
  #   })

# drought_chunks <- pmap_dfr(drought_chunks, function(...) {
#   current_chunk <- tibble(...)
#   drought_dates_subset <- drought_dates %>%
#     filter(date >= current_chunk$start_date & date<=current_chunk$break_date)
#   current_chunk %>%
#     mutate(max_single_day_droughts = max(drought_dates_subset$n_droughts),
#            total_drought_days = sum(drought_dates_subset$n_droughts))
# })

get_drought_info <- function(drought_df, chunk_df) {
  pmap_dfr(chunk_df, function(...) {
    current_chunk <- tibble(...)
    drought_subset <- drought_df %>%
      filter(date >= current_chunk$start_date & date<=current_chunk$break_date)
    current_chunk %>%
      mutate(max_single_day_droughts = max(drought_subset$n_droughts),
             total_drought_days = sum(drought_subset$n_droughts),
             n_days_w_droughts = nrow(drought_subset))
  })
}
drought_chunks <- get_drought_info(drought_dates, drought_chunks)
# %>%
#   group_by(break_num) %>%
#   group_modify( ~ {
#     drought_dates_subset <- drought_dates %>%
#       filter(date >= .x$start_date & date<=.x$break_date)
#     .x <- mutate(.x, max_single_day_droughts = max(drought_dates_subset$n_droughts), 
#                  total_drought_days = sum(drought_dates_subset$n_droughts))
#   })
# subset to larger chunks only
combined_drought_chunks <- drought_chunks %>%
  filter(chunk_length_days>=365/3) %>%
  select(break_date) %>%
  # recalcuate break_num, start date, and break length
  mutate(chunk_num = row_number(),
         start_date = case_when(
           chunk_num==1 ~ min(full_date_sequence),
           TRUE ~ lag(break_date)+1
         ),
         chunk_length_days = as.numeric(break_date-start_date),
         chunk_length_year = chunk_length_days/365) #%>%

combined_drought_chunks <- get_drought_info(drought_dates, combined_drought_chunks)

check <- p2_site_prop_2 %>% 
  filter(start >= selected_breaks$start_date[1]) %>%
  filter(start <= selected_breaks$break_date[1]) %>% 
  mutate(onset_day = as.integer(start - selected_breaks$start_date[1])+1) %>% 
  mutate(end_day = as.integer(end - selected_breaks$start_date[1])+1) %>%
  mutate(end_day_2 = onset_day+duration) %>% 
  arrange(onset_day) %>%
  mutate(idx = row_number())

##### targets #####

##### functions #####
create_event_swarm <- function(event_data, start_period, end_period){
  event_subset <- event_data %>% 
    filter(start > start_period) %>%
    filter(start <= end_period) %>% 
    mutate(onset_day = as.integer(start - start_period)) %>% 
    mutate(end_day = as.integer(end - start_period)) %>% 
    arrange(onset_day)
  
  # set up an empty "swarm grid" to place drought events into
  n <- 600 # set arbitrarily large number of possible simultaneous drought events positions. Trimmed prior to plotting
  
  E <- as_tibble(matrix(NaN,nrow=n,ncol=max(event_subset$end_day)+1))
  E <- E %>% mutate(priority = 1:n)
  E <- E %>% arrange(desc(priority)) %>% 
    bind_rows(E) %>% 
    mutate(rnum = 1:(2*n))
  
  # loop through each event and place into best available spot in grid
  progress_bar <- txtProgressBar(min = 1, max = nrow(event_subset), style = 3)
  
  for (idx in 1:nrow(event_subset)){
    temp_dur <- event_subset[[idx,'duration']]
    temp_startd <-event_subset[[idx,'onset_day']]
    # find available spots looking within 1 day +/- the start date (to encourage a little compactness)
    avail_rows <- E %>% select(all_of(temp_startd:(temp_startd + temp_dur - 1)),priority,rnum) %>% 
      filter(is.na(if_all(starts_with("V")))) %>% 
      mutate(pos = 0)
    avail_rows_plus1d <- E %>% select(all_of((temp_startd+1):(temp_startd + temp_dur)),priority,rnum) %>%
      filter(is.na(if_all(starts_with("V")))) %>%
      mutate(pos = 1)
    avail_rows_minus1d <- E %>% select(all_of((temp_startd-1):(temp_startd + temp_dur - 2)),priority,rnum) %>%
      filter(is.na(if_all(starts_with("V")))) %>%
      mutate(pos = -1)
    # find spot closest to central axis
    all_avail_rows <- bind_rows(avail_rows, avail_rows_minus1d, avail_rows_plus1d) %>% 
      arrange(priority) %>%
      group_by(priority) %>%
      slice_sample(prop = 1) %>% # adds a little randomness by assigning to spot above or below central axis randomly
      ungroup()
    temp_rnum <- all_avail_rows[[1,'rnum']]
    temp_pos_key <- all_avail_rows[[1, 'pos']]
    if (temp_startd == 1){
      temp_pos_key <- 0
    }
    # assign event to identified spot by using duration value
    E[temp_rnum,((temp_startd + temp_pos_key):(temp_startd + temp_dur - 1 + temp_pos_key))] <- event_subset[[idx,'duration']]
    E[temp_rnum,(temp_startd + temp_dur + temp_pos_key)] <- 0 # enforces a space between subsequent events
    
    setTxtProgressBar(progress_bar, idx)
  }
  # trim unused rows
  ind <- E %>% select(-priority,-rnum) %>% 
    apply( 1, function(x) all(is.na(x)))
  E <-E[ !ind, ]
  
  E[E == 0] = NaN # remove spaces added to avoid events appearing connected
  
  ncols = ncol(E)-2
  
  E <- mutate(E, decade = as.factor(floor(year(start_period)/10)*10))
  
  plot_dat <- E %>% select(-priority) %>% 
    pivot_longer(cols=1:ncols, names_to = "names", values_to = "duration")
  plot_dat$names<- str_remove(plot_dat$names,"V")
  plot_dat <- plot_dat %>% mutate(dt = as.integer(names)) %>% 
    mutate(date = as.Date(start_period + dt - 1)) %>% 
    mutate(rnum = rnum - n) %>% 
    drop_na()
  
  return(plot_dat)
}
# event_subset_2000_2009 <- p2_site_prop_2 %>%
#   filter(start > as.Date('2000-01-01')) %>%
#   filter(start <= as.Date('2009-12-31')) %>%
#   mutate(onset_day = as.integer(start - as.Date('2000-01-01'))) %>%
#   mutate(end_day = as.integer(end - as.Date('2000-01-01'))) %>%
#   arrange(onset_day) %>%
#   mutate(idx = row_number())
# sum(event_subset_2000_2009$duration) # 116667
# event_subset_2000_2004 <- p2_site_prop_2 %>%
#   filter(start > as.Date('2000-01-01')) %>%
#   filter(start <= as.Date('2004-12-31')) %>%
#   mutate(onset_day = as.integer(start - as.Date('2000-01-01'))) %>%
#   mutate(end_day = as.integer(end - as.Date('2000-01-01'))) %>%
#   arrange(onset_day) %>%
#   mutate(idx = row_number())
# sum(event_subset_2000_2004$duration) #66677
# event_subset_2000 <- p2_site_prop_2 %>% 
#   filter(start > as.Date('2000-01-01')) %>%
#   filter(start <= as.Date('2000-12-31')) %>% 
#   mutate(onset_day = as.integer(start - as.Date('2000-01-01'))) %>% 
#   mutate(end_day = as.integer(end - as.Date('2000-01-01'))) %>% 
#   arrange(onset_day) %>%
#   mutate(idx = row_number())
# sum(event_subset_2000$duration) #12340
# create_event_swarm() for 1 year (2000-01-01 -> 2000-12-31) = (a) 73.44 seconds, (b) 64 secs, (c) 69 secs [(a) 10961 obs, (b) 10725, (c) 10683] || w/o progress bar 60 seconds [10962]
# create_event_swarm_mod() w/ group_modify for 1 year (2000-01-01 -> 2000-12-31) = 81 seconds
# create_event_swarm_mod() w/ purrr w/i for loop for 1 year (2000-01-01 -> 2000-12-31) = 66 seconds
# create_event_swarm_mod() w/ filter before slice_sample (a) = 38 seconds, (b) 34 secs, (c) 31 secs, (d) 33 secs [(b) 10918, (c) 10648, (d) 10669] || w/o progress bar 26 seconds [10968]
# create_event_swarm_dt() w/ data.table = (a) 18 seconds, (b) 17 secs, (c) 18 secs [(a) 12340, (b) 12340, (c) 12340 ] || w/o progress bar 14 seconds [12340] || dt for 200-2004 [66677]
# create_event_swarm_dplyr 2000-2004 [57726]
# tar_make() w/ create_event_swarm() for 20 year (2000-01-01 -> 2020-12-31) = 1940s = 30 min
# tar_make() w/ create_event_swarm_dt() for 20 year (2000-01-01 -> 2020-12-31) = 2502s = 40 min

create_event_swarm_mod <- function(event_data, start_period, end_period, max_droughts){
  #browser()
  event_subset <- event_data %>% 
    filter(start > start_period) %>%
    filter(start <= end_period) %>% 
    mutate(onset_day = as.integer(start - start_period)) %>% 
    mutate(end_day = as.integer(end - start_period)) %>% 
    arrange(onset_day) %>%
    mutate(idx = row_number())
  
  # set up an empty "swarm grid" to place drought events into
  n <- round(max_droughts/2)+10 #600 # set arbitrarily large number of possible simultaneous drought events positions. Trimmed prior to plotting
  
  #### alt options : 
  ###### keep as matrix
  ###### use data.table
  E <- as_tibble(matrix(NaN,nrow=n,ncol=max(event_subset$end_day)+1))
  E <- E %>% mutate(priority = 1:n)
  E <- E %>% arrange(desc(priority)) %>% 
    bind_rows(E) %>% 
    mutate(rnum = 1:(2*n))
  
  # loop through each event and place into best available spot in grid
  progress_bar <- txtProgressBar(min = 1, max = nrow(event_subset), style = 3)
  
  #### alt options:
  ###### use purrr
  ###### use data.table
  for (idx in 1:nrow(event_subset)){
    temp_dur <- event_subset[[idx,'duration']]
    temp_startd <-event_subset[[idx,'onset_day']]
    search_windows <- tibble(
      window = c('d','plus1d', 'minus1d'),
      start = c(temp_startd, temp_startd+1, temp_startd-1), # 12, 13, 11
      end = c(temp_startd + temp_dur - 1, temp_startd + temp_dur, temp_startd + temp_dur - 2), # 18, 19, 17
      pos = c(0, 1, -1)
    )
    
    # find available spots looking within 1 day +/- the start date (to encourage a little compactness)
    all_avail_rows <- purrr::pmap_dfr(search_windows, function(...) {
      current_window <- tibble(...)
      E %>% select(all_of(current_window$start:current_window$end),priority,rnum) %>%
        filter(is.na(if_all(starts_with("V")))) %>%
        mutate(pos = current_window$pos)
    }) %>%
      filter(priority==min(priority)) %>%
      slice_sample(prop = 1) # adds a little randomness by assigning to spot above or below central axis randomly
    
    temp_rnum <- all_avail_rows[[1,'rnum']]
    temp_pos_key <- all_avail_rows[[1, 'pos']]
    if (temp_startd == 1){
      temp_pos_key <- 0
    }
    
    # assign event to identified spot by using duration value
    E[temp_rnum,((temp_startd + temp_pos_key):(temp_startd + temp_dur - 1 + temp_pos_key))] <- temp_dur #event_subset[[idx,'duration']]
    E[temp_rnum,(temp_startd + temp_dur + temp_pos_key)] <- 0 # enforces a space between subsequent events
    
    setTxtProgressBar(progress_bar, idx)
  }
  
  # purrr
  
  
  # window_list <- list()
  # event_subset %>%
  #   group_by(idx) %>%
  #   group_map( ~ {
  #     # browser()
  #     current_matrix <- E
  #     temp_dur <- .x$duration
  #     temp_startd <- .x$onset_day
  #     
  #     search_windows <- tibble(
  #       window = c('d','plus1d', 'minus1d'),
  #       start = c(temp_startd, temp_startd+1, temp_startd-1),
  #       end = c(temp_startd + temp_dur - 1, temp_startd + temp_dur, temp_startd + temp_dur - 2),
  #       pos = c(0, 1, -1)
  #     )
  #     
  #     all_avail_rows <- purrr::pmap_dfr(search_windows, function(...) {
  #       current_window <- tibble(...)
  #       current_matrix %>% select(all_of(current_window$start:current_window$end),priority,rnum) %>% 
  #         filter(is.na(if_all(starts_with("V")))) %>% 
  #         mutate(pos = current_window$pos)
  #     }) %>%
  #       arrange(priority) %>%
  #       group_by(priority) %>%
  #       slice_sample(prop = 1) %>% # adds a little randomness by assigning to spot above or below central axis randomly
  #       ungroup()
  #     temp_rnum <- all_avail_rows[[1,'rnum']]
  #     temp_pos_key <- all_avail_rows[[1, 'pos']]
  #     if (temp_startd == 1){
  #       temp_pos_key <- 0
  #     }
  # 
  #     # taken_window <- tibble(
  #     #   dt = ((temp_startd + temp_pos_key):(temp_startd + temp_dur - 1 + temp_pos_key)),
  #     #   rnum = temp_rnum,
  #     #   duration = temp_dur
  #     # )
  #     # 
  #     # dt_run_tupes <- sets::tuple(taken_window$rnum, taken_window$dt)
  #     
  #     # assign event to identified spot by using duration value
  #     current_matrix[temp_rnum,((temp_startd + temp_pos_key):(temp_startd + temp_dur - 1 + temp_pos_key))] <- temp_dur #event_subset[[idx,'duration']]
  #     current_matrix[temp_rnum,(temp_startd + temp_dur + temp_pos_key)] <- 0 # enforces a space between subsequent events
  #     
  #     E <<- current_matrix
  #     
  #     setTxtProgressBar(progress_bar, .y$idx)
  #   })
  
  
  # trim unused rows
  ind <- E %>% select(-priority,-rnum) %>% 
    apply( 1, function(x) all(is.na(x)))
  E <-E[ !ind, ]
  
  E[E == 0] = NaN # remove spaces added to avoid events appearing connected
  
  ncols = ncol(E)-2
  
  E <- mutate(E, decade = as.factor(floor(year(start_period)/10)*10))
  
  plot_dat <- E %>% select(-priority) %>% 
    pivot_longer(cols=1:ncols, names_to = "names", values_to = "duration")
  plot_dat$names<- str_remove(plot_dat$names,"V")
  plot_dat <- plot_dat %>% mutate(dt = as.integer(names)) %>% 
    mutate(date = as.Date(start_period + dt - 1)) %>% 
    mutate(rnum = rnum - n) %>% 
    drop_na()
  
  return(plot_dat)
}

create_event_swarm_dplyr <- function(event_data, start_period, end_period, max_droughts){
  #browser()
  event_subset <- event_data %>% 
    filter(start >= start_period) %>%
    filter(start <= end_period) %>% 
    mutate(onset_day = as.integer(start - start_period)) %>% 
    mutate(end_day = as.integer(end - start_period)) %>% 
    arrange(onset_day) %>%
    mutate(idx = row_number())
  
  # set up an empty "swarm grid" to place drought events into
  n <- max_droughts+10 #600 # set arbitrarily large number of possible simultaneous drought events positions. Trimmed prior to plotting
  
  #### alt options : 
  ###### keep as matrix
  ###### use data.table
  E <- as_tibble(matrix(NaN,nrow=n,ncol=max(event_subset$end_day)+1))
  E <- E %>% mutate(priority = 1:n)
  E <- E %>% arrange(desc(priority)) %>% 
    bind_rows(E) %>% 
    mutate(rnum = 1:(2*n))
  
  # loop through each event and place into best available spot in grid
  progress_bar <- txtProgressBar(min = 1, max = nrow(event_subset), style = 3)
  
  #### alt options:
  ###### use purrr
  ###### use data.table
  # for (idx in 1:nrow(event_subset)){
  #   temp_dur <- event_subset[[idx,'duration']]
  #   temp_startd <-event_subset[[idx,'onset_day']]
  #   search_windows <- tibble(
  #     window = c('d','plus1d', 'minus1d'),
  #     start = c(temp_startd, temp_startd+1, temp_startd-1), # 12, 13, 11
  #     end = c(temp_startd + temp_dur - 1, temp_startd + temp_dur, temp_startd + temp_dur - 2), # 18, 19, 17
  #     pos = c(0, 1, -1)
  #   )
  #   
  #   # find available spots looking within 1 day +/- the start date (to encourage a little compactness)
  #   all_avail_rows <- purrr::pmap_dfr(search_windows, function(...) {
  #     current_window <- tibble(...)
  #     E %>% select(all_of(current_window$start:current_window$end),priority,rnum) %>%
  #       filter(is.na(if_all(starts_with("V")))) %>%
  #       mutate(pos = current_window$pos)
  #   }) %>%
  #     filter(priority==min(priority)) %>%
  #     slice_sample(prop = 1) # adds a little randomness by assigning to spot above or below central axis randomly
  #   
  #   temp_rnum <- all_avail_rows[[1,'rnum']]
  #   temp_pos_key <- all_avail_rows[[1, 'pos']]
  #   if (temp_startd == 1){
  #     temp_pos_key <- 0
  #   }
  #   
  #   # assign event to identified spot by using duration value
  #   E[temp_rnum,((temp_startd + temp_pos_key):(temp_startd + temp_dur - 1 + temp_pos_key))] <- temp_dur #event_subset[[idx,'duration']]
  #   E[temp_rnum,(temp_startd + temp_dur + temp_pos_key)] <- 0 # enforces a space between subsequent events
  #   
  #   setTxtProgressBar(progress_bar, idx)
  # }
  # 
  # # purrr
  
  
  # window_list <- list()
  event_subset %>%
    group_by(idx) %>%
    group_map( ~ {
      # browser()
      current_matrix <- E
      temp_dur <- .x$duration
      temp_startd <- .x$onset_day
      
      search_windows <- tibble(
        window = c('d','plus1d', 'minus1d'),
        start = c(temp_startd, temp_startd+1, temp_startd-1),
        end = c(temp_startd + temp_dur - 1, temp_startd + temp_dur, temp_startd + temp_dur - 2),
        pos = c(0, 1, -1)
      )
      
      all_avail_rows <- purrr::pmap_dfr(search_windows, function(...) {
        current_window <- tibble(...)
        current_matrix %>% select(all_of(current_window$start:current_window$end),priority,rnum) %>%
          filter(is.na(if_all(starts_with("V")))) %>%
          mutate(pos = current_window$pos)
      }) %>%
        filter(priority==min(priority)) %>%
        slice_sample(prop = 1)
      # arrange(priority) %>%
      # group_by(priority) %>%
      # slice_sample(prop = 1) %>% # adds a little randomness by assigning to spot above or below central axis randomly
      # ungroup()
      temp_rnum <- all_avail_rows[[1,'rnum']]
      temp_pos_key <- all_avail_rows[[1, 'pos']]
      if (temp_startd == 1){
        temp_pos_key <- 0
      }
      
      # taken_window <- tibble(
      #   dt = ((temp_startd + temp_pos_key):(temp_startd + temp_dur - 1 + temp_pos_key)),
      #   rnum = temp_rnum,
      #   duration = temp_dur
      # )
      #
      # dt_run_tupes <- sets::tuple(taken_window$rnum, taken_window$dt)
      
      # assign event to identified spot by using duration value
      current_matrix[temp_rnum,((temp_startd + temp_pos_key):(temp_startd + temp_dur - 1 + temp_pos_key))] <- temp_dur #event_subset[[idx,'duration']]
      current_matrix[temp_rnum,(temp_startd + temp_dur + temp_pos_key)] <- 0 # enforces a space between subsequent events
      
      E <<- current_matrix
      
      setTxtProgressBar(progress_bar, .y$idx)
    })
  
  
  # trim unused rows
  ind <- E %>% select(-priority,-rnum) %>% 
    apply( 1, function(x) all(is.na(x)))
  E <-E[ !ind, ]
  
  E[E == 0] = NaN # remove spaces added to avoid events appearing connected
  
  ncols = ncol(E)-2
  
  E <- mutate(E, decade = as.factor(floor(year(start_period)/10)*10))
  
  plot_dat <- E %>% select(-priority) %>% 
    pivot_longer(cols=1:ncols, names_to = "names", values_to = "duration")
  plot_dat$names<- str_remove(plot_dat$names,"V")
  plot_dat <- plot_dat %>% mutate(dt = as.integer(names)) %>% 
    mutate(date = as.Date(start_period + dt - 1)) %>% 
    mutate(rnum = rnum - n) %>% 
    drop_na()
  
  return(plot_dat)
}

create_event_swarm_mod_2 <- function(event_data, start_period, end_period, max_droughts){
  #browser()
  event_subset <- event_data %>% 
    filter(start >= start_period) %>%
    filter(start <= end_period) %>% 
    mutate(onset_day = as.integer(start - start_period)) %>% 
    mutate(end_day = as.integer(end - start_period)) %>% 
    arrange(onset_day) %>%
    mutate(idx = row_number())
  
  # set up an empty "swarm grid" to place drought events into
  n <- max_droughts+10 #600 # set arbitrarily large number of possible simultaneous drought events positions. Trimmed prior to plotting
  
  #### alt options : 
  ###### keep as matrix
  ###### use data.table
  E <- as_tibble(matrix(NaN,nrow=n,ncol=max(event_subset$end_day)+1))
  E <- E %>% mutate(priority = 1:n)
  E <- E %>% arrange(desc(priority)) %>%
    bind_rows(E) %>%
    mutate(rnum = 1:(2*n))
  
  mat <- matrix(NaN,nrow=n,ncol=max(event_subset$end_day)+1)
  E_1 <- setDT(as.data.frame(mat))[]
  E_1[,priority:=1:n]
  E_2 <- E_1[order(-priority)]
  E_3 <- rbindlist(list(E_2, E_1))
  E_3[,rnum:=1:(2*n)]
  
  # loop through each event and place into best available spot in grid
  progress_bar <- txtProgressBar(min = 1, max = nrow(event_subset), style = 3)
  
  # store priority, rnum for all rows
  rnum_priority <- E_3[,.(priority,rnum)]
  
  #### alt options:
  ###### use purrr
  ###### use data.table
  for (idx in 1:nrow(event_subset)){
    temp_dur <- event_subset[[idx,'duration']]
    temp_startd <-event_subset[[idx,'onset_day']]
    
    # if(idx==50) {
    #   browser()
    # }
    # 
    
    # find available spots looking within 1 day +/- the start date (to encourage a little compactness)
    avail_rows <- E %>% select(all_of(temp_startd:(temp_startd + temp_dur - 1)),priority,rnum) %>%
      filter(is.na(if_all(starts_with("V")))) %>%
      mutate(pos = 0)
    avail_rows_plus1d <- E %>% select(all_of((temp_startd+1):(temp_startd + temp_dur)),priority,rnum) %>%
      filter(is.na(if_all(starts_with("V")))) %>%
      mutate(pos = 1)
    avail_rows_minus1d <- E %>% select(all_of((temp_startd-1):(temp_startd + temp_dur - 2)),priority,rnum) %>%
      filter(is.na(if_all(starts_with("V")))) %>%
      mutate(pos = -1)
    # find spot closest to central axis
    all_avail_rows <- bind_rows(avail_rows, avail_rows_minus1d, avail_rows_plus1d) %>%
      filter(priority==min(priority)) %>%
      slice_sample(prop = 1)
    
    
    dt_d <- E_3[,temp_startd:(temp_startd + temp_dur - 1)]
    dt_d <- cbind(dt_d, rnum_priority)
    dt_d <- dt_d[rowSums(is.na(dt_d))==(ncol(dt_d)-2),] # pull rows where all but priority and rnum are NA
    dt_d[,pos:=0]
    dt_plus1d <- E_3[,(temp_startd+1):(temp_startd + temp_dur)]
    dt_plus1d <- cbind(dt_plus1d, rnum_priority)
    dt_plus1d <- dt_plus1d[rowSums(is.na(dt_plus1d))==(ncol(dt_plus1d)-2),]
    dt_plus1d[,pos:=1]
    dt_minus1d <- E_3[,(temp_startd-1):(temp_startd + temp_dur - 2)]
    dt_minus1d <- cbind(dt_minus1d, rnum_priority)
    dt_minus1d <- dt_minus1d[rowSums(is.na(dt_minus1d))==(ncol(dt_minus1d)-2),]
    dt_minus1d[,pos:=-1]
    all_dt <- rbindlist(list(dt_d, dt_minus1d, dt_plus1d), fill=TRUE) # rbindlist(list(dt_d, dt_plus1d, dt_minus1d)) # DROPS columns that aren't shared
    all_dt <- all_dt[priority==min(priority)]
    all_dt <- all_dt[sample(.N,1)]
    
    
    temp_rnum <- all_avail_rows[[1,'rnum']]
    temp_pos_key <- all_avail_rows[[1, 'pos']]
    temp_rnum_dt <- all_dt[,rnum]
    temp_pos_key_dt <- all_dt[,pos]
    if (temp_startd == 1){
      temp_pos_key <- 0
      temp_pos_key_dt <- 0
    }
    
    # assign event to identified spot by using duration value
    E[temp_rnum,((temp_startd + temp_pos_key):(temp_startd + temp_dur - 1 + temp_pos_key))] <- temp_dur #event_subset[[idx,'duration']]
    E[temp_rnum,(temp_startd + temp_dur + temp_pos_key)] <- 0 # enforces a space between subsequent events
    
    E_3[temp_rnum_dt,((temp_startd + temp_pos_key_dt):(temp_startd + temp_dur - 1 + temp_pos_key_dt))] <- temp_dur #event_subset[[idx,'duration']]
    E_3[temp_rnum_dt,(temp_startd + temp_dur + temp_pos_key_dt)] <- 0 # enforces a space between subsequent events
    
    setTxtProgressBar(progress_bar, idx)
  }
  
  browser()
  # trim unused rows
  ind <- E %>% select(-priority,-rnum) %>%
    apply( 1, function(x) all(is.na(x)))
  E <-E[ !ind, ]
  
  E_3_ind <- rowSums(is.na(E_3))==(ncol(E_3)-2)
  E_3 <- E_3[ !E_3_ind, ]
  
  E[E == 0] = NaN # remove spaces added to avoid events appearing connected
  E_3[E_3 == 0] <- NaN # remove spaces added to avoid events appearing connected
  
  ncols = ncol(E)-2
  
  E <- mutate(E, decade = as.factor(floor(year(start_period)/10)*10))
  E_3[,decade:=as.factor(floor(year(start_period)/10)*10)]
  
  plot_dat <- E %>% select(-priority) %>%
    pivot_longer(cols=1:ncols, names_to = "names", values_to = "duration")
  plot_dat$names<- str_remove(plot_dat$names,"V")
  plot_dat <- plot_dat %>% mutate(dt = as.integer(names)) %>%
    mutate(date = as.Date(start_period + dt - 1)) %>%
    mutate(rnum = rnum - n) %>%
    drop_na()
  
  E_3 <- E_3[,priority:=NULL] # drop priority column
  plot_dat_dt <- melt(E_3, measure.vars=colnames(E_3)[1:(ncol(E_3)-2)], # all but last 2 columns (rnum, decade)
                      variable.name="names",value.name = "duration")
  plot_dat_dt[,dt:=as.integer(str_remove(names,"V"))]
  plot_dat_dt[,date:=as.Date(start_period + dt - 1)]
  plot_dat_dt[,rnum:=rnum - n]
  plot_dat_dt <- na.omit(plot_dat_dt)
  
  plot_dat_df <- as.data.frame(plot_dat_dt)
  
  
  # return(plot_dat)
  return(plot_dat_df)
}

create_event_swarm_max_droughts <- function(event_data, start_period, end_period, max_droughts){
  event_subset <- event_data %>% 
    filter(start >= start_period) %>%
    filter(start <= end_period) %>% 
    mutate(onset_day = as.integer(start - start_period)+1) %>% 
    mutate(end_day = onset_day+duration-1) %>% #as.integer(end - start_period)
    arrange(onset_day)
  # event_subset_2 <- event_data %>% 
  #   filter(start >= start_period) %>%
  #   mutate(onset_day = as.integer(start - start_period)+1) %>% 
  #   mutate(end_day = onset_day+duration-1) %>% #as.integer(end - start_period)
  #   filter(end <= end_period) %>% 
  #   arrange(onset_day)
  # set up an empty "swarm grid" to place drought events into
  n <- round(max_droughts/2)+10 #600 # set arbitrarily large number of possible simultaneous drought events positions. Trimmed prior to plotting
  
  E <- as_tibble(matrix(NaN,nrow=n,ncol=max(event_subset$end_day)+1))
  E <- E %>% mutate(priority = 1:n)
  E <- E %>% arrange(desc(priority)) %>% 
    bind_rows(E) %>% 
    mutate(rnum = 1:(2*n))
  
  # loop through each event and place into best available spot in grid
  progress_bar <- txtProgressBar(min = 1, max = nrow(event_subset), style = 3)
  
  for (idx in 1:nrow(event_subset)){
    temp_dur <- event_subset[[idx,'duration']]
    temp_startd <-event_subset[[idx,'onset_day']]
    # find available spots looking within 1 day +/- the start date (to encourage a little compactness)
    avail_rows <- E %>% select(all_of(temp_startd:(temp_startd + temp_dur - 1)),priority,rnum) %>% 
      filter(is.na(if_all(starts_with("V")))) %>% 
      mutate(pos = 0)
    avail_rows_plus1d <- E %>% select(all_of((temp_startd+1):(temp_startd + temp_dur)),priority,rnum) %>%
      filter(is.na(if_all(starts_with("V")))) %>%
      mutate(pos = 1)
    avail_rows_minus1d <- E %>% select(all_of((temp_startd-1):(temp_startd + temp_dur - 2)),priority,rnum) %>%
      filter(is.na(if_all(starts_with("V")))) %>%
      mutate(pos = -1)
    # find spot closest to central axis
    all_avail_rows <- bind_rows(avail_rows, avail_rows_minus1d, avail_rows_plus1d) %>% 
      arrange(priority) %>%
      group_by(priority) %>%
      slice_sample(prop = 1) %>% # adds a little randomness by assigning to spot above or below central axis randomly
      ungroup()
    temp_rnum <- all_avail_rows[[1,'rnum']]
    temp_pos_key <- all_avail_rows[[1, 'pos']]
    if (temp_startd == 1){
      temp_pos_key <- 0
    }
    # assign event to identified spot by using duration value
    E[temp_rnum,((temp_startd + temp_pos_key):(temp_startd + temp_dur - 1 + temp_pos_key))] <- event_subset[[idx,'duration']]
    E[temp_rnum,(temp_startd + temp_dur + temp_pos_key)] <- 0 # enforces a space between subsequent events
    
    setTxtProgressBar(progress_bar, idx)
  }
  # trim unused rows
  ind <- E %>% select(-priority,-rnum) %>% 
    apply( 1, function(x) all(is.na(x)))
  E <-E[ !ind, ]
  
  E[E == 0] = NaN # remove spaces added to avoid events appearing connected
  
  ncols = ncol(E)-2
  
  E <- mutate(E, decade = as.factor(floor(year(start_period)/10)*10))
  
  plot_dat <- E %>% select(-priority) %>% 
    pivot_longer(cols=1:ncols, names_to = "names", values_to = "duration")
  plot_dat$names<- str_remove(plot_dat$names,"V")
  plot_dat <- plot_dat %>% mutate(dt = as.integer(names)) %>% 
    mutate(date = as.Date(start_period + dt - 1)) %>% 
    mutate(rnum = rnum - n) %>% 
    drop_na()
  
  return(plot_dat)
}

create_event_swarm_dt <- function(event_data, start_period, end_period, max_droughts){

  event_subset <- event_data %>% 
    filter(start >= start_period) %>%
    filter(start <= end_period) %>% 
    mutate(onset_day = as.integer(start - start_period)+1) %>% 
    mutate(end_day = onset_day + duration-1) %>%  # onset_day + duration as.integer(end - start_period)
    arrange(onset_day) %>%
    mutate(idx = row_number())
  
  # set up an empty "swarm grid" to place drought events into
  n <- round(max_droughts/2)+10 #600 # set arbitrarily large number of possible simultaneous drought events positions. Trimmed prior to plotting
  
  mat <- matrix(NaN,nrow=n,ncol=max(event_subset$end_day)+1)
  E_1 <- setDT(as.data.frame(mat))[]
  E_1[,priority:=1:n]
  E_2 <- E_1[order(-priority)]
  E_3 <- rbindlist(list(E_2, E_1))
  E_3[,rnum:=1:(2*n)]
  
  # loop through each event and place into best available spot in grid
  progress_bar <- txtProgressBar(min = 1, max = nrow(event_subset), style = 3)
  
  # store priority, rnum for all rows
  rnum_priority <- E_3[,.(priority,rnum)]
  
  #### alt options:
  ###### use purrr
  ###### use data.table
  for (idx in 1:nrow(event_subset)){
    temp_dur <- event_subset[[idx,'duration']]
    temp_startd <-event_subset[[idx,'onset_day']]
    
    dt_d <- E_3[,temp_startd:(temp_startd + temp_dur - 1)]
    dt_d <- cbind(dt_d, rnum_priority)
    dt_d <- dt_d[rowSums(is.na(dt_d))==(ncol(dt_d)-2),] # pull rows where all but priority and rnum are NA
    dt_d[,pos:=0]
    dt_plus1d <- E_3[,(temp_startd+1):(temp_startd + temp_dur)]
    dt_plus1d <- cbind(dt_plus1d, rnum_priority)
    dt_plus1d <- dt_plus1d[rowSums(is.na(dt_plus1d))==(ncol(dt_plus1d)-2),]
    dt_plus1d[,pos:=1]
    dt_minus1d <- E_3[,(temp_startd-1):(temp_startd + temp_dur - 2)]
    dt_minus1d <- cbind(dt_minus1d, rnum_priority)
    dt_minus1d <- dt_minus1d[rowSums(is.na(dt_minus1d))==(ncol(dt_minus1d)-2),]
    dt_minus1d[,pos:=-1]
    all_dt <- rbindlist(list(dt_d, dt_minus1d, dt_plus1d), fill=TRUE) # rbindlist(list(dt_d, dt_plus1d, dt_minus1d)) # DROPS columns that aren't shared
    all_dt <- all_dt[priority==min(priority)]
    all_dt <- all_dt[sample(.N,1)]
    
    temp_rnum_dt <- all_dt[,rnum]
    temp_pos_key_dt <- all_dt[,pos]
    if (temp_startd == 1){
      temp_pos_key_dt <- 0
    }
    
    # assign event to identified spot by using duration value
    E_3[temp_rnum_dt,((temp_startd + temp_pos_key_dt):(temp_startd + temp_dur - 1 + temp_pos_key_dt))] <- temp_dur
    E_3[temp_rnum_dt,(temp_startd + temp_dur + temp_pos_key_dt)] <- 0 # enforces a space between subsequent events
    
    setTxtProgressBar(progress_bar, idx)
  }
  
  # trim unused rows
  E_3_ind <- rowSums(is.na(E_3))==(ncol(E_3)-2) # identify rows where all but priority and rnum are NA
  E_3 <- E_3[ !E_3_ind, ]
  
  E_3[E_3 == 0] <- NaN # remove spaces added to avoid events appearing connected
  
  E_3[,decade:=as.factor(floor(year(start_period)/10)*10)]
  
  E_3 <- E_3[,priority:=NULL] # drop priority column
  plot_dat_dt <- melt(E_3, measure.vars=colnames(E_3)[1:(ncol(E_3)-2)], # all but last 2 columns (rnum, decade)
                      variable.name="names",value.name = "duration")
  plot_dat_dt[,dt:=as.integer(str_remove(names,"V"))]
  plot_dat_dt[,date:=as.Date(start_period + dt - 1)]
  plot_dat_dt[,rnum:=rnum - n]
  plot_dat_dt <- na.omit(plot_dat_dt)
  
  plot_dat_df <- as.data.frame(plot_dat_dt)
  
  return(plot_dat_df)
}

count_events <- function(event_data, metadata, target_threshold){
  event_counts_per_decade <- event_data %>% 
    left_join(metadata, by = "StaID", suffix = c("",".gages")) %>% 
    filter(HUC02 == 14) %>% # restrict to upper Colorado river basin
    filter(threshold == target_threshold) %>% 
    mutate(decade = as.factor(floor(year(start)/10)*10)) %>% 
    group_by(decade) %>% 
    summarize(num_events = n())
  
  return(event_counts_per_decade)
  
}

longest_events <- function(event_data, metadata, target_threshold){
  longest_event_per_decade <- event_data %>% 
    left_join(metadata, by = "StaID", suffix = c("",".gages")) %>% 
    filter(HUC02 == 14) %>% # restrict to upper Colorado river basin
    filter(threshold == target_threshold) %>% 
    mutate(decade = as.factor(floor(year(start)/10)*10)) %>% 
    group_by(decade) %>% 
    slice_max(duration, n = 1)
  
  return(longest_event_per_decade)
  
}



expand_drought_prop <- function(drought_prop) {
  drought_prop %>% 
    uncount(duration, .remove=FALSE, .id="id") %>%
    mutate(date = start+id-1, .after=end)
}  


identify_drought_chunks <- function(drought_prop_expanded, min_chunk_days) {
  # pull unique drought dates and get count of droughts for each date
  drought_dates <- drought_prop_expanded %>%
    count(date, name='n_droughts')
  
  # get full sequence of dates
  full_date_sequence <- seq.Date(from=min(drought_dates$date),to=max(drought_dates$date),by=1)
  
  # find dates that didn't have droughts
  dates_w_o_drought <- full_date_sequence[!(full_date_sequence %in% drought_dates$date)]
  
  # identify chunks if used all dates w/o drought as breaks
  date_chunks <- tibble(break_date = c(dates_w_o_drought, max(full_date_sequence))) %>%
    mutate(chunk_num = row_number(),
           start_date = case_when(
             chunk_num==1 ~ min(full_date_sequence),
             TRUE ~ lag(break_date)+1),
           chunk_length_days = as.numeric(break_date-start_date))
  # subset to larger chunks only
  selected_chunks <- date_chunks %>%
    filter(chunk_length_days>=min_chunk_days | break_date==max(full_date_sequence)) %>%
    select(break_date) %>%
    # recalculate break_num, start date, and break length
    mutate(chunk_num = row_number(),
           start_date = case_when(
             chunk_num==1 ~ min(full_date_sequence),
             TRUE ~ lag(break_date)+1
           ),
           chunk_length_days = as.numeric(break_date-start_date),
           chunk_length_year = chunk_length_days/365) %>%
    select(chunk_num, start_date, break_date, chunk_length_days, chunk_length_year) %>%
    # figure out the max # of droughts on a single day within each chunk
    group_by(chunk_num) %>%
    group_modify( ~ {
      drought_dates_subset <- drought_dates %>%
        filter(date >= .x$start_date, date<=.x$break_date)
      .x <- mutate(.x, 
                   max_single_day_droughts = max(drought_dates_subset$n_droughts),
                   total_drought_days = sum(drought_dates_subset$n_droughts),
                   n_days_w_droughts = nrow(drought_dates_subset))
    })
}


event_swarm_plot <- function(swarm_data){
  
  max_dur <- max(swarm_data$duration)
  max_rnum <- max(swarm_data$rnum)
  
  hbreaks <- BAMMtools::getJenksBreaks(swarm_data$duration, k=10)
  scaledBreaks <- scales::rescale(c(0,hbreaks), c(0,1))
  
  # add font
  font_fam <- 'Noto Sans Display'
  font_add_google(font_fam, regular.wt = 300, bold.wt = 700) 
  showtext_opts(dpi = 300)
  showtext_auto(enable = TRUE)
  
  p <- swarm_data %>% 
    ggplot()+
    #geom_hline(yintercept=0, color="#dddddd",size = 1)+
    geom_tile(aes(x=date, y=rnum, fill = duration), height=0.5)+
    scale_fill_scico(values = scaledBreaks, palette = "lajolla", begin = 0.25, end = 1 , direction = 1,
                     guide_legend(title = "Drought Duration (Days)"), breaks = c(5, 100, 200, 300))+
    theme_minimal()+
    ylab(element_blank())+
    xlab(element_blank())+
    scale_x_date(breaks = scales::date_breaks(width = '1 years'),
                 labels = scales::label_date_short(),
                 expand = c(0,0)) +
    theme(axis.text.y=element_blank(),
          axis.text.x=element_text(size = 14, family = font_fam, hjust = 0),
          panel.grid = element_blank(),
          axis.line.x = element_line(color = "black"),
          legend.position = 'none')
  
}


##### early violin plots #####

library(tidyverse)
library(ggplot2)
library(targets)
tar_load(p2_1951_2020_drought_summary_jd_7d)
tar_load(p2_1951_2020_metadata)

drought_summary_subset <- p2_1951_2020_drought_summary_jd_7d %>%
  left_join(p2_1951_2020_metadata) %>%
  filter(STATE %in% c('ME','MO','FL','CO','CA', 'ID','NE','VA','MI'))

violin_plot <- ggplot(drought_summary_subset) +
  geom_violin(data = filter(drought_summary_subset, threshold==30), aes(x=ave_duration, y=number_of_droughts, fill=threshold), trim=FALSE) +
  geom_violin(data = filter(drought_summary_subset, threshold==20), aes(x=ave_duration, y=number_of_droughts, fill=threshold), trim=FALSE) +
  geom_violin(data = filter(drought_summary_subset, threshold==10), aes(x=ave_duration, y=number_of_droughts, fill=threshold), trim=FALSE) + 
  geom_violin(data = filter(drought_summary_subset, threshold==5), aes(x=ave_duration, y=number_of_droughts, fill=threshold), trim=FALSE) + 
  geom_violin(data = filter(drought_summary_subset, threshold==2), aes(x=ave_duration, y=number_of_droughts, fill=threshold), trim=FALSE) +
  facet_wrap(~STATE, scales="free")

violin_plot <- ggplot(drought_summary_subset) +
  geom_violin(data = filter(drought_summary_subset, threshold==30), aes(x=number_of_droughts, y=ave_duration, fill=threshold), trim=FALSE) +
  geom_violin(data = filter(drought_summary_subset, threshold==20), aes(x=number_of_droughts, y=ave_duration, fill=threshold), trim=FALSE) +
  geom_violin(data = filter(drought_summary_subset, threshold==10), aes(x=number_of_droughts, y=ave_duration, fill=threshold), trim=FALSE) + 
  geom_violin(data = filter(drought_summary_subset, threshold==5), aes(x=number_of_droughts, y=ave_duration, fill=threshold), trim=FALSE) + 
  geom_violin(data = filter(drought_summary_subset, threshold==2), aes(x=number_of_droughts, y=ave_duration, fill=threshold), trim=FALSE) +
  facet_wrap(~STATE, scales="free")

violin_plot <- ggplot(drought_summary_subset) +
  geom_violin(data = filter(drought_summary_subset, threshold==30), aes(x=mean_severity, y=number_of_droughts, fill=threshold), trim=FALSE) +
  geom_violin(data = filter(drought_summary_subset, threshold==20), aes(x=mean_severity, y=number_of_droughts, fill=threshold), trim=FALSE) +
  geom_violin(data = filter(drought_summary_subset, threshold==10), aes(x=mean_severity, y=number_of_droughts, fill=threshold), trim=FALSE) + 
  geom_violin(data = filter(drought_summary_subset, threshold==5), aes(x=mean_severity, y=number_of_droughts, fill=threshold), trim=FALSE) + 
  geom_violin(data = filter(drought_summary_subset, threshold==2), aes(x=mean_severity, y=number_of_droughts, fill=threshold), trim=FALSE) +
  facet_wrap(~STATE, scales="free")



ggplot(drought_summary_subset) +
  geom_scatter()