library(targets)

tar_option_set(packages = c("retry",
                            "sbtools",
                            "secret",
                            "tidyverse",
                            "zip",
                            'ggplot2',
                            'lubridate',
                            'scales',
                            'ggforce',
                            'showtext',
                            'paletteer',
                            'BAMMtools',
                            'scico',
                            'scales',
                            'ggthemes',
                            'maps',
                            'mapdata',
                            'cowplot',
                            'mapproj',
                            'svglite'))

# Phase target makefiles
source("0_config.R")
source("1_fetch.R")
source("2_process.R")
source("3_visualize.R")

# Global options
focal_StaID = '03221000'
focal_year = 1963
focal_threshold = 10

# Combined list of target outputs
c(p0_targets, p1_targets, p2_targets, p3_targets)
