library(tidyverse)
library(lubridate)

fdf <- data_frame(
    files = paste0("temp_data/", c("greenhouse_experiment", "greenhouse_plants", 
                                   "greenhouse_south", "growing_room"), ".txt"),
    loc = c("greenhouse - right room", "greenhouse - left, center", 
            "greenhouse - left, back", "B1 growing room")) %>% 
    mutate(loc = factor(loc),
           temps = map(files, function(.x) {
               read_tsv(.x, col_types = 'cddd', col_names = c('dt', 'temp', 'dew', 'rh'), 
                        skip = 1) %>% 
                   mutate(dt = mdy_hms(dt, tz = "America/Chicago")) %>% 
                   select(dt, temp, rh)
           })) %>% 
    unnest() %>% 
    select(-files) %>% 
    mutate(time = as.numeric(hour(dt)) + (as.numeric(minute(dt)) / 60))

source(".Rprofile")

fdf %>% 
    ggplot(aes(dt, temp, color = time)) +
    theme_classic() +
    geom_line() +
    facet_wrap(~ loc, nrow = 2) + 
    ylab(expression("Temperature (" * degree * C * ")")) +
    xlab("Time") +
    scale_color_continuous("Time\nof day", low = 'gray90', high = 'black')


fdf %>% 
    group_by(loc) %>% 
    summarize(hots = sum(temp > 30))


fdf %>% 
    filter(grepl("B1", loc)) %>% 
    ggplot(aes(dt, temp,color = time)) +
    theme_classic() +
    geom_line() +
    scale_color_continuous(low = 'gray90', high = 'black')
