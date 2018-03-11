suppressPackageStartupMessages({
    library(tidyverse)
    library(lubridate)
})

fdf <- data_frame(
    files = paste0(
        "temp_data/2018-02-20/",
        # "temp_data/2018-02-11/", 
                   c("greenhouse_experiment", "greenhouse_plants", 
                                   "greenhouse_south", "growing_room"), ".txt"),
    loc = c("greenhouse - right room", "greenhouse - left, center", 
            "greenhouse - left, back", "B1 growing room")) %>% 
    mutate(loc = factor(loc),
           temps = map(files, function(.x) {
               read_tsv(.x, col_types = 'cddd', col_names = c('dt', 'temp', 'dew', 'rh'), 
                        skip = 1) %>% 
                   mutate(dt = mdy_hms(dt)) %>% 
                   select(dt, temp, rh)
           })) %>% 
    unnest() %>% 
    select(-files) %>% 
    mutate(time = as.numeric(hour(dt)) + (as.numeric(minute(dt)) / 60)) %>% 
    filter(dt > mdy_hms("02/16/18 15:30:00"))

source(".Rprofile")


fdf %>% 
    filter(loc %in% c("greenhouse - right room", "greenhouse - left, back")) %>%
    mutate(loc = recode_factor(loc, `greenhouse - right room` = "B",
                               `greenhouse - left, back` = "A")) %>% 
    ggplot(aes(dt, temp, color = time)) +
    theme_classic() +
    theme(strip.background = element_blank(), 
          strip.text = element_text(size=12)) +
    geom_line() +
    facet_wrap(~ loc, nrow = 2) + 
    scale_y_continuous(expression("Temperature (" * degree * C * ")"),
                       breaks = seq(18, 26, 4)
                       ) +
    xlab("Time") +
    scale_color_continuous("Hour\nof day", low = 'gray90', high = 'black')


fdf %>% 
    group_by(loc) %>% 
    summarize(hots = sum(temp > 30))


fdf %>% 
    filter(grepl("B1", loc)) %>% 
    ggplot(aes(dt, temp,color = time)) +
    theme_classic() +
    geom_line() +
    scale_color_continuous(low = 'gray90', high = 'black')
