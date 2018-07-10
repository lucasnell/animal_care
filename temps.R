suppressPackageStartupMessages({
    library(tidyverse)
    library(lubridate)
})

# fdf <- data_frame(
#     files = paste0(
#         "temp_data/2018-02-20/",
#         # "temp_data/2018-02-11/", 
#                    c("greenhouse_experiment", "greenhouse_plants", 
#                                    "greenhouse_south", "growing_room"), ".txt"),
#     loc = c("greenhouse - right room", "greenhouse - left, center", 
#             "greenhouse - left, back", "B1 growing room")) %>% 
#     mutate(loc = factor(loc),
#            temps = map(files, function(.x) {
#                read_tsv(.x, col_types = 'cddd', col_names = c('dt', 'temp', 'dew', 'rh'), 
#                         skip = 1) %>% 
#                    mutate(dt = mdy_hms(dt)) %>% 
#                    select(dt, temp, rh)
#            })) %>% 
#     unnest() %>% 
#     select(-files) %>% 
#     mutate(time = as.numeric(hour(dt)) + (as.numeric(minute(dt)) / 60)) %>% 
#     filter(dt > mdy_hms("02/16/18 15:30:00"))
# 
# source(".Rprofile")
# 
# 
# fdf %>% 
#     filter(loc %in% c("greenhouse - right room", "greenhouse - left, back")) %>%
#     mutate(loc = recode_factor(loc, `greenhouse - right room` = "B",
#                                `greenhouse - left, back` = "A")) %>% 
#     ggplot(aes(dt, temp, color = time)) +
#     theme_classic() +
#     theme(strip.background = element_blank(), 
#           strip.text = element_text(size=12)) +
#     geom_line() +
#     facet_wrap(~ loc, nrow = 2) + 
#     scale_y_continuous(expression("Temperature (" * degree * C * ")"),
#                        breaks = seq(18, 26, 4)
#                        ) +
#     xlab("Time") +
#     scale_color_continuous("Hour\nof day", low = 'gray90', high = 'black')
# 
# 
# fdf %>% 
#     group_by(loc) %>% 
#     summarize(hots = sum(temp > 30))
# 
# 
# fdf %>% 
#     filter(grepl("B1", loc)) %>% 
#     ggplot(aes(dt, temp,color = time)) +
#     theme_classic() +
#     geom_line() +
#     scale_color_continuous(low = 'gray90', high = 'black')



# -starts-2018-05-01 16/31/52-ends-2018-06-05 08/18/37
# -starts-2018-05-01 16/22/47-ends-2018-06-05 08/19/31


date <- "2018-07-10"

temp_df <- map_dfr(LETTERS[1:2],
                   ~ read_csv(sprintf("temp_data/%s/504%s.csv.gz", date, .x),
                              col_names = c("datetime", "temp", "humid"), skip = 1,
                              col_types = "Tdd") %>% 
                       mutate(room = paste0("504", .x))) %>% 
    select(-humid)

# first_time <- {temp_df %>% filter(room == "504A")}$datetime[1]

# temp_df <- temp_df %>% 
    # mutate(diff = map_dbl(datetime, ~ difftime(.x, first_time, units = "mins"))) %>% 
    # filter(diff >= 0, diff %% 30 == 0) %>% 
    # select(-diff, -humid)

daily_temps <- read_csv(sprintf("temp_data/%s/cliMATE.csv.gz", date),
                        skip = 9,
                        col_names = c("date", "max", "min", "mean", "X1"),
                        comment = "#", na = c("M", "NC"), col_types = "Ddddc") %>% 
    select(-X1) %>% 
    mutate_at(vars(max, min, mean), function(f) (f - 32) * 5/9) %>% 
    mutate(datetime = as.POSIXct(paste(date, " 12:00:00 CDT"))) %>% 
    rename(temp = mean) %>% 
    # gather("type", "temp", max:mean) %>% 
    # mutate(type = factor(type, levels = c("max", "mean", "min"))) %>% 
    select(-date)

source(".Rprofile")
temp_df %>% 
    add_row(datetime = as.POSIXct("2018-06-09 0:01:00 UTC"), temp = NA,
            room = c("504A", "504B")) %>% 
    mutate(room = factor(room)) %>% 
    ggplot(aes(datetime, temp, color = room)) +
    theme_classic() +
    geom_ribbon(data = daily_temps, aes(ymin = min, ymax = max), 
                color = NA, fill = "gray80", na.rm = TRUE) +
    geom_line(data = daily_temps, color = "gray50", na.rm = TRUE) +
    geom_hline(yintercept = c(18, 22), color = "black", linetype = 3) +
    geom_hline(yintercept = 30, linetype = 2, color = "black") +
    geom_segment(data = data_frame(room = c("504A", "504A", "504A", "504B"), 
                                   int = as.POSIXct(c("2018-05-02 1:00:00 UTC",
                                                      "2018-06-13 16:00:00 UTC",
                                                      "2018-06-26 16:00:00 UTC",
                                                      "2018-06-27 16:00:00 UTC")),
                                   temp = c(33, 12, 12, 12),
                                   temp_end = temp + 2),
               aes(x = int, xend = int, yend = temp_end), 
               linetype = 1, color = "black", 
               arrow = grid::arrow(length = unit(0.02, "npc"))) +
    geom_text(data = data_frame(room = "504A",
                                datetime = as.POSIXct("2018-05-02 12:00:00 UTC"),
                                temp = 34),
             hjust = 0, vjust = 0.5, label = "= added shading", color = "black") +
    geom_text(data = data_frame(room = "504A",
                                datetime = date %>% as.POSIXct() %>% `+`(60^2 * 36),
                                temp = c(30, 22, 18) + 0.5,
                                label = c("alert", "high", "low")),
              aes(label = label), 
              hjust = 0, vjust = 0, color = "black", fontface = "italic") +
    geom_text(data = data_frame(room = "504A",
                                datetime = as.POSIXct("2018-05-17 01:00:00 UTC"),
                                temp = 15),
              label = "daily avg.\ntemps",
              hjust = 0.5, vjust = 1, color = "gray50", fontface = "italic") +
    geom_line(na.rm = TRUE) +
    scale_color_brewer(palette = "Dark2", guide = FALSE) +
    facet_grid(room ~ .) +
    ylab("Temperature (ºC)") +
    scale_x_datetime("Time", date_breaks = "7 days", date_labels = "%d %b") +
    coord_cartesian(ylim = c(14 - 3, 31.5 + 3)) +
    NULL
