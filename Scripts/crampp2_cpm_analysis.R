#CRAMMP2 CPM Analysis
#Written by Sarah Darnell

#set working directory
setwd("~/Sarah work stuff/2025 Data Projects/CRAMPP2")

token <- Sys.getenv("CRAMPP2_REDCAP_TOKEN")

library(jsonlite)
library(dplyr)
library(lubridate)
library(ggdist)
library(ggplot2)
library(tidyr)

#pull NFR/CPM information from redcap
url <- "https://survey.northshore.org/api/"
formData <- list("token"=token,
                 content='report',
                 format='json',
                 report_id='4540',
                 csvDelimiter='',
                 rawOrLabel='raw',
                 rawOrLabelHeaders='raw',
                 exportCheckboxLabel='false',
                 returnFormat='json'
)
response <- httr::POST(url, body = formData, encode = "form")

#convert to dateframe
response_text <- httr::content(response, as = "text")
nfr_cpm <- fromJSON(response_text, flatten = TRUE)

#pull group information from redcap
url <- "https://survey.northshore.org/api/"
formData <- list("token"=token,
                 content='report',
                 format='json',
                 report_id='4422',
                 csvDelimiter='',
                 rawOrLabel='raw',
                 rawOrLabelHeaders='raw',
                 exportCheckboxLabel='false',
                 returnFormat='json'
)
response <- httr::POST(url, body = formData, encode = "form")

#convert to dateframe
response_text <- httr::content(response, as = "text")
groups <- fromJSON(response_text, flatten = TRUE)

#merge group information into nfr_cpm df
nfr_cpm_groups <- nfr_cpm %>%
  left_join(groups %>% select(record_id, group_arm2), by = "record_id")

#make group a categorical and factor var
nfr_cpm_groups <- nfr_cpm_groups %>%
  mutate(group_arm2 = case_when(
    group_arm2 == "1" ~ "DYS", 
    group_arm2 == "2" ~ "C", 
    group_arm2 == "3" ~ "DYSB")) %>%
  mutate(group_arm2 = as.factor(group_arm2))


#fixing columns to be numeric
numeric_cols <- c("record_id", "leg", "nfr_found_yn", "nfr_thresh", 
                 "nfr_not_found", "pain30_found_yn", "pain30_thresh", 
                 "pain30_not_found", "cpm_thresh", "warm_temp", "warm_1", 
                 "warm_2", "warm_3", "warm_4", "warm_5", "warm_6", "warm_7", 
                 "warm_8", "warm_9", "cold_temp", "cold_1", "cold_2", "cold_3", 
                 "cold_4", "cold_5", "cold_6", "cold_7", "cold_8", "cold_9")

nfr_cpm_groups <- nfr_cpm_groups %>%
  mutate(across(all_of(numeric_cols), as.numeric))

#removing those who didn't find Pain30
nfr_cpm_groups <- nfr_cpm_groups %>%
  filter(pain30_found_yn == 1)

#uncomment to view group #s
#summary(nfr_cpm_groups$group_arm2)
#C   DYS  DYSB 
#25   83   41

#####################
## Raincloud plots ##
#####################

#pain30 threshold
raincloud_pain30 <- ggplot(nfr_cpm_groups, aes(x = group_arm2, y = pain30_thresh, fill = group_arm2)) +
  stat_halfeye(
    adjust = 1,
    width = 0.6,
    justification = -0.3,
    point_colour = NA
  ) +
  geom_boxplot(
    width = 0.15,
    outlier.shape = NA,
    alpha = 0.4
  ) +
  geom_jitter(
    width = 0.08,
    alpha = 0.5,
    size = 1
  ) +
  stat_summary(
    fun.data = function(y) {
      data.frame(y = Inf, label = paste0("n = ", sum(!is.na(y))))
    },
    geom = "text",
    hjust = 1
  )  +
  coord_cartesian(clip = "off") +
  theme_classic() +
  labs(
    title = "Pain 30",   
    x = "",         
    y = "mA") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

#NFR threshold
raincloud_nfr <- ggplot(nfr_cpm_groups, aes(x = group_arm2, y = nfr_thresh, fill = group_arm2)) +
  stat_halfeye(
    adjust = 1,
    width = 0.6,
    justification = -0.3,
    point_colour = NA
  ) +
  geom_boxplot(
    width = 0.15,
    outlier.shape = NA,
    alpha = 0.4
  ) +
  geom_jitter(
    width = 0.08,
    alpha = 0.5,
    size = 1
  ) +
  stat_summary(
    fun.data = function(y) {
      data.frame(y = Inf, label = paste0("n = ", sum(!is.na(y))))
    },
    geom = "text",
    hjust = 1
  )  +
  coord_cartesian(clip = "off") +
  theme_classic() +
  labs(
    title = "NFR",   
    x = "",         
    y = "mA") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

#CPM threshold
raincloud_cpm <- ggplot(nfr_cpm_groups, aes(x = group_arm2, y = cpm_thresh, fill = group_arm2)) +
  stat_halfeye(
    adjust = 1,
    width = 0.6,
    justification = -0.3,
    point_colour = NA
  ) +
  geom_boxplot(
    width = 0.15,
    outlier.shape = NA,
    alpha = 0.4
  ) +
  geom_jitter(
    width = 0.08,
    alpha = 0.5,
    size = 1
  ) +
  stat_summary(
    fun.data = function(y) {
      data.frame(y = Inf, label = paste0("n = ", sum(!is.na(y))))
    },
    geom = "text",
    hjust = 1
  )  +
  coord_cartesian(clip = "off") +
  theme_classic() +
  labs(
    title = "CPM",   
    x = "",         
    y = "mA") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

################
## Line plots ##
################

#warm water
#reshape to long format, compute avg of warm water ratings
warm_long <- nfr_cpm_groups %>%
  pivot_longer(
    cols = warm_1:warm_9,   # replace with your 9 variable names
    names_to = "measure",
    values_to = "value"
  ) %>%
  group_by(measure) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    n = n()
  )

warm_ratings <- ggplot(warm_long, aes(x = measure, y = mean_value, group = 1)) +
  geom_line() +
  geom_point(size = 3) +
  scale_y_continuous(limits = c(0, 40))  +
  theme_classic() +
  labs(
    title = "Stims",   
    x = "",         
    y = "Pain Rating") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

#cold water
#reshape to long format, compute avg of warm water ratings
cold_long <- nfr_cpm_groups %>%
  pivot_longer(
    cols = cold_1:cold_9,   # replace with your 9 variable names
    names_to = "measure",
    values_to = "value"
  ) %>%
  group_by(measure) %>%
  summarise(
    mean_value = mean(value, na.rm = TRUE),
    n = n()
  )

cold_ratings <- ggplot(cold_long, aes(x = measure, y = mean_value, group = 1)) +
  geom_line() +
  geom_point(size = 3) +
  scale_y_continuous(limits = c(0, 40))  +
  theme_classic() +
  labs(
    title = "Stims",   
    x = "",         
    y = "Pain Rating") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

#combined plot

warm_long <- warm_long %>%
  mutate(measure_num = 1:9, stim = "Warm")

cold_long <- cold_long %>%
  mutate(measure_num = 1:9, stim = "Cold")

combined_long <- bind_rows(warm_long, cold_long)

  
warm_vs_cold <- ggplot(combined_long, aes(x = measure_num, y = mean_value, group = stim, color = stim)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 1:9, labels = 1:9) +
  scale_y_continuous(limits = c(0, 40)) +
  theme_classic() +
  labs(
    title = "Warm vs Cold",
    x = "Stimulations",
    y = "Pain Rating",
    color = "Stim Type"
  ) +
  theme(plot.title = element_text(hjust = 0.5))


################
## CPM plots ##
################

#compute avg of warm and cold stims
nfr_cpm_groups <- nfr_cpm_groups %>%
  rowwise() %>%
  mutate(
    warm_avg = mean(c_across(warm_1:warm_9))
  ) %>%
  mutate(
    cold_avg = mean(c_across(cold_1:cold_9))
  ) %>%
  ungroup()

long_df <- nfr_cpm_groups %>%
  select(record_id, group_arm2, warm_avg, cold_avg) %>%
  pivot_longer(
    cols = c(warm_avg, cold_avg),
    names_to = "stim",
    values_to = "pain_rating"
  ) %>% 
  mutate(stim = factor(stim, levels = c("warm_avg", "cold_avg")))

cpm <- ggplot(long_df, aes(x = stim, y = pain_rating, group = record_id)) +
  # individual paired lines
  geom_line(alpha = 0.3, color = "darkgray") +
  geom_point(size = 2, alpha = 0.5, color = "darkgray") +
  # summary boxplot per stim
  geom_boxplot(
    aes(group = stim),
    width = 0.4,
    alpha = 0.6,
    fill = "skyblue",
    outlier.shape = NA  # hide outliers since individual points are shown
  ) +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) +
  labs(
    title = "Avg Warm and Cold Ratings",
    x = "",
    y = "Pain Rating"
  ) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~ group_arm2, strip.position = "bottom")


#saving plots
plots <- c("raincloud_pain30", "raincloud_nfr", "raincloud_cpm",
           "warm_ratings", "cold_ratings", "warm_vs_cold", "cpm")


for (i in seq_along(plots)) {
  ggsave(
    filename = sprintf("Plots/%s.png", plots[i]),
    plot = get(plots[i]),
    width = 7, height = 5, dpi = 300
  )
}





















