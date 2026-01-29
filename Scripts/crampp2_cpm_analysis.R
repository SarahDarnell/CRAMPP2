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

#saving file
write.csv(nfr_cpm_groups, "Edited data files/nfr_cpm_groups.csv")

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

#compute avg of warm and cold stims, only for those with at least 6 ratings
nfr_cpm_groups <- nfr_cpm_groups %>%
  rowwise() %>%
  mutate(warm_count = sum(!is.na(c_across(warm_1:warm_9)))) %>%
  mutate(warm_avg = case_when(
    warm_count > 5 ~ mean(c_across(warm_1:warm_9), na.rm = TRUE)
  )) %>%
  mutate(cold_count = sum(!is.na(c_across(cold_1:cold_9)))) %>%
  mutate(cold_avg = case_when(
    cold_count > 5 ~ mean(c_across(cold_1:cold_9), na.rm = TRUE) 
  )) %>%
  ungroup()

nfr_cpm_groups <- nfr_cpm_groups %>%
  filter(
    !is.na(warm_avg),
    !is.na(cold_avg)
  )

long_df <- nfr_cpm_groups %>%
  select(record_id, group_arm2, warm_avg, cold_avg) %>%
  pivot_longer(
    cols = c(warm_avg, cold_avg),
    names_to = "stim",
    values_to = "pain_rating"
  ) %>% 
  mutate(stim = factor(stim, levels = c("warm_avg", "cold_avg"))) 

n_long_df <- long_df %>%
  distinct(record_id, group_arm2) %>%
  count(group_arm2, name = "n")

cpm <- ggplot(long_df, aes(x = stim, y = pain_rating, group = record_id)) +
  geom_line(alpha = 0.3, color = "darkgray") +
  geom_point(size = 2, alpha = 0.5, color = "darkgray") +
  geom_boxplot(
    aes(group = stim),
    width = 0.4,
    alpha = 0.6,
    fill = "skyblue",
    outlier.shape = NA  
  ) +
  geom_text(
    data = n_long_df,
    aes(
      x = 1.5,
      y = 2,
      label = paste0("n = ", n)
    ),
    inherit.aes = FALSE,
    size = 3
  ) +
  scale_y_continuous(limits = c(0, 70), breaks = seq(0, 70, 10)) +
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


###########
## Stats ##
###########

#NOTE: Ensure using the version that has cold_avg and warm_avg calculated 
#so we have proper filters in place

#uncomment to save output
sink("Logs/log.txt")

#calculate median warm and cold ratings for each participant
nfr_cpm_groups <- nfr_cpm_groups %>%
  rowwise() %>%
  mutate(warm_median = median(c_across(warm_1:warm_9), na.rm = TRUE)) %>%
  mutate(cold_median = median(c_across(cold_1:cold_9), na.rm = TRUE)) %>%
#calculate cpm change
  mutate(cpm_change = warm_median - cold_median) %>%
  ungroup()

#table with cpm change medians stratified by group
cpm_change_medians <- nfr_cpm_groups %>%
  select(cpm_change, group_arm2) %>%
  pivot_longer(cols = -group_arm2, names_to = "Item", values_to = "Value") %>% 
  group_by(group_arm2, Item) %>%
  dplyr::summarize(`Median [IQR]` = sprintf("%.1f [%.1f-%.1f], n=%d", 
                                            median(Value, na.rm = TRUE), 
                                            quantile(Value, 0.25, na.rm = TRUE),
                                            quantile(Value, 0.75, na.rm = TRUE),
                                            sum(!is.na(Value))),
                   .groups = "drop") %>%
  pivot_wider(names_from = group_arm2, values_from = `Median [IQR]`) 


#plot of cpm change by group
cpm_change_plot <- ggplot(nfr_cpm_groups, aes(x = group_arm2, y = cpm_change, group = group_arm2)) +
  geom_line(alpha = 0.3, color = "darkgray") +
  geom_point(size = 2, alpha = 0.5, color = "darkgray") +
  geom_boxplot(
    aes(group = group_arm2),
    width = 0.4,
    alpha = 0.6,
    fill = "skyblue",
    outlier.shape = NA  
  )+
  labs(
    title = "CPM change across groups",
    x = "",
    y = "cpm_change"
  ) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) 

#one-way anova comparing cpm change dys vs dysb
anova_dys_dysb <- aov(cpm_change ~ group_arm2, data = nfr_cpm_groups)
summary(anova_dys_dysb)

#kruskal wallis comparing cpm change dys vs dysb
kruskal.test(cpm_change ~ group_arm2, data = nfr_cpm_groups)

#table with cpm change medians stratified combined groups
nfr_cpm_groups <- nfr_cpm_groups %>%
  mutate(combined_groups = case_when(
    group_arm2 == "DYS" ~ "DYS+DYSB", 
    group_arm2 == "DYSB" ~ "DYS+DYSB",
    group_arm2 == "C" ~ "C"
  ))

cpm_change_medians_combined <- nfr_cpm_groups %>%
  select(cpm_change, combined_groups) %>%
  pivot_longer(cols = -combined_groups, names_to = "Item", values_to = "Value") %>% 
  group_by(combined_groups, Item) %>%
  dplyr::summarize(`Median [IQR]` = sprintf("%.1f [%.1f-%.1f], n=%d", 
                                            median(Value, na.rm = TRUE), 
                                            quantile(Value, 0.25, na.rm = TRUE),
                                            quantile(Value, 0.75, na.rm = TRUE),
                                            sum(!is.na(Value))),
                   .groups = "drop") %>%
  pivot_wider(names_from = combined_groups, values_from = `Median [IQR]`) 

#one-way anova comparing cpm change hc vs dys/b
anova_hc_dys_b <- aov(cpm_change ~ combined_groups, data = nfr_cpm_groups)
summary(anova_hc_dys_b)

#kruskal wallis comparing cpm change hc vs dys/b
kruskal.test(cpm_change ~ combined_groups, data = nfr_cpm_groups)

#one way anova comparing pain30 dys vs dysb
anova_dys_dysb_pain30 <- aov(pain30_thresh ~ group_arm2, data = nfr_cpm_groups)
summary(anova_dys_dysb_pain30)

#kruskal wallis comparing pain30 dys vs dysb
kruskal.test(pain30_thresh ~ group_arm2, data = nfr_cpm_groups)

#one way anova comparing pain30 dys vs dysb
anova_dys_dysb_nfr <- aov(nfr_thresh ~ group_arm2, data = nfr_cpm_groups)
summary(anova_dys_dysb_nfr)

#kruskal wallis comparing pain30 dys vs dysb
kruskal.test(nfr_thresh ~ group_arm2, data = nfr_cpm_groups)

#uncomment to stop logging
sink()


#saving plots
plots <- c("cpm_change_plot")


for (i in seq_along(plots)) {
  ggsave(
    filename = sprintf("Plots/%s.png", plots[i]),
    plot = get(plots[i]),
    width = 7, height = 5, dpi = 300
  )
}











