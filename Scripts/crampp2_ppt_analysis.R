#CRAMMP2 PPT Analysis
#Written by Sarah Darnell

#set working directory
setwd("~/Sarah work stuff/2025 Data Projects/CRAMPP2")

token <- Sys.getenv("CRAMPP2_REDCAP_TOKEN")

library(jsonlite)
library(dplyr)
library(lubridate)
library(readxl)
library(tidyr)

#import baseline ppt data
ppt_long <- read_excel("Raw data files/EH22-236_PPT_baseline_rawdata.xlsx", 
                       sheet = "Cleaned PPT")

#pivot wider
ppt_wide <- ppt_long %>%
  pivot_wider(
    id_cols = c(subid_arm2), 
    names_from = trial_number, 
    values_from = c(strain, rate, duration)
  )
  
#import group info
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
response_text <- httr::content(response, as = "text")
groups <- fromJSON(response_text, flatten = TRUE)

#convert characters to numerics
numeric_cols <- c("record_id", "subid_arm1", "subid_arm2", "group_arm2")

groups <- groups %>%
  mutate(across(all_of(numeric_cols), as.numeric))

#merge group info into PPT df
ppt_wide <- left_join(
  ppt_wide,
  groups %>% select(subid_arm2, group_arm2, record_id),
  by = "subid_arm2"
)
  
#rename groups
ppt_wide <- ppt_wide %>%
  mutate(group_arm2 = case_match(
    group_arm2, 
    1 ~ "DYS",
    2 ~ "HC", 
    3 ~ "DYSB",
    4 ~ "CP"
  ))

  
#calculate mean of shoulder and knee strain trials per pt
ppt_wide <- ppt_wide %>%
  rowwise %>%
  mutate(knee_avg = mean(c(strain_1, strain_3))) %>%
  mutate(shoulder_avg = mean(c(strain_2, strain_4))) %>%
  ungroup()

#import bladder pain and mh23 info
formData <- list("token"=token,
                 content='report',
                 format='json',
                 report_id='4595',
                 csvDelimiter='',
                 rawOrLabel='raw',
                 rawOrLabelHeaders='raw',
                 exportCheckboxLabel='false',
                 returnFormat='json'
)
response <- httr::POST(url, body = formData, encode = "form")
response_text <- httr::content(response, as = "text")
pain <- fromJSON(response_text, flatten = TRUE)

#convert characters to numerics
numeric_cols <- c("record_id", "vbt_fu_pain", "mh23")

pain <- pain %>%
  mutate(across(all_of(numeric_cols), as.numeric))

#merge pain info into PPT df
ppt_wide <- left_join(
  ppt_wide,
  pain %>% select(record_id, vbt_fu_pain, mh23),
  by = "record_id"
)

#save file
write.csv(ppt_wide, "Edited data files/ppt_wide.csv")
 
#table with ppt strain avgs stratified by group
ppt_strain_medians <- ppt_wide %>%
  select(knee_avg, shoulder_avg, group_arm2) %>%
  pivot_longer(cols = -group_arm2, names_to = "Item", values_to = "Value") %>% 
  group_by(group_arm2, Item) %>%
  dplyr::summarize(`Median [IQR]` = sprintf("%.1f [%.1f-%.1f], n=%d", 
                                            median(Value, na.rm = TRUE), 
                                            quantile(Value, 0.25, na.rm = TRUE),
                                            quantile(Value, 0.75, na.rm = TRUE),
                                            sum(!is.na(Value))),
                   .groups = "drop") %>%
  pivot_wider(names_from = group_arm2, values_from = `Median [IQR]`) 

sink("Logs/log_ppt_group_diffs.txt")
print(ppt_strain_medians)

#group differences dys vs dysb
ppt_wide_filtered <- ppt_wide %>%
  filter(group_arm2 != "HC") %>%
  filter(group_arm2 != "CP") 

anova_knee <- aov(knee_avg ~ group_arm2, data = ppt_wide_filtered)
summary(anova_knee)

anova_shoulder <- aov(shoulder_avg ~ group_arm2, data = ppt_wide_filtered)
summary(anova_shoulder)

kruskal.test(knee_avg ~ group_arm2, data = ppt_wide_filtered)
  
kruskal.test(shoulder_avg ~ group_arm2, data = ppt_wide_filtered)  

#corr coefs for fu_pain and knee/shoulder avg strain
#spearman, vbt pain and knee
cor(ppt_wide$vbt_fu_pain, ppt_wide$knee_avg, method = "spearman", use = "complete.obs")
#spearman, vbt pain and shoulder
cor(ppt_wide$vbt_fu_pain, ppt_wide$shoulder_avg, method = "spearman", use = "complete.obs")
#pearson, vbt pain and knee
cor(ppt_wide$vbt_fu_pain, ppt_wide$knee_avg, method = "pearson", use = "complete.obs")
#pearson, vbt pain and shoulder
cor(ppt_wide$vbt_fu_pain, ppt_wide$shoulder_avg, method = "pearson", use = "complete.obs")

#corr coefs for mh23 and knee/shoulder avg strain
#spearman, mh23 and knee
cor(ppt_wide$mh23, ppt_wide$knee_avg, method = "spearman", use = "complete.obs")
#spearman, mh23 and shoulder
cor(ppt_wide$mh23, ppt_wide$shoulder_avg, method = "spearman", use = "complete.obs")
#pearson, mh23 and knee
cor(ppt_wide$mh23, ppt_wide$knee_avg, method = "pearson", use = "complete.obs")
#pearson, mh23 and shoulder
cor(ppt_wide$mh23, ppt_wide$shoulder_avg, method = "pearson", use = "complete.obs")

sink()



  
  
