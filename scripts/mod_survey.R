# Load packages.
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# Load  survey data.
survey_df <- read_xlsx("results/20220118-Uncertainty and Misrepresentation in spatial data (1-53).xlsx")

# Initial clean.
survey_clean_df <- survey_df %>% 
  rename(hartlepool_original_1 = `1`,
         birmingham_dorling_2  = `2`,
         burnley_hex_3         = `3`,
         hartlepool_dorling_4  = `4`,
         burnley_original_5    = `5`,
         birmingham_hex_6      = `6`,
         hartlepool_hex_7      = `7`,
         burnley_dorling_8     = `8`,
         birmingham_original_9 = `9`) %>% 
  filter(ID != 6,      # likely a test
         ID != 13,     # useful but gave text answers
         ID != 19) %>% # useful but gave text answers
  mutate(across(2:10, as.numeric),
         across(2:10, round, 0)) %>% 
  pivot_longer(cols = -ID, names_to = "la_map_question", values_to = "estimate") %>% 
  mutate(la = if_else(str_detect(la_map_question, "hartlepool"), "hartlepool", la_map_question),
         la = if_else(str_detect(la_map_question, "birmingham"), "birmingham", la),
         la = if_else(str_detect(la_map_question, "burnley")   , "burnley"   , la)) %>% 
  filter(estimate != 0) # assume misunderstood or test. Important to discuss.

# Load in the correct answers.
residents_df <- read_csv("results/residents_imd_df.csv")

# Subset what we need and mimic other data frame for a rowbind.
residents_clean_df <- residents_df %>% 
  filter(IMD19rank_new5 == 1) %>% 
  select(LA11_name, prop_pop) %>% 
  rename(local_authority = LA11_name,
         estimate = prop_pop) 

residents_clean_df

birm_pop <- residents_clean_df$estimate[1]
burn_pop <- residents_clean_df$estimate[2]
hart_pop <- residents_clean_df$estimate[3]

# Create matchable data frame if needed.
pops_df <- tibble(la_map_question = unique(survey_clean_df$la_map_question)) %>% 
  mutate(estimate = if_else(str_detect(la_map_question, "birmingham"), birm_pop, 0),        # 0 is a temp filler.
         estimate = if_else(str_detect(la_map_question, "burnley")   , burn_pop, estimate),
         estimate = if_else(str_detect(la_map_question, "hartlepool"), hart_pop, estimate),
         ID = 999, # this just tells us it's the real one!
         la = if_else(str_detect(la_map_question, "hartlepool"), "hartlepool", la_map_question),
         la = if_else(str_detect(la_map_question, "birmingham"), "birmingham", la),
         la = if_else(str_detect(la_map_question, "burnley")   , "burnley"   , la)) %>% 
  select(ID, la_map_question, estimate, la)

# Bind rows.
survey_clean_pops_df <- bind_rows(survey_clean_df, pops_df)
  
# Plot.
  ggplot() +
    geom_density(data = filter(survey_clean_df, ID != 999), aes(x = estimate)) +
    geom_vline  (data = filter(survey_clean_df, la == "hartlepool"), aes(xintercept = hart_pop), col = "red"  , linetype = "dotted", size = 1) +
    geom_vline  (data = filter(survey_clean_df, la == "birmingham"), aes(xintercept = birm_pop), col = "blue" , linetype = "dotted", size = 1) +
    geom_vline  (data = filter(survey_clean_df, la == "burnley")   , aes(xintercept = burn_pop), col = "green", linetype = "dotted", size = 1) +
    facet_wrap(~la_map_question) +
    theme_minimal()
  
  
