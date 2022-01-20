# Load packages.
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(janitor)
library(forcats)
library(stringr)
library(ggplot2)

# Load  survey data.
survey_df <- read_xlsx("results/Uncertainty_and_Misrepresentation_in_spatial_data_(1-70).xlsx")

# Initial clean.
survey_clean_df <- survey_df %>% 
  rename(hartlepool_original_1 = `Column`,
         birmingham_dorling_2  = `2`,
         burnley_hex_3         = `3`,
         hartlepool_dorling_4  = `4`,
         burnley_original_5    = `5`,
         birmingham_hex_6      = `Question`,
         hartlepool_hex_7      = `6`,
         burnley_dorling_8     = `7`,
         birmingham_original_9 = `8`) %>% 
  clean_names() %>% 
  rename(ID = id) %>%  # sorry, this stops me from having to edit things later.
  filter(ID != 5,      # sam test
         ID != 6,      # nat test
         ID != 13,     # useful but gave text answers
         ID != 19) %>% # useful but gave text answers
  select(-name, -email, -start_time, -completion_time) %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed("%")),
         across(2:10, as.numeric),
         across(2:10, round, 0)) %>% 
  drop_na(birmingham_original_9) %>% # sum(is.na(survey_clean_df)) confirms that this dumps ppl who missed questions.
  pivot_longer(cols = -ID, names_to = "la_map_question", values_to = "estimate") %>% 
  mutate(la = if_else(str_detect(la_map_question, "hartlepool"), "hartlepool", la_map_question),
         la = if_else(str_detect(la_map_question, "birmingham"), "birmingham", la),
         la = if_else(str_detect(la_map_question, "burnley")   , "burnley"   , la)) #%>% 
# filter(estimate != 0) # assume misunderstood or test. Important to discuss.

# How many complete resonses?
length(unique(survey_clean_df$ID)) # 64

# Load in the correct answers.
residents_df <- read_csv("results/residents_imd_df.csv")

# Subset what we need and mimic other data frame for a rowbind.
residents_clean_df <- residents_df %>% 
  filter(IMD19rank_new5 == 1) %>% 
  select(LA11_name, prop_pop) %>% 
  rename(local_authority = LA11_name,
         real_estimate = prop_pop) 

# residents_clean_df

birm_pop <- residents_clean_df$real_estimate[1]
burn_pop <- residents_clean_df$real_estimate[2]
hart_pop <- residents_clean_df$real_estimate[3]

# Create matchable data frame if needed.
pops_df <- tibble(la_map_question = unique(survey_clean_df$la_map_question)) %>% 
  mutate(real_estimate = if_else(str_detect(la_map_question, "birmingham"), birm_pop, 0),        # 0 is a temp filler.
         real_estimate = if_else(str_detect(la_map_question, "burnley")   , burn_pop, real_estimate),
         real_estimate = if_else(str_detect(la_map_question, "hartlepool"), hart_pop, real_estimate),
         ID = 999, # this just tells us it's the real one!
         la = if_else(str_detect(la_map_question, "hartlepool"), "hartlepool", la_map_question),
         la = if_else(str_detect(la_map_question, "birmingham"), "birmingham", la),
         la = if_else(str_detect(la_map_question, "burnley")   , "burnley"   , la)) %>% 
  select(ID, la_map_question, real_estimate, la)

# Check.
# max(survey_clean_df$ID) # No real one in there.

# Calculate mean estimate of the survey respondents.
pops_survey_df <- survey_clean_df %>% 
  group_by(la_map_question, la) %>% 
  summarise(estimate = mean(estimate)) %>% 
  ungroup() %>% 
  select(la_map_question, la, estimate) %>% 
  left_join(pops_df) %>% 
  select(-ID) %>% 
  mutate(diff_estimate   = estimate-real_estimate)

# Bind rows and reorder factor for the plot.
survey_clean_pops_df <- survey_clean_df %>% 
  rename(real_estimate = estimate) %>% # confusing tbh.
  bind_rows(pops_df) %>% 
  mutate(la_map_question = fct_relevel(la_map_question,
                                       "birmingham_original_9",
                                       "burnley_original_5",
                                       "hartlepool_original_1",
                                       "birmingham_hex_6",
                                       "burnley_hex_3",
                                       "hartlepool_hex_7",
                                       "birmingham_dorling_2",
                                       "burnley_dorling_8",
                                       "hartlepool_dorling_4"))

# How many are we going to plot? Take off one because it's the 'real' ID of 999!
# length(unique(survey_clean_pops_df$ID)) # 65-1=64

# Plot. Note we do remove the real estimate first.
ggplot() +
  geom_density(data = filter(survey_clean_pops_df, ID != 999), aes(x = real_estimate, fill = la, colour = la)) +
  geom_vline  (data = filter(survey_clean_pops_df, la == "hartlepool",  ID != 999), aes(xintercept = hart_pop), linetype = "dotted", colour = "black") +
  geom_vline  (data = filter(survey_clean_pops_df, la == "birmingham",  ID != 999), aes(xintercept = birm_pop), linetype = "dotted", colour = "black") +
  geom_vline  (data = filter(survey_clean_pops_df, la == "burnley"   ,  ID != 999), aes(xintercept = burn_pop), linetype = "dotted", colour = "black") +
  facet_wrap(~la_map_question, nrow = 3, scales = "fixed") +
  scale_fill_viridis_d() +
  scale_colour_viridis_d() +
  labs(x = "Distribution of respondent estimates. Dotted line represents reality", y = NULL) +
  theme_bw() +
  theme(legend.position = "none")

# Join with individual data.
ind_pops_diff_df <- survey_clean_pops_df %>% 
  rename(estimate = real_estimate) %>% # confusing again.
  left_join(pops_df, by = c("la_map_question", "la") ) %>% 
  mutate(ind_diff_estimate = estimate-real_estimate,
         la_map_question = fct_relevel(la_map_question,
                                       "birmingham_original_9",
                                       "burnley_original_5",
                                       "hartlepool_original_1",
                                       "birmingham_hex_6",
                                       "burnley_hex_3",
                                       "hartlepool_hex_7",
                                       "birmingham_dorling_2",
                                       "burnley_dorling_8",
                                       "hartlepool_dorling_4")) %>% 
  filter(ID.x != 999)

# max(ind_pops_diff_df$ID.x) # no 999
# length(unique(ind_pops_diff_df$ID.x)) # 64

# Plot estimated points.
ggplot() +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_jitter(data = ind_pops_diff_df, mapping = aes(x = ind_diff_estimate,
                                                     y = la_map_question,
                                                     fill = la),
              alpha = 0.5, height = 0.3, pch = 21)  +
  geom_errorbar(data = pops_survey_df,
                mapping = aes(x = diff_estimate, xmin = 0, ymin = la_map_question,
                              ymax = la_map_question),
                size = 10) +
  geom_segment(data = pops_survey_df,
               mapping = aes(x = 0, xend = diff_estimate,
                             yend = la_map_question, y = la_map_question),
               size = 1) +
  scale_x_continuous(limits = c(-60, 60)) +
  scale_fill_viridis_d() +
  labs(x = "Difference between respondent estimates and reality",
       y = NULL) +
  theme_bw() +
  theme(legend.position = "none") +
  annotate(geom = "text", x = 60, y = 2, label = "O r i g i n a l", angle = -90) +
  annotate(geom = "text", x = 60, y = 5, label = "H e x"     , angle = -90) +
  annotate(geom = "text", x = 60, y = 8, label = "D o r l i n g" , angle = -90)