library(readr)
library(dplyr)

repo_home = "~/Dropbox/1_Work/1_Research/Whitney-Rudgers Lab/Sev/Plants/sevilleta-plant-traits/"

# import and prep raw trait data
raw_traits <- read_csv(paste0(repo_home, "data/processed/sev_all_traits_raw.csv")) %>%
  
  # just not sure about this ID, plust not much data
  filter(! kartez %in% c("DAJA?")) %>% 
  
  # lump big Aristida under ARIST
  mutate_at(vars(kartez), funs(ifelse(. == "ARPU9", "ARIST", .))) %>% 
  
  # lump Sporobolus 
  mutate_at(vars(kartez), funs(ifelse(. %in% c("SPCO4", "SPFL2"), "SPORO", .))) %>% 
  
  # lump Astragalus
  mutate_at(vars(kartez), funs(ifelse(. %in% c("ASFE2", "ASNU4"), "ASTRA", .))) %>% 
  
  # numeric annual vs. perennial
  mutate_at(vars(a_p), funs(ifelse(. == "a", 0, 1))) %>% 
  
  # group by plant species
  group_by(kartez) 

#load("~/Dropbox/1_Work/1_Research/Whitney-Rudgers Lab/Sev/Plants/sev_traits_cv_manuscript/Data/raw_bounce_wide_q1090m.RData")
vars_wide <- read_csv(paste0(repo_home, "data/processed/reponse_vars_spp_raw.csv"))

all_traits <- 
  # now there are duplicate kartez codes, so summarize
  inner_join(raw_traits %>% summarize_at(vars(family:apgf, -a_p), funs(max)),
             raw_traits %>% summarize_at(vars(a_p, avg.height_A:seed_mass), funs(mean)), 
             by = "kartez") %>% 
  # remove outliers
  mutate_at(vars(lma), funs(ifelse(. > 0.1, NA, .))) %>%
  mutate_at(vars(p95.height_A), funs(ifelse(. > 100, NA, .))) %>%
  mutate_at(vars(sdens), funs(ifelse(. > 0.003, NA, .))) %>%
  mutate_at(vars(rdens), funs(ifelse(. > 0.003, NA, .))) %>%
  mutate_at(vars(fr.diam), funs(ifelse(. > 0.15, NA, .))) %>%
  
  # join in cv values
  inner_join(vars_wide)
