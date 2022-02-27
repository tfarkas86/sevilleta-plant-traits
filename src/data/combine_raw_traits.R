library(readxl)
library(dplyr)

repo_home = "~/Dropbox/1_Work/1_Research/Whitney-Rudgers Lab/Sev/Plants/sevilleta-plant-traits/"

# sample identifiers
samp <- read_excel(paste0(repo_home, "data/raw/sev_plant_traits_collected_samples_2017.xlsx"), 
                   sheet=1) %>%
  mutate_at(vars(kartez), as.factor) %>%
  select(code, kartez, site)

#### Import / Prep New Trait Data #### 

# leaf area
lf_area <- read_excel(paste0(repo_home, "data/raw/sev_leaf_area_2017-18.xlsx"), sheet=1) %>% 
  group_by(code) %>%
  summarise(lf_sum_area=sum(area), 
            lf_n_pieces = max(n_pieces),
            lf_whole_ind = as.logical(max(leaves))) 

# leaf mass
lf_mass <- read_excel(paste0(repo_home, "data/raw/sev_leaf_masses_2017-18.xlsx"), sheet=1) %>%
  rename(lf_wet_mass = wet, 
         lf_dry_mass = dry,
         lf_petiole_ind = petiole)

# leaf isotopes
lf_iso <- read_excel(paste0(repo_home, "data/raw/sev_plant_isotopes_2017-18.xlsx"), sheet=1) %>%
  select(code = id, mass:cn)

# stem and root data
st_rt <- read_excel(paste0(repo_home, "data/raw/sev_stem_root_data_2017-18.xlsx"), sheet=1) %>%
  mutate_at(vars(code), funs(substr(., 2, nchar(.)))) # remove leading "p"

# fine root shapes
fr_shp <- read_excel(paste0(repo_home, "data/processed/fine_root_shapes_2017-18.xlsx")) %>%
  mutate_at(vars(code), funs(as.character(.)))

# fine root mass
fr_mass <- read_excel(paste0(repo_home, "data/raw/fine_root_mass.xlsx")) %>%
  mutate_at(vars(code), funs(as.character(.)))

# seed mass
seed_mass <- read_excel(paste0(repo_home, "data/raw/sev_seed_masses.xlsx")) %>% 
  # went through all photos and identified samples to exclude (immature, not seeds, etc.)
  filter(include=="T") %>% 
  select(code, seed_mass_sum=mass, seed_n=count)

#### Join Trait Data ####

indv_data <- samp %>%
  left_join(lf_area, by = "code") %>%
  left_join(lf_mass, by = "code") %>%
  left_join(lf_iso , by = "code") %>%
  left_join(st_rt  , by = "code") %>%
  left_join(fr_shp , by = "code") %>%
  left_join(fr_mass, by = "code") %>%
  left_join(seed_mass, by = "code") %>%
  write_csv(path = paste0(repo_home, "data/processed/indv_samp_traits_2017-18.csv"))