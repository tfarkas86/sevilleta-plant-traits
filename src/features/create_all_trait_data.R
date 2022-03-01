library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(maditr)
library(purrr)
library(here)

sd <- read_excel(here("data", "raw", "sev_plant_traits_collected_samples_2017.xlsx"), 
                 sheet=2) %>%
  mutate_at(vars(kartez), as.factor) 

#### Leaves ####
# load raw data
ad <- read_excel(here("data", "raw", "sev_leaf_area_2017-18.xlsx"))
md <- read_excel(here("data", "raw", "sev_leaf_masses_2017-18.xlsx"),
                 na="NA")

# merge datasets and make new variables
ld <- dcast(ad, formula= ... ~ ., value.var = "area", fun.aggregate = sum) %>%
  rename(area = ".") %>%
  left_join(md, "code") %>%
  left_join(sd[c("kartez", "sla_code")], by=c("code" = "sla_code")) %>%
  select(kartez, code, file, n_pieces:petiole) %>%
  as_tibble() %>%
  mutate(ldmc = dry / wet, sla= area / dry, lma= 1 / sla, 
         avg_area = if_else(leaves, area/n_pieces, as.double(NA))) %>%
  group_by(kartez)

ld_ag <- ld %>%
  summarise_at(vars(ldmc, sla, lma, area), mean, na.rm=TRUE) %>%
  mutate_at(vars(area), funs(ifelse(is.nan(.), NA, .)))

#### Stems and Root ####
srd.raw <- read_xlsx(here("data", "raw", "sev_stem_root_data_2017-18.xlsx"),
                     sheet=1,
                     na="NA")

# new variables
srd <- srd.raw %>%
  mutate(sdmc = stm_dry2/stm_wet) %>% # stem dry matter content
  mutate(rdmc = rt_dry2/rt_wet) %>% # taproot dry matter content
  mutate(stm_vol = pi * stm_len * ((stm_d1 / 2) ^ 2 + (stm_d1 / 2) * # stem volume
                                     (stm_d2 / 2) + (stm_d2/2) ^ 2) / 3) %>%
  mutate(sdens = stm_dry2/stm_vol) %>% # stem density
  mutate(rt_vol = pi * rt_len * ((rt_d1 / 2) ^ 2 + (rt_d1 / 2) * # taproot volume
                                   (rt_d2 / 2) + (rt_d2/2) ^ 2) / 3) %>%
  mutate(rdens = rt_dry2/rt_vol) %>% # taproot density
  mutate_at(vars(code), # remove preceding "p" from code
            .funs= function(x) substr(.$code, 
                                      2, 
                                      nchar(.$code))) %>%
  left_join(sd %>% select(kartez, rt_stm_code), # get spp. codes with merge
            by=c("code" = "rt_stm_code")) %>%
  filter(code != "325A" & (rdmc < 1 | is.na(rdmc))) %>% # sample with unknown spp. id & 2 exrtreme root densities
  mutate_at(vars(stm_wet:rdens), funs(if_else(is.nan(.), as.numeric(NA), .))) %>%
  group_by(kartez)

srd_ag <- srd %>%
  summarize_at(vars(sdmc, rdmc, sdens, rdens), mean, na.rm=TRUE)

#### Specific Root Length ####
rd.raw <- read_csv(here("data", "processed", "fine_root_shapes_2017-18.csv"))
rmd <- read_excel(here("data", "raw", "fine_root_mass.xlsx")) %>%
  mutate(frdmc = dry / wet) %>%
  mutate_at(vars(code), funs(as.character(.)))


rd <- rd.raw %>%
  mutate(across(code, ~ as.character(.x))) %>%
  left_join(sd[, c("kartez", "code")], by="code") %>%
  left_join(rmd, by="code") %>%
  select(code, kartez, len:frdmc) %>%
  rename(diam = "diam.95%") %>%
  mutate(srl = dry / len, 
         frdens = dry/vol) %>%
  droplevels() %>%
  group_by(kartez)

srl_ag <- rd %>% 
  summarize(fr.diam = mean(diam),
            fr.dmc = mean(frdmc), 
            fr.srl = mean(srl), 
            fr.dens = mean(frdens))

#### Isotopes ####
cnd <- read_excel(here("data", "raw", "sev_plant_isotopes_2017-18.xlsx")) %>%
  select(-(tray:tray_id)) %>%
  left_join(sd, c("id" = "sla_code")) %>%
  select(kartez, id, site, mass:cn) %>%
  group_by(kartez)

cnd_ag <- cnd %>%
  summarize_at(vars(d15N, d13C, pN, pC, cn), mean, na.rm=TRUE)

#### Seed Mass ####
sed <- read_excel(here("data", "raw", "sev_seed_masses.xlsx")) %>%
  mutate(seed_mass = mass/count) %>%
  filter(include == "T")

sed_ag <- sed %>%
  mutate(seed_mass = mass/count) %>%
  group_by(kartez) %>%
  summarize(count = sum(count), 
            mass = sum(mass),
            sd = sd(seed_mass),
            cv = sd/mean(seed_mass),
            samples = n()) %>%
  mutate(seed_mass = mass / count) %>%
  arrange(desc(seed_mass)) %>%
  select(kartez, seed_mass)

#### Height Data ####

# load quadrat data for flats and PJ

qdf <- read.csv(here("data", "raw", "sev129_nppcorequadrat_20170621.csv"))

qdp <- read.csv(here("data", "raw", "sev278_npppinjquadrat_20161214.csv")) %>%
  rename(web = plot, plot = transect)

nas <- c("-888", "-999", -888, -999, "NONE") # na values

# manipulate quadrat data for height calculations
qd <- rbind(qdf, qdp) %>%
  select(c("year", "season", "site", "species", "obs", "height", "count", "comment")) %>%
  rename(kartez = species) %>%
  filter(season == 3) %>%
  mutate_all(funs(replace(., . %in% nas, NA))) %>%
  mutate_at(vars(kartez), funs(replace(., . == " BREUC2", "BREUC2"))) %>%
  mutate_at(vars(kartez), funs(replace(., . == "ecfef3", "ECFEF3"))) %>%
  droplevels()

# get height data aggregates

hdI <- qd %>% # all sites separately 
  group_by(kartez, site) %>%
  filter(count > 0) %>%
  summarize(n_obs = sum(count, na.rm=TRUE),
            avg.height = sum(count * height, na.rm=TRUE)/sum(count, na.rm=TRUE),
            max.height = max(height, na.rm=TRUE), 
            p95.height = quantile(height, .95, na.rm=TRUE))

hdA <- qd %>% # all sites pooled
  group_by(kartez) %>%
  filter(count > 0) %>%
  summarize(n_obs = sum(count, na.rm=TRUE),
            avg.height = sum(count * height, na.rm=TRUE)/sum(count, na.rm=TRUE),
            max.height = max(height, na.rm=TRUE), 
            p95.height = quantile(height, .95, na.rm=TRUE)) %>%
  mutate(site = as.factor("A")) %>%
  select(1, site, 2:5)

hdF <- qd %>% # flats only
  filter(site != "P") %>%
  group_by(kartez) %>%
  filter(count > 0) %>%
  summarize(n_obs = sum(count, na.rm=TRUE),
            avg.height = sum(count * height, na.rm=TRUE)/sum(count, na.rm=TRUE),
            max.height = max(height, na.rm=TRUE), 
            p95.height = quantile(height, .95, na.rm=TRUE)) %>%
  mutate(site=as.factor("F")) %>%
  select(1, site, 2:5)

# combine heights from site sets and make wide format

hd_ag <- bind_rows(hdA, hdF, hdI) %>%
  mutate_at(vars(site), as.factor) %>%
  as.data.table() %>%
  dcast(form = kartez ~ site,
        value.var= c("avg.height", "max.height", "p95.height"),
        fill=NA) %>%
  as_tibble() 

#### Life History ####
allsplst <- read_csv(here("data", "raw", "sev_all_spp_list_augmented.csv")) %>%
  select(kartez, life_cycle, life_form, native, path, a_p, g_f, apgf) %>%
  mutate_at(vars(2:8), as.factor)

splst <- read_csv(here("data", "raw", "traits_spp_list.csv"))

lhd <- left_join(splst, allsplst, by="kartez") %>% 
  as_tibble() %>%
  mutate(species.old = substr(taxon, regexpr(pattern="_", text=taxon) + 1, nchar(taxon)),
         species.new = substr(new.taxon, regexpr(pattern="_", text=new.taxon) + 1, nchar(new.taxon)),
         genus.old = substr(taxon, 1, regexpr(pattern="_", text=taxon) - 1),
         genus.new = substr(new.taxon, 1, regexpr(pattern="_", text=new.taxon) - 1)) %>%
  droplevels() %>%
  select(kartez, family, genus.old, genus.new, species.old, species.new, 
         taxon.old=taxon, taxon.new=new.taxon, life_cycle:apgf)

#### Combine Trait Data ####
# all samples data
ad <- sd %>% ungroup() %>%
  left_join(select(ld %>% ungroup(), code, n_pieces:avg_area) , by=c("sla_code" = "code")) %>%
  left_join(select(srd %>% ungroup(), -kartez), by=c("rt_stm_code" = "code")) %>%
  left_join(select(rd %>% ungroup(), -kartez), by="code") %>%
  left_join(select(cnd %>% ungroup(), -kartez, -site), by=c("sla_code" = "id")) 

# data aggregated by species  

ad_ag <- reduce(list(lhd, hd_ag, ld_ag, cnd_ag, srd_ag, srl_ag, sed_ag), 
                left_join, 
                by="kartez") %>%
  mutate_at(vars(avg.height_A:seed_mass), round, digits=4)

#### Write Out ####
write.csv(ad_ag, file=here("data", "processed", "sev_all_traits_raw.csv"), 
          row.names=FALSE)
