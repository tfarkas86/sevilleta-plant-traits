library(readr)
library(dplyr)

repo_home = "~/Dropbox/1_Work/1_Research/Whitney-Rudgers Lab/Sev/Plants/sevilleta-plant-traits/"

all_traits <- read_csv(paste0(repo_home, "data/interim/all_traits_pic_prep.csv"))

spp_tree <- read.tree(paste0(repo_home, "data/raw/sev_tree_101spp.tre"))

source(paste0(repo_home, "src/features/pic_fxns.R"))

# manually chosen transformations
# see reports/figures/branch_length_diagnostic plots
trait_trans <- c(
           a_p=3, area=3, 
           avg.height_A=3, avg.height_B=3, avg.height_C=2, 
           avg.height_F=2, avg.height_G=28, avg.height_P=2, 
           max.height_A=3, max.height_B=3, max.height_C=2, 
           max.height_F=2, max.height_G=3, max.height_P=2,
           p95.height_A=2, p95.height_B=2, p95.height_C=2,
           p95.height_F=2, p95.height_G=4, p95.height_P=2,
           cn=2, d13C=3, d15N=2, pN=2, pC = 2, 
           fr.dens= 3, fr.diam=3, fr.dmc=3, fr.srl=3, 
           ldmc=2, lma=4, sla=2,
           rdens=2, rdmc=28, sdens=2, sdmc=2, seed_mass=3 )
response_trans <- c( 
           F_A_BM_acor=2, F_B_BM_acor=26, F_C_BM_acor=21, 
           F_F_BM_acor=28, F_G_BM_acor=3, F_P_BM_acor=27,
           F_A_BM_cv=2, F_B_BM_cv=2, F_C_BM_cv=27, 
           F_F_BM_cv=2, F_G_BM_cv=2, F_P_BM_cv=20 
#           F_A_BM_q1090=2, F_B_BM_q1090=29, F_C_BM_q1090=29, 
#           F_F_BM_q1090=2, F_G_BM_q1090=2, F_P_BM_q1090=2
           )

trans <- c(trait_trans, response_trans)

# associate correct transformation with choice from 
# output figures
#names <- names(all_traits)[15:ncol(all_traits)]
#names(trans) <- names[order(names)]

trans_options <- names(brl_transform(x=all_traits, trait="a_p",
                                     spp_tree, 
                                     taxa = "taxon.new"))
trans_options <- trans_options[order(trans_options)]

# perform all transformations for traits of interest, then select
# manually chosen option and combine into one long list
all_pics_list <- setNames(lapply(names(trans), function(t) {
  
  all_trans <- brl_transform(x=all_traits, trait=t,
                             spp_tree, 
                             taxa = "taxon.new")
  all_trans[[trans_options[trans[t]]]]
  
}

), names(trans))

# join all traits together

all_pics_tib <- lapply(names(all_pics_list), function(t) {
  
  all_pics_list[[t]] %>%
    select(node, 
           !!t:= pic)
}) %>%
  reduce(full_join, by="node")

write_csv(all_pics_tib, paste0(repo_home, "data/processed/all_pics_joined.csv"))

