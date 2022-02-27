# Funcitons to create phylogenetic contrasts, and evaluate branch length transformations. 

#### Single PIC Function ####
pics <- function(x, # a tibble or data frame with trait data
                 trait, # a character string of a single trait column name
                 phy, # a phylogenetic tree of class "phylo" & order "cladewise"
                 taxa, # a character string of the taxon column, to match tips in phy
                 scale=TRUE) { # logical indicator whether to mean center PIC output
  
  require(ape)
  require(dplyr)
  
  #### modify data ####
  
  dd <- x %>%
    as_tibble() %>%
    select(taxa, trait) %>%
    filter(complete.cases(.)) %>% # filter complete cases
    as.data.frame() # to data frame in case of tibble
  # get row names for ape::pic function
  taxon.names <- dd[, taxa]
  
  # get trait values data frame
  traits <- dd[, trait, drop=FALSE]
  
  # assign row names
  rownames(traits) <- taxon.names
  
  #### modify tree ####   
  
  # resolve multichotomies
  new_phy <-  multi2di(phy, random=FALSE)
  
  # label nodes
  new_phy$node.label <- paste("node", 
                              formatC(c(1:new_phy$Nnode), 
                                      digits=0, width=nchar(new_phy$Nnode), flag="0"))
  
  # prune tree and data
  pruned <- drop.tip(new_phy, setdiff(phy$tip.label, taxon.names))
  
  traits <- traits[! rownames(traits) %in% setdiff(phy$tip.label, taxon.names),
                   , drop=FALSE]
  
  #### apply pic function ####
  
  pic.out <- pic(x = set_names(traits[[1]], rownames(traits)), 
                 phy = pruned, 
                 scaled = scale, 
                 var.contrasts = TRUE)
  
  pic.out <- pic.out %>%
    as_tibble() %>%
    mutate(node = rownames(pic.out)) %>%
    mutate(pic_abs = abs(contrasts), 
           sd = sqrt(variance)) %>%
    select(node, pic=contrasts, pic_abs, var=variance, sd)
  
  return(pic.out)
  #return(pruned)
}


brl_transform <- function(x, # a tibble or data frame with trait data
                          trait, # a character string of trait column
                          phy, # a phylogenetic tree of class "phylo" & order "cladewise"
                          taxa, # a character string of the taxon column, to match tips in phy
                          scale=TRUE) {
  
  require(geiger)
  require(tidyverse)
  
  phy.all1 <- compute.brlen(phy=phy, runif, min=0, max=1) # all equals one is base
  
  # raw
  pic.raw <- list(pics(x=x, trait=trait, phy=phy, taxa=taxa, scale=scale) %>% 
                    mutate(trans = "raw"))
  
  # All equals 1
  pic.all1 <- list(pics(x=x, trait=trait, phy=phy.all1, taxa=taxa, scale=scale) %>% 
                     mutate(trans = "all1"))
  
  
  # Pagel transformations
  lambdas <- seq(0, .9, by=.1)  
  
  pic.pagel <- lapply(lambdas, function(l) {
    
    phy.pagel <- rescale(x=phy.all1, model="lambda", l)
    pics(x=x, trait=trait, phy=phy.pagel, taxa=taxa, scale=scale) %>%
      as_tibble() %>%
      mutate(trans = paste("Pagel: lambda = ", l, sep=""))
    
  }) 
  
  names(pic.pagel) <- paste("Pagel: lambda = ", lambdas, sep="")
  
  # Grafen "transformations"
  rhos <- seq(.1, 1, by=.1)
  
  pic.grafen <- lapply(rhos, function(p) {
    
    phy.grafen <- compute.brlen(phy=phy.all1, power=p)
    pics(x=x, trait=trait, phy=phy.grafen, taxa=taxa, scale=scale) %>%
      as_tibble() %>%
      mutate(trans = paste("Grafen: power = ", p, sep=""))
    
  }) 
  
  names(pic.grafen) <- paste("Grafen: power = ", rhos, sep="")
  
  # Ornstein-Uhlenbeck transformations
  alphas <- c(.01, .05, .1, .25, .5, .75, 1, 2)
  
  pic.OU <- lapply(alphas, function(alp) {
    
    phy.OU <- rescale(x=phy.all1, model="OU", alp)
    pics(x=x, trait=trait, phy=phy.OU, taxa=taxa, scale=scale) %>%
      as_tibble() %>%
      mutate(trans = paste("OU: alpha = ", alp, sep=""))
    
  }) 
  
  names(pic.OU) <- paste("OU: alpha = ", alphas, sep="")
  
  # combine into one list
  pics.out <- c(raw=pic.raw, all1=pic.all1, 
                pic.pagel, pic.grafen, pic.OU)
  
  return(pics.out)
  
}