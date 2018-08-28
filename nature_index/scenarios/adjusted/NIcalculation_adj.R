library(tidyverse)
# The script calculates the Nature Index from the output of model simulations of the Barents Sea.
# Barent Sea model ....
#
# Indicators: biomass
#
# Reference values are estimated from 

# Functions that scale indicator observations
# Used when there are uncertainties in the indicator observations or reference values.
# So far not used in the Barents Sea calculations.

scaleobs<-function(ValueID=1:2,FK_OmraadeID=rep(1,2),FK_IndicatorID=rep(1,2),FK_RefAarID=c(0,1),nsim=100,
                   bootmat=matrix(1,nrow = 2,ncol = nsim),ref.value.code=0,IndicatorID=1,FK_Scalingmodel=1){
  
  
  Reference.values <- FK_RefAarID == ref.value.code
  refmat <- 0*bootmat
  valuemat <- bootmat
  N.values <- length(ValueID)
  
  for (i in 1:N.values) {
    refmat[i,] <- bootmat[Reference.values & FK_OmraadeID == FK_OmraadeID[i],]
  }
  
  scaled.bootmat <- valuemat/refmat
  dimnames(scaled.bootmat)[[1]] <- ValueID
  
  model.mat <- matrix(1,nrow=N.values,ncol=nsim)
  FK_Scalingmodel <- as.character(FK_Scalingmodel)
  for (i in 1:N.values) {
    model.mat[i,] <- rep(FK_Scalingmodel[IndicatorID == FK_IndicatorID[i]],nsim)
  }
  scaled.bootmat[model.mat == 2] <- (-1)*scaled.bootmat[model.mat == 2] + 2
  scaled.bootmat[scaled.bootmat < 0] <- 0.0
  scaled.bootmat[scaled.bootmat > 1] <- 1.0
  if (!(is.matrix(scaled.bootmat))) {scaled.bootmat <- t(matrix(scaled.bootmat))}
  
  return(scaled.bootmat)
}

# Functions that calculate weights

calculate.fidelity.weights <- function(fidelity = NULL,
                                       functional.group = NULL, 
                                       presence = NULL) {
  
  # Function that calculates fidelity weights
  # functional.groups could be integer, character or factor
  
  fidelity.matrix <- matrix(rep(fidelity,1,each=dim(presence)[1]),ncol=dim(presence)[2])*presence
  fidelity.weight <- fidelity.matrix*0
  
  if (dim(fidelity.matrix)[1] > 1) {
    for (lev in unique(functional.group)) {
      fidelity.weight[,functional.group == lev] <- 
        fidelity.matrix[,functional.group == lev]/
        rowSums(as.matrix(fidelity.matrix[,functional.group == lev]))
    }
  }
  if (dim(fidelity.matrix)[1] == 1) {
    for (lev in unique(functional.group)) {
      fidelity.weight[,functional.group == lev] <- 
        fidelity.matrix[,functional.group == lev]/
        rowSums(t(as.matrix(fidelity.matrix[,functional.group == lev])))
    }
  }
  
  fidelity.weight[is.nan(fidelity.weight)] <- 0
  
  return(fidelity.weight)
}

#-------------

calculate.trophic.weights <- function(functional.group = NULL, 
                                      presence = NULL, 
                                      key.indicator.code = "key",
                                      code.for.absence = 0,
                                      weights.key.indicators = 0.5) {
  
  # Function that calculates trophic weights
  # functional.groups could be integer, character or factor
  
  count.func.group <- function(functional.group, 
                               key.indicator.code = "key",
                               code.for.absence = 0) {
    # Function that counts the number of functional groups other than key indicators
    # in a set of indicators
    
    return(length(unique(functional.group[!(functional.group %in% c(code.for.absence,
                                                                    key.indicator.code))])))
  }
  
  check.for.key.indicators <- function(functional.group,
                                       key.indicator.code = "key") {
    # Function that checks for key indicators among a set of indicators
    
    return(key.indicator.code %in% functional.group)
  }
  
  assign.trophic.weights <- function(functional.group,
                                     n.functional.group,
                                     key.indicators.present, 
                                     key.indicator.code = "key",
                                     code.for.absence = 0,
                                     w = 0.5) {
    # Function that assigns trophic weights to each indicator observed in a BSunit.
    
    # trophic weight for key indicators
    
    trophic.weight <- (functional.group*0) +
      (functional.group == key.indicator.code)*(w + (1-w)*(n.functional.group == 0))
    
    # trophic weight for nonkey indicators
    
    n.functional.group[n.functional.group == 0] <- 1
    trophic.weight <- trophic.weight +
      ((!(functional.group %in% c(code.for.absence,key.indicator.code)))*
         ((1-(w*key.indicators.present))/n.functional.group))
    return(trophic.weight)
  }
  
  if (is.character(functional.group)) {functional.group <- as.factor(functional.group)}
  
  if (is.factor(functional.group)) {
    key.indicator.code <- which(levels(functional.group) == key.indicator.code)
    if (length(key.indicator.code) == 0) {key.indicator.code <- length(levels(functional.group))+10}
    functional.group <- as.integer(functional.group)
    code.for.absence <- 0
  }
  
  functional.group.matrix <- matrix(rep(functional.group,1,each=dim(presence)[1]),
                                    ncol=dim(presence)[2])
  functional.group.matrix <- functional.group.matrix*presence + (!presence)*code.for.absence
  
  n.functional.group <- apply(functional.group.matrix,1,count.func.group,
                              key.indicator.code,code.for.absence)
  key.indicators.present <- apply(functional.group.matrix,1,check.for.key.indicators,
                                  key.indicator.code)
  
  return(apply(functional.group.matrix,2,assign.trophic.weights,
               n.functional.group,key.indicators.present,
               key.indicator.code,code.for.absence,w))
}

#-------------

calculate.BSunit.weights <- function(fidelity,
                                     functional.group, 
                                     presence, 
                                     key.indicator.code = "key",
                                     code.for.absence = 0,
                                     w = 0.5) {
  
  # Function that calculates BSunit.weights. 
  # BSunit.weights are the product of fidelity weights and trophic weights.
  # NI for BSunits is the weighted average of scaled indicator observations within the BSunit
  # using BSunit.weights as weights.
  # BSunit.weights should therefore sum to 1 within each BS-unit.
  
  fidelity.weights <- calculate.fidelity.weights(fidelity, 
                                                 functional.group, 
                                                 presence)
  trophic.weights <- calculate.trophic.weights(functional.group, 
                                               presence, 
                                               key.indicator.code, 
                                               code.for.absence, 
                                               w)
  return(fidelity.weights*trophic.weights)
}

#----------------

#Next two functions are not used in the Barents Sea calculations as there is 
#only one spatial unit in the analysis

#calculate.area.weights <- function(BSunit.area = NULL,
#                                   NIunits = NULL) {
#  
#  # Function that calculates area weights
#  
#  area.weights <- NIunits*0
#  for (i in 1:(dim(area.weights)[2])) {
#    tot.area <- sum(BSunit.area[NIunits[,i] == 1])
#    area.weights[,i] <- BSunit.area*NIunits[,i]/tot.area
#  }
#  return(area.weights)
#}
#
##----------------
#
#
#calculate.NI.weights <- function(BSunit.area = NULL,
#                                 NIunits = NULL,
#                                 fidelity = NULL,
#                                 functional.group = NULL, 
#                                 presence = NULL,
#                                 key.indicator.code = "key",
#                                 code.for.absence = 0,
#                                 w = 0.5) {
#  
#  # Function that calculates weights used in the calculation of the Nature Index for a set of NIunits
#  
#  BSunit.weights <- calculate.BSunit.weights(fidelity, functional.group, presence, key.indicator.code, 
#                                             code.for.absence,w)
#  area.weights <- calculate.area.weights(BSunit.area,NIunits)
#  
#  NI.weights <- vector("list",dim(NIunits)[2])
#  names(NI.weights) <- dimnames(NIunits)[[2]]
#  
#  for (i in 1:dim(NIunits)[2]) {
#    NI.weights[[i]] <- BSunit.weights*
#      matrix(rep(area.weights[,i],dim(BSunit.weights)[2]),ncol=dim(BSunit.weights)[2])
#    dimnames(NI.weights[[i]]) <- dimnames(presence)
#  }
#  
#  return(NI.weights)
#}

# FIRST CALCULATION. 
# REFERENCE VALUES CONSTANT, REFERENCE VALUES READ FROM "virgin_biomass_natureindex"

# Read input data
fmsy0_natureindex<- read.csv("nature_index/scenarios/adjusted/fmsy0_natureindex_adj.csv")
fmsy1_natureindex<- read.csv("nature_index/scenarios/adjusted/fmsy1_natureindex_adj.csv")
fmsy2_natureindex<- read.csv("nature_index/scenarios/adjusted//fmsy2_natureindex_adj.csv")
virgin_biomass_natureindex<- read.csv("nature_index/scenarios/adjusted/virgin_biomass_natureindex_adj.csv")
ni_indicators_noba<- read.csv("nature_index/scenarios/adjusted/ni_indicators_noba_adj.csv")

valuemat_fmsy0 <- as.matrix(fmsy0_natureindex[,4:38])
dimnames(valuemat_fmsy0)[[1]] <- fmsy0_natureindex[,2]
dimnames(valuemat_fmsy0)[[2]] <- 1981:2015

valuemat_fmsy1 <- as.matrix(fmsy1_natureindex[,4:38])
dimnames(valuemat_fmsy1)[[1]] <- fmsy1_natureindex[,2]
dimnames(valuemat_fmsy1)[[2]] <- 1981:2015

valuemat_fmsy2 <- as.matrix(fmsy2_natureindex[,4:38])
dimnames(valuemat_fmsy2)[[1]] <- fmsy2_natureindex[,2]
dimnames(valuemat_fmsy2)[[2]] <- 1981:2015

valuemat_virgin<- as.matrix(virgin_biomass_natureindex[,5]) #4=VB, 5=2100
dimnames(valuemat_virgin)[[1]]<- virgin_biomass_natureindex[,2]

# Read or calculate reference values


refmat <- matrix(rep(virgin_biomass_natureindex[,5],dim(valuemat_virgin)[2]), #also change here for 2100
                 nrow = dim(valuemat_virgin)[1], ncol=dim(valuemat_virgin)[2])

refmat<- as.vector(refmat)
# Scale indicators according to the LOW model
  
scaled.bootmat_fmsy0 <- valuemat_fmsy0/refmat
scaled.bootmat_fmsy0[scaled.bootmat_fmsy0 < 0] <- 0.0
scaled.bootmat_fmsy0[scaled.bootmat_fmsy0 > 1] <- 1.0

scaled.bootmat_fmsy1 <- valuemat_fmsy1/refmat
scaled.bootmat_fmsy1[scaled.bootmat_fmsy1 < 0] <- 0.0
scaled.bootmat_fmsy1[scaled.bootmat_fmsy1 > 1] <- 1.0

scaled.bootmat_fmsy2 <- valuemat_fmsy2/refmat
scaled.bootmat_fmsy2[scaled.bootmat_fmsy2 < 0] <- 0.0
scaled.bootmat_fmsy2[scaled.bootmat_fmsy2 > 1] <- 1.0

# CALCULATE WEIGHTS.

# Create input objects to the weight calculations

#The indicators fidelity to the Barents Sea, pelagic and bethic systems.



fidelity.pelagic <- ni_indicators_noba[,"fidelity_pelagic"] > 0
fidelity.benthic <- ni_indicators_noba[,"fidelity_benthic"] > 0


functional.group <- ni_indicators_noba[,"fk_func_groupID"]
fidelity.whole.ecosystem <- rep(1,length(functional.group))
#Option to reduce the number of functional groups. Lumps all top predators 
#into one group and all intermediate predators into one group

functional.group[functional.group == 9] <- 8
functional.group[functional.group == 2] <- 1

#Option to include key elements. If skipped, there are no key elements
#recognized in the calculation, instead the candidate key elements are treated just like the
#other indicators

functional.group[ni_indicators_noba[,"key_element"]] <- 10

key.indicator.code <- c(10) # Necessary input to weight calculation
code.for.absence <- c(0)
functional.group.benthic <- functional.group*fidelity.benthic
functional.group.pelagic <- functional.group*fidelity.pelagic

# ICunits corresponds to the matrix "presence" that appears in the functions for calculating weights.
# For the Barents Sea calculations, there is only one spatial unit where all indicators are present.
# So, in order to calculate weighgts correctly, using the above functions,
# ICunit should be an 1 x n.ind matrix containing the sequence 1:n.ind, while
# NIunits should be an 1 x 1 matrix of 1.

ICunits <- t(as.matrix(1:length(functional.group)))
dimnames(ICunits)[[1]] <- as.list("Barents Sea")
dimnames(ICunits)[[2]] <- ni_indicators_noba$indicator_name

NIunits <- as.matrix(1)
dimnames(NIunits)[[1]] <- as.list(dimnames(ICunits)[[1]])
dimnames(NIunits)[[2]] <- as.list("Barents Sea")

w = 0 # The total weight given to key indicators, when key indicators are present in the data.

Indicator.weights.whole.ecosystem <- calculate.BSunit.weights(fidelity.whole.ecosystem,
                         functional.group, 
                         (ICunits > 0), 
                         key.indicator.code,
                         code.for.absence,
                         w)

Indicator.weights.pelagic <- calculate.BSunit.weights(fidelity.pelagic,
                                              functional.group.pelagic, 
                                              (ICunits > 0), 
                                              key.indicator.code,
                                              code.for.absence,
                                              w)

Indicator.weights.benthic <- calculate.BSunit.weights(fidelity.benthic,
                                              functional.group.benthic, 
                                              (ICunits > 0), 
                                              key.indicator.code,
                                              code.for.absence,
                                              w)

# CALCULATE NATURE INDEX VALUES
# In the case of the Barents Sea simulations, NIs (vector of length n.time) is calculated as 
# the matrix product between Indicator,weights (1 x n.ind matrix) and 
# scaled.bootmat (n.ind x n.time matrix)


nature.index_fmsy0 <- Indicator.weights.whole.ecosystem %*% scaled.bootmat_fmsy0
nature.index_fmsy1 <- Indicator.weights.whole.ecosystem %*% scaled.bootmat_fmsy1
nature.index_fmsy2 <- Indicator.weights.whole.ecosystem %*% scaled.bootmat_fmsy2

as.data.frame(nature.index_fmsy0) %>% gather(year, score, 1:35) -> fmsy0
fmsy0$scenario<- "fmsy0"
as.data.frame(nature.index_fmsy1) %>% gather(year, score, 1:35) -> fmsy1
fmsy1$scenario<- "fmsy1"

as.data.frame(nature.index_fmsy2) %>% gather(year, score, 1:35) -> fmsy2
fmsy2$scenario<- "fmsy2"

index3<- rbind(fmsy0, fmsy1, fmsy2)
index3$year<- as.integer(index3$year)

plot(1981:2015,nature.index_fmsy2,ylim = c(0,1),type="l",col="red",lwd=2,ylab="Nature index")
lines(1981:2015,nature.index_fmsy1,col="blue",lwd=2)
lines(1981:2015,nature.index_fmsy0,col="black",lwd=2)
title("Barents Sea")

ggplot(index3, aes(year,score)) + geom_line(aes(colour=scenario)) + ylim(0,1)+ggtitle("Norway Nature Index for the Barents Sea")

nature.index_fmsy0.benthic <- Indicator.weights.benthic %*% scaled.bootmat_fmsy0
nature.index_fmsy1.benthic <- Indicator.weights.benthic %*% scaled.bootmat_fmsy1
nature.index_fmsy2.benthic <- Indicator.weights.benthic %*% scaled.bootmat_fmsy2

as.data.frame(nature.index_fmsy0.benthic) %>% gather(year, score, 1:35) -> fmsy0_benthic
fmsy0_benthic$scenario<- "fmsy0"
as.data.frame(nature.index_fmsy1.benthic) %>% gather(year, score, 1:35) -> fmsy1_benthic
fmsy1_benthic$scenario<- "fmsy1"

as.data.frame(nature.index_fmsy2.benthic) %>% gather(year, score, 1:35) -> fmsy2_benthic
fmsy2_benthic$scenario<- "fmsy2"
benthic_nni<- rbind(fmsy0_benthic, fmsy1_benthic, fmsy2_benthic)
benthic_nni$year<- as.integer(benthic_nni$year)

plot(1981:2015,nature.index_fmsy2.benthic,ylim = c(0,1),type="l",col="red",lwd=2,ylab="Nature index")
lines(1981:2015,nature.index_fmsy1.benthic,col="blue",lwd=2)
lines(1981:2015,nature.index_fmsy0.benthic,col="black",lwd=2)
title("Benthic")
ggplot(benthic_nni, aes(year,score)) + geom_line(aes(colour=scenario)) + ylim(0,1)+ggtitle("Benthic Norway Nature Index for the Barents Sea")

nature.index_fmsy0.pelagic <- Indicator.weights.pelagic %*% scaled.bootmat_fmsy0
nature.index_fmsy1.pelagic <- Indicator.weights.pelagic %*% scaled.bootmat_fmsy1
nature.index_fmsy2.pelagic <- Indicator.weights.pelagic %*% scaled.bootmat_fmsy2

as.data.frame(nature.index_fmsy0.pelagic) %>% gather(year, score, 1:35) -> fmsy0_pelagic
fmsy0_pelagic$scenario<- "fmsy0"
as.data.frame(nature.index_fmsy1.pelagic) %>% gather(year, score, 1:35) -> fmsy1_pelagic
fmsy1_pelagic$scenario<- "fmsy1"

as.data.frame(nature.index_fmsy2.pelagic) %>% gather(year, score, 1:35) -> fmsy2_pelagic
fmsy2_pelagic$scenario<- "fmsy2"
pelagic_nni<- rbind(fmsy0_pelagic, fmsy1_pelagic, fmsy2_pelagic)
pelagic_nni$year<- as.integer(pelagic_nni$year)

ggplot(pelagic_nni, aes(year,score)) + geom_line(aes(colour=scenario)) + ylim(0,1)+ggtitle("Pelagic Norway Nature Index for the Barents Sea")


plot(1981:2015,nature.index_fmsy2.pelagic,ylim = c(0,1),type="l",col="red",lwd=2,ylab="Nature index")
lines(1981:2015,nature.index_fmsy1.pelagic,col="blue",lwd=2)
lines(1981:2015,nature.index_fmsy0.pelagic,col="black",lwd=2)
title("Pelagic")

# 


write.csv(index3, "nature_index/ni_scores.csv", row.names=F)
write.csv(benthic_nni, "nature_index/ni_benthic.csv", row.names = F)
write.csv(pelagic_nni, "nature_index/ni_pelagic.csv", row.names = F)
