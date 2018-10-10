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
local_natureindex<- read.csv("chapter_4/nature_index/local_natureindex.csv")
world_natureindex<- read.csv("chapter_4/nature_index/world_natureindex.csv")
national_natureindex<- read.csv("chapter_4/nature_index/national_natureindex.csv")
global_natureindex<- read.csv("chapter_4/nature_index/global_natureindex.csv")
virgin_biomass_natureindex<- read.csv("chapter_4/nature_index/virgin_biomass_natureindex.csv")
ni_indicators_noba<- read.csv("chapter_4/nature_index/ni_indicators_noba.csv")

valuemat_local <- as.matrix(local_natureindex[,4:92])
dimnames(valuemat_local)[[1]] <- local_natureindex[,2]
dimnames(valuemat_local)[[2]] <- 1980:2068

valuemat_world <- as.matrix(world_natureindex[,4:92])
dimnames(valuemat_world)[[1]] <- world_natureindex[,2]
dimnames(valuemat_world)[[2]] <- 1980:2068

valuemat_national <- as.matrix(national_natureindex[,4:92])
dimnames(valuemat_national)[[1]] <- national_natureindex[,2]
dimnames(valuemat_national)[[2]] <- 1980:2068

valuemat_global <- as.matrix(global_natureindex[,4:92])
dimnames(valuemat_global)[[1]] <- global_natureindex[,2]
dimnames(valuemat_global)[[2]] <- 1980:2068

valuemat_virgin<- as.matrix(virgin_biomass_natureindex[,4]) #4=VB, 5=2100
dimnames(valuemat_virgin)[[1]]<- virgin_biomass_natureindex[,2]

# Read or calculate reference values


refmat <- matrix(rep(virgin_biomass_natureindex[,4],dim(valuemat_virgin)[2]), #also change here for 2100
                 nrow = dim(valuemat_virgin)[1], ncol=dim(valuemat_virgin)[2])

refmat<- as.vector(refmat)
# Scale indicators according to the LOW model
  
scaled.bootmat_local <- valuemat_local/refmat
scaled.bootmat_local[scaled.bootmat_local < 0] <- 0.0
scaled.bootmat_local[scaled.bootmat_local > 1] <- 1.0

scaled.bootmat_world <- valuemat_world/refmat
scaled.bootmat_world[scaled.bootmat_world < 0] <- 0.0
scaled.bootmat_world[scaled.bootmat_world > 1] <- 1.0

scaled.bootmat_national <- valuemat_national/refmat
scaled.bootmat_national[scaled.bootmat_national < 0] <- 0.0
scaled.bootmat_national[scaled.bootmat_national > 1] <- 1.0

scaled.bootmat_global <- valuemat_global/refmat
scaled.bootmat_global[scaled.bootmat_global < 0] <- 0.0
scaled.bootmat_global[scaled.bootmat_global > 1] <- 1.0

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

w = 0.50 # The total weight given to key indicators, when key indicators are present in the data.

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


nature.index_local <- Indicator.weights.whole.ecosystem %*% scaled.bootmat_local
nature.index_world <- Indicator.weights.whole.ecosystem %*% scaled.bootmat_world
nature.index_national <- Indicator.weights.whole.ecosystem %*% scaled.bootmat_national
nature.index_global <- Indicator.weights.whole.ecosystem %*% scaled.bootmat_global


as.data.frame(nature.index_local) %>% gather(year, score, 1:89) -> local
local$scenario<- "local"
#local<- local[c(TRUE,rep(FALSE,4)), ]

as.data.frame(nature.index_world) %>% gather(year, score, 1:89) -> world
world$scenario<- "world"
#world<- world[c(TRUE,rep(FALSE,4)), ]

as.data.frame(nature.index_national) %>% gather(year, score, 1:89) -> national
national$scenario<- "national"
#national<- national[c(TRUE,rep(FALSE,4)), ]
as.data.frame(nature.index_global) %>% gather(year, score, 1:89) -> global
global$scenario<- "global"
#national<- national[c(TRUE,rep(FALSE,4)), ]

index3<- rbind(local, world, national, global)
index3$year<- as.integer(index3$year)

plot(1980:2068,nature.index_national,ylim = c(0,1),type="l",col="red",lwd=2,ylab="Nature index")
lines(1980:2068,nature.index_world,col="blue",lwd=2)
lines(1980:2068,nature.index_local,col="black",lwd=2)
lines(1980:2068,nature.index_global,col="black",lwd=2)

title("Barents Sea")

a<- ggplot(index3, aes(year,score)) + 
  geom_line(aes(colour=scenario)) + 
  ylim(0.8,1)+
  xlim(2015, 2068)+ggtitle("Overall")+
  theme_bw()+
  ggplot2::theme(text = ggplot2::element_text(size=14),
                 axis.text.x = ggplot2::element_text(size=12))+
  xlab("")+
  ylab("Norway Nature Index Score")+ theme(legend.position="none")

nature.index_local.benthic <- Indicator.weights.benthic %*% scaled.bootmat_local
nature.index_world.benthic <- Indicator.weights.benthic %*% scaled.bootmat_world
nature.index_national.benthic <- Indicator.weights.benthic %*% scaled.bootmat_national
nature.index_global.benthic <- Indicator.weights.benthic %*% scaled.bootmat_global


as.data.frame(nature.index_local.benthic) %>% gather(year, score, 1:89) -> local_benthic
local_benthic$scenario<- "local"
#local_benthic<- local_benthic[c(TRUE,rep(FALSE,4)), ]
as.data.frame(nature.index_world.benthic) %>% gather(year, score, 1:89) -> world_benthic
world_benthic$scenario<- "world"
#world_benthic<- world_benthic[c(TRUE,rep(FALSE,4)), ]
as.data.frame(nature.index_national.benthic) %>% gather(year, score, 1:89) -> national_benthic
national_benthic$scenario<- "national"

as.data.frame(nature.index_global.benthic) %>% gather(year, score, 1:89) -> global_benthic
global_benthic$scenario<- "global"
#national_benthic<- national_benthic[c(TRUE,rep(FALSE,4)), ]
benthic_nni<- rbind(local_benthic, world_benthic, national_benthic, global_benthic)
benthic_nni$year<- as.integer(benthic_nni$year)

plot(1980:2068,nature.index_national.benthic,ylim = c(0,1),type="l",col="red",lwd=2,ylab="Nature index")
lines(1980:2068,nature.index_world.benthic,col="blue",lwd=2)
lines(1980:2068,nature.index_local.benthic,col="black",lwd=2)
lines(1980:2068,nature.index_global.benthic,col="black",lwd=2)

title("Benthic")

b<- ggplot(benthic_nni, aes(year,score)) +
  geom_line(aes(colour=scenario)) +
  ylim(0.9,1)+
  xlim(2015, 2068)+
ggtitle("Benthic")+
  theme_bw()+
  ggplot2::theme(text = ggplot2::element_text(size=14),
                 axis.text.x = ggplot2::element_text(size=12))+
  xlab("Year")+
  ylab("")+ theme(legend.position="none")

nature.index_local.pelagic <- Indicator.weights.pelagic %*% scaled.bootmat_local
nature.index_world.pelagic <- Indicator.weights.pelagic %*% scaled.bootmat_world
nature.index_national.pelagic <- Indicator.weights.pelagic %*% scaled.bootmat_national
nature.index_global.pelagic <- Indicator.weights.pelagic %*% scaled.bootmat_global


as.data.frame(nature.index_local.pelagic) %>% gather(year, score, 1:89) -> local_pelagic
local_pelagic$scenario<- "local"
#local_pelagic<- local_pelagic[c(TRUE,rep(FALSE,4)), ]

as.data.frame(nature.index_world.pelagic) %>% gather(year, score, 1:89) -> world_pelagic
world_pelagic$scenario<- "world"
#world_pelagic<- world_pelagic[c(TRUE,rep(FALSE,4)), ]

as.data.frame(nature.index_national.pelagic) %>% gather(year, score, 1:89) -> national_pelagic
national_pelagic$scenario<- "national"
#national_pelagic<- national_pelagic[c(TRUE,rep(FALSE,4)), ]
as.data.frame(nature.index_global.pelagic) %>% gather(year, score, 1:89) -> global_pelagic
global_pelagic$scenario<- "global"

pelagic_nni<- rbind(local_pelagic, world_pelagic, national_pelagic, global_pelagic)
pelagic_nni$year<- as.integer(pelagic_nni$year)

c<- ggplot(pelagic_nni, aes(year,score)) + 
  geom_line(aes(colour=scenario)) + ylim(0.6,1)+
  xlim(2015, 2068)+
  ggtitle("Pelagic")+
  theme_bw()+
  ggplot2::theme(text = ggplot2::element_text(size=14),
                 axis.text.x = ggplot2::element_text(size=12))+
  xlab("")+
  ylab("")+ theme(legend.position="none")+ theme(legend.position="none")

legend<- ggplot(pelagic_nni, aes(year,score)) + 
  geom_line(aes(colour=scenario)) + ylim(0.7,1)+
  xlim(1980, 2068)+
  ggtitle("Pelagic Norway Nature Index for the Barents Sea")+
  theme_bw()+
  ggplot2::theme(text = ggplot2::element_text(size=14),
                 axis.text.x = ggplot2::element_text(size=12))+
  xlab("Year")+
  ylab("Score")

plot(1980:2068,nature.index_national.pelagic,ylim = c(0,1),type="l",col="red",lwd=2,ylab="Nature index")
lines(1980:2068,nature.index_world.pelagic,col="blue",lwd=2)
lines(1980:2068,nature.index_local.pelagic,col="black",lwd=2)
title("Pelagic")

# 
prow<- plot_grid(a, b, c, labels = c("A", "B", "C"), align = "v", ncol=1)
legend<- get_legend(legend)
plot_grid(prow, legend, rel_widths = c(3, .3))

benthic_nni$type<- "benthic"
index3$type<- "overall"
pelagic_nni$type<- "pelagic"
nni<- rbind(index3, benthic_nni, pelagic_nni)
write.csv(nni, "chapter_4/nature_index/nni_scores.csv")

write.csv(index3, "chapter_4/nature_index/scores/ni_scores.csv", row.names=F)
write.csv(benthic_nni, "chapter_4/nature_index/scores/ni_benthic.csv", row.names = F)
write.csv(pelagic_nni, "chapter_4/nature_index/scores/ni_pelagic.csv", row.names = F)
