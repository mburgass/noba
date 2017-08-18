Observations <- Verdier[,1:7]
dimnames(Observations)[[2]] <- c("ValueID","IndicatorID","Expected.value","Lower","Upper",
"ReferenceYearID","IndicatorareaID")
Observations <- Observations[Observations$ReferenceYearID %in% c(0,2,3,4,8),]
dimnames(Observations)[[1]] <- 1:dim(Observations)[1]

Indic <- data.frame(Indikatorer[,c(1,3:6)],sample(Fidelity))
dimnames(Indic)[[2]] <- c("IndicatorID","Indicator_name","TrophicgroupID","Scalingmodel",
"Key.indicators","Fidelity")
Indic$Indicator_name <- paste("Ind",1:33)
dim(Indic)
dimnames(Indic)[[1]] <- 1:dim(Indic)[1]

BSunits <- data.frame(Kommuner[,c(1,2,8)],rep(c("All area"),4),rep(paste("Part",1:2),2))
dimnames(BSunits)[[2]] <- c("BasicunitID","Basicunit","Area","NIarea1","NIarea2")
BSunits$Basicunit <- paste("Mun",1:4)
dimnames(BSunits)[[1]] <- 1:dim(BSunits)[1]

Indicator.area.ind <- Omraader.ind[,c(1,4)]
dimnames(Indicator.area.ind)[[2]] <- c("IndicatorareaID","IndicatorID")
xxx <- paste("indic",1:length(Indicator.area.ind[,1]))
for (i in 1:length(Indic[,1])) {
	jj <- which(Indicator.area.ind[,2] == Indic[i,1])
	xxx[jj] <- Indic[i,2]
}
Indicator.area.ind <- data.frame(Indicator.area.ind,xxx)
dimnames(Indicator.area.ind)[[2]] <- c("IndicatorareaID","IndicatorID","Indicator_name")
dimnames(Indicator.area.ind)[[1]] <- 1:dim(Indicator.area.ind)[1]

Indicator.area <- Omraader
dimnames(Indicator.area)[[2]] <- c("BasicsunitID","IndicatorareaID")
xxx <- paste("indic",1:length(Indicator.area[,1]))
for (i in 1:length(BSunits[,1])) {
	jj <- which(Indicator.area[,1] == BSunits[i,1])
	xxx[jj] <- BSunits[i,2]
}
Indicator.area <- data.frame(Indicator.area[,c(2,1)],xxx)
dimnames(Indicator.area)[[2]] <- c("IndicatorareaID","BasicsunitID","Basicunit")
dimnames(Indicator.area)[[1]] <- 1:dim(Indicator.area)[1]

file.path.name.extdat2 <- "~/nature_index/TestdataNI.RData"
save(Observations,Indic,BSunits,Indicator.area.ind,Indicator.area, file = paste(file.path.name.extdat2))

load

L <- list(a = 1, b = 2:4, p = pi, ff = gl(3, 4, labels = LETTERS[1:3]))
e <- list2env(L)
ls(e)
