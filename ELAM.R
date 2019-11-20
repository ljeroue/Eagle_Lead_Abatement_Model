# Run ELAM

#######################################################################
# Golden Eagle Lead Abatement Model (GELAM) version 1
#######################################################################

## Description: Runs simulation to estimate acute mortality of golden
#eagles due to ingesting lead from gut-piles of big game
#animals and to assess possible mitigation strategies
#to quantify mortality rates and uncertainty under
#two scenarios:  non-lead ammunition and gut-pile retrieval

## Addapted from MatLab Code presented in:
# Cochrane J.F., E. Lonsdorf, T.D. Allison and CF.A. Sanders-Reed.
# Modeling with uncertain science: estimation mitigation credits
# from abating lead poisoning in Golden Eagles. 2015. Ecological
# Applications 25(6): 1518-1533

##Author: Lacey Jeroue (jerouelm@gmail.com)


#---------------------------------------------------------------------
# Set up simulation run -- clear variables and plots          
# Seed random number generator so results can be replicated   
# Note:  no significance to seed number of 123
#---------------------------------------------------------------------

rm(list = ls())
set.seed(123)

#----------------------------------------------------------------------
# Load current location information for hunting units
#----------------------------------------------------------------------

# 
harvestUnits <- read.csv("C:/LACEY/CODE/gitHub/Eagle_LeadAbatement/Eagle_Lead_Abatement_Model/huntunits.csv", header = T)
numColumns <- c("Harvest", "Area_km2")
harvestUnits[, numColumns] <- sapply(harvestUnits[, numColumns], as.numeric)

eagleDensity_km2 <- 0.1
gutPileHarvest <- 0.9


#----------------------------------------------------------------------
# output
#----------------------------------------------------------------------

#output for gut pile removal
mort_rate
summary(mort_rate)

# output for lead bullet reduction  
mort_ratea2

#----------------------------------------------------------------------
# Calculate total deaths for state
#----------------------------------------------------------------------

toteagle = harvestUnits$huntUnit_100km2 * (harvestUnits$eagles_km2 * 100)
deadeaglea1 = array(0, dim=c(num_iters,num_loc,length(alpha1_levels)))
deadeaglea2 = array(0, dim=c(num_iters,num_loc,length(alpha1_levels)))
deadeaglesuma1 = array(0, dim=c(num_iters,length(alpha1_levels)))
deadeaglesuma2 = array(0, dim=c(num_iters,length(alpha1_levels)))

for(i in 1:num_iters){
  for(a in 1:length(alpha1_levels)){
    deadeaglea1[i,1:num_loc,a] = t(drop(mort_rate[i,1:num_loc,a,1])) * toteagle
    deadeaglea2[i,1:num_loc,a] = t(drop(mort_ratea2[i,1:num_loc,a,1])) * toteagle
    deadeaglesuma1[i,a] = sum(deadeaglea1[i,1:num_loc,a])
    deadeaglesuma2[i,a] = sum(deadeaglea2[i,1:num_loc,a])
  } # end 
} # end


# for entire state
state_mort_ratea1 = deadeaglesuma1/sum(toteagle)
state_mort_ratea2 = deadeaglesuma2/sum(toteagle)
round(apply(state_mort_ratea1, 2, quantile, probs = c(0,.10,.20,.30,.40,.50,.60,.70,.80,.90,1)),3)
round(apply(state_mort_ratea2, 2, quantile, probs = c(0,.10,.20,.30,.40,.50,.60,.70,.80,.90,1)),3)
apply(state_mort_ratea1, 2, mean)
apply(state_mort_ratea2, 2, mean)


## FOR A SELECT REGION
# these FID correspond to hunting units 22 34 66 67 88 89
#selectFID = c(57, 70, 129, 88, 84, 82)
selectFID = c(0)
selectFID = c(1)

deadeaglea1Casper = array(0, dim=c(num_iters,num_loc,length(alpha1_levels)))
deadeaglea2Casper = array(0, dim=c(num_iters,num_loc,length(alpha1_levels)))
deadeaglesuma1Casper = array(0, dim=c(num_iters,length(alpha1_levels)))
deadeaglesuma2Casper = array(0, dim=c(num_iters,length(alpha1_levels)))

toteagleCasper = NULL
for(m in 1:length(selectFID)){   #RUNS
  loc_index = which(locations[,1] == selectFID[m])
  eagle_loc =  locations[loc_index,2] * (locations[loc_index,4] * 100) #total eagle at unit m
  toteagleCasper = rbind(toteagleCasper, eagle_loc)   
  for(i in 1:num_iters){
    for(a in 1:length(alpha1_levels)){
      deadeaglea1Casper[i,m,a] = drop(t(mort_rate[i,loc_index,a,1])) * eagle_loc
      deadeaglea2Casper[i,m,a] = drop(t(mort_ratea2[i,loc_index,a,1])) * eagle_loc
    } # end
  }# end
}# end

for(i in 1:num_iters){      #RUNS
  for(a in 1:length(alpha1_levels)){
    deadeaglesuma1Casper[i,a] = sum(deadeaglea1Casper[i,,a])
    deadeaglesuma2Casper[i,a] = sum(deadeaglea2Casper[i,,a])
  }# end
}# end 

Casper_mort_ratea1 = deadeaglesuma1Casper/sum(toteagleCasper)
Casper_mort_ratea2 = deadeaglesuma2Casper/sum(toteagleCasper)
round(apply(Casper_mort_ratea1, 2, quantile, probs = c(0,.10,.20,.30,.40,.50,.60,.70,.80,.90,1.00)),3)
round(apply(Casper_mort_ratea2, 2, quantile, probs = c(0,.10,.20,.30,.40,.50,.60,.70,.80,.90,1.00)),3)
apply(Casper_mort_ratea1, 2, mean)
apply(Casper_mort_ratea2, 2, mean)

# MORTALITY RATE - Table D1
m1 <- round(apply(Casper_mort_ratea1, 2, quantile, probs = c(0,.10,.20,.30,.40,.50,.60,.70,.80,.90,1.00)),3)
colnames(m1) <- c(paste(as.character(seq(0,100,10)), "%", sep = ""))
m2 <- round(apply(Casper_mort_ratea2, 2, quantile, probs = c(0,.10,.20,.30,.40,.50,.60,.70,.80,.90,1.00)),3)
colnames(m2) <- c(paste(as.character(seq(0,100,10)), "%", sep = ""))
#write.csv(m1, "MortalityRate_GutPileRemoved_Midwest.csv")
#write.csv(m2, "MortalityRate_AmmunitionNonLead_Midwest.csv")

# How can we reduce eagle deaths by X per year? Produce Fig. 6 - NON-LEAD AMMO

# Eagle Mortalities by % mitigation rate (columns) and quantile (rows)
mit2View <- round(data.frame(sum(toteagleCasper) *
                               apply(Casper_mort_ratea2, 2, quantile,
                                     probs = c(0,.10,.20,.30,.40,.50,.60,.70,.80,.90,1.00))),3)
colnames(mit2View) <- c(paste(as.character(seq(0,100,10)), "%", sep = ""))
mit2View 
#write.csv(mt2view, "")

# look at avoided eagle mortalities by mitigation rate.
noMit2 <- cbind(mit2View[,1])
reducedEagleDeaths <- data.frame(t(noMit2 - mit2View))
row.names(reducedEagleDeaths) <- NULL
reducedEagleDeaths$mitRate <- seq(0,100,10)
reducedEagleDeaths

# eagle =5 # reduce eagle deaths by say 5 per year
# lm20 <- lm(reducedEagleDeaths$X20.~reducedEagleDeaths$mitRate)
# lm50 <- lm(reducedEagleDeaths$X50.~reducedEagleDeaths$mitRate)
# lm80 <- lm(reducedEagleDeaths$X80.~reducedEagleDeaths$mitRate)
# xy <- data.frame(rbind((eagle-lm20$coefficients[1]) / lm20$coefficients[2],
#             (eagle-lm50$coefficients[1]) / lm50$coefficients[2],
#             (eagle-lm80$coefficients[1]) / lm80$coefficients[2]))
# xy$y <- eagle # mitigation needed (x) for reduction in y eagle motalities
# names(xy)[1] <- "x"

jpeg('EagleDeathsAvoided_byMitigation_MidWest_PierceCO.jpg', height = 6, width = 8, units = 'in', res = 300)
par(mar = c(9.5, 4, 10, 0))
par(mfrow=c(1,2))
plot(reducedEagleDeaths$X50.~reducedEagleDeaths$mitRate, type="n", 
     main="MidWest - Pierce County, WI
Area: 1,533 km2
Eagles per km2: 0.0386
Total gut piles: 2,250",
     xlab="Mitigation rate (% ammunition non-lead)", ylab= "Number of eagle deaths avoided")
lines(reducedEagleDeaths$X20.~reducedEagleDeaths$mitRate, lty=2)
lines(reducedEagleDeaths$X50.~reducedEagleDeaths$mitRate)
lines(reducedEagleDeaths$X80.~reducedEagleDeaths$mitRate, lty=2)
#points(xy$x, xy$y, type="p")
#legend("topright", c("80th percentile", "median", "20th percentile"), lty = c(2,1,2))

## How can we reduce eagle deaths by X per year? Produce Fig. 6 - GUT PILE REMOVAL
# Eagle Mortalities by % mitigation rate (columns) and quantile (rows)
mit2View <- round(data.frame(sum(toteagleCasper) *
                               apply(Casper_mort_ratea1, 2, quantile,
                                     probs = c(0,.10,.20,.30,.40,.50,.60,.70,.80,.90,1.00))),3)

names(mit2View) <- as.character(seq(0,100,10))
mit2View 

# look at avoided eagle mortalities by mitigation rate.
noMit2 <- cbind(mit2View[,1])
reducedEagleDeaths <- data.frame(t(noMit2 - mit2View))
row.names(reducedEagleDeaths) <- NULL
reducedEagleDeaths$mitRate <- seq(0,100,10)
reducedEagleDeaths

# eagle =5 # reduce eagle deaths by say 5 per year
# lm20 <- lm(reducedEagleDeaths$X20.~reducedEagleDeaths$mitRate)
# lm50 <- lm(reducedEagleDeaths$X50.~reducedEagleDeaths$mitRate)
# lm80 <- lm(reducedEagleDeaths$X80.~reducedEagleDeaths$mitRate)
# xy <- data.frame(rbind((eagle-lm20$coefficients[1]) / lm20$coefficients[2],
#                        (eagle-lm50$coefficients[1]) / lm50$coefficients[2],
#                        (eagle-lm80$coefficients[1]) / lm80$coefficients[2]))
# xy$y <- eagle # mitigation needed (x) for reduction in y eagle motalities
# names(xy)[1] <- "x"

par(mar = c(9.5, 2, 10, 2))
plot(reducedEagleDeaths$X50.~reducedEagleDeaths$mitRate, type="n", 
     xlab="Mitigation rate (% gut piles removed)", ylab= "", yaxt="n")
# abline(lm20, lty=2)
# abline(lm50)
# abline(lm80, lty=2)
lines(reducedEagleDeaths$X20.~reducedEagleDeaths$mitRate, lty=2)
lines(reducedEagleDeaths$X50.~reducedEagleDeaths$mitRate)
lines(reducedEagleDeaths$X80.~reducedEagleDeaths$mitRate, lty=2)
#points(xy$x, xy$y, type="p")
legend("topleft", c("80th percentile", "median", "20th percentile"), lty = c(2,1,2), cex=.75)
dev.off()