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

# This data came from the Iowa DNR harvest log for 2018
gitURL <- RCurl::getURL("https://raw.githubusercontent.com/ljeroue/Eagle_Lead_Abatement_Model/master/huntunits.csv")
harvestUnits <- readr::read_csv(gitURL)


# Convert tibble to traditional dataframe
harvestUnits <- as.data.frame(harvestUnits)

# Ensure that location area and harvest are numerical
# and thank your tibble because they are
summary(harvestUnits)


#----------------------------------------------------------------------
# Run the simulation
#----------------------------------------------------------------------

results <- simulateLeadMitigation(df = harvestUnits,
                                  col_locationName = "HuntUnit",
                                  col_locationArea_km2 = "Area_km2",
                                  col_locationHarvest = "Harvest",
                                  eagleDensity_km2 = 0.1,
                                  gutPileHarvest = 0.9,
                                  alpha_levels = seq(0, 1, 0.1),
                                  num_iters = 50
                                  )


# Output is a list containing two 4d matrix of mortality rate, one for each 
# mitigation type
inputData <- results$inputData
gutRemoval <- results$gutRemoval
shotConversion <- results$shotConversion


# matrix dimensions are locations, simulations, and 
# mitigation levels (% gutpiles removed or % nonlead ammunition)
dim(gutRemoval) 
dim(shotConversion)


#----------------------------------------------------------------------
# Calculate location specific mortality rates; incorporate eagle density
#----------------------------------------------------------------------
alpha_levels = seq(0, 1, 0.1)
num_iters <- 50


mortality <- mortalityRate(
  inputData,
  gutRemoval,
  shotConversion,
  locations = FALSE,
  num_iters = num_iters,
  alpha_levels = alpha_levels)


# Visualize MORTALITY RATE 

create_mortRate_table <- function(df, alpha_levels, decimalPlaces = 3){
  table <- round(apply(df, 2, quantile, probs = alpha_levels), 3)
  colnames(table) <- c(paste(as.character(alpha_levels*10), "%", sep = ""))
  table
}

(gutpile_mortality <- create_mortRate_table(mortality$mortRate_gutPile, alpha_levels))
(ammo_mortality <- create_mortRate_table(mortality$mortRate_shotConversion, alpha_levels))


#write.csv(gutpile_mortality, "MortalityRate_GutPileRemoved.csv")
#write.csv(ammo_mortality, "MortalityRate_NonLeadAmmo.csv")



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