
#######################################################################
# (GOLDEN) Eagle Lead Abatement Model (ELAM) - Run Script
#######################################################################

## Description: Estimates acute mortality of eagles due to ingesting 
#lead from gut-piles of big game animals and assesses possible 
#mitigation strategies to quantify mortality rates and uncertainty under
#two scenarios:  non-lead ammunition conversion and gut-pile retrieval

## Reference:
# Cochrane J.F., E. Lonsdorf, T.D. Allison and CF.A. Sanders-Reed.
# Modeling with uncertain science: estimation mitigation credits
# from abating lead poisoning in Golden Eagles. 2015. Ecological
# Applications 25(6): 1518-1533

##Author: Lacey Jeroue (jerouelm@gmail.com)


#----------------------------------------------------------------------
# Setup - Load location information for hunting units
#----------------------------------------------------------------------
rm(list = ls())
set.seed(123)


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
  locations = NULL, # leave NULL for overall summary
  col_locationName = "HuntUnit",
  num_iters = num_iters,
  alpha_levels = alpha_levels)



#----------------------------------------------------------------------
# Create density tables
#----------------------------------------------------------------------

# Visualize MORTALITY RATE in a table
# df <- mortality$mortRate_gutPile
mortRate_table <- function(df, alpha_levels, decimalPlaces = 3){
  table <- round(apply(df, 2, quantile, probs = alpha_levels), decimalPlaces)
  table <- data.frame(t(table))
  colnames(table) <- c(paste(as.character(alpha_levels*100), "%", sep = ""))
  row.names(table) <- NULL
  table$mitRate <- c(paste(as.character(alpha_levels*100), "%", sep = ""))
  table
}
(gutpile_mortality_rate <- mortRate_table(mortality$mortRate_gutPile, alpha_levels))
(ammo_mortality_rate <- mortRate_table(mortality$mortRate_shotConversion, alpha_levels))


# Visualize TOTAL MORTALITY in a table
# Eagle Mortalities by % mitigation rate (columns) and quantile (rows)
# due to rounding, can not just multiply the mortality rate array by total eagles
# total eagles here must be that which sums from the dataset processed in mortalityRate()
# so it may be a subset
mortTot_table <- function(df, total_eagle, alpha_levels, decimalPlaces = 0){
  table <- data.frame(round(total_eagle * apply(df, 2, quantile, probs = alpha_levels), decimalPlaces))
  table <- data.frame(t(table))
  colnames(table) <- c(paste(as.character(alpha_levels*100), "%", sep = ""))
  row.names(table) <- NULL
  table$mitRate <- c(paste(as.character(alpha_levels*100), "%", sep = ""))
  table
}
(gutpile_mortality_total <- mortTot_table(mortality$mortRate_gutPile, 
                                                 total_eagle = sum(inputData$total_eagles),
                                                 alpha_levels))
(ammo_mortality_total <- mortTot_table(mortality$mortRate_shotConversion, 
                                              total_eagle = sum(inputData$total_eagles),
                                              alpha_levels))


# Visualize AVOIDED MORTALITY in a table
# ie How can we reduce eagle deaths by X per year by converting to non-lead ammunition?
# look at avoided eagle mortalities by mitigation rate.
avoidedEagleMort_table <- function(df){
  no_ammoMit <- as.numeric(as.character(array(df[1,-12])))
  avoidedEagleMort <- as.data.frame(t(apply(as.matrix(df[1:11,1:11]), 1, 
                                              function(x) no_ammoMit-x)))
  avoidedEagleMort$mitRate <- df$mitRate
  avoidedEagleMort$mitRate <- factor(avoidedEagleMort$mitRate,
                                     levels = avoidedEagleMort$mitRate)
  avoidedEagleMort
}
avoidedEagleMort_gut <- avoidedEagleMort_table(gutpile_mortality_total)
avoidedEagleMort_ammo <- avoidedEagleMort_table(ammo_mortality_total)


#----------------------------------------------------------------------
# Create figures
#----------------------------------------------------------------------


# jpeg('C:/LACEY/CODE/gitHub/Eagle_LeadAbatement/Eagle_Lead_Abatement_Model/EagleDeathsAvoided_gutpile.jpg',
#      height = 4, width = 5, units = 'in', res = 300)
# par(mar = c(9.5, 4, 10, 0))
plot(`50%` ~ mitRateNum, data = reducedEagleDeaths, type="n", 
     main="Iowa
Harvest Year 2018
Eagle Density assumed to be 0.1/km2",
     xlab="Mitigation rate (% gut pile removed)", ylab= "Eagle deaths avoided")
lines(reducedEagleDeaths$`20%` ~ reducedEagleDeaths$mitRateNum, lty=2)
lines(reducedEagleDeaths$`50%` ~ reducedEagleDeaths$mitRateNum)
lines(reducedEagleDeaths$`70%` ~ reducedEagleDeaths$mitRateNum, lty=2)
# points(`50%` ~ mitRateNum, data = reducedEagleDeaths, type="p")
# legend("bottomright", c("80th percentile", "median", "20th percentile"), lty = c(2,1,2))
# dev.off()

