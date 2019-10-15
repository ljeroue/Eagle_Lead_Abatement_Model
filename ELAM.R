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

##Author: Lacey Jeroue (ljeroue@west-inc.com) translated from 
#E. Lonsdorf and C. Sanders-Reed last updated 5/21/2014
##Created: 11/14/2015


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
setwd("C:\\Users\\ljeroue\\Google Drive\\Projects\\GoldenEagleRcodefromMATLAB\\")


# PREPARE IOWA DATA/MIDAMERICA, FOR TODD MATTSON 4/11/2016
setwd("C:\\Users\\ljeroue\\Google Drive\\Projects\\GoldenEagleRcodefromMATLAB\\MidAmerica_740-01.009\\")
locationsraw <- read.csv("IowaHarvest_DNR.csv", header = T)
areaCounty <- read.csv("Iowa county Areas_Wikipedia.csv", header = T)

#gut piles are 90% of deer harvest
locationsraw$total.gut.piles <- locationsraw$Total.Deer * .9

#Use midwest eagle density (from Wally) 0.062517 eagles / km2
locationsraw$eagles.per.km2 <- 0.062517

#subset to fields of interest
locations <- locationsraw[,c("County.Name", "total.gut.piles", "eagles.per.km2")]
names(locations) <- gsub("County.Name", "Hunt.Unit", names(locations))

#get 100 km blocks
head(areaCounty)
names(areaCounty)
areaCounty$area_km2 <- substr(areaCounty$Area..4., nchar(as.character(areaCounty$Area..4.))-9,
                              nchar(as.character(areaCounty$Area..4.))-5)
areaCounty$area_km2 <- gsub(",", "", areaCounty$area_km2)
areaCounty$area_km2 <- gsub(" ", "", areaCounty$area_km2)
areaCounty <- areaCounty[,c("County", "area_km2")]
areaCounty[areaCounty$County == "Dickinson County", "area_km2"] = 987
areaCounty$area_km2 <- as.numeric(areaCounty$area_km2)
table(areaCounty$area_km2, useNA = "ifany")



# PREPARE MIDWEST / PIERCE CO DATA SET
locationsraw <- read.csv("MidWest.csv", header=T)
locationsraw$X..100.km.blocks <- (locationsraw$mile2 * 2.58999) / 100 # convert to km2 and by block
locationsraw$eagles.per.km2 <- locationsraw$eagles.per.mi2 / 2.58999 # convert to km2
locations <- locationsraw[,c("FID","X..100.km.blocks","total.gut.piles","eagles.per.km2","Hunt.Unit")]
locations$total.gut.piles <- locations$total.gut.piles * .90 #90% harvest is left as gut pile
num_loc = nrow(locations)
head(locationsraw)
head(locations)

# PREPARE WILD HORSE DATA SET
locationsraw <- read.csv("WildHorse.csv", header=T)
locationsraw$X..100.km.blocks <- (locationsraw$acre * 0.00404686) / 100 # CHANGE ACRE TO QTY. 100KM^2 BLOCKS
locations <- locationsraw[,c("FID","X..100.km.blocks","total.gut.piles","eagles.per.km2","Hunt.Unit")]
locations$total.gut.piles <- locations$total.gut.piles * .90
num_loc = nrow(locations)
head(locationsraw)

# PREPARE DATA SET USED IN PUBLICATION
locationsraw <- read.csv("Location2West.csv", header = T)
#c1 = FID; c2 = number of 100km blocks; c3 = total shot 
#c4 = density eagles per km c5 = hunting area
#Select only the hunting units with eagle density estimates in West data
#90% of animals shot result in gutpiles
#there are some blank/NA that need to be removed. -lmj
locations = locationsraw[!is.na(locationsraw$eagles.per.km2),]# DATA SET USED IN PUBLICATION
locations$total.gut.piles <- locations$total.gut.piles * .90

head(locations)
#   FID X..100.km.blocks total.gut.piles eagles.per.km2 Hunt.Unit
# 1   0             9.26            81.0        0.02116       105
# 2   1            12.84           118.8        0.01111       106
# 3   2            31.11           696.6        0.02107        17
# 4   3            19.62           873.9        0.00831        25
# 5   4            39.74           286.2        0.01880        18
# 6   5            25.68           395.1        0.02125       122

# range(locations$eagles.per.km2) #0.00457 to 0.08578
# mean(locations$eagles.per.km2)  #0.02392871
# num_loc = nrow(locations)

# # LETS LOOK AT EAGLE DENSITY
# locs <- read.csv("Location2West.csv", header = T)
# locs <- rbind(locs, locations)
# locs <- locs[order(locs$eagles.per.km2),]
# # 
# # compare eagle density at Wild Horse hunting units (n=2) to WY hunting units
# jpeg('EagleDensity.jpg', height = 6, width = 8, units = 'in', res = 300)
# cols <- c(ifelse(locs$Hunt.Unit %in% c(329,371), "gold", "blue"))
# barplot(locs$eagles.per.km2, col=cols, ylim=c(0,0.1),
#         xlab = "", ylab="Eagle density (km^2)")
# title(xlab="Hunting unit", line=.5, cex.lab=1)
# text(21.5, .03, "Great Basin")
# arrows(20,0.025,20,0.012, length=.1, code = 2)
# dev.off()
# 
# # COMPARE GUT PILES PER 100km block
# jpeg('GutPileDensity.jpg', height = 6, width = 8, units = 'in', res = 300)
# locs$gp100 <- locs$total.gut.piles / locs$X..100.km.blocks
# locs <- locs[order(locs$gp100),]
# # cols <- c(ifelse(locs$Hunt.Unit %in% c(329,371), "gold", "blue"))
# cols <- c(ifelse(locs$Hunt.Unit %in% c("midwest", "pierce county"), "gold", "blue"))
# barplot(locs$gp100, col=cols,
#         xlab = "", ylab="Gut piles per 100km^2 block")
# title(xlab="Hunting unit", line=.5, cex.lab=1)
# text(35, 180, "Wild Horse")
# arrows(34,140,34,20, length=.1, code = 2)
# dev.off()

# clear dataframe 
rm(locationsraw)

#----------------------------------------------------------------------
# Set parameters for all iterations
#----------------------------------------------------------------------

# Number of iterations run
num_iters = 5000

# Range of mitigation effects  
# gutpile mitigation alpha1_levels = % gutpiles removed
alpha1_levels = seq(0, 1, 0.1)
1:length(alpha1_levels)

# ammunition mitigation alpha2_levels % nonlead ammunition
alpha2_levels = seq(0, 1, 0.1)

# Maximum lead (pb) concentration levels (pb_levels)
pb_levels = seq(0, 1000, 10)
num_pb_levels = 101

# Number of days to blood half-life (T-half in Equation 5)
# Experts believe is between 10 & 20 days
max_T_half_blood_pb = 20  
min_T_half_blood_pb = 10

## Cauchy (parameters in Equation 3)
# Emode is likely increase in blood lead per scavenge of a lead-containg gut pile
min_E_mode   = 25 # 
max_E_mode   = 75
gamma_Cauchy = 25

# Scavenge parameters - note low and high refer to the mortality line
#  (parameters in Equation 1)
half_sat_gp_low = 5
half_sat_gp_high = 10
power_gp_low = 3 # at least 3 days between scavenging events for eagle
power_gp_high = 1.5
scalar_gp = 1
max_gp_low = 1
max_gp_high = 5

# Mortality parameters in mg/dL (parameters in Equation 7)
half_sat_mort_low  = 150
half_sat_mort_high = 700
power_mort         = 2.5

# Carcass parameters
# max number of gut piles eaten
max_carcass = 5
carcass_levels = seq(0, 5, 1)
num_carcass_levels = length(carcass_levels)



#----------------------------------------------------------------------
# Pre-allocate variables to hold simulation results
#----------------------------------------------------------------------

# Iteration values (array of zeros): 

prob_pb           = array(0, dim=c(1,num_pb_levels))
prob_pb_temp      = array(0, dim=c(1,num_pb_levels))
max_gp            = array(0, dim=c(num_iters,1))
T_half_blood_pb   = array(0, dim=c(num_iters,1))
L                 = array(0, dim=c(num_iters,1))
E_mode            = array(0, dim=c(num_iters,1))
half_sat_mort     = array(0, dim=c(num_iters,1))
gp_no_pb          = array(0, dim=c(num_iters,1))
prob_pb_no_pb     = array(0, dim=c(num_iters,num_pb_levels,1))
m_gp_no_mit       = array(0, dim=c(num_iters,num_loc,1))
prob_pb_no_pb_mit = array(0, dim=c(num_iters,11,num_pb_levels,1))

# Mitigation values (array of zeros): 
m_gp              = array(0, dim=c(num_iters,num_loc,length(alpha1_levels),1))
mort_ratea2       = array(0, dim=c(num_iters,num_loc,length(alpha2_levels),1))
mort_rate         = array(0, dim=c(num_iters,num_loc,11,1))
  
#----------------------------------------------------------------------
# Begin simulation runs 
#----------------------------------------------------------------------

for(i in 1:num_iters){
  
  #Set parameters that vary by iteration
  # matlab 'rand' returns a single uniformly distributed random number in the interval (0,1).
  scavenge_rand = runif(1)
  
  # maximum number of gut piles ingested
  max_gp[i,1] = 1+4*scavenge_rand
  
  # determine days between eating gut piles  (Equation 4)
  # If ave gut piles eaten is equal to 1 set L to between 1 and 29
  if(max_gp[i,1] < 2){
    L[i,1] = round(3 + runif(1) * 26, 0)
  }else{
    L[i,1] = round(3 + runif(1) * (30/(max_gp[i,1]-1)-3), 0)
  } #end if

  # number of days before blood lead decays by 50%  T half in Eq 5
  T_half_blood_pb[i,1] = round(min_T_half_blood_pb + runif(1) * 
                                 (max_T_half_blood_pb - min_T_half_blood_pb), 0)
  
  # blood lead decay calculation (Equation 4) eq5
  D = exp(log(0.5)/ T_half_blood_pb[i,1])
  
  # Lead per gut pile (D^L parameter in Equation 6)
  lpgp = D^L[i,1]
  
  # clear decay_rate
  rm(D)
  
  # Percent gut piles with no lead  Equation 3b
  gp_no_pb[i,1]  = 0.1 + runif(1) * (0.40)
  
  # determining lead from gut piles consumed     
  lead_consump = round( t(t(0:max_carcass)) %*% (matrix(pb_levels, nrow = 1)), 0) #outside
  
  # mode of blood lead ingested per gp -- Cauchy distribution
  # E-Mode in Equation 3
  E_mode[i,1] = min_E_mode + runif(1) * (max_E_mode - min_E_mode)
  
  # Probability distribution for lead ingested per gp 
  #  Equation 6

  #prob_pb_temp <- NULL #should be a 1x101 matrix
  prob_pb_temp      = array(0, dim=c(1,num_pb_levels))
  for(h in 1:num_pb_levels){ #101 levels
    for(j in 0:max_carcass){ #5 carcass max
      if(j==0){
        lead_consump[j+1,h] = 0
      }else if(j==1){
        lead_consump[j+1,h] = pb_levels[h]
      }else{
        lead_consump[j+1,h] = lead_consump[j,h] * lpgp + pb_levels[h]
      }
    } 
    # Equation 3 -- Cauchy pdf
    prob_pb_temp[h] = (gamma_Cauchy/pi)*(1/((pb_levels[h] - E_mode[i,1])^2 + gamma_Cauchy^2))
  }
  
  # need to standardize so that probability distribution equals one
  # Equation 3b
  prob_pb = prob_pb_temp / sum(prob_pb_temp)
  
  ## probability distribution of lead per gp removing % gutpiles with
  ## no lead   Equation 3b
  #prob_pb_no_pb <- matrix(rep(0,(num_iters*num_pb_levels)), ncol=num_iters)
  prob_pb_no_pb[i,1,1] = (prob_pb[1] * (1 - gp_no_pb[i,1])) + gp_no_pb[i,1]
  prob_pb_no_pb[i,2:num_pb_levels,1] = prob_pb[1,2:num_pb_levels]*(1 - gp_no_pb[i,1])

  # Clear variables                
  rm(prob_pb, prob_pb_temp)

#----------------------------------------------------------------------
# Mortality calculation  
#----------------------------------------------------------------------
  mort_rand = runif(1)
  
  # Half saturation constant - consentation of pb in blood that leads to mortality rate of 50%
  half_sat_mort[i,1] = half_sat_mort_low + mort_rand * 
    (half_sat_mort_high - half_sat_mort_low)
  
  # Equation 7    #Matlab code uses element by element notation here

  mort_prob = (lead_consump^power_mort) / (lead_consump^power_mort + half_sat_mort[i,1]^power_mort)
  
  prop_carc = array(0, dim=c(1,num_carcass_levels))
  
#----------------------------------------------------------------------
# Location Loop 
#----------------------------------------------------------------------
  
  for(g in 1:num_loc){
    ee = locations$eagles.per.km2[g] * 100 # = eagles/100 km block for specific location

    ## Scenarios of mitigation
    
    #----------------------------------------------------------------------
    # mitigation loop 
    #----------------------------------------------------------------------
    for(a in 1:length(alpha1_levels)){

      # gutpiles per 100 km2 blocks per eagle
      gp = (locations$total.gut.piles[g] / (locations$X..100.km.blocks[g] * ee)) * (1-alpha1_levels[a]) 
      
      # Equation 1 (when a = 0) Equation 11 (when a > 0)
      m_gp_low = max_gp_low * scalar_gp * gp^power_gp_low / 
        (scalar_gp * gp^power_gp_low + half_sat_gp_low^power_gp_low)
      
      m_gp_high = (max_gp_high * scalar_gp * gp ^ power_gp_high) / 
        (scalar_gp * gp^power_gp_high + half_sat_gp_high^power_gp_high)
      
      ## determine ave number of gut piles
      if(gp==0){
        m_gp[i,g,a,1] = 0                                                    
        prop_carc[,] = 0
      } else {
        # choosing random value based on Equation 1
        m_gp[i,g,a,1] =  m_gp_low + scavenge_rand * (m_gp_high - m_gp_low)
        # Equation 2
        if(m_gp[i,g,a,1] < max_carcass/2){
          prop_carc[,] = dpois(0:max_carcass, m_gp[i,g,a,1])           
        } else {
          prop_carc[,] = dpois(max_carcass - (0:max_carcass), max_carcass - m_gp[i,g,a,1])
      }#end
      
      # Sum to 1
      temp_sum = sum(prop_carc[2:(max_carcass+1)])
      prop_carc_2 <- c(0:max_carcass) # prepare a place for data
      prop_carc_2[2:(max_carcass+1)] = prop_carc[2:(max_carcass+1)]/temp_sum
      temp_val = max(m_gp[i,g,a,1], sum(t(c(1:max_carcass)) %*% prop_carc_2[2:(max_carcass+1)]))
      prop_carc_2[1] = 1 - m_gp[i,g,a,1] / temp_val
      temp_sum2 = sum(prop_carc_2)
      # Probability distribution for the number of gp scavenged
      prop_carc = matrix(prop_carc_2/temp_sum2, nrow = 1) # a 1x6 matrix
    } #end
    
    #Equation 8                                             
    # Probability distribution of blood lead cosentration per gut pile consumed
    prob_ingest = t(prop_carc) %*% drop(prob_pb_no_pb[i,1:num_pb_levels,1]) 

    ## estimate mortality  Equation 9 - Joint Probability
    mort_rate[i,g,a,1] = sum(prob_ingest * mort_prob) #removed 2nd sum here, may be a problem later
    }#end for  a in 1:11
  
    
  ##MITIGATION METHOD 2 - Lead Bullet Reduction
    
  # gutpiles per 100 km2 blocks per eagle WITHOUT MITIGATION/Removal
  gp = locations$total.gut.piles[g] /(locations$X..100.km.blocks[g] *ee)
  
  # Equation 1
  m_gp_low = max_gp_low * scalar_gp * gp ^ power_gp_low / 
    (scalar_gp*gp ^ power_gp_low + half_sat_gp_low ^ power_gp_low)
  m_gp_high = max_gp_high * scalar_gp * gp ^ power_gp_high / 
    (scalar_gp*gp ^ power_gp_high + half_sat_gp_high ^ power_gp_high)
  
  ## determine ave number of gut piles without mitigation
  if(gp == 0){
    m_gp_no_mit[i,g,1] = 0
    prop_carc[,] = 0
  } else {
    # Select random number based on Equation 1
    m_gp_no_mit[i,g,1] =  m_gp_low + scavenge_rand * (m_gp_high - m_gp_low) 
    # Equation 2
    if(m_gp_no_mit[i,g,1] < max_carcass/2){
      prop_carc[,] = dpois(c(0:max_carcass), m_gp_no_mit[i,g,1])
    } else {
      prop_carc[,] =  dpois(max_carcass - (c(0:max_carcass)), max_carcass - m_gp_no_mit[i,g,1])
    }#end
    
    #Sum to 1
    temp_sum = sum(prop_carc[2:(max_carcass+1)])
    prop_carc_2 <- c(0:max_carcass)
    prop_carc_2[2:(max_carcass+1)] = prop_carc[2:(max_carcass+1)]/temp_sum
    temp_val = max(m_gp_no_mit[i,g,1], sum(t(c(1:max_carcass)) %*% prop_carc_2[2:(max_carcass+1)]))
    prop_carc_2[1] = 1-m_gp_no_mit[i,g,1]/temp_val # for zero carcasses
    temp_sum2 = sum(prop_carc_2)
    prop_carc = matrix(prop_carc_2/temp_sum2, nrow=1)
  } #end if
  
  for(p in 1:length(alpha2_levels)){    
    ## probability distribution of lead per gp -- mitigation by a2
    ## amount   Equation 12
    prob_pb_no_pb_mit[i,p,1,1] = prob_pb_no_pb[i,1,1]*(1 - alpha2_levels[p]) + alpha2_levels[p]
    prob_pb_no_pb_mit[i,p,2:num_pb_levels,1] = prob_pb_no_pb[i,2:num_pb_levels,1]*(1 - alpha2_levels[p])
    #Equation 8
    prob_ingest = t(prop_carc) %*% drop(prob_pb_no_pb_mit[i,p,1:num_pb_levels,1])
  
    ## estimate mortality -- Equation 9
    mort_ratea2[i,g,p,1] = sum(prob_ingest * mort_prob)

    } #end  % for p
 } # end  % for g = 1:1:num_loc
} # end  % for i = 1:1;num_iters

  
#----------------------------------------------------------------------
# Calculate total deaths for state
#----------------------------------------------------------------------

toteagle = locations$X..100.km.blocks * (locations$eagles.per.km2 * 100)
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