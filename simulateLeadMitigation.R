simulateLeadMitigation <- structure(
  function(
    ### a dataframe containing harvest locations
    df, 
    # name of column in df containing harvest location name
    col_locationName,
    # name of column in df containing harvest location area in square km
    col_locationArea,
    # name of column in df containing total deer harvest at each location
    col_locationHarvest,
    # eagle density estimated at harvest location
    eagleDensity_km2,
    # proportion of total harvest left on the landscape to become available gut piles
    gutPileHarvest = 0.9){
    
    
    # Add useful variables
    harvestUnits$total_gutPiles <- harvestUnits$Harvest * gutPileHarvest
    harvestUnits$huntUnit_100km2 <- harvestUnits$Area_km2 / 100
    harvestUnits$eagles_km2 <- eagleDensity_km2
    
    num_loc <- length(harvestUnits$HuntUnit)
    
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
        ee = harvestUnits$eagles_km2[g] * 100 # = eagles/100 km block for specific location
        
        ## Scenarios of mitigation
        
        #----------------------------------------------------------------------
        # mitigation loop 
        #----------------------------------------------------------------------
        for(a in 1:length(alpha1_levels)){
          
          # gutpiles per 100 km2 blocks per eagle
          gp = (harvestUnits$total_gutPiles[g] / (harvestUnits$huntUnit_100km2[g] * ee)) * (1-alpha1_levels[a]) 
          
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
        gp = harvestUnits$total_gutPiles[g] /(harvestUnits$huntUnit_100km2[g] *ee)
        
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
  }  # end fx
)  # end structure
