mortalityRate <- function(
  inputData,
  gutRemoval,
  shotConversion,
  locations = FALSE,
  num_iters = 5000,
  alpha_levels = seq(0, 1, 0.1)){
  
  
  # Allow calculation over a select subset of locations from original dataset
  if(locations){
    regions <- inputData$loc_index[inputData[col_locationName] %in% locations]
  } else {
    regions <- inputData$loc_index
  }
  
  
  mortEagle_gut <- array(0, dim=c(num_iters, num_loc, length(alpha_levels)))
  mortEagle_shot <- array(0, dim=c(num_iters, num_loc, length(alpha_levels)))
  sum_mortEagle_gut <- array(0, dim=c(num_iters, length(alpha_levels)))
  sum_mortEagle_shot <- array(0, dim=c(num_iters, length(alpha_levels)))
  
  
  total_eagle = NULL
  for(m in regions){   #RUNS
    loc_index = which(inputData$loc_index == regions[m])
    eagle_sum =  inputData$total_eagles[inputData$loc_index == regions[m]] #total eagle at unit m
    total_eagle = rbind(total_eagle, eagle_sum)   
    for(i in 1:num_iters){
      for(a in 1:length(alpha_levels)){
        mortEagle_gut[i,m,a] = drop(t(gutRemoval[i,loc_index,a,1])) * eagle_sum
        mortEagle_shot[i,m,a] = drop(t(shotConversion[i,loc_index,a,1])) * eagle_sum
      } # end
    }# end
  }# end
  
  for(i in 1:num_iters){      #RUNS
    for(a in 1:length(alpha_levels)){
      sum_mortEagle_gut[i,a] = sum(mortEagle_gut[i,,a])
      sum_mortEagle_shot[i,a] = sum(mortEagle_shot[i,,a])
    }# end
  }# end 
  
  result <- list(mortRate_gutPile = sum_mortEagle_gut / sum(total_eagle),
                 mortRate_shotConversion = sum_mortEagle_shot / sum(total_eagle))
  return(result)
}