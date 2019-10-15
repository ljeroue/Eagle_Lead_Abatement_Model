# Eagle_Lead_Abatement_Model
Determines mitigation effort needed for reducing eagle mortality from lead poisoning based on Cochrane et al. (2015).

## Calculation
Expected mortality is influenced by availability of gut piles per eagle and amount of blood lead concentration 
increase per gut pile ingested. Mortality is obtained with the joint probability - quantity of gut piles scavenged and blood 
lead concentration per gut pile consumed.

<p align="center">
	<b> Expected Eagle Mortality = Population size x Mortality Rate </b>
</p>

The model incorporates two mitigation scenarios at several levels and uses proportional decreases in mitigation at levels 10% thru 100% at 10% increments.

Mitigation Scenarios:
1)	Reduce the number of gut piles containing lead
2)	Reduce lead in the gut piles (ie. convert hunters to non-lead shot)

## Model Input
A .csv file containing 
- hunt unit name
- hunt unit area
- total deer harvest for each respective hunt unit
- eagle density for each hunt unit
Deer harvest reports by county/hunt unit can be found on the Department of Natural Resources (DNR) website for each specific state.
For example, Iowa DNR Deer & Turkey Harvest Report can be found [here](https://gooutdoorsiowa.com/RealTimeHarvestReport.aspx)
Eagle density is not as easily available...


## Summary of assumptions by model estimate

### Estimate Number of Gut Piles consumed per eagle
- Expected quantity consumed increases with more gut piles per unit area and decreases with increased eagle density
- Uses a Type III functional response to model gut piles eaten per eagle
- Maximum number of gut piles ingested by one eagle in a month is five
  - Assumes gut piles are not divided between eagles
  - Drawn from a ~poisson (0, 5)
- Uncertainty in scavenging rate accounted for by defining a minimum and maximum at any given gut pile density and drawing from a ~uniform(minimum, maximum)
  - For the Min: k = 5, shape parameter = 3
  - For the Max: k = 10, shape parameter = 1.5
  - Where k is the half-saturation constant: available density of gut piles per eagle that would yield half the expected number of gut piles ingested in a 100 km2 area.

### Estimate blood lead concentration per gut pile
- Assumes eagles ingest lead in direct proportion to lead ammunition use/lead fragment abundance
- Uses Cauchy distribution with two parameters (mode and shape)
  - Cauchy was chosen because it, “best describes the logic expressed by our experts” and its fat tails provide non-zero probabilities when a gut pile contains lead.
  - Modified to account for 10 – 50% gut piles not containing lead when shot with lead ammunition by drawing randomly from a ~uniform (10, 50).
  - Uncertainty in mode of lead absorption rate ranged from a maximum blood lead of 25 – 75 mg/dL; accounted for by selecting from a ~uniform (25, 75).
  - Shape parameter set to 25
  - Maximum increase in blood lead concentration per scavenge set to 1000; therefore output ranges from 0 to 1000 in 10 mg/dL increments and has 101 levels 

### Estimate days between scavenging
- Accounts for lead concentration decay in an eagle after a scavenging event
- Model assumes eagles wait at least three days between scavenge events and between 7 - 30 days maximum.
  - Maximum lag time based on max 5 gut piles/month and days in month; Drawn from ~uniform (7,30)

### Estimate maximum blood lead by quantity of gut piles
- Assume the average blood lead half-life to be between 10 – 20 days; drawn from ~uniform(10, 20)
- Calculate probability distribution of max blood lead concentration as a function of:
  1. number of gut piles ingested, 
  2. lag time between scavenge events, 
  3. daily blood decay rate, and 
  4. blood lead concentration increase per gut pile consumed

### Estimate mortality by maximum blood lead
- Model does not account for indirect sub-lethal effects with low blood lead concentrations
- Concentrations > 100-120 mg/dL are associated with acute mortality
- Blood lead concentration follows a saturation cure with 
  - Shape parameter set to 2.5
  - Max mortality rate set to 1 (mortality cannot be larger than 1)
  - Allowed half-saturation constant to vary by random selection from a ~uniform(150,700) to account for expert uncertainty.
    - Half-saturation constant is the concentration of lead in the blood that leads to a mortality rate of 50%

## Reference
Cochrane J.F., E. Lonsdorf, T. D. Allison and C.A. Sanders-Reed. 2015. Modeling with 
uncertain science: estimation mitigation credits from abating lead poisoning in Golden 
Eagles. Ecological Applications, 25(6): p 1518 – 1533.

Link to the publication online [here](https://esajournals.onlinelibrary.wiley.com/doi/abs/10.1890/14-0996.1)
