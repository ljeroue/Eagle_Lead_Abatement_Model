# Eagle_Lead_Abatement_Model
Determines mitigation effort needed for reducing eagle mortality from lead poisoning

## Reference
Cochrane J.F., E. Lonsdorf, T. D. Allison and C.A. Sanders-Reed. 2015. Modeling with 
uncertain science: estimation mitigation credits from abating lead poisoning in Golden 
Eagles. Ecological Applications, 25(6): p 1518 – 1533.

Link to the publication can be found [here](https://www.ncbi.nlm.nih.gov/pubmed/26552261)

## Summary of assumptions by model estimate

### Estimate Number of Gut Piles consumed per eagle
- Expected quantity consumed increases with increased quantity of gut piles per unit area and decreases with increased eagle density
- Uses a Type III functional response to model gut piles eaten per eagle
- Maximum number of gut piles ingested by one eagle in a month is five based on expert opinion
   Assumes gut piles are not divided between eagles
   Drawn from a ~poisson (0, 5)
- Accounted for uncertainty in scavenging rate by defining a minimum and maximum at any given gut pile density and drawing from a ~uniform(minimum, maximum)
   For the Min: k = 5, shape parameter = 3
   For the Max: k = 10, shape parameter = 1.5
   Where k is the half-saturation constant: available density of gut piles per eagle that would yield half the expected number of gut piles ingested in a 100 km2 area.

### Estimate blood lead concentration per gut pile
- Assumes eagles ingest lead in direct proportion to lead ammunition use/lead fragment abundance
- Uses Cauchy distribution- 2 parameters (mode and shape)
   Cauchy was chosen because it, “best describes the logic expressed by our experts” and its fat tails provide non-zero probabilities when a gut pile contains lead.
   Modified to account for 10 – 50% gut piles not containing lead when shot with lead ammunition by drawing randomly from a ~uniform (10, 50).
   Uncertainty in mode of lead absorption rate ranged from a maximum blood lead of 25 – 75 mg/dL.
   Accounted for by selecting from a ~uniform (25, 75).
   Set shape parameter to 25
   Set maximum increase in blood lead concentration per scavenge to 1000.
   From 0 to 1000 in 10 mg/dL increments that is 101 levels 

### Estimate days between scavenging
- This accounts for lead concentration decay in an eagle after a scavenging event
- They assume at least a 3 day lag time between scavenge events (minimum)
   Maximum lag time assumed to be between 7 and 30 days
      Maximum lag time based on max 5 gut piles/month and days in month
      Drawn from ~uniform (7,30)

### Estimate maximum blood lead by quantity of gut piles
•	Assume the average blood lead half-life to be between 10 – 20 days
o	Uncertainty accounted for by drawing from ~uniform(10, 20)
•	Calculate probability distribution of max blood lead concentration as a function of:
o	1) number of gut piles ingested, 
o	2) lag time between scavenge events, 
o	3) daily blood decay rate, and 
o	4) blood lead concentration increase per gut pile consumed

Estimate mortality by maximum blood lead
•	Model does not account for indirect sub-lethal effects with low blood lead concentrations
•	Concentrations > 100-120 mg/dL are associated with acute mortality
•	Uses a saturating curve
o	Shape parameter set to 2.5
o	Max mortality rate set to 1 (mortality cannot be larger than 1)
o	Allowed half-saturation constant to vary by random selection from a ~uniform(150,700) to account for expert uncertainty.
	Half-saturation constant is the concentration of lead in the blood that leads to a mortality rate of 50%

Integrating blood lead concentration
•	Expected mortality is influenced by availability of gut piles per eagle and amount of blood lead concentration increase per gut pile ingested.
o	Obtained with the joint probability - quantity of gut piles scavenged and blood lead concentration per gut pile consumed.
•	Quantity of eagle mortalities/area = population size * mortality rate
