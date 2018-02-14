
# Functions

# Function to retriece the p-value
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}


# Alteck Catchment area
catchment_area = 47 * 10000

rendement.soils = 1/0.24
rendement.waters = 1 

# Error propagation for graphs

# Cuve SD
initialDeltaError = 0.4

# Experiment 
initialDeltaErrorExtraction = 0.5

# Analytical
maxAnalyticalError = 0.5

# Mean of soil moisture conditions: 0.2 and 0.4
alteck = c(0.993, 1.000, -0.179, 1.184, 0.810, 0.732, 0.566, 0.831, 1.008, 0.704)
paddy = c(-1.468, 0.572, -0.033, 0.378, -0.297, -0.918, 1.500, -1.047, 0.052, -0.186)

if (round(mean(c(alteck)), 1) <= 0.5){
  meanshift = 0
} else {
  meanshift = round(mean(c(alteck)), 1)
}

# Soil methods - Experiment
error_soils = round(mean(c(alteck)), 1)
sd_SoilsShift = round( sd(c(alteck)), 1) 

propagatedError = (round((sd_SoilsShift^2 + # SD of extraction experiment
                           maxAnalyticalError^2 + # SD of analytical error during sample measurement 
                           initialDeltaErrorExtraction^2 # SD of the initial signature for the extraction experiment 
                            )^0.5, 1) + # 
                     error_soils )

# Shifts extraction water
#mQ = #  # Removed those at x150 

mQ = c(-0.118, -0.497, -0.203, -0.432, 1.062, -0.114)
envW = c(1.218, 0.966, 0.422, -0.131)

if (round(mean(c(envW, mQ)), 1) <= 0.5){
  meanshift_w = 0
} else {
  meanshift_w = round(mean(c(envW, mQ)), 1)
}

meanshift_miliQ = 0
prpagatedError_w = round((sd(c(mQ, envW))^2 + initialDeltaError^2)^0.5, 1) 

# Water methods reported
round(mean(c(mean(envW), mean(mQ) )), 1)

round(mean(sd(envW), sd(mQ) ), 1)

# Rac-metolachklor:
round( mean(c(0.01, 0.3)), 1) # means
mean(c(0.8, 0.4)) # SDs

# Initial signature measured in tank
# Pure and cuve isotope average
initialDelta = d13Co = round((-32.253 - meanshift_miliQ), 1)


# Ehssan values:
epsilon_mean= -1.5
dE <- 0.47
epsilon_max = epsilon_mean + dE 
epsilon_min = epsilon_mean - dE
epsilon_lab = epsilon_mean


# MEITE's enrichment experiment
# Ponderated initial concentration across replicas:
c_ini = 2.668 # ug/g
delta_ini = -31.3 # -30.913


# Calculated in Book 9.1 from Bulk signatures and bulk concentrations
epsilonField_max = -1.372 + 0.53 
epsilonField_min = -1.372 - 0.53  
epsilonField_mean = -1.372 # ± 0.53

# Closed system assumption applied to field for demonstration
epsilon_field = epsilonField_mean


# Literature DT50 values for S-met +- max/min


# Degradation experiment DT50 values +- CI
# SFO
# https://www.epa.gov/pesticide-science-and-assessing-pesticide-risks/degradation-kinetics-equations
median_half <- 21 
max_half <- 7.6
min_half <- 37.6  # Better to use 49.5 Wu et al., 2011

# http://ec.europa.eu/food/plant/pesticides/eu-pesticides-database/public/?event=activesubstance.ViewReview&id=381

# Median = field:
# https://sitem.herts.ac.uk/aeru/ppdb/en/Reports/1027.htm

# Min:
# http://www.pesticideinfo.org/Detail_Chemical.jsp?Rec_Id=PC36015

# Maximum ranges:
# Metolachlor half-life in soil ranges from 2.5 to 289 days (Sanyal & Kulshrestha, 1999; Wu et al., 2011) 
# according to soil management, edaphic factors and environmental conditions (Table 2).
# httpwww.scielo.brscielo.phpscript=sci_arttext&pid=S0100-83582014000300022