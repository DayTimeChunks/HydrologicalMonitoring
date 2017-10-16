
rendement.soils = 1/0.24
rendement.waters = 1 

# Error propagation for graphs

# Cuve SD
initialDeltaError = 0.4

# Mean of soil moisture conditions: 0.2 and 0.4
alteck = c(0.993, 1.000, -0.179, 1.184, 0.810, 0.732, 0.566, 0.831, 1.008, 0.704)
paddy = c(-1.468, 0.572, -0.033, 0.378, -0.297, -0.918, 1.500, -1.047, 0.052, -0.186)

if (round(mean(c(alteck, paddy)), 1) <= 0.5){
  meanshift = 0
} else {
  meanshift = round(mean(c(alteck, paddy)), 1)
}

sd_alteckSoilShift= sd(alteck)

propagatedError = round((sd_alteckSoilShift^2 + initialDeltaError^2)^0.5, 1) 

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


# Initial signature measured in tank
# Pure and cuve isotope average
initialDelta = d13Co = round((-32.253 - meanshift_miliQ), 1)


# Ehssan values:
epsilon_mean= -1.476
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