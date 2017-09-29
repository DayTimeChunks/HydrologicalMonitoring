
rendement.soils = 1/0.24
rendement.waters = 1 

# Initial signature measured in tank
# Pure and cuve isotope average
initialDelta = d13Co = -32.253

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