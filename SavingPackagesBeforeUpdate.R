
getwd()

tmp <- installed.packages()
installedpkgs <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])

setwd("D:/Documents/these_pablo/Alteckendorf2016/HydrologicalMonitoring")
save(installedpkgs, file="installed_old.rda")

setwd("/Users/DayTightChunks/Documents/PhD/HydrologicalMonitoring")
save(installedpkgs, file="installed_old_mac.rda")

# After laoding new R version

getwd()
setwd("D:/Documents/these_pablo/Alteckendorf2016/HydrologicalMonitoring")
load("installed_old.rda")
setwd("/Users/DayTightChunks/Documents/PhD/HydrologicalMonitoring")
load("installed_old_mac.rda")

tmp <- installed.packages()
installedpkgs.new <- as.vector(tmp[is.na(tmp[,"Priority"]), 1])
missing <- setdiff(installedpkgs, installedpkgs.new)
install.packages(missing)
update.packages()