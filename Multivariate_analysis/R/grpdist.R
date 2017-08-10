grpdist <- function(x) {
  require (cluster)
  temp <- as.data.frame(x)
  temp[,1] <- as.factor(temp[,1])
  groupdist <- daisy(temp,"gower")
  return(groupdist)
}
