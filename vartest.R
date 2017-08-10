# Function vartest(), F. Gillet 11.11.2007
# Permutation tests of each explanatory variable with separate RDAs or CCAs

vartest = function(spe, env, method=c("rda","cca")) {
	testvar = data.frame(Variable=names(env), Variation=0, Perm=0, Prob=1, Sig="ns ")
	levels(testvar$Sig) = c("ns ", ".  ", "*  ", "** ", "***")
	for (i in 1:ncol(env)) {
		if (method == "rda") aa = rda(spe ~ ., as.data.frame(env[,i]))
		if (method == "cca") aa = cca(spe ~ ., as.data.frame(env[,i]))
		testvar$Variation[i] = 100 * sum(aa$CCA$eig) / aa$tot.chi
		pt = anova(aa, step=1000)
		testvar$Perm[i] = pt$N.Perm[1]
		testvar$Prob[i] = pt$Pr[1]
		if (pt$Pr[1] <= 0.1)   testvar$Sig[i] = ".  "
		if (pt$Pr[1] <= 0.05)  testvar$Sig[i] = "*  "
		if (pt$Pr[1] <= 0.01)  testvar$Sig[i] = "** "
		if (pt$Pr[1] <= 0.001) testvar$Sig[i] = "***"
	}
	testvar$Variation = round(testvar$Variation, 2)
	testvar$Prob = round(testvar$Prob, 3)
	testvar$Prob[testvar$Prob<0.001] = "< 0.001"
	testvar
}
