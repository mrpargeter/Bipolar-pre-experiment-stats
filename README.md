Bipolar-pre-experiment-stats
============================

#Cobble splitting data from pre-experiments


class(r.cobble.data) #check the object: I will upload this .csv file to google drive
r.cobble.data.new<-arrange(r.cobble.data, desc(sample)) #load plyr package and sort 
r.cobble.data.random<-subset(r.cobble.data.new, sample == "random") #extract data based on sample type
r.cobble.data.collected<-subset(r.cobble.data.new, sample == "collected") #extract data based on sample type
r.cobble.data.expert<-subset(r.cobble.data, participant == "expert") #extract data based on participant
r.cobble.data.novice<-subset(r.cobble.data, participant == "novice")
r.cobble.data.expert.collected<-subset(r.cobble.data.expert, sample == "collected") #extract expert data based on sample type
r.cobble.data.expert.random<-subset(r.cobble.data.expert, sample == "random") #extract expert data based on sample type
r.cobble.data.novice.collected<-subset(r.cobble.data.novice, sample == "collected") #extract expert data based on sample type
r.cobble.data.novice.random<-subset(r.cobble.data.novice, sample == "random") #extract expert data based on sample type
summary(r.cobble.data.expert.collected) #or any of the data sets to check what class they are (data.frame etc)

#test for normality
shapiro.test(r.cobble.data.expert.collected$length) #YES
var(r.cobble.data.expert.collected$length)
qqnorm(r.cobble.data.expert.collected$length);qqline(r.cobble.data.expert.collected$length, col = 2)
shapiro.test(r.cobble.data.expert.collected$breadth) #YES
qqnorm(r.cobble.data.expert.collected$breadth);qqline(r.cobble.data.expert.collected$breadth, col = 2)
shapiro.test(r.cobble.data.expert.collected$thickness) #NO
qqnorm(r.cobble.data.expert.collected$thickness);qqline(r.cobble.data.expert.collected$thickness, col = 2)
shapiro.test(r.cobble.data.expert.collected$mass) #NO

shapiro.test(r.cobble.data.novice.collected$length) #YES
var(r.cobble.data.novice.collected$length)
shapiro.test(r.cobble.data.novice.collected$breadth) #YES
shapiro.test(r.cobble.data.novice.collected$thickness) #YES
shapiro.test(r.cobble.data.novice.collected$mass) #NO

#Skewness, kurtosis

skewness(r.cobble.data.expert.collected$length) #load moments package
skewness(r.cobble.data.expert.collected$breadth)
skewness(r.cobble.data.expert.collected$thickness)
skewness(r.cobble.data.expert.collected$mass)

kurtosis(r.cobble.data.expert.collected$length)
kurtosis(r.cobble.data.expert.collected$breadth)
kurtosis(r.cobble.data.expert.collected$thickness)
kurtosis(r.cobble.data.expert.collected$mass)

r.cobble.data.novices.collected<-subset(r.cobble.data.novice, sample == "collected") 
summary(r.cobble.data.novices.collected)

skewness(r.cobble.data.novice.collected$length)
skewness(r.cobble.data.novice.collected$breadth)
skewness(r.cobble.data.novice.collected$thickness)
skewness(r.cobble.data.novice.collected$mass)

kurtosis(r.cobble.data.novice.collected$length)
kurtosis(r.cobble.data.novice.collected$breadth)
kurtosis(r.cobble.data.novice.collected$thickness)
kurtosis(r.cobble.data.novice.collected$mass)

#get CV data for expert and novice collected
co.var<-function(x)( #function to calculate CV values
100*sd(x)/mean(x)
  )

co.var(r.cobble.data.expert.collected$length)
co.var(r.cobble.data.expert.collected$breadth)
co.var(r.cobble.data.expert.collected$thickness)

co.var(r.cobble.data.novice.collected$length)
co.var(r.cobble.data.novice.collected$breadth)
co.var(r.cobble.data.novice.collected$thickness)

#t-tests (random vs collected: ALL)
t.test(r.cobble.data.collected$length,r.cobble.data.random$length)
t.test(r.cobble.data.collected$breadth,r.cobble.data.random$breadth)
t.test(r.cobble.data.collected$thickness,r.cobble.data.random$thickness)
t.test(r.cobble.data.collected$mass,r.cobble.data.random$mass)

#plot collected vs random for both expert and novice
collected.plot.length<-ggplot(r.cobble.data.new, aes(length, fill = sample)) + geom_density(alpha = 0.2) #overlayed histograms
collected.plot.length + ggtitle("All Collected vs Random Lengths") + xlab("p-value = < 2.2e-16")
collected.plot.breadth<-ggplot(r.cobble.data.new, aes(breadth, fill = sample)) + geom_density(alpha = 0.2)
collected.plot.breadth + ggtitle("All Collected vs Random Breadths") + xlab("p-value = < 2.2e-16")
collected.plot.thickness<-ggplot(r.cobble.data.new, aes(thickness, fill = sample)) + geom_density(alpha = 0.2)
collected.plot.thickness + ggtitle("All Collected vs Random Thickness") + xlab("p-value = < 2.2e-16")
collected.plot.mass<-ggplot(r.cobble.data.new, aes(mass, fill = sample)) + geom_density(alpha = 0.2)
collected.plot.mass + ggtitle("All Collected vs Random Mass") + xlab("p-value = < 2.2e-16")

#PERMUTATION TEST
#LENGTH
breadths.combined<-c(r.cobble.data.novice.collected$breadth, r.cobble.data.expert.collected$breadth) #recombine the male and female glucose data sets

        # create an observed difference
diff.means.breadth<-mean(r.cobble.data.novice.collected$breadth)-mean(r.cobble.data.expert.collected$breadth) #calculate the difference in their means for later comparison
number_of_permutations<- 500000  #set number of permutations
diff.random<- NULL #create vector to store results

for (i in 1:number_of_permutations) {
        # Sample from the combined dataset
  a.random = sample (lengths.combined, length(r.cobble.data.expert.collected$breadth), TRUE)
  b.random = sample (lengths.combined, length(r.cobble.data.novice.collected$breadth), TRUE)
     
  diff.random[i]<- mean(a.random) - mean(b.random)   # Null (permuated) difference
}

pvalue<- sum(abs(diff.random) >= abs(diff.means.breadth)) / number_of_permutations
print (pvalue)  # P-value is the fraction of how many times the permuted difference is equal or more extreme than the observed difference

#BREADTH
breadth.combined<-c(r.cobble.data.expert.collected$breadth, r.cobble.data.novice.collected$breadth) #recombine the male and female glucose data sets
diff.means.breadth<-mean(r.cobble.data.novice.collected$breadth)-mean(r.cobble.data.expert.collected$breadth) #calculate the difference in their means for later comparison
number_of_permutations<- 1000000  #set number of permutations
diff.random<- NULL #create vector to store results

for (i in 1:number_of_permutations) {
  # Sample from the combined dataset
  a.random = sample (breadth.combined, length(r.cobble.data.novice.collected$length), TRUE)
  b.random = sample (breadth.combined, length(r.cobble.data.expert.collected$length), TRUE)
  
  diff.random[i] = mean(b.random) - mean(a.random)   # Null (permuated) difference
}

pvalue<- sum(abs(diff.random) >= abs(diff.means.lengths)) / number_of_permutations
print (pvalue)  # P-value is the fraction of how many times the permuted difference is equal or more extreme than the observed difference
#why get a zero result here????

#t-tests (random vs collected: Experts)
t.test(r.cobble.data.expert.random$length,r.cobble.data.expert.collected$length)
t.test(r.cobble.data.expert.random$breadth,r.cobble.data.expert.collected$breadth)
t.test(r.cobble.data.expert.random$thickness,r.cobble.data.expert.collected$thickness)
t.test(r.cobble.data.expert.random$mass,r.cobble.data.expert.collected$mass)

#plot collected vs random for experts
collected.vs.random.expert.length.plot<-ggplot(r.cobble.data.expert, aes(length, fill = sample)) + geom_density(alpha = 0.2)
collected.vs.random.expert.length.plot + ggtitle("Expert Collected vs Random Lengths") + xlab("p-value = 2.027e-10")
collected.vs.random.expert.breadth.plot<-ggplot(r.cobble.data.expert, aes(breadth, fill = sample)) + geom_density(alpha = 0.2)
collected.vs.random.expert.breadth.plot + ggtitle("Expert Collected vs Random Breadths") + xlab("p-value = 4.479e-11")
collected.vs.random.expert.thickness.plot<-ggplot(r.cobble.data.expert, aes(thickness, fill = sample)) + geom_density(alpha = 0.2)
collected.vs.random.expert.thickness.plot + ggtitle("Expert Collected vs Random Thickness") + xlab("p-value = 4.479e-11")
collected.vs.random.expert.mass.plot<-ggplot(r.cobble.data.expert, aes(mass, fill = sample)) + geom_density(alpha = 0.2)
collected.vs.random.expert.mass.plot + ggtitle("Expert Collected vs Random Mass") + xlab("p-value = 5.216e-12")

#t-tests (random vs collected: Novice)
t.test(r.cobble.data.novice.random$length,r.cobble.data.novice.collected$length)
t.test(r.cobble.data.novice.random$breadth,r.cobble.data.novice.collected$breadth)
t.test(r.cobble.data.novice.random$thickness,r.cobble.data.novice.collected$thickness)
t.test(r.cobble.data.novice.random$mass,r.cobble.data.novice.collected$mass)

#plot collected vs random for novices
collected.vs.random.novice.length.plot<-ggplot(r.cobble.data.novice, aes(length, fill = sample)) + geom_density(alpha = 0.2)
collected.vs.random.novice.length.plot + ggtitle("Novice Collected vs Random Lengths") + xlab("p-value = < 2.2e-16")
collected.vs.random.novice.breadth.plot<-ggplot(r.cobble.data.novice, aes(breadth, fill = sample)) + geom_density(alpha = 0.2)
collected.vs.random.novice.breadth.plot + ggtitle("Novice Collected vs Random Breadths") + xlab("p-value = < 2.2e-16")
collected.vs.random.novice.thickness.plot<-ggplot(r.cobble.data.novice, aes(thickness, fill = sample)) + geom_density(alpha = 0.2)
collected.vs.random.novice.thickness.plot + ggtitle("Novice Collected vs Random Thickness") + xlab("p-value = < 2.2e-16")
collected.vs.random.novice.mass.plot<-ggplot(r.cobble.data.novice, aes(mass, fill = sample)) + geom_density(alpha = 0.2)
collected.vs.random.novice.mass.plot + ggtitle("Novice Collected vs Random Mass") + xlab("p-value = < 2.2e-16")

#t-tests (expert vs novice collected)
t.test(r.cobble.data.novice.collected$length,r.cobble.data.expert.collected$length)
t.test(r.cobble.data.novice.collected$breadth,r.cobble.data.expert.collected$breadth)
t.test(r.cobble.data.novice.collected$thickness,r.cobble.data.expert.collected$thickness)
t.test(r.cobble.data.novice.collected$mass,r.cobble.data.expert.collected$mass)

#plot expert vs novice collected 
expert.vs.novice.length.plot<-ggplot(r.cobble.data.collected, aes(length, fill = participant)) + geom_density(alpha = 0.2)
expert.vs.novice.length.plot + ggtitle("Expert vs Novice Collected Cobble Length") + xlab("p-value = 1.291e-07")
expert.vs.novice.breadth.plot<-ggplot(r.cobble.data.collected, aes(breadth, fill = participant)) + geom_density(alpha = 0.2)
expert.vs.novice.breadth.plot + ggtitle("Expert vs Novice Collected Cobble Breadth") + xlab("p-value = 0.04525")
expert.vs.novice.thickness.plot<-ggplot(r.cobble.data.collected, aes(thickness, fill = participant)) + geom_density(alpha = 0.2)
expert.vs.novice.thickness.plot + ggtitle("Expert vs Novice Collected Cobble Thickness") + xlab("p-value = 5.999e-05")
expert.vs.novice.mass.plot<-ggplot(r.cobble.data.collected, aes(mass, fill = participant)) + geom_density(alpha = 0.2)
expert.vs.novice.mass.plot + ggtitle("Expert vs Novice Collected Cobble Mass") + xlab("p-value = 1.069e-06")

boxplot(length~participant,data=r.cobble.data.collected, main="Cobble length", xlab="p-value = 1.291e-07")
boxplot(breadth~participant,data=r.cobble.data.collected, main="Cobble breadth", xlab="p-value = 0.04525")
boxplot(thickness~participant,data=r.cobble.data.collected, main="Cobble thickness", xlab="p-value = 5.999e-05")

#Boxplots with outliers labelled-load CAR package
Boxplot(length~participant,data=r.cobble.data.collected, main="Expert vs Novice Collected Cobble Length", xlab="p-value = 1.291e-07", id.n=Inf)
Boxplot(breadth~participant,data=r.cobble.data.collected, main="Expert vs Novice Collected Cobble Breadth", xlab="p-value = 0.04525", id.n=Inf)
Boxplot(thickness~participant,data=r.cobble.data.collected, main="Expert vs Novice Collected Cobble Thickness", xlab="p-value = 5.999e-05", id.n=Inf)
Boxplot(mass~participant,data=r.cobble.data.collected, main="Expert vs Novice Collected Cobble Mass", xlab="p-value = 1.069e-06", id.n=Inf)

#plot variables against each other (experts)
expert.length.thickness.plot <- ggplot(r.cobble.data.expert.collected, aes(x = length, y = thickness)) +
  geom_point() #ggplot of length vs thickness for collected expert cobbles
expert.length.thickness.plot + ggtitle("Expert Collected Cobble Length vs. Thickness") + xlab("r2 = 0.3643")
expert.length.breadth.plot <- ggplot(r.cobble.data.expert.collected, aes(x = length, y = breadth)) +
  geom_point()
expert.length.breadth.plot + ggtitle("Expert Collected Cobble Length vs. Breadth") + xlab("r2 = 0.633")
expert.thickness.breadth.plot <- ggplot(r.cobble.data.expert.collected, aes(x = thickness, y = breadth)) +
  geom_point()
expert.thickness.breadth.plot + ggtitle("Expert Collected Cobble Thickness vs. Breadth") + xlab("r2 = 0.46")

#Regression
#expert
expert.length.breadth.lm <- lm(length~breadth, data=r.cobble.data.expert.collected)
summary(expert.length.breadth.lm)
plot(r.cobble.data.expert.collected$length, r.cobble.data.expert.collected$breadth)
abline(expert.length.breadth.lm)
identify(x=r.cobble.data.expert.collected$length, y=r.cobble.data.expert.collected$breadth, labels=rownames(Duncan))

expert.length.thickness.lm <- lm(length~thickness, data=r.cobble.data.expert.collected)
summary(expert.length.thickness.lm)
plot(r.cobble.data.expert.collected$length, r.cobble.data.expert.collected$thickness)
abline(0,1)

expert.breadth.thickness.lm <- lm(breadth~thickness, data=r.cobble.data.expert.collected)
summary(expert.breadth.thickness.lm)
plot(r.cobble.data.expert.collected$breadth, r.cobble.data.expert.collected$thickness)
abline(0,1)

#Regression novice
novice.length.breadth.lm <- lm(length~breadth, data=r.cobble.data.novice.collected)
summary(novice.length.breadth.lm)
plot(r.cobble.data.novice.collected$length, r.cobble.data.novice.collected$breadth)
abline(novice.length.breadth.lm)

novice.length.thickness.lm <- lm(length~thickness, data=r.cobble.data.novice.collected)
summary(novice.length.thickness.lm)
plot(r.cobble.data.novice.collected$length, r.cobble.data.novice.collected$thickness)
abline(0,1)

novice.breadth.thickness.lm <- lm(breadth~thickness, data=r.cobble.data.novice.collected)
summary(novice.breadth.thickness.lm)
plot(r.cobble.data.novice.collected$breadth, r.cobble.data.novice.collected$thickness)
abline(0,1)

#plot variables against each other (novices)
novice.length.thickness.plot <- ggplot(r.cobble.data.novice.collected, aes(x = length, y = thickness)) +
  geom_point() #ggplot of length vs thickness for collected expert cobbles
novice.length.thickness.plot + ggtitle("Novice Collected Cobble Length vs. Thickness") + xlab("r2 = 0.5556")
novice.length.breadth.plot <- ggplot(r.cobble.data.novice.collected, aes(x = length, y = breadth)) +
  geom_point()
novice.length.breadth.plot + ggtitle("Novice Collected Cobble Length vs. Breadth") + xlab("r2 = 0.5947")
novice.thickness.breadth.plot <- ggplot(r.cobble.data.novice.collected, aes(x = thickness, y = breadth)) +
  geom_point()
novice.thickness.breadth.plot + ggtitle("Novice Collected Cobble Thickness vs. Breadth") + xlab("r2 = 0.4561")

#Scatterplot matrices
#expert collected
library(gclus)
dta <- r.cobble.data.expert.collected[c(5,6,7,8)] # get data 
dta.r <- abs(cor(dta)) # get correlations
dta.col <- dmat.color(dta.r) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
dta.o <- order.single(dta.r) 
cpairs(dta, dta.o, panel.colors=dta.col, gap=.5,
       main="Expert collected cobbles" )

#novice collected
dta.n<- r.cobble.data.novice.collected[c(5,6,7,8)] # get data 
dta.n.r <- abs(cor(dta)) # get correlations
dta.n.col <- dmat.color(dta.r) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
dta.n.o <- order.single(dta.r) 
cpairs(dta.n, dta.n.o, panel.colors=dta.n.col, gap=.5,
       main="Novice collected cobbles" )

