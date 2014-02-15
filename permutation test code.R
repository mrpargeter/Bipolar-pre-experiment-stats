#PERMUTATION TESTS: EXPERT VS NOVICE METRICS
#LENGTH
length.combined<-c(r.cobble.data.novice.collected$length, r.cobble.data.expert.collected$length) #recombine the male and female glucose data sets

# create an observed difference
diff.means.length<-mean(r.cobble.data.novice.collected$length)-mean(r.cobble.data.expert.collected$length) #calculate the difference in their means for later comparison
number_of_permutations<- 100000  #set number of permutations
diff.random<- NULL #create vector to store results

for (i in 1:number_of_permutations) {
  # Sample from the combined dataset
  a.random = sample (length.combined, length(r.cobble.data.expert.collected$length), TRUE)
  b.random = sample (length.combined, length(r.cobble.data.novice.collected$length), TRUE)
  
  diff.random[i]<- mean(a.random) - mean(b.random)   # Null (permuated) difference
}

pvalue<- sum(abs(diff.random) >= abs(diff.means.length)) / number_of_permutations
print (pvalue)  # < 0.001
#BREADTH
breadth.combined<-c(r.cobble.data.expert.collected$breadth, r.cobble.data.novice.collected$breadth) #recombine the male and female glucose data sets
diff.means.breadth<-mean(r.cobble.data.novice.collected$breadth)-mean(r.cobble.data.expert.collected$breadth) #calculate the difference in their means for later comparison
number_of_permutations<- 100000  #set number of permutations
diff.random<- NULL #create vector to store results

for (i in 1:number_of_permutations) {
  # Sample from the combined dataset
  a.random = sample (breadth.combined, length(r.cobble.data.novice.collected$breadth), TRUE)
  b.random = sample (breadth.combined, length(r.cobble.data.expert.collected$breadth), TRUE)
  
  diff.random[i] = mean(b.random) - mean(a.random)   # Null (permuated) difference
}

pvalue<- sum(abs(diff.random) >= abs(diff.means.breadth)) / number_of_permutations
print (pvalue)  # 0.01176

#THICKNESS
thickness.combined<-c(r.cobble.data.expert.collected$thickness, r.cobble.data.novice.collected$thickness) #recombine the male and female glucose data sets
diff.means.thickness<-mean(r.cobble.data.novice.collected$thickness)-mean(r.cobble.data.expert.collected$thickness) #calculate the difference in their means for later comparison
number_of_permutations<- 100000  #set number of permutations
diff.random<- NULL #create vector to store results

for (i in 1:number_of_permutations) {
  # Sample from the combined dataset
  a.random = sample (thickness.combined, length(r.cobble.data.novice.collected$thickness), TRUE)
  b.random = sample (thickness.combined, length(r.cobble.data.expert.collected$thickness), TRUE)
  
  diff.random[i] = mean(b.random) - mean(a.random)   # Null (permuated) difference
}

pvalue<- sum(abs(diff.random) >= abs(diff.means.thickness)) / number_of_permutations
print (pvalue) # < 0.001

#MASS
mass.combined<-c(r.cobble.data.expert.collected$mass, r.cobble.data.novice.collected$mass) #recombine the male and female glucose data sets
diff.means.mass<-mean(r.cobble.data.novice.collected$mass)-mean(r.cobble.data.expert.collected$mass) #calculate the difference in their means for later comparison
number_of_permutations<- 100000  #set number of permutations
diff.random<- NULL #create vector to store results

for (i in 1:number_of_permutations) {
  # Sample from the combined dataset
  a.random = sample (mass.combined, length(r.cobble.data.novice.collected$mass), TRUE)
  b.random = sample (mass.combined, length(r.cobble.data.expert.collected$mass), TRUE)
  
  diff.random[i] = mean(b.random) - mean(a.random)   # Null (permuated) difference
}

pvalue<- sum(abs(diff.random) >= abs(diff.means.mass)) / number_of_permutations
print (pvalue) #< 0.001