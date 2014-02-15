Bipolar-pre-experiment-stats
============================
  #Cobble splitting data from pre-experiments
  
  #Load r.cobble.data; r.cobble.data.experts.only; r.cobble.data.novice.only from dropbox
  
  class(r.cobble.data) #check the object: I will upload this .csv file to google drive
r.cobble.data.new<-arrange(r.cobble.data, desc(sample)) #load plyr package and sort 
r.cobble.data.random<-subset(r.cobble.data.new, sample == "random") #extract data based on sample type
r.cobble.data.collected<-subset(r.cobble.data.new, sample == "collected") #extract data based on sample type
r.cobble.data.expert<-subset(r.cobble.data, participant == "expert") #extract data based on participant
r.cobble.data.novice<-subset(r.cobble.data, participant == "novice")
r.cobble.data.expert.collected<-subset(r.cobble.data.expert, sample == "collected") #extract expert data based on sample type
r.cobble.data.expert.collected.new<-subset(r.cobble.data.experts.only, sample == "collected") #extract expert data based on sample type
r.cobble.data.expert.random<-subset(r.cobble.data.expert, sample == "random") #extract expert data based on sample type
r.cobble.data.novice.collected<-subset(r.cobble.data.novice, sample == "collected") #extract expert data based on sample type
r.cobble.data.novice.collected.new<-subset(r.cobble.data.novice.only, sample == "collected") #extract expert data based on sample type
r.cobble.data.novice.random<-subset(r.cobble.data.novice, sample == "random") #extract expert data based on sample type
summary(r.cobble.data.expert.collected) #or any of the data sets to check what class they are (data.frame etc)

#plot expert vs novice collected 
expert.vs.novice.length.plot<-ggplot(r.cobble.data.collected, aes(length, fill = participant)) + geom_density(alpha = 0.2)
expert.vs.novice.length.plot<-expert.vs.novice.length.plot + ggtitle("Expert vs Novice Collected Cobble Length") + xlab("p < 0.001")
expert.vs.novice.breadth.plot<-ggplot(r.cobble.data.collected, aes(breadth, fill = participant)) + geom_density(alpha = 0.2)
expert.vs.novice.breadth.plot<-expert.vs.novice.breadth.plot + ggtitle("Expert vs Novice Collected Cobble Breadth") + xlab("p < 0.05")
expert.vs.novice.thickness.plot<-ggplot(r.cobble.data.collected, aes(thickness, fill = participant)) + geom_density(alpha = 0.2)
expert.vs.novice.thickness.plot<-expert.vs.novice.thickness.plot + ggtitle("Expert vs Novice Collected Cobble Thickness") + xlab("p < 0.001")
expert.vs.novice.mass.plot<-ggplot(r.cobble.data.collected, aes(mass, fill = participant)) + geom_density(alpha = 0.2)
expert.vs.novice.mass.plot<-expert.vs.novice.mass.plot + ggtitle("Expert vs Novice Collected Cobble Mass") + xlab("p < 0.001")

#Plotting all of the expert vs novice GG plots together
library(ggplot2)
library(grid)
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

# 4 figures arranged in 2 rows and 2 columns
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))
print(expert.vs.novice.length.plot, vp = vplayout(1, 1))
print(expert.vs.novice.breadth.plot, vp = vplayout(1, 2))
print(expert.vs.novice.thickness.plot, vp = vplayout(2, 1))
print(expert.vs.novice.mass.plot, vp = vplayout(2, 2))

#plot collected vs random for experts
collected.vs.random.expert.length.plot<-ggplot(r.cobble.data.expert, aes(length, fill = sample)) + geom_density(alpha = 0.2)
collected.vs.random.expert.length.plot<-collected.vs.random.expert.length.plot + ggtitle("Expert Collected vs Random Lengths") + xlab("p < 0.001")
collected.vs.random.expert.breadth.plot<-ggplot(r.cobble.data.expert, aes(breadth, fill = sample)) + geom_density(alpha = 0.2)
collected.vs.random.expert.breadth.plot<-collected.vs.random.expert.breadth.plot + ggtitle("Expert Collected vs Random Breadths") + xlab("p < 0.001")
collected.vs.random.expert.thickness.plot<-ggplot(r.cobble.data.expert, aes(thickness, fill = sample)) + geom_density(alpha = 0.2)
collected.vs.random.expert.thickness.plot<-collected.vs.random.expert.thickness.plot + ggtitle("Expert Collected vs Random Thickness") + xlab("p < 0.001")
collected.vs.random.expert.mass.plot<-ggplot(r.cobble.data.expert, aes(mass, fill = sample)) + geom_density(alpha = 0.2)
collected.vs.random.expert.mass.plot<-collected.vs.random.expert.mass.plot + ggtitle("Expert Collected vs Random Mass") + xlab("p < 0.001")

#collected vs random expert ggplots plots
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))
print(collected.vs.random.expert.length.plot, vp = vplayout(1, 1))
print(collected.vs.random.expert.breadth.plot, vp = vplayout(1, 2))
print(collected.vs.random.expert.thickness.plot, vp = vplayout(2, 1))
print(collected.vs.random.expert.mass.plot, vp = vplayout(2, 2))

#plot collected vs random for novices
collected.vs.random.novice.length.plot<-ggplot(r.cobble.data.novice, aes(length, fill = sample)) + geom_density(alpha = 0.2)
collected.vs.random.novice.length.plot<-collected.vs.random.novice.length.plot + ggtitle("Novice Collected vs Random Lengths") + xlab("p < 0.001")
collected.vs.random.novice.breadth.plot<-ggplot(r.cobble.data.novice, aes(breadth, fill = sample)) + geom_density(alpha = 0.2)
collected.vs.random.novice.breadth.plot<-collected.vs.random.novice.breadth.plot + ggtitle("Novice Collected vs Random Breadths") + xlab("p < 0.001")
collected.vs.random.novice.thickness.plot<-ggplot(r.cobble.data.novice, aes(thickness, fill = sample)) + geom_density(alpha = 0.2)
collected.vs.random.novice.thickness.plot<-collected.vs.random.novice.thickness.plot + ggtitle("Novice Collected vs Random Thickness") + xlab("p < 0.001")
collected.vs.random.novice.mass.plot<-ggplot(r.cobble.data.novice, aes(mass, fill = sample)) + geom_density(alpha = 0.2)
collected.vs.random.novice.mass.plot<-collected.vs.random.novice.mass.plot + ggtitle("Novice Collected vs Random Mass") + xlab("p < 0.001")

#collected vs random novicce ggplots plots
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))
print(collected.vs.random.novice.length.plot, vp = vplayout(1, 1))
print(collected.vs.random.novice.breadth.plot, vp = vplayout(1, 2))
print(collected.vs.random.novice.thickness.plot, vp = vplayout(2, 1))
print(collected.vs.random.novice.mass.plot, vp = vplayout(2, 2))

#boxplot expert vs novice all together
par(mfrow=c(2,2))
boxplot.length.participant<-boxplot(length~participant,data=r.cobble.data.collected, main="Collected Cobble length", xlab="p < 0.001")
boxplot.breadth.participant<-boxplot(breadth~participant,data=r.cobble.data.collected, main="Collected Cobble breadth", xlab="p < 0.05")
boxplot.thickness.participant<-boxplot(thickness~participant,data=r.cobble.data.collected, main="Collected Cobble thickness", xlab="p < 0.001")
boxplot.mass.participant<-boxplot(mass~participant,data=r.cobble.data.collected, main="Collected Cobble mass", xlab="p < 0.001")

#Boxplots with outliers labelled-load CAR package
par(mfrow=c(2,2))
Boxplot(length~participant,data=r.cobble.data.collected, main="Expert vs Novice Collected Cobble Length", xlab="p-value = 1.291e-07", id.n=Inf)
Boxplot(breadth~participant,data=r.cobble.data.collected, main="Expert vs Novice Collected Cobble Breadth", xlab="p-value = 0.04525", id.n=Inf)
Boxplot(thickness~participant,data=r.cobble.data.collected, main="Expert vs Novice Collected Cobble Thickness", xlab="p-value = 5.999e-05", id.n=Inf)
Boxplot(mass~participant,data=r.cobble.data.collected, main="Expert vs Novice Collected Cobble Mass", xlab="p-value = 1.069e-06", id.n=Inf)

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

