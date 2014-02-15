#NON PARAMETRIC ANOVA
library(pgirmess)

#Expert vs novice
kruskalmc(length ~ participant,probs=0.001, data=r.cobble.data.collected) # <0.001
kruskalmc(breadth~ participant,probs=0.05, data=r.cobble.data.collected) # <0.05
kruskalmc(thickness ~ participant,probs=0.001, data=r.cobble.data.collected) # <0.001
kruskalmc(mass ~ participant,probs=0.001, data=r.cobble.data.collected) # <0.001

#collected versus random
kruskalmc(length ~ sample,probs=0.001, data=r.cobble.data) # <0.001
kruskalmc(breadth~ sample,probs=0.001, data=r.cobble.data) # <0.001
kruskalmc(thickness ~ sample,probs=0.001, data=r.cobble.data) # <0.001
kruskalmc(mass ~ sample,probs=0.001, data=r.cobble.data) # <0.001

#collectd versus random expert
kruskalmc(length ~ sample,probs=0.001, data=r.cobble.data.expert) # <0.001
kruskalmc(breadth~ sample,probs=0.001, data=r.cobble.data.expert) # <0.001
kruskalmc(thickness ~ sample,probs=0.001, data=r.cobble.data.expert) # <0.001
kruskalmc(mass ~ sample,probs=0.001, data=r.cobble.data.expert) # <0.001

#collectd versus random novice
kruskalmc(length ~ sample,probs=0.001, data=r.cobble.data.novice) # <0.001
kruskalmc(breadth~ sample, probs=0.001,data=r.cobble.data.novice) # <0.001
kruskalmc(thickness ~ sample,probs=0.001, data=r.cobble.data.novice) # <0.001
kruskalmc(mass ~ sample,probs=0.001, data=r.cobble.data.novice) # <0.001

#Comparisons within novice collected
kruskalmc(length ~ participant.number,probs=0.001, data=r.cobble.data.novice.collected.new) #for some reason the Expert data
kruskalmc(breadth ~ participant.number,probs=0.001, data=r.cobble.data.novice.collected.new)
kruskalmc(thickness~ participant.number,probs=0.001, data=r.cobble.data.novice.collected.new)
kruskalmc(mass ~ participant.number,probs=0.001, data=r.cobble.data.novice.collected.new)
#is in here too, but the result still shows

#Comparisons within expert collected
kruskalmc(length ~ participant.number,probs=0.001, data=r.cobble.data.expert.collected.new)
kruskalmc(breadth ~ participant.number,probs=0.001, data=r.cobble.data.expert.collected.new)
kruskalmc(thickness ~ participant.number,probs=0.001, data=r.cobble.data.expert.collected.new)
kruskalmc(mass ~ participant.number,probs=0.001, data=r.cobble.data.expert.collected.new)
