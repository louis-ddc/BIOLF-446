Chi carré binôme: 

data = data.frame(matrix(nrow=5, ncol=2))
colnames(data) = c("comportements","présence")
data$comportements = as.factor(c("Couvain","Proie","Toilettage",
                             "Deplacement", "Inactivité"))
data$présence = c(295,	0,	0,	5,	0)

frequences_attendues = c(1/5, 1/5, 1/5, 1/5, 1/5)
valeurs_attendues = sum(data$présence)*frequences_attendues
chi2 = sum(((data$présence-valeurs_attendues)^2)/valeurs_attendues)
ddl = length(data$comportements)-1
pchisq(chi2, ddl, lower.tail=FALSE)


