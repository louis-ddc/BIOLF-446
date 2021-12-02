setwd("F:/MA1 2021-2022/Acquisition  des donées/TP bruches")
getwd()

#Chi^2: 

data = data.frame(matrix(nrow=7, ncol=2))
colnames(data) = c("legumineuses","oeufs")
data$legumineuses = as.factor(c("pois_chiche","haricot_rouge","niébé","lentille","aduki","mungo","boite"))
data$oeufs = c(5,1,8,0,8,7,5)
frequences_attendues = c(1/7, 1/7, 1/7, 1/7, 1/7, 1/7, 1/7)
valeurs_attendues = sum(data$oeufs)*frequences_attendues
chi2 = sum(((data$oeufs-valeurs_attendues)^2)/valeurs_attendues)
ddl = length(data$legumineuses)-1
pchisq(chi2, ddl, lower.tail=FALSE)
"j1 0.02337877 | j2 3.043214e-12  avec boite j1 0.04397964 | j2 2.090214e-15 "

tab1 = read.csv("BQ1C.csv", head=TRUE, sep=";")
head(tab1)
str(tab1)
tab2 = tab1; tab2[,] = NA # création d'un nouveau tableau aux dimensions de tab1
for (i in 1:dim(tab1)[1])
{
  tab2[i,] = tab1[i,]/sum(tab1[i,])
}
boxplot(tab2)

proportions_oeufs = c(tab2[,1],tab2[,2],tab2[,3],tab2[,4],tab2[,5],tab2[,6])
n = dim(tab2)[1] 
legumineuses = colnames(tab2)[c(rep(1,n),rep(2,n),rep(3,n),rep(4,n),rep(5,n),rep(6,n))]
myANOVA = aov(lm(proportions_oeufs ~ legumineuses))

qqnorm(residuals(myANOVA)); qqline(residuals(myANOVA))
shapiro.test(residuals(myANOVA))
# p-value = 0.001403 donc pas normalement distribuées 
bartlett.test(proportions_oeufs ~ legumineuses)
#p-value < 2.2e-16 donc hétéroscédasticité 
kruskal.test(proportions_oeufs ~ legumineuses) 
# p-value < 2.2e-16 donc différence statistiquement très signifcative entre les distributions 

shapiro.test(tab2[,"Aduki"])
#W = 0.96609, p-value = 0.1389
shapiro.test(tab2[,"Mungo"])
#W = 0.9565, p-value = 0.2143
t.test(tab2[,"Aduki"], tab2[,"Mungo"], paired=T)
#t = 3.5742, df = 19, p-value = 0.103

wilcox.test(tab2[,"Aduki"], tab2[,"Mungo"], paired=T)
#p-value = 0.004107


##Question2 diff niébé ##
#Chi^2
data = data.frame(matrix(nrow=8, ncol=2))
colnames(data) = c("legumineuses","oeufs")
data$legumineuses = as.factor(c("niebe_1","niebe_2","niebe_3","niebe_4",
                                "niebe_5","niebe_6","niebe_7","niebe_8"))
data$oeufs = c(8, 2, 1, 1, 0, 0, 0, 1) #jour2
data$oeufs = c(9,	0,	0,	0,	0,	0,	0,	1)
frequences_attendues = c(1/8,1/8,1/8,1/8,1/8,1/8,1/8,1/8) #jour1
valeurs_attendues = sum(data$oeufs)*frequences_attendues
chi2 = sum(((data$oeufs-valeurs_attendues)^2)/valeurs_attendues)
ddl = length(data$legumineuses)-1
pchisq(chi2, ddl, lower.tail=FALSE)
# 7.084505e-05 pour jour 2 et pour jour 1 1.133731e-09

#Tests de différences entre graines
tab1=read.csv("BQ2C.csv", head=T, sep=";")
#tab1=tab1[-c(17),]
tab2 = tab1; tab2[,] = NA 
for (i in 1:dim(tab1)[1])
{
  tab2[i,] = tab1[i,]/sum(tab1[i,])
}
boxplot(tab2)
proportions_oeufs = c(tab2[,1],tab2[,2],tab2[,3],tab2[,4],tab2[,5],tab2[,6],tab2[,7],tab2[,8])
n=dim(tab2)[1]
graines = colnames(tab2)[c(rep(1,n),rep(2,n),rep(3,n),rep(4,n),rep(5,n),rep(6,n),rep(7,n),rep(8,n))]
myANOVA = aov(lm(proportions_oeufs ~ graines))

qqnorm(residuals(myANOVA)); qqline(residuals(myANOVA))
shapiro.test(residuals(myANOVA))
#W = 0.77019, p-value = 3.692e-14 donc pas normalement distribuées 
bartlett.test(proportions_oeufs ~ graines)
#p-value = 8.491e-12
kruskal.test(proportions_oeufs ~ graines) 
#p-value = 0.06772






#Test pontes entre T1 et T2:
tab1 = RatioP
proportions_oeufs = c(tab1[,1],tab1[,2],tab1[,3],tab1[,4],tab1[,5],tab1[,6])
n = dim(tab1)[1] 
legumineuses = colnames(tab1)[c(rep(1,n),rep(2,n),rep(3,n),rep(4,n),rep(5,n),rep(6,n))]

boxplot(proportions_oeufs~legumineuses)

kruskal.test(proportions_oeufs~legumineuses)

#POUR TEST PAIRE: IMPORTANT
pairwise.wilcox.test(proportions_oeufs , legumineuses, p.adjust.method = "none", paired=TRUE)
#none : pas de correction






##Question3 parasite/vierge## 

data = data.frame(matrix(nrow=4, ncol=2))
colnames(data) = c("niebe","oeufs")
data$niebe = as.factor(c("niebe_vierge_1","niebe_vierge_2",
                         "niebe_parasite_1","niebe_parasite_2"))
data$oeufs = c(0, 0, 14, 17)
data = data.frame(matrix(nrow=2, ncol=2))
colnames(data) = c("niebe","oeufs")
data$niebe = as.factor(c("niebe_vierge","niebe_parasite"))
data$oeufs = c(0, 14+17)

frequences_attendues = c(1/2,1/2)
valeurs_attendues = sum(data$oeufs)*frequences_attendues
chi2 = sum(((data$oeufs-valeurs_attendues)^2)/valeurs_attendues)
ddl = length(data$niebe)-1
pchisq(chi2, ddl, lower.tail=FALSE)
#2.580284e-08

tab1 = read.csv("BQ3C.csv", head=TRUE, sep=";")
tab1 = tab1[-c(17,18),]

tab2 = matrix(nrow=dim(tab1)[1], ncol=2)
colnames(tab2) = c("niebe_vierge","niebe_parasite")
for (i in 1:dim(tab1)[1])
{
  tab2[i,"niebe_vierge"] = tab1[i,"niebe_vierge_1"]+tab1[i,"niebe_vierge_2"]       #erreur à ce stade 
  tab2[i,"niebe_parasite"] = tab1[i,"niebe_parasite_1"]+tab1[i,"niebe_parasite_2"]
}
for (i in 1:dim(tab2)[1])
{
  tab2[i,] = tab2[i,]/sum(tab2[i,])
}
boxplot(tab2)
proportions_oeufs = c(tab2[,1],tab2[,2])
n=dim(tab2)[1]
niebe = colnames(tab2)[c(rep(1,n),rep(2,n))]
myANOVA = aov(lm(proportions_oeufs ~ niebe))

qqnorm(residuals(myANOVA)); qqline(residuals(myANOVA))
shapiro.test(residuals(myANOVA))
#W = , p-value =  
bartlett.test(proportions_oeufs ~ niebe)
#p-value = 
kruskal.test(proportions_oeufs ~ niebe) 
#p-value = 


##Question4 perle##

data = data.frame(matrix(nrow=3, ncol=2))
colnames(data) = c("type","temps")
data$type = as.factor(c("temps_perle_marquee","temps_zone_neutre","temps_perle_vierge"))
data$temps = c(900, 540, 960)

frequences_attendues = c(1/3,1/3,1/3)
valeurs_attendues = sum(data$temps)*frequences_attendues
chi2 = sum(((data$temps-valeurs_attendues)^2)/valeurs_attendues)
ddl = length(data$temps)-1
pchisq(chi2, ddl, lower.tail=FALSE)
#0.3412978 9.727605e-29

tab1=read.csv("BQ4C.csv", head=T, sep=";")
tab2 = tab1; tab2[,] = NA 
for (i in 1:dim(tab1)[1])
{
  tab2[i,] = tab1[i,]/sum(tab1[i,])
}
boxplot(tab2)
proportions_zone = c(tab2[,1],tab2[,2],tab2[,3])
n=dim(tab2)[1]
zone = colnames(tab2)[c(rep(1,n),rep(2,n),rep(3,n))]
myANOVA = aov(lm(proportions_zone ~ zone))

qqnorm(residuals(myANOVA)); qqline(residuals(myANOVA))
shapiro.test(residuals(myANOVA))
#W = 0.95601, p-value = 0.03012 donc pas normalement distribuées 
bartlett.test(proportions_zone ~ zone)
#p-value = p-value = 0.05955
kruskal.test(proportions_zone ~ zone) 
# Kruskal-Wallis chi-squared = 3.7027, df = 2, p-value = 0.157
