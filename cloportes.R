#Code couleur général: 
mistyrose3

#Ligne de code pour les boxplots: attention modifier axes et titres en fonction:
boxplot(frequence~quartier, data = Bin, 
        xlab ="quartiers", 
        ylab ="Fréquence d'observation", 
        main="Fréquence d'observation des individus par quartiers", 
        col="mistyrose3")

#Code couleur pour les différents quartiers :
skyblue = AC
palegreen3 = AF 
mediumvioletred = QH
yellow2 = QS 

#Question 1 binôme
data = data.frame(matrix(nrow=4, ncol=2))
colnames(data) = c("quartiers","présence")
data$quartiers = as.factor(c("AC","AF","QH",
                                "QS"))
data$présence = c(3, 3, 1, 1)
frequences_attendues = c(1/4, 1/4, 1/4, 1/4)
valeurs_attendues = sum(data$présence)*frequences_attendues
chi2 = sum(((data$présence-valeurs_attendues)^2)/valeurs_attendues)
ddl = length(data$quartiers)-1
pchisq(chi2, ddl, lower.tail=FALSE)


#Question 1 classe 
data = data.frame(matrix(nrow=4, ncol=2))
colnames(data) = c("quartiers","présence")
data$quartiers = as.factor(c("AC","AF","QH",
                             "QS"))
data$présence = c(51,	43,	34,	22)
frequences_attendues = c(1/4, 1/4, 1/4, 1/4)
valeurs_attendues = sum(data$présence)*frequences_attendues
chi2 = sum(((data$présence-valeurs_attendues)^2)/valeurs_attendues)
ddl = length(data$quartiers)-1
pchisq(chi2, ddl, lower.tail=FALSE)

#Question 2.1 binôme 

data = data.frame(matrix(nrow=4, ncol=2))
colnames(data) = c("quartiers","présence")
data$quartiers = as.factor(c("AC","AF","QH",
                             "QS"))
data$présence = c(2/8,	19/8,	41/8,	2/8) #/8 c'est avec les moyennes 
frequences_attendues = c(1/4, 1/4, 1/4, 1/4)
valeurs_attendues = sum(data$présence)*frequences_attendues
chi2 = sum(((data$présence-valeurs_attendues)^2)/valeurs_attendues)
ddl = length(data$quartiers)-1
pchisq(chi2, ddl, lower.tail=FALSE)

#Quesiton 2.2 binome

data = data.frame(matrix(nrow=4, ncol=2))
colnames(data) = c("quartiers","présence")
data$quartiers = as.factor(c("AC","AF","QH",
                             "QS"))
data$présence = c(0, 3, 4, 1)
frequences_attendues = c(1/4, 1/4, 1/4, 1/4)
valeurs_attendues = sum(data$présence)*frequences_attendues
chi2 = sum(((data$présence-valeurs_attendues)^2)/valeurs_attendues)
ddl = length(data$quartiers)-1 
pchisq(chi2, ddl, lower.tail=FALSE)

#Quesiton 2.2 classe

data = data.frame(matrix(nrow=4, ncol=2))
colnames(data) = c("quartiers","présence")
data$quartiers = as.factor(c("AC","AF","QH",
                             "QS"))
data$présence = c(47,	51,	39,	20)
frequences_attendues = c(1/4, 1/4, 1/4, 1/4)
valeurs_attendues = sum(data$présence)*frequences_attendues
chi2 = sum(((data$présence-valeurs_attendues)^2)/valeurs_attendues)
ddl = length(data$quartiers)-1 
pchisq(chi2, ddl, lower.tail=FALSE)

col=c("cadetblue3","aquamarine3","coral3","darkgoldenrod3","cadetblue3","aquamarine3","coral3","darkgoldenrod3")
boxplot(frequence~sexe.quartier, data=exp1.1,
        main="Fréquence d'observation des individus mâle et femelle en fonction du quartier",
        col=c("skyblue","palegreen3","mediumvioletred","yellow2")
        )
#-------------------------------------------------#
tab2=exp1.3
frequence_obs = c(tab2[,1],tab2[,2],tab2[,3],tab2[,4])
n = dim(tab2)[1] 
quartier = colnames(tab2)[c(rep(1,n),rep(2,n),rep(3,n),rep(4,n))]
myANOVA = aov(lm(frequence_obs ~ quartier))
qqnorm(residuals(myANOVA)); qqline(residuals(myANOVA))
shapiro.test(residuals(myANOVA))
bartlett.test(frequence_obs ~ quartier)
kruskal.test(frequence_obs ~ quartier) 
boxplot(frequence_obs~quartier,
                xlab ="quartier", 
                ylab ="Fréquence d'observation", 
                main="Fréquence d'observation des individus par quartiers", 
                col="mistyrose3")
#-----------------------------------------------------#
#Binôme : 
data = data.frame(matrix(nrow=4, ncol=2))
colnames(data) = c("quartier","présence")
data$quartiers = as.factor(c("AC","AF","QH",
                             "QS"))
data$présence = c(0, 3, 1, 1)
frequences_attendues = c(1/4, 1/4, 1/4, 1/4)
valeurs_attendues = sum(data$présence)*frequences_attendues
chi2 = sum(((data$présence-valeurs_attendues)^2)/valeurs_attendues)
ddl = length(data$quartiers)-1
pchisq(chi2, ddl, lower.tail=FALSE)



#classe : 
tab3 = read.csv("2.3M.csv", head=TRUE, sep=";")
frequence_obs = c(tab3[,1],tab3[,2],tab3[,3],tab3[,4])
n = dim(tab3)[1] 
quartier = colnames(tab3)[c(rep(1,n),rep(2,n),rep(3,n),rep(4,n))]
myANOVA = aov(lm(frequence_obs ~ quartier))
qqnorm(residuals(myANOVA)); qqline(residuals(myANOVA))
shapiro.test(residuals(myANOVA))
bartlett.test(frequence_obs ~ quartier)
kruskal.test(frequence_obs ~ quartier) 
boxplot(frequence_obs~quartier,
        xlab ="quartier", 
        ylab ="Nombre de mâles à T30", 
        main="Nombre de mâles à T30 pour l'ensemble des binômes par quartier", 
        col="mistyrose3")
#----------------------------------------------------#
tab4 = read.csv("2.3F.csv", head=TRUE, sep=";")
frequence_obs = c(tab4[,1],tab4[,2],tab4[,3],tab4[,4])
n = dim(tab4)[1] 
quartier = colnames(tab4)[c(rep(1,n),rep(2,n),rep(3,n),rep(4,n))]
myANOVA = aov(lm(frequence_obs ~ quartier))
qqnorm(residuals(myANOVA)); qqline(residuals(myANOVA))
shapiro.test(residuals(myANOVA))
bartlett.test(frequence_obs ~ quartier)
one.way <- aov(frequence_obs ~ quartier)
summary(one.way)
kruskal.test(frequence_obs ~ quartier) 
boxplot(frequence_obs~quartier,
        xlab ="quartier", 
        ylab ="Nombre de femlles à T30", 
        main="Nombre de femelles à T30 pour l'ensemble des binômes par quartier", 
        col="mistyrose3")

