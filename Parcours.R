Test chi² pour le choix du sexe de la blatte pour les garçons.
data = data.frame(matrix(nrow=2, ncol=2))
colnames(data) = c("garçon","sexe_blatte")
data$sexe_blatte = as.factor(c("mâle","femelle"))
data$nombresexe = c(3,8)
frequences_attendues = c(1/2, 1/2)
valeurs_attendues = sum(data$nombresexe)*frequences_attendues
chi2 = sum(((data$nombresexe-valeurs_attendues)^2)/valeurs_attendues)
ddl = 1
pchisq(chi2, ddl, lower.tail=FALSE)
//résultat : 0.131668
Test chi² pour le choix du sexe de la blatte pour les filles.
data = data.frame(matrix(nrow=2, ncol=2))
colnames(data) = c("filles","sexe_blatte")
data$sexe_blatte = as.factor(c("mâle","femelle"))
data$nombresexe = c(10,12)
frequences_attendues = c(1/2, 1/2)
valeurs_attendues = sum(data$nombresexe)*frequences_attendues
chi2 = sum(((data$nombresexe-valeurs_attendues)^2)/valeurs_attendues)
ddl = 1
pchisq(chi2, ddl, lower.tail=FALSE)
//résultat : 0.6698154

Expérience papillons 
tab1 = read.csv("tab2.csv", head=TRUE, sep=";")

performance = c(tab1[,1])
temps_projection = c(tab1[,2])
data.frame(performance,temps_projection)

o = ordered(tab1$temps, levels=c("une","deux","trois"))
o

boxplot(performance~o,
        xlab ="temps de projection (en seconde)", 
        ylab ="Performance", 
        main="Performance en fonction du temps de projection de l'image", 
        col="mistyrose3")

#-----------------------------------------------------------------------------#
#"papillon 2.
tab2 = read.csv("papi2.csv", head=TRUE, sep=";")

performance = c(tab2[,1])
nombre_papillon = c(tab2[,2])
data.frame(performance,nombre_papillon)

#o = ordered(tab2$temps, levels=c("0","1","2","3","4","5","6","7","8","9"))
#o

boxplot(performance~nombre_papillon,
        xlab ="nombre de papillons sur l'image", 
        ylab ="Performance", 
        main="Performance en fonction du nombre de papillons sur l'image", 
        col="mistyrose3")

myANOVA = aov(lm(performance ~ temps_projection))
qqnorm(residuals(myANOVA)); qqline(residuals(myANOVA))
shapiro.test(residuals(myANOVA))
bartlett.test(performance ~ temps_projection)

kruskal.test(performance ~ temps_projection) 
pairwise.wilcox.test(performance, temps_projection, p.adjust.method = "none", paired=FALSE)

#-------------------------------------------------------------"------------#
#2.3 Papi

tab3 = read.csv("papi3.csv", head=TRUE, sep=";")

performance = c(tab3[,1])
informations = c(tab3[,2])
data.frame(performance,informations)

#o = ordered(tab2$temps, levels=c("0","1","2","3","4","5","6","7","8","9"))
#o

boxplot(performance~informations,
        xlab ="Informations", 
        ylab ="Performance", 
        main="Performance en fonction du nombre d'informations sur l'image", 
        col="mistyrose3")


myANOVA = aov(lm(performance ~ informations))
qqnorm(residuals(myANOVA)); qqline(residuals(myANOVA))
shapiro.test(residuals(myANOVA))
bartlett.test(performance ~ informations)

kruskal.test(performance ~ informations) 
pairwise.wilcox.test(performance, informations, p.adjust.method = "none", paired=FALSE)

#-----------------------------------------------------#
#2.4 papi4 
tab4 = read.csv("papi4.csv", head=TRUE, sep=";")

performance = c(tab4[,1])
sexe = c(tab4[,2])
data.frame(performance,sexe)

#o = ordered(tab2$temps, levels=c("0","1","2","3","4","5","6","7","8","9"))
#o

boxplot(performance~sexe,
        xlab ="sexe", 
        ylab ="Performance", 
        main="Performance en fonction du sexe", 
        col="mistyrose3")


myANOVA = aov(lm(performance ~ sexe))
qqnorm(residuals(myANOVA)); qqline(residuals(myANOVA))
shapiro.test(residuals(myANOVA))
bartlett.test(performance ~ sexe)

kruskal.test(performance ~ sexe) 
pairwise.wilcox.test(performance, sexe, p.adjust.method = "none", paired=FALSE)
fisher.test(tab4)
