setwd("D:/MA1 2021-2022/BIOLF446 Comportement animal/TP comportement/Bourdon")
getwd()
tab1 = read.csv("bourdon1.csv", head=TRUE, sep=";")
ratio = c(tab1[,1])
comportements = c(tab1[,2])
data.frame(ratio,comportements)

#o = ordered(tab1$temps, levels=c("une","deux","trois"))
#o

boxplot(ratio~comportements,
        xlab ="Comportements", 
        ylab ="Ratio de la fréqeunce du comportement par rapport à l'esemble des comportements observés",
        main="Performance en fonction du temps de projection de l'image", 
        col="steelblue")

myANOVA = aov(lm(ratio ~ temps_projection))
qqnorm(residuals(myANOVA)); qqline(residuals(myANOVA))
shapiro.test(residuals(myANOVA))
bartlett.test(ratio ~ comportements)

kruskal.test(ratio ~ comportements) 
pairwise.wilcox.test(ratio, comportements, p.adjust.method = "none", paired=TRUE)
