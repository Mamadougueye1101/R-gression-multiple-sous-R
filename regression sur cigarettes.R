#vider la m�moire
rm(list=ls())

#charger les donn�es
#dans ce format, le s�parateur est tabulation, la premi�re ligne contient
#le nom des variables, le point d�cimal est ".", la premi�re colonne est 
#le nom des observations
#changement de dossier
setwd("C:/Users/math/Desktop/IA School/Cours IA 2022/cours de Math�matiques/TP4 Regression Multiple sous R")
cigarettes <- read.table(file="cigarettes_pour_regression.txt",sep="\t",header=TRUE,dec=".",row.names=1)

#V�rifications
#affichage des valeurs
print(cigarettes)

#nombre de lignes et de colonnes dans le data.frame
print(nrow(cigarettes))
print(ncol(cigarettes))

#Affichage des noms
print(rownames(cigarettes))
print(colnames(cigarettes))

#Stat. descriptives simples
print(summary(cigarettes))

#Nuages de points deux � deux
pairs(cigarettes)

#R�gression lin�aire multiple
modele <- lm(CO ~ TAR + NICOTINE + WEIGHT, data = cigarettes)

#objet summary
sm <- summary(modele)
print(sm)

#coefficients
print(sm$coefficients)

#print classe de $coefficients
print(class(sm$coefficients))

#dimensions
print(dim(sm$coefficients))

#ecarts-type des coefficients estim�s
print(sm$coefficients[,2])

#quantile de la loi de STudent
qs <- qt(0.975,24-3-1)

#bornes basses
print(sm$coefficients[,1]-qs*sm$coefficients[,2])

#bornes hautes
print(sm$coefficients[,1]+qs*sm$coefficients[,2])

#R�sidus
e <- modele$residuals #ou encore e <- residuals(modele)
print(mean(e))

#Graphique des r�sidus
plot(cigarettes$CO,e,ylab="R�sidus",xlab="CO")
abline(h=0)

#Droite de Henry
qqnorm(e)

#asym�trie
g1 <- mean(e^3)/(mean(e^2)^1.5)
print(g1)

#applatissement
g2 <- mean(e^4)/(mean(e^2)^2)-3
print(g2)

#test de Jarque-bera
Tjb <- ((24-3-1)/6)*(g1^2+(g2^2)/4)
print(Tjb)

#p-value du test de Jarque-Bera
print(pchisq(Tjb,2,lower.tail = FALSE))

#R�sidus studentis�s
res.student <- rstudent(modele)

#Seuil critique
#risque alpha = 0.1
alpha <- 0.1
#calcul du seuil � partir de la loi de Student � (n-p-2) ddl ==> n = 24 obs., p = 3 explicatives
seuil.student <- qt(1-alpha/2,24-3-2)
print(seuil.student)

#d�tection des cigarettes en dehors des tuyaux
#vecteur de bool�en indiquant les atypiques
atypiques.rstudent <- (res.student < -seuil.student | res.student > +seuil.student)
ab.student <- cigarettes[atypiques.rstudent,]
print(ab.student)

#mettre en �vidence les points atypiques dans le graphique des r�sidus
#construction du graphique des r�sidus studentis�s
plot(cigarettes$CO,res.student)
abline(h=-seuil.student)
abline(h=+seuil.student)
abline(h=0)
text(cigarettes$CO[atypiques.rstudent],res.student[atypiques.rstudent],rownames(cigarettes)[atypiques.rstudent])

#levier
indicateurs <- influence.measures(modele)

#quels sont les descripteurs disponibles
attributes(indicateurs)

#on s'int�resse � la matrice infmat
print(indicateurs$infmat)

#on r�cup�re la colonne "hat" qui correspond au levier
res.hat <- indicateurs$infmat[,"hat"]
print(res.hat)

#le seuil est d�fini par 2x(p+1)/n ==> p = 3 expl., n = 24 obs.
seuil.hat <- 2*(3+1)/24
print(seuil.hat)

#les points atypiques au sens du levier
atypiques.levier <- (res.hat > seuil.hat)
ab.hat <- cigarettes[atypiques.levier,]
print(ab.hat)

#supprimer les points atypiques de la base
#identifier les �l�ments � exclure
excluded <- (atypiques.rstudent | atypiques.levier)
print(excluded)

#nouveau data frame : on garde les non-exclus ==> !excluded
cigarettes.clean <- cigarettes[!excluded,]
print(cigarettes.clean)
print(dim(cigarettes.clean))

#Nouvelle r�gression
modele.clean <- lm(CO ~ ., data = cigarettes.clean)
sm.clean <- summary(modele.clean)
print(sm.clean)

#r�gression avec TAR seulement
modele.simplified <- lm(CO ~ TAR, data = cigarettes.clean)
sm.simplified <- summary(modele.simplified)
print(sm.simplified)

#F du test
FTest <- ((sm.clean$r.squared-sm.simplified$r.squared)/2)/((1-sm.clean$r.squared)/(21-3-1))
print(FTest)

#p-value
print(pf(FTest,2,21-3-1,lower.tail=FALSE))

#S�lection de variables
library(MASS)
modele.reduit <- stepAIC(modele.clean,direction="backward")
summary(modele.reduit)

#chargement du second fichier
autres <- read.table(file="autres_cigarettes.txt",sep="\t",header=TRUE,dec=".",row.names=1)

#Pr�diction
pred <- predict(modele.reduit,newdata=autres,interval="prediction",level=0.9)
print(pred)

#vraies valeurs de l'endogene
true_endo <- c(13.5,21.3,8.25,6.0)
names(true_endo) <- c("Benz","GoodLook","RiverPlate","Melia")

#verification
quid <- (true_endo >= pred[,'lwr']) & (true_endo < pred[,'upr'])
print(quid)

#data frame avec la pr�diction et les r�sidus
autres.plus <- cbind(autres,pred)
print(summary(autres.plus))

#sauvegarde
#write.table(autres.plus,file="output_regression.txt",quote=F, sep="\t",dec=".",row.names=T,col.names=T)
