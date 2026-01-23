#============================================================
# Projet de Statistique non paramétrique: méthodes de base
#============================================================

# Libraries utilisées
#------------------------------------
library(sfsmisc)
library(symmetry)
library(rstatix)
library(FSA)
library(DescTools)
library(dgof)

student.old=read.csv("student_data.csv")
student= student.old[, c("sex", "internet", "studytime", "health", "age", "absences","failures", "G1","G2","G3")]
n= length(student[,1])
student$sex= as.factor(student$sex)

#=============================================
# Statistiques descriptives
#=============================================

# Note finale globale
#------------------------------------------------
boxplot(student$G3, ylab="G3",main= "Boxplot G3")
# Les donnes paraissent assez symetriques dans la médiane 10 - Donc, on peut utiliser le test de Wilcoxon pour un echantillon
shapiro.test(student$G3)
symmetry_test(student$G3, stat="MI")


#------------------------------------------
# Note finale par rapport à sexe
#------------------------------------------

boxplot(student$G3~student$sex, ylab= "G3", main= "Boxplot G3~ Age")
# On peut appliquer le test de Wilcoxon pour les differents sexes car c'est symetrique autour de la médiane


# Histogramme des garçons

G3.F=student$G3[student$sex == "F"]
G3.M= student$G3[student$sex == "M"]
n.F=length(G3.F)
n.M= length(G3.M)


hist(G3.F, breaks = seq(0, 20, by = 2), freq = FALSE, col = rgb(0, 0, 1, 0.4), xlim = c(0, 20), ylim = c(0, 0.12), main = "Distribution de G3 par sexe", xlab = "Note finale G3")
hist(G3.M,breaks = seq(0, 20, by = 2),freq = FALSE,col = rgb(1, 0, 0, 0.4),add = TRUE)
legend("topright",legend = c("Garçons", "Filles"),fill = c(rgb(0,0,1,0.4), rgb(1,0,0,0.4)))

symmetry_test(G3.M, stat="MI")
symmetry_test(G3.F, stat="MI")

shapiro.test(G3.M)
shapiro.test(G3.F)


par(mfrow=c(1,2))
hist(student$health, main= "Histogramme de health", xlab="Health", freq=F, col="red")
hist(student$studytime, main= "Histogramme de studytime", xlab="Studytime", freq=F, col="green")
hist(student$failures, main= "Histogramme de échecs", xlab="Échecs", freq=F, col="blue")
hist(student$absences, main= "Histogramme de Absences", xlab="Absences", freq=F, col="violet")



shapiro.test(student$health)
sahpiro.test(student$studytime)
shapiro.test(student$failures)
shapiro.test(student$absences)
dev.off()


#--------------------------------------------------------------------------
# Statistiques descriptives visuelles des notes (des données dependants)
#--------------------------------------------------------------------------
grades.long = data.frame(G1 = student$G1,G2 = student$G2,G3 = student$G3)
grades.long
grades.long = reshape(
  grades.long,
  varying = c("G1", "G2", "G3"),
  v.names = "Grade",
  times = c("G1", "G2", "G3"),
  direction = "long"
)
boxplot(grades.long$Grade~grades.long$time, ylab= "Final Grade")


#=========================================
# ECDF et son IC
#=========================================
hist(student$G3, freq=F)
lines(density(student$G3), col="red")
#La distribution des notes finales est discrète, bornée entre 0 et 20, et présente une forme asymétrique avec une possible multimodalité. Donc on essaye de modeliser cela par une distribution non parametrique
#Une modélisation paramétrique plus complexe, par exemple via un modèle de mélange, pourrait être envisagée, mais dépasse le cadre de ce travail.

plot(ecdf(student$G3))
ecdf.ksCI(student$G3)

# Si on veut ploter pour age
#plot(ecdf(student$age))
#ecdf.ksCI(student$age)


#==============================================
# Test de localisation pour un échantillon
#==============================================
# on veut tester si les notes de G3 sont au dessous ou endessous de 10
# On veut tester si la mediane >10
# Test binomiale (Test du signe)
#---------------------------------------
# Test exact
x=length(which(student$G3>10))
binom.test(n=n,x=x, p=0.5, alternative= "greater")
# Test appoximatif - version TCL
z= (x- n*0.5)/sqrt(n*0.5^2)
pnorm(z, lower.tail=F)

# On ne peut pas rejetter H0 à un niveau de 0.05

# Test du quantile
#---------------------------------------
# Maintenant on veut tester 60% on reussi
binom.test(n=n,x=x, p=0.4, alternative= "greater")

# Test de wilcoxon pour un echantillon
#---------------------------------------
# Le test de Wilcoxon a besoin d'une alternative plus stricte - symetrique autour du quantile souhaité
wilcox.test(student$G3, alternative="greater", mu=10, exacte=F)
# On rejette H0 et on dit que la pluspart des étudians on passe le test


# Test - t pour une moyenne
#-------------------------------------------

# Pour confirmer que les données ne sont pas normales

# Assume que les donnes suivent une loi normale - parametrique. On a visuelment vu ce que n'est pas le cas
t.test(student$G3, mu=10, alterantive="greater")
# On ne rejette pas H0 


# Faire la comparaison de la ERP entre le Wilcoxon et le t-test
# On veut maintenant faire la comparaisson entre ces deux tests par rapport à l'efficacité relative de Pitamn
# Parcontre comme vu au cours, l'ERP dependend de beacuoup de parametres inconnus comme alpha_n(theta), beta_n(theta), n1 et n2.
# Donc on essaye de voir l'asymptotic relative efficiency de ces deux tests.
# On sait que dasn la theorie que ARE(W,t) est toujours >0.864 (c'est-à dire q'au pire le test de Wilcoxon perd 14% de l'efficacité que le test t)
# Et sous la loi normale on a ARE(W,t)=3/pi. Comme vu anteriorment, les données ne sont pas normales (test de shapiro) mais elles sont symetriqes (symmetry test), donc l'hypothese du test de Wilcoxon est satisfaite et le'hypothese du t-test ne l'ai pas
# Donc, ça suggere que le test de Wilcoxon est au moins aussi efficace, voire plus efficace que le test t.



#====================================================
# Test de localisation pour 2 echantillons
#====================================================
# Notre but dans cette approche est de voir si les filles sont mieux que les garçons
# Test exact de Fisher
#---------------------------------------
# On veut maintenant analyser si les filles étudient plus que les garçons
# Version binaire
student$studytime.binary = cut(student$studytime, 
                             breaks = c(-Inf, 2.5, Inf), 
                             labels = c("Lower than 5", "5 or bigger"))
M=table(student$sex, student$studytime.binary)
fisher.test(M, alternative= "less")

dhyper(x=140,m=208, n= 187, k=303)
# Thus we can reject the null and say that girls study more than boys

# Test non binaire (pas dans le contenue du cours)
fisher.test(student$sex, student$studytime, alternative= "less")
# On peut bien rejetter
#----------------------------------------
# Test de kolmogorv Smirnov
#---------------------------------------
# Maintenant notre but est de voir si les garçons ont des meilleures resultats que les filles dans la note final à l'examen des mathematiques

ks.test(G3.F,G3.M, alternative="less")

# On ne peut pas rejetter l'hypothese nulle à un niveau de 0.05%
# Par contre le test de Kolmogorov Smirnov à cause de sa nature n'est pas bonne pour des donnes discretes car sa statitsique de test est ..
# Aussi au decalage decalage de localisations


# test de Wilcoxon et Mann- Whitney
#------------------------------------------
# On a vu dans les statistiques descriptives que les notes finales par rapport à sexe sont plutot symetriques. Donc, on peut appliquer le test de WMW. (ce test posera seulemnt probleme si la distribution est tres asymetrique)

wilcox.test(G3.F,G3.M, alternative="less")
# On peut rejetter l'hypothese nulle à un niveau 0.05%

# Test t pour 2 moyennes 
#-----------------------------------------------
shapiro.test(G3.F)
shapiro.test(G3.M)
# On rejette la normalité des données

t.test(G3.F,G3.M, alternative="less")
# Selon le test t, on peut rejetter l'hypothese nulle à un niveau 0.05%

# Comparer la ARE de Wilcoxon avec le test t - confirmer tes resultats par le cours theorique
# Puisque la symetrie est assure et la normalite ne l'est pas, alors le test de Wilcoxon est plus performant ou egale au test t

#=======================================================
# Test de localisation pour k - echantillons
#=======================================================


# Test de Krustal- Wallis
#-------------------------------------------
# Maintenant la question qui nous interessent maintenant est de voir si il existe une differnec significative entre les differentes distributions d'etude

kruskal.test(G3~studytime, data=student)
# Dans cet test on remarque qu'il exsite une differnec broderline etre les temps d'etude

# Maintenant on veut tester s'il existe une differnce significative entre ces qui ont raté ou pas
kruskal.test(G3~failures, data=student)
# On peut rejetter l'hypothese nulle et dire que les distributions par rapport à age ne sont pas égales
# Test de Dunn (ad hoc)
#-------------------------------------------
# Faisons maintenant un test de Dunn pour voir quelle est la distribution/distributions qui sont significativement differentes
dunnTest(G3~failures, data= student, method="holm")

# On remarque que toutes les combinaissons sont differentes. Donc, on peut dire que raté dans le passé est trés significative pour l'examen finale
#--------------------------------------------
# Test de Friedman
#--------------------------------------------
# Maintenant on veut tester si les notes G1, G2, G3 sont significativement differentes entre elles
# On peut utiliser le test de Friedman car on a des mesures repetées, donc ces mesures sont bien dependantes

friedman.test(Grade~time| id, data=grades.long )

# On ne peut pas rejetter l'hypothese nulle
# On ne peut pas affirmer que il existe une difference significative entre les notes des differntes periodes


#==================================================
# Test de dispersion pour 2 echantillons
#==================================================
#On sait par des anayses precedentes que les garçons ont des meilleures notes que les filles. Mais est ce qu'elles ont la meme dispersion.
# On veut maintenant tester si les dispersions des filles sont differntes que la dispersion des graçons, en d'autres mots on veut tester si sigma_F<sigma_M

# Comparer la robustesse des non parametrique par rapport au parametrique
#-------------------------------------
# Test parametrique de Fisher´
#-------------------------------------
# Pour ce test on suppose que les données sont normales - on a vu qu'ils ne sont pas

var.test(G3.F,G3.M)

# Resultat analytique
pf(var(G3.F)/var(G3.M),n.F-1,n.M-1)

# Selon ce test, on ne peut pas rejetter l'hypothese nulle et dire que c'est significativement different
# Donc, la difeference entre ces deux distributions porte sur la localisation. Mais attention que cet test se base sur la normalité


# Test de Siegel - Tukey
#-------------------------------------
SiegelTukeyTest(G3.F,G3.M)

# Test de Mood 
#-------------------------------------
mood.test(G3.F,G3.M)

# Test de Sukhatme
#-------------------------------------
# On centre les données de telle sorte que maintenant les donnes sont bien centrés et on peut aplliquer la distribution de Sukhatme
G3.F.center=G3.F -median(G3.F)
G3.M.center= G3.M - median(G3.M)

# Notre condition supplementaire est bien valide
median(G3.M.center)
median(G3.M.center)

# Il n'y a pas de test automatique, on doit la faire manuelment
S=0
for (i in 1: n.F){
  for (j in 1: n.M){
    if (G3.M.center[j] < G3.F.center[i] && G3.F.center[i] < 0) S= S+1
    if (0 < G3.F.center[i] && G3.F.center[i] < G3.M.center[j]) S= S+1
  }
  
}
(S-n.F*n.M/4)/sqrt(n.F*n.M*((n.F+n.M)+7)/48)

2*(1-pnorm(abs((S-n.F*n.M/4)/sqrt(n.F*n.M*((n.F+n.M)+7)/48))))

# Le test de Sukhatme rejette completement l'hypothese nulle, donc ça contrarie les conclusions des autres tests.
# Ceci peut etre du au fait de becaucoup d'egalites et des données discretes. Becoup de donneées sont 10 ou 11, donc notre statistique S ne prend pas ceux en compte
# Alors que les test de Siegel-Tukey et Mood prend les egalités en compte (en mettant des rangs ou des poids egales)
# Donc, puisque le test de Sukhatme a ce probleme, on met en evidence les conclusions des autres tests et on ne rejette pas H0

# Comparaison du ARE
# On siat par le cours theorique que ARE(Siegel, F)=0.608 ; ARE(Mood, F)=0.76; ARE(Sukhatme, F)=0.608
# Donc le test de Mood est superieurs au autres tests (plus proche de 1 le mieux)


#======================================
# Test d'ajustement
#=======================================

# Test chi- carré
#------------------------------------
# Maintenant on est interesse au niveau de santé des etudiants  
# A la main
frequences.health=table(student$health)
expected.health=n/5
frequences.health
Q.n= sum((frequences.health-expected.health)^2/expected.health)
1-pchisq(Q.n, df=5-1)

# En R
chisq.test(frequences.health, correct=T, p=rep(0.2,5))

# We reject the null and say that health does not follow a uniform distribution
# À partir de la table des frequnces on peut soupçonner qu'il y a plus de gens en santé que des gens qui ne sont pas en sante

#----------------------------------------
# Test de KS pour un echantillon
#---------------------------------------
# On peut faire pour soit continue, soit discret. On va confirmer le cas discret du chi-carre

ks.freq=c(rep(1,frequences.health[1]),rep(2,frequences.health[2]),rep(3,frequences.health[3]),rep(4,frequences.health[4]),rep(5,frequences.health[5]))
ks.expected=c(rep(1,expected.health),rep(2,expected.health),rep(3,expected.health),rep(4,expected.health),rep(5,expected.health))
dgof::ks.test(ks.freq, ks.expected, alternative="two.sided")
# On confirme bien le test chi-carrée et on rejette l'hypothese nulle, pourr dire que la distribution de health n'est pas uniforme



#=============================================
# Mesures d'association
#=============================================
# Maintenant la question qui nous interesse c'est est ce que les absences et les failures sont corrollées entre elles?
#--------------------------------------
# Tau de kendall + Test
#--------------------------------------
# À la main
s=0   
for (i in 1:(n-1)){
  for (j in (i+1):n){
    s= s+ sign(student$absences[i]-student$absences[j])*sign(student$failures[i]-student$failures[j])
      
  }
}
kendall.tau=s/choose(n,2)
# On a un tau de kendall de 0.04. On a une association positive entre les absences et les failures
# Mais il n'existe presque pas de correlation entre elles
# On doit faire aussi attention aux ties - observations egales
cor.test(student$absences, student$failures, method="kendall", alternative= "two.sided")

# ce test donne un résultat borderline

#------------------------------------------
# Rho de Spearman + Test
#-------------------------------------------
# A la main
rho.spearman=1-(6/(n*((n^2)-1)))*sum((rank(student$absences)- rank(student$failures))^2)
rho.spearman
# Ne marche pas, car on a beaucoup d'observations égales

cor.test(student$absences, student$failures, method="spearman", alternative= "two.sided")

# Ça donne borderline significatif. Donc, on peu dire que c'est borderline significatif

#===========================================
# Tables de contingence 
#===========================================
# Maintenant la question de notre etude est de savoir si avoir de ne pas avoir de l'internet est un facteur limitatif pour étudier

freq=table(student$studytime, student$internet)
freq
#---------------------------------------------
# Test d'independance
#---------------------------------------------

# À la main
expected.freq= matrix(nrow=4, ncol=2)
for (i in 1:4){
  for (j in 1:2){
    expected.freq[i,j]= sum(freq[i,])*sum(freq[,j])/n
  }
}
Q.2.n=sum((freq - expected.freq)^2 / expected.freq)

1-pchisq(Q.2.n, df=3)

# Automatique
study.internet=xtabs(~ studytime + internet, data = student)
summary(study.internet)

# On  ne peut pas rejetter H0, donc on ne peut pas affirmer que ces variables categorielles sont dependantes netre elles

#---------------------------------------------
# Coefficient d'association
#---------------------------------------------
# On veut maintenant calculer un coefficient de correlation entre ces deux variables categorielles. 

# À la main
phi.squared= Q.2.n/n
V.cramer= sqrt(phi.squared/min(41,2-1))
C.pearson= sqrt(Q.2.n/(Q.2.n+n))

# Automatique
Phi(study.internet)^2
CramerV(study.internet)
ContCoef(study.internet)

# Donc, on a une relation tres faible entre avoir internet et etudier. Donc, on peut soucçonner qu'avoir d'internet n'est pas un facteur determinant pour etudier



