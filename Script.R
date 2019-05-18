donne=read.table("~/Analyse de donn?es/Cencus_income_data_analysis-master/adult.data", header=TRUE)
donne=donne[,-c(3)]
donne=donne[1:32561,]
donne=na.omit(donne)




#pairs(donne)

donne$age.<-as.numeric(donne$age.)
donne$capital.loss.<-as.numeric(donne$capital.loss.)
donne$capital.gain.<-as.numeric(donne$capital.gain.)
donne$hoursperweek.<-as.numeric(donne$hoursperweek.)
donne$education.num.<-as.numeric(donne$education.num.)
donne.quanti=cbind(donne[,1],donne[,10:12])
donne.quali=cbind(donne[,2:3], donne[,5:9], donne[,13:14])
head(donne)
attach(donne)
summary(donne)
str(donne)
#table(donne)
dim(donne)
ligne=nrow(donne)


test=read.table("~/Analyse de donn?es/Cencus_income_data_analysis-master/adult.test", header=TRUE)
test=test[,-c(3)]
test=na.omit(test)

test$age.<-as.numeric(test$age.)
test$capital.loss.<-as.numeric(test$capital.loss.)
test$capital.gain.<-as.numeric(test$capital.gain.)
test$hoursperweek.<-as.numeric(test$hoursperweek.)
test$education.num.<-as.numeric(test$education.num.)

head(test)
attach(test)
summary(test)
str(test)
#table(donne)

ligne_test=nrow(test)






#chisq.test(donne)

#Classficiation NON supervis?e
library(FactoMineR)
#ACM + Kmeans
acm=MCA(donne.quali,quali.sup=9,graph=T)
plot(acm, choix="ind", invisible="var", habillage=9)
donne.quanti.acm=cbind(donne.quanti, acm$ind$coord[,1:2])
k=nlevels(donne[,14])
kmeans.acm <-  kmeans(na.omit(donne.quanti.acm), centers=k,nstart=100)
table(kmeans.acm$cluster,na.omit(donne[,14]))
plot(acm, choix="ind", invisible="var",col.ind=kmeans.acm$cluster)


#On teste pour plusieurs classes
inertie.intra <- rep(0,10)
for (k in 1:10){
  kmeans.acm <- kmeans(donne.quanti.acm,centers=k,nstart=100)
  inertie.intra[k] <- kmeans.acm$tot.withinss/kmeans.acm$totss
}
plot(1:10,inertie.intra,type="b",xlab="Nb. de groupes",ylab="% inertie intra")
title("M?thode ACM: Proportion d'inertie intra-classe en fonction du nombre de cluster")


# FAMD + Kmeans
famd=FAMD(donne[-c(4)])
plot(famd, choix="ind", lim.cos2.var = 0.7, habillage = 13 )
kmean.famd=kmeans(famd$ind$coord[,1:2], centers=2,nstart=100)
table(kmean.famd$cluster,donne[,14])


inertie.intra <- rep(0,10)
for (k in 1:10){
  kmean.famd <- kmeans(famd$ind$coord[,1:2],centers=k,nstart=100)
  inertie.intra[k] <- kmean.famd$tot.withinss/kmean.famd$totss
}
plot(1:10,inertie.intra,type="b",xlab="Nb. de groupes",ylab="% inertie intra")
title("m?thode FAMD: Proportion d'inertie intra-classe en fonction du nombre de cluster")

#CAH (avec famd)
donne_tronce_cah=donne[1:7000,]
famd_tronce_cah=FAMD(donne_tronce_cahe[-c(4)])
famd_tronce_kmean=kmeans(famd_tronce_cah$ind$coord[,1:2], centers=2)
cah.famd<-HCPC(famd_tronce_cah,nb.clust=2)

# comparaison des clustering obtenus avec kmeans et cah :
table(famd_tronce_kmean$cluster, cah.famd$data.clust$clust)
install.packages("mclust") 
library("mclust")
adjustedRandIndex(famd_tronce_kmean$cluster, cah.famd$data.clust$clust) #Calcule l'indice de Rand: indice calculant la similarit? entre 2 clustering
#randindex=0.96


#donne.qualitative=donne
# donne.qualitative[,1]<- cut(donne[,1],c(0,25,35,45,55,Inf),labels=c('1-25','26-35','36-45','46-55','56+'))
# donne.qualitative[,10]<- cut(donne[,10],c(0,2,50,Inf),labels=c('1','2-50','>50'))
# donne.qualitative[,11]<- cut(donne[,11],c(0,2,50,Inf),labels=c('1','2-50','>50'))
# donne.qualitative[,12]<- cut(donne[,12],c(0,15,35,40,50,70,Inf),labels=c('<15','16-35','36-40','41-50','51-70','>70'))



#Classification Supervis?e
#CART
library(rpart)
library(rpart.plot)
arbre=rpart(incomes~.,donne[c(-4)])
print(arbre)
summary(arbre)
rpart.plot(arbre,type=4)
arbre=rpart(incomes~.,donne,control=rpart.control(minsplit=500,cp=0))
rpart.plot(arbre, type=4)
plotcp(arbre)
arbre.opt=rpart(incomes~.,donne,control=rpart.control(cp=0.0043))
rpart.plot(arbre.opt,type=4)

#prediction
cart_pred= predict(arbre.opt, newdata=test,  type="prob")
cart_pred
class_cart= predict(arbre.opt, newdata=test,  type="class")
class_cart
cart_confus=table(class_cart,test$incomes)
cart_confus

#randomforrest
library(randomForest)
forrest <- randomForest(incomes~.,donne)
forrest
plot(forrest)

test <- rbind(donne[1, ] , test)
test <- test[-1,]

#prediction
forrest_pred <- predict(forrest, newdata=test, type="prob")
forrest_pred

forrest_class <- predict(forrest,newdata=test, type="response")
forrest_class

importance_var=importance(forrest)
importance_var

forrest_confus=table(forrest_class,test$incomes)
forrest_confus




#regression logistique
reg_logit <- glm(incomes ~ ., family = binomial , data=donne[c(-4)])
OR=exp(reg_logit$coefficients)
OR
res0 =glm(incomes ~ 1, family = "binomial", data=donne[c(-4)])
anova(res0,reg_logit,test="Chisq")
summary(reg_logit)
library(MASS)
res_AIC <- step(reg_logit,direction = "both")  
summary(res_AIC)
anova(res_AIC,reg_logit,test="Chisq")
OR=exp(res_AIC$coefficients)

library(car)
#crPlots(res_AIC)
#vif(res_AIC)
#par(mfrow=c(2,2))
#plot(res_AIC)

#abs(residuals(res_AIC,type="pearson"))[abs(residuals(res_AIC,type="pearson"))>2]
#abs(residuals(res_AIC,type="deviance"))[abs(residuals(res_AIC,type="deviance"))>2]

#infl = influence.measures(res_AIC)
#leviers = infl$infmat[,"hat"]
#p<-length(res_AIC$coefficients)
#n<-nrow(donne)
leviers[leviers > 3* p/n]

logit_pred=predict(res_AIC, newdata=test, type="response")  #donne proba d'être dans classe 1
logit_pred
class_logit=1*(pred>0.5)
class_logit

logit_confus=table(class_logit,test$incomes)
logit_confus


#comparaison des m?thodes

accuracy_cart=(cart_confus[1]+cart_confus[4])/16281
accuracy_forrest=(forrest_confus[1]+forrest_confus[4])/16281
accuracy_logit=(logit_confus[1]+logit_confus[4])/16281

#ROC
library(ROCR)
predictions_cart <- prediction(cart_pred[,2],  test$incomes)
predictions_forrest <- prediction(forrest_pred[,2],  test$incomes)
predictions_logit <- prediction(logit_pred,  test$incomes)
perf_cart <- performance(predictions_cart , "tpr", "fpr" )
perf_forrest <- performance(predictions_forrest, "tpr", "fpr" )
perf_logit <- performance(predictions_logit, "tpr", "fpr" )


plot(perf_cart)
plot(perf_forrest, add=TRUE, col=2)
plot(perf_logit, add=TRUE, col=3)
legend('bottom', col=1:5, paste(c('cart', 'random_forest', 'regression_logistique' )), lwd=1)


