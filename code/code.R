######################################################################################################################
# Header file
######################################################################################################################

#---------------------------------------------------------------------------------------------------------------------
### Setup
#---------------------------------------------------------------------------------------------------------------------
rm(list=ls())
options(scipen=999)
# options(java.parameters = "-Xmx10000m")
options(java.parameters = "-Xmx1g")



#---------------------------------------------------------------------------------------------------------------------
### Library
#---------------------------------------------------------------------------------------------------------------------
library(dplyr)
library(rgdal)
library(geoR)
library(fields)
library(ggplot2)
library(randomForest)
library(gbm)
library(cvTools)
library(caret)
library(akima)
library(Metrics)
library(GGally)
library(missForest)
library(e1071)
library(mice)
#---------------------------------------------------------------------------------------------------------------------
### Project folder path
#---------------------------------------------------------------------------------------------------------------------
repo_path = "Z:/smallproject"



######################################################################################################################
# Load Data
######################################################################################################################

#---------------------------------------------------------------------------------------------------------------------
### Data path
#---------------------------------------------------------------------------------------------------------------------

setwd(file.path(repo_path, "/data"))
#---------------------------------------------------------------------------------------------------------------------
### Load data
#---------------------------------------------------------------------------------------------------------------------

#######Create new data sets############################################

########Production data sets#########################################

production2011<-read.csv('04A OFM Daily Vols up to 2011 64937 Reformatted v1 05072015.csv')
production2012<-read.csv('04B OFM Daily Vols 2012 59844 Reformatted v1 05072015.csv')
production2013<-read.csv('04C OFM Daily Vols 2013 82297 Reformatted v2 05122015.csv')
production2014<-read.csv('04D OFM Daily Vols 2014 110862 Reformatted v1 05072015.csv')
production2015<-read.csv('04E OFM Daily Vols 2015 43937 Reformatted v1 05092015.csv')

##union five files together and choose only three variables####
production<-rbind(production2011,production2012,production2013,production2014,production2015)
production<-production[,c('well','Date','Condensate')]

##Convert string to date format############
production$Date<-as.Date(production$Date, "%m/%d/%Y")

##group by Well and order by Date##
production<-dplyr::group_by(production,well)
production<-dplyr::arrange(production,Date)

##Define a function to summarize first 180 days and filter zeros and negatives##
newsum<-function(Date,Condensate)
{
  
  index<-(Condensate>0&!is.na(Condensate))
  Date<-Date[index]
  Condensate<-Condensate[index]
  if(length(unique(Date))<180)
  {
    result<-0
  }
  else{
    index<- Date<=(unique(Date)[180])
    Condensate<-Condensate[index]
    result<-sum(Condensate)
  }
}

##Summarize the production of the first 180 days for each Well
sumproduction<-dplyr::summarize(production,Condensate=newsum(Date,Condensate))
sumproduction<-dplyr::filter(sumproduction,Condensate>0)


#######################Covariate Data sets######################

covariate<-read.csv('01a Initial Calibration PMDB 515 Reformatted 05072015.csv',na.strings='')
covariate<-filter(covariate,Producing_Formation=='BONE SPRING')

#################change the name of 'API_14' to 'well'
names(covariate)[4]<-'well'



######################Combine together############################

Bonespring<-inner_join(sumproduction,covariate,by='well')
write.csv(Bonespring,file='Updated data set without cleaning.csv')


#######################Cleaning the data########################


#Remove Date variables*7, duplicate variables*3 
Bonespring<-Bonespring[,c(1,6,7:16,18:29,32:41,46:90,94:98,2)]
Bonespring<-as.data.frame(Bonespring)

total<-dim(Bonespring)[1]
cols<-dim(Bonespring)[2]

############Delete unrepresented variables (miss proportion>0.1)####################

haha<-rep(0,cols-3)
for (i in 3:(cols-1))
{
  haha[i-2]=sum(is.na(Bonespring[,i]))/total
}
Bonespring<-Bonespring[,c(1,2,2+which(haha<0.1),cols)]

############Delete variables having only one value###########
cols<-dim(Bonespring)[2]
lala<-rep(0,cols-3)
for (i in 3:(cols-1))
{
  lala[i-2]=sd(Bonespring[,i],na.rm=TRUE)
}
Bonespring<-Bonespring[,c(1,2,2+which(lala>0),cols)]




newBonespring<-Bonespring
newBonespring<-newBonespring[,-c(19,21,24,25,28,29,30)]
cols<-dim(newBonespring)[2]

#write.csv(newBonespring,file='Updated data set.csv')
####Delete outliers########

#ggscatmat(newBonespring,columns=c(3:cols))

#for (i in c(3:(cols-1)))
#{
  
  #if((is.factor(newBonespring[,i])==FALSE)&(!names(newBonespring)[i]%in%c('Surface_Latitude','Surface_Longitude','Stages')))
  #{
  #  mediann<-median(newBonespring[,i],na.rm=TRUE)
  #  iqrr<-IQR(newBonespring[,i], na.rm = TRUE, type = 7)
  #  upper<-quantile(newBonespring[,i], na.rm = TRUE)[4]+3*iqrr
  #  lower<-quantile(newBonespring[,i], na.rm = TRUE)[2]-3*iqrr
  ##  index<-((newBonespring[,i]>upper)|(newBonespring[,i]<lower))&(!is.na(newBonespring[,i]))
  #  if (sum(index,na.rm=TRUE)>0)
  #  {
  #    print(names(newBonespring)[i])
  #    print(sum(index,na.rm=TRUE)/276)
  #  }
  #}  
#}


for (i in c(3:(cols-1)))
{
    
    if((is.factor(newBonespring[,i])==FALSE)&(!names(newBonespring)[i]%in%c('Surface_Latitude','Surface_Longitude')))
    {
      mediann<-median(newBonespring[,i],na.rm=TRUE)
      iqrr<-IQR(newBonespring[,i], na.rm = TRUE, type = 7)
      upper<-quantile(newBonespring[,i], na.rm = TRUE)[4]+2.1*iqrr
      lower<-quantile(newBonespring[,i], na.rm = TRUE)[2]-2.1*iqrr
      index<-((newBonespring[,i]>upper)|(newBonespring[,i]<lower))&(!is.na(newBonespring[,i]))
      if (sum(index,na.rm=TRUE)>0)
        {
        newBonespring[index,i]<-NA
        }
   }  
}


  
  
#ggscatmat(newBonespring,columns=c(3:cols))
#ggscatmat(newBonespring,columns=c(3:31,32))

#############Imputation for variables#########################

#hhh<-newBonespring[,3:(cols-1)]
#A<-missForest(hhh,variablewise = TRUE, verbose=TRUE)
#newBonespring[,3:(cols-1)]<-A$ximp
#A<-mice(newBonespring[,3:14],m=2,MaxNWts = 5000)
#B<-mice(newBonespring[,15:20],m=2,MaxNWts = 5000)
#C<-mice(newBonespring[,21:25],m=2,MaxNWts = 5000)
#D<-mice(newBonespring[,26:31],m=2,MaxNWts = 5000)
#newBonespring[,3:(cols-1)]<-cbind(complete(A),complete(B),complete(C),complete(D))



imp1<-lm(Total_Proppant_Pumped~Total_Fluid_Pumped,data=newBonespring)
imp2<-lm(Total_Fluid_Pumped~Total_Proppant_Pumped,data=newBonespring)
index1<-which(is.na(newBonespring$Total_Proppant_Pumped))
index2<-which(is.na(newBonespring$Total_Fluid_Pumped))
newBonespring$Total_Proppant_Pumped[index1]<-predict(imp1,newBonespring[index1,])
newBonespring$Total_Fluid_Pumped[index2]<-predict(imp2,newBonespring[index2,])

imp3<-lm(Ground_Elevation~ Surface_Latitude, data=newBonespring)
index3<-which(is.na(newBonespring$Ground_Elevation))
newBonespring$Ground_Elevation[index3]<-predict(imp3,newBonespring[index3,])

imp5<-lm(TD_MD~Bottom_Perf,data=newBonespring)
imp6<-lm(Bottom_Perf~TD_MD,data=newBonespring)
index5<-which(is.na(newBonespring$TD_MD))
index6<-which(is.na(newBonespring$Bottom_Perf))
newBonespring$TD_MD[index5]<-predict(imp5,newBonespring[index5,])
newBonespring$Bottom_Perf[index6]<-predict(imp6,newBonespring[index6,])

imp7<-lm(TD_TVD~Ground_Elevation ,data=newBonespring)
index7<-which(is.na(newBonespring$TD_TVD))
newBonespring$TD_TVD[index7]<-predict(imp7,newBonespring[index7,])

imp9<-lm(Stages~Total_Fluid_Pumped,data=newBonespring)
index9<-which(is.na(newBonespring$Stages))
newBonespring$Stages[index9]<-predict(imp9,newBonespring[index9,])

imp10<-lm(TD_MD~TD_TVD ,data=newBonespring)
index10<-which(is.na(newBonespring$TD_MD))
newBonespring$TD_MD[index10]<-predict(imp10,newBonespring[index10,])

imp11<-lm(Bottom_Perf~TD_MD,data=newBonespring)
newBonespring$Bottom_Perf[index10]<-predict(imp11,newBonespring[index10,])

index2<-which(is.na(newBonespring$CVA))
index<-which(complete.cases(newBonespring))
NewBonespring<-newBonespring[index,]
NewBonespring$CVA<-factor(NewBonespring$CVA)
random2<-randomForest(CVA~.,data=NewBonespring[,3:(cols-1)])
newBonespring$CVA[index2]<-predict(random2,newdata=newBonespring[index2,])



index4<-which(is.na(newBonespring$Drilling_Area))
index<-which(complete.cases(newBonespring))
NewBonespring<-newBonespring[index,]
NewBonespring$Drilling_Area<-factor(NewBonespring$Drilling_Area)
random4<-randomForest(Drilling_Area~.,data=NewBonespring[,3:(cols-1)])
newBonespring$Drilling_Area[index4]<-predict(random4,newdata=newBonespring[index4,])


index3<-which(is.na(newBonespring$Target))
index<-which(complete.cases(newBonespring))
NewBonespring<-newBonespring[index,]
NewBonespring$Target<-factor(NewBonespring$Target)
random3<-randomForest(Target~.,data=NewBonespring[,3:(cols-1)])
newBonespring$Target[index3]<-predict(random3,newdata=newBonespring[index3,])





#numna<-function(x)
#{sum(is.na(x))}
#sapply(newBonespring,numna)





for (i in 3:(cols-1))
{
  if(sum(is.na(newBonespring[,i]))>0)
  {
  if(is.factor(newBonespring[,i])==TRUE)
   {
    index<-is.na(newBonespring[,i]) 
    newBonespring[index,i]<-names(which.max(table(newBonespring[,i])))
    }else{
    index<-is.na(newBonespring[,i]) 
    newBonespring[index,i]<-mean(newBonespring[,i],na.rm=TRUE)  
    }
  }
}

write.csv(newBonespring,file='Updated data set.csv')




#NEWBonespring<-read.csv('xiaosvm.csv')
#newBonespring<-cbind(newBonespring[,1:2],NEWBonespring)
#newBonespring<-newBonespring[,-c(25)]



#################################################################################
##                          Machine Learning                             ########
#################################################################################



#########Add MAE to summary statitics#############
maeSummary <- function (data,
                        lev = NULL,
                        model = NULL) {
  out1 <- mae(data$obs, data$pred)  
  out2 <- mse(data$obs, data$pred)
  out3 <- rmse(data$obs, data$pred)
  out<-c(out1,out2,out3)
  names(out) <- c("MAE",'MSE','RMSE')
  out
}





##########Random Forest#####################
cols<-dim(newBonespring)[2]

fitControl <- trainControl(## 10-fold CV
  method = "cv",
  number = 10,
  summaryFunction = maeSummary,
  savePredictions=TRUE)

rfGrid <- expand.grid(mtry=35)

rfFit <- train(Condensate ~ ., data = newBonespring[,3:cols],
               method = "rf",
               tuneGrid=rfGrid,
               trControl = fitControl,
               ntree=220,
               verbose = FALSE,
               importance=TRUE)





set.seed(666)
mmm<-rep(0,100)
mmmm<-rep(0,100)
for (i in 1:100)
{
  
  rfGrid <- expand.grid(mtry=c(35))
  rfFit <- train(Condensate ~ ., data = newBonespring[,3:cols],
                 method = "rf",
                 tuneGrid=rfGrid,
                 trControl = fitControl,
                 ntree=220,
                 verbose = FALSE,
                 importance=TRUE)
  
  print(i)
  print(rfFit$results)
  mmm[i]<-as.numeric(rfFit$results[4])
  mmmm[i]<-as.numeric(rfFit$results[2])
  
}


plot(as.numeric(mmm)[1:50],type='l')
#RMSE=20683.21(274) m=35(10), ntree220, 2.1 outlier mean -19,21,24,25,28,29,30(Complete_stage_length + ALL average)
#MAE=15520.46(201) m=35(10), ntree=220, 2.1 outlier mean  -19,21,24,25,28,29,30(Complete_stage_leghjt + ALL average)



runRFRegCV <- function(dat, m, no.tree, k){
  
  folds <- cvFolds(nrow(dat), K=k)
  mse <- NULL;  pred <- NULL; sol <- NULL;
  
  for(i in 1:k){  
    # Split data into train/test set
    
    test  <- dat[folds$subsets[folds$which==i],]
    train <- dplyr::setdiff(dat, test)
    model <- randomForest(Condensate~., data=train[,3:cols], importance=T, mtry=m, ntree=no.tree)
     #####################################################################################################
    # Predict test dataset and calculate mse
    
    test.pred <- cbind(test[,c(2,cols)], Pred=predict(model,newdata=test[,3:(cols-1)])) 
    mse <- c(mse, sum((test.pred[,2]-test.pred[,3])^2)/nrow(test.pred))
    pred <- rbind(pred, test.pred)  # save prediction results for fold i
  }
  # CV results
  m <- model$mtry  # get default value of mtry
  sol <- data.frame(K=k, mse=mean(mse), rmse=sqrt(mean(mse)), m=m, n.Tree=no.tree)
  return(list(sol, pred))
}

#@@ 10-fold CV 
set.seed(1681)
rf <- runRFRegCV(dat=newBonespring,  m=10, no.tree=220, k=10)
predRF<- rf[[2]] 

#RMSE=22158.75 m=50(10), ntree=220,seed=1681



mmm<-rep(0,500)
for (i in 1:500)
{
  set.seed(i*5+1)
  rf <- runRFRegCV(dat=newBonespring,  m=10, no.tree=220, k=10)
  print(i)
  print(rf[[1]])
  mmm[i]<-rf[[1]][3]
}

##Plot########

plot(predRF[,3]~predRF[,2],xlab='Actual', ylab='Predicted',col='blue',main='Condensate 180 Day Cumulative Production:
Predicted vs. Actual')
abline(a=0,b=1)




#####################Boosting##################################

fitControl <- trainControl(## 10-fold CV
  method = "cv",
  number = 10,
  summaryFunction = maeSummary,
  savePredictions=TRUE)

gbmGrid <- expand.grid(interaction.depth=c(7),n.trees =5000, shrinkage=c(1)*0.01, 
                       n.minobsinnode=10)


gbmFit <- train(Condensate ~ ., data = newBonespring[,3:cols],
                method = "gbm",
                trControl = fitControl,
                tuneGrid=gbmGrid,
                verbose = FALSE)
set.seed(666)
mmm<-rep(0,400)
mmmm<-rep(0,400)
for (i in 1:400)
{
  
  gbmGrid <- expand.grid(interaction.depth=c(7),n.trees =c(50)*100, shrinkage=c(1)*0.01,  n.minobsinnode=10)
  gbmFit<- train(Condensate ~ ., data = newBonespring[,3:cols],
                 method = "gbm",
                 trControl = fitControl,
                 tuneGrid=gbmGrid,
                 verbose = FALSE)
  print(i)
  print(gbmFit$results)
  mmm[i]<-gbmFit$results[7]
  mmmm[i]<-gbmFit$results[5]
}


plot(as.numeric(mmm)[1:50],type='l')
#RMSE=19913.02(419.) interaction=7, ntree=5000,shringkage=0.01, 2.1 outlier, imputation -19,21,24,25,28,29,30
#MAE=15125.81(314.4) interaction=7, ntree=5000,shringkage=0.01, 2.1 outlier, imputation -19,21,24,25,28,29,30

predict(gbmFit,newBonespring[,3:(cols-1)])

runboostRegCV<- function(dat, no.tree, shrinkage, interaction, k)
{
  folds <- cvFolds(nrow(dat), K=k)
  mse <- NULL;  pred <- NULL; sol <- NULL;
  
  for(i in 1:k){  
    # Split data into train/test set
    
    test  <- dat[folds$subsets[folds$which==i],]
    train <- dplyr::setdiff(dat, test)
    model <- gbm(Condensate~., data=train[,3:cols], n.trees=no.tree, shrinkage=shrinkage,distribution='gaussian',interaction.depth=interaction) 
    #####################################################################################################
    
    # Predict test dataset and calculate mse
    test.pred <- cbind(test[,c(2,cols)], Pred=predict(model,newdata=test[,3:(cols-1)],n.trees<-no.tree))
    mse <- c(mse, sum((test.pred[,2]-test.pred[,3])^2)/nrow(test.pred))
    pred <- rbind(pred, test.pred)  # save prediction results for fold i
  }
  # CV results
  sol <- data.frame(K=k,mse=mean(mse), rmse=sqrt(mean(mse)),n.Tree=no.tree,shrinkage=shrinkage,interaction=interaction)
  return(list(sol, pred))
}
#@@ 10-fold CV
set.seed(6)
boost <- runboostRegCV(dat=newBonespring,  no.tree=5000, shrinkage=0.01,interaction=7,k=10)
predboost<- boost[[2]] 

#RMSE=23290.25 interaction=10, ntree=275,shringkage=0.01, seed=6

mmm<-rep(0,500)
for (i in 1:500)
{
  set.seed(i*5+1)
  boost <- runboostRegCV(dat=newBonespring,  no.tree=5000, shrinkage=0.01,interaction=7,k=10)
  print(i)
  print(boost[[1]])
  mmm[i]<-boost[[1]][3]
}


###Plot##########

plot(predboost[,3]~predboost[,2],xlab='Actual', ylab='Predicted',col='blue',main='Condensate 180 Day Cumulative Production:
Predicted vs. Actual')
abline(a=0,b=1)






##########################Surpport Vector regression#####################################

runsvmRegCV <- function(dat, k, nu, gamma, cost, epsilon ){
  
  folds <- cvFolds(nrow(dat), K=k)
  mse <- NULL;  pred <- NULL; sol <- NULL; mae=NULL
  
  for(i in 1:k){  
    # Split data into train/test set
    
    test  <- dat[folds$subsets[folds$which==i],]
    train <- dplyr::setdiff(dat, test)
    model <- svm(Condensate~., data=train[,3:cols], type='nu-regression', cost=cost, nu=nu, epsilon=epsilon, gamma=gamma)
    #####################################################################################################
    # Predict test dataset and calculate mse
    
    test.pred <- cbind(test[,c(2,cols)], Pred=predict(model,newdata=test[,3:(cols-1)]))
    test.pred[test.pred[,3]<0,3]<-350
    mae <- c(mae, sum(abs(test.pred[,2]-test.pred[,3]))/nrow(test.pred))
    mse <- c(mse, sum((test.pred[,2]-test.pred[,3])^2)/nrow(test.pred))
    pred <- rbind(pred, test.pred)  # save prediction results for fold i
  }
  # CV results
  sol <- data.frame(K=k, mse=mean(mse), rmse=mean(sqrt(mse)),mae=mean(mae))
  return(list(sol, pred))
}


mmm<-rep(0,100)
mmmm<-rep(0,100)
for (i in 1:100)
{
  SVM <- runsvmRegCV(dat=newBonespring, epsilon=0 , nu=0.92, cost=3.9, gamma=0.063, k=10)
  print(i)
  print(SVM[[1]])
  mmm[i]<-SVM[[1]][3]
  mmmm[i]<-SVM[[1]][4]
  
}
nnn<-as.numeric(mmm)
nnnn<-as.numeric(mmmm)
plot(nnn,type='l')


#RMSE=19977.7(355) 2.1 outlier imputation -19,21,24,25,28,29,30(Complete_stage_length + ALL average)
#MAE=14713.8(284)  2.1 outlier imputation  -19,21,24,25,28,29,30(Complete_stage_leghjt + ALL average)





mmm<-rep(0,100)
for (i in 1:100)
{
  
  A<-tune(svm, Condensate~., data=newBonespring[,3:cols], ranges = list(gamma =0.063, cost =3.9, nu=0.92,            
          type='nu-regression'),tunecontrol = tune.control(sampling = "cross",cross=10))
  #print(i)
  print(A$performance)
  mmm[i]<-A$performance[6]^0.5
  
}
nnn<-as.numeric(mmm)


#RMSE=21348.29(243.1) Xiao's data
#MAE=16276.63(194.4) Xiao's data









#######################Kriging###############################

runKriCV <- function(dat, k){
  
  folds <- cvFolds(nrow(dat), K=k)
  mse <- NULL;  pred <- NULL; sol <- NULL; mae<-NULL
  
  cord1.dec = SpatialPoints(cbind(dat$Surface_Longitude, dat$Surface_Latitude), proj4string=CRS("+proj=longlat"))
  cord1.UTM <- spTransform(cord1.dec, CRS("+proj=utm +north +zone=14"))
  dat$Longitude <- coordinates(cord1.UTM)[,1]
  dat$Latitude <- coordinates(cord1.UTM)[,2]
  
  for(i in 1:k){  
    # Split data into train/test set
    
    test  <- dat[folds$subsets[folds$which==i],]
    train <- dplyr::setdiff(dat, test)
    
    
    #####################################################################################################
    
    # Predict test dataset and calculate mse
    
    lookb=variog(coords=train[,c(12,11)],data=train[,cols],trend='1st')
     
    covpar<-variofit(lookb,kappa=0.5)
    if(covpar$cov.pars[2]==0) 
    {covpar$cov.pars[2]=0.01}
    model <- Krig(x=train[,c(12,11)],Y=train[,cols],theta=covpar$cov.pars[2],m=2) 
    test.pred <- cbind(test[,c(2,cols)], Pred=predict(model,as.matrix(test[,c(12,11)]))) 
    
    # Uwi, Target, Pred, Latitude, Longitude
    mse <- c(mse, sum((test.pred[,2]-test.pred[,3])^2)/nrow(test.pred))
    mae <- c(mae, sum(abs(test.pred[,2]-test.pred[,3]))/nrow(test.pred))
    pred <- rbind(pred, test.pred)  # save prediction results for fold i
  }
  # CV results
  sol <- data.frame(K=k,mse=mean(mse), rmse=sqrt(mean(mse)),mae=mean(mae))
  return(list(sol, pred))
  
}
set.seed(11)
Kri <- runKriCV(dat=newBonespring, k=10)
predKri<- Kri[[2]] 

#RMSE=26996 seed=11


set.seed(666)
mmm<-rep(0,100)
mmmm<-rep(0,100)
for (i in 1:100)
{
  set.seed(i)
  Kri <- runKriCV(dat=newBonespring, k=10)
  print(i)
  print(Kri[[1]])
  mmm[i]<-Kri[[1]][3]
  mmmm[i]<-Kri[[1]][4]
}

#RMSE=27580.22(482.3) 
#MAE=20771.61(317.65)


########Neural network#################################

runRegneuralCV <- function(dat, k, hidden, epochs){
  
  folds <- cvFolds(nrow(dat), K=k)
  mse <- NULL;  pred <- NULL; sol <- NULL;
  localH2O <- h2o.init()
  
  
  for(i in 1:k){  
    # Split data into train/test set
    
    Test  <- dat[folds$subsets[folds$which==i],]
    true  <- Test[,cols]
    Train <- dplyr::setdiff(dat, Test)
    Train <- as.h2o(Train[,3:cols],conn=localH2O)
    Test  <- as.h2o(Test[,3:(cols-1)],conn=localH2O)
    model <- h2o.deeplearning(x=1:22, y=23, training_frame=Train,activation = "TanhWithDropout",# or 'Tanh'
                              #input_dropout_ratio = 0.2, # % of inputs dropout
                              #hidden_dropout_ratios = c(0.5), # % for nodes dropout
                              #balance_classes = TRUE, 
                              hidden = hidden, # three layers of 50 nodes
                              reproducible=T,
                              #seed=4,
                              epochs = epochs) # max. no. of epochs)  
    
    #####################################################################################################
    
    # Predict test dataset and calculate mse
    test.pred <- cbind(true, Pred=as.matrix(predict(model,newdata=Test))) # Uwi, Target, Pred, Latitude, Longitude
    print(sqrt(sum((test.pred[,1]-test.pred[,2])^2)/nrow(test.pred)))
    mse <- c(mse, sum((test.pred[,1]-test.pred[,2])^2)/nrow(test.pred))
    pred <- rbind(pred, test.pred)  # save prediction results for fold i
  }
  # CV results
  sol <- data.frame(K=k,mse=mean(mse), rmse=sqrt(mean(mse)))
  return(list(sol, pred))
}

set.seed(4)
Neural <- runRegneuralCV(dat=newBonespring, k=10, hidden=c(50), epochs=5000)
predneural<- Neural[[2]] 


mmm<-rep(0,50)
for (i in 1:50)
{
  Neural <- runRegneuralCV(dat=newBonespring, k=10, hidden=c(50), epochs=100) 
  print(i)
  print(Neural[[1]])
  mmm[i]<-Neural[[1]][3]
}

nn.cv = function(data, r = 4, K = 10)
{

  localH2O = h2o.init()
  data = as.h2o(conn = localH2O, object = data)
  cat("r:", '\t', r, '\n')
  set.seed(r)
  folds10 = cvTools::cvFolds(n = dim(data)[1], K = 10, type = "random")
  rmse      = 0
  mae       = 0
  
  for(i in 1:K)
  {
    test.idx = folds10$subsets[folds10$which==i]
    test     = data[test.idx, ]
    train.idx <-dplyr::setdiff(1:dim(data)[1],test.idx)
    train    = data[train.idx, ]
    
    prostate.dl = h2o.deeplearning(x = 1:(dim(train)[2]-1), y = dim(train)[2], 
                                   training_frame = train,
                                   epochs = 100,
                                   seed = r,
                                   activation = "TanhWithDropout",
                                   hidden = c(240, 60, 40),
                                   #loss='MeanSquare',
                                   # input_dropout_ratio = 0.1,
                                   reproducible = T)
    # l1 = 0.0005,
    # l2 = 0.0005)
    
    test.y.pred = predict(prostate.dl, newdata = test[, 1:(dim(test)[2]-1)])
    
    delta.rmse = sqrt(mean((test.y.pred - test[, dim(test)[2]])^2))
    delta.mae  = mean(abs(test.y.pred - test[, dim(test)[2]]))
    cat(i, '\t', delta.rmse, '\n')
    rmse     = rmse + delta.rmse
    mae      = mae  + delta.mae
  }
  
  cat(rmse / K, '\t\t', mae / K, '\n')
  return(list(rmse / K, mae / K))
}

nn.cv(newBonespring[,3:cols])

##RMSE 24566(515)






######Summary#############

svm<-read.csv('xiaosvm.csv')

qRecCurv <- function(x) {
  
  x <- as.data.frame(na.omit(x))
  
  n.row.x <- nrow(x)  
  n.col.x <- ncol(x)  
  
  ranks <- x %>% dplyr::mutate_each(funs(row_number)) %>% dplyr::arrange(desc(Condensate))  # ranks for each col and then ordered by 1st col(true value)
  
  rec.q <- data.frame(matrix(-1, nrow = n.row.x , ncol = n.col.x))  # recover quantiles
  rec.q[1,] <- (ranks[1,] == n.row.x)
  for (i in 2:n.row.x)
  {
    #rec.q[i,] <- ranks %>% slice(1:i) %>% summarise_each (funs(sum(.<=i)/i))
    rec.q[i,] <- ranks %>% dplyr::slice(1:i) %>% dplyr::summarise_each (funs(sum(.>=(n.row.x-i+1))/i))
  }
  names(rec.q)[1]<- "True"
  rec.q[,1]<-1:n.row.x/n.row.x
  
  #row.names(rec.q) <- sapply(100*(1:n.row.x)/n.row.x,  FUN = function(x) paste("P",round(x,digits = 0),sep = ""))
  
  return(rec.q)
}  



#@@ Comparison of different model
# Prediction of  models (30 vars)
pred.boost<-dplyr::select(predboost,Well_Alias, Condensate, boost=Pred)
pred.RF<-dplyr::select(predRF, Condensate, RF=Pred)
#pred.svm<-dplyr::select(svm,Condensate,svm=Condensate.predict)
#pred.Kri<-dplyr::select(predKri,Condensate, Kri=Pred)


jo <- dplyr::left_join(pred.boost, pred.RF, by="Condensate")
#jo <- dplyr::left_join(jo,pred.svm,by='Condensate')
#jo <- dplyr::left_join(jo,pred.Kri,by='Condensate')
jo <- jo[,-1]  # rm Uwi

q.rec <- qRecCurv(jo) * 100

# Round to integer percentage
index <- ceiling(nrow(q.rec)*seq(0.3,100,0.3)/100)
q.rec <- q.rec[index, ]

q.rec1 <- q.rec %>% dplyr::select(True) %>% dplyr::mutate(RecRate=True, Method="Baseline")
q.rec2 <- q.rec %>% dplyr::select(True, X2) %>% dplyr::rename(RecRate=X2) %>% dplyr::mutate(Method="boost")
q.rec3 <- q.rec %>% dplyr::select(True, X3) %>% dplyr::rename(RecRate=X3) %>% dplyr::mutate(Method="RandomForest")
#q.rec4 <- q.rec %>% dplyr::select(True, X4) %>% dplyr::rename(RecRate=X4) %>% dplyr::mutate(Method="SVM")
#q.rec5 <- q.rec %>% dplyr::select(True, X5) %>% dplyr::rename(RecRate=X5) %>% dplyr::mutate(Method="Kriging")

q.rec <- dplyr::union(q.rec1, q.rec2)
q.rec <- dplyr::union(q.rec, q.rec3)
#q.rec <- dplyr::union(q.rec, q.rec4)
#q.rec <- dplyr::union(q.rec, q.rec5)


ggplot(q.rec, aes(x=True, y=RecRate, colour=Method, group=Method)) + 
  geom_line(lwd=1.2) +
  scale_color_manual(values=c("#fe506e", "black", "#228b22")) +
  xlab("Top Quantile Percentage") + ylab("Recovery Rate") + 
  theme(#legend.position="none",
    axis.title.x = element_text(size=24),
    axis.title.y = element_text(size=24),
    axis.text.x = element_text(colour="grey20",size=15),
    axis.text.y = element_text(colour="grey20",size=15),
    legend.title=element_blank(),
    legend.text = element_text(size = 20),
    legend.justification=c(1,0), legend.position=c(1,0),
    legend.background = element_rect(fill="gray90", size=.5, linetype="dotted")
  )
# plot(q.rec, type="l", xlab="Top Quantile Percentage", ylab="Recover Rate")
# lines(q.rec[,1],q.rec[,1], col="red")





##########


nnetfit <- train(Condensate ~ ., data=newBonespring[,3:cols], method="nnet", maxit=100, tuneGrid=mygrid,trControl=fitControl,
                linout=TRUE,MaxNWts = 10000) 



mygrid <- expand.grid(layer1=5,layer2=0,layer3=0,hidden_dropout=0,visible_dropout=0)
hhfit <- train(x=newBonespring[,21:24], y=newBonespring$Condensate, method="dnn",trControl=fitControl,tuneGrid=mygrid) 


jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

map<-get_map(location=c(left = -103.8, bottom = 31.4, right = -102.9, top = 31.9), zoom = 10, maptype='terrain') 
p<-ggmap(map, extent='normal')
p+geom_point(data = newBonespring, aes(x = Surface_Longitude, y = Surface_Latitude
  ),  alpha = 0.8,size=2)#+
scale_colour_gradientn(colours = jet.colors(7))
  


