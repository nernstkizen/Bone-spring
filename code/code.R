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
library(directlabels)
library(randomForest)
library(gbm)
library(cvTools)
library(caret)
library(akima)
library(stats)
library(Hmisc)


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
production2013<-read.csv('04C OFM Daily Vols 2013 82297 Reformatted v1 05072015.csv')
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
  n<-length(Date)
  limit=Date[1]+179
  if(Date[n]<=limit)
  {Condensate<-Condensate}
  else{Condensate<-Condensate[Date<=limit]}
  Condensate<-Condensate[Condensate>0]
  return(sum(Condensate))
}

##Summarize the production of the first 180 days for each Well
sumproduction1<-dplyr::summarize(production,Condensate=sum(Condensate))
sumproduction2<-dplyr::summarize(production,Condensate=newsum(Date,Condensate))



#######################Covariate Data sets######################

covariate<-read.csv('01a Initial Calibration PMDB 515 Reformatted 05072015.csv')
covariate<-filter(covariate,Producing_Formation=='BONE SPRING')

#################change the name of 'API_14' to 'well'
names(covariate)[4]<-'well'



######################Combine together############################

Bonespring<-inner_join(sumproduction2,covariate,by='well')










Bonespring<-read.csv('Bonespring.csv',na.strings='')
Bonespring<-Bonespring[,c(1,3,4:95,107)]
total<-dim(Bonespring)[1]
cols<-dim(Bonespring)[2]


haha<-rep(0,cols-3)
for (i in 3:(cols-1))
{
  haha[i-2]=sum(is.na(Bonespring[,i]))/total
}
Bonespring<-Bonespring[,c(1,2,2+which(haha<0.45),cols)]



cols<-dim(Bonespring)[2]
lala<-rep(0,cols-3)
for (i in 3:(cols-1))
{
  lala[i-2]=sd(Bonespring[,i],na.rm=TRUE)
}
Bonespring<-Bonespring[,c(1,2,2+which(lala>0),cols)]



cols<-dim(Bonespring)[2]
Bonespring<-Bonespring[-which(is.na(Bonespring[,cols])),]


newBonespring<-Bonespring
#newBonespring<-sapply(newBonespring,FUN=impute)
newBonespring<-Bonespring[complete.cases(newBonespring),]


cols<-dim(newBonespring)[2]
lala<-rep(0,cols-3)
for (i in 3:(cols-1))
{
  lala[i-2]=sd(newBonespring[,i],na.rm=TRUE)
}
newBonespring<-newBonespring[,c(1,2,2+which(lala>0),cols)]


write.csv(newBonespring,file='Updated data set.csv')


fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  repeats=2)


rfGrid <- expand.grid(mtry=c(3,10,50,100))


rfFit <- train(Q1_2015_Oil_EUR ~ ., data = newBonespring[,3:28],
               method = "rf",
               tuneGrid=rfGrid,
               trControl = fitControl,
               verbose = FALSE)

predict(rfFit,newBonespring[,3:27])



fitControl <- trainControl(## 10-fold CV
  method = "cv",
  number = 10)


gbmGrid <- expand.grid(interaction.depth=c(3),n.trees =c(500,1000,2000), shrinkage=c(1)*0.01, 
                       n.minobsinnode=10)


gbmFit <- train(Q1_2015_Oil_EUR ~ ., data = newBonespring[,3:28],
                method = "gbm",
                trControl = fitControl,
                tuneGrid=gbmGrid,
                verbose = FALSE)


predict(gbmFit,newBonespring[,3:27])




runKriCV <- function(dat, k){
  
  folds <- cvFolds(nrow(dat), K=k)
  mse <- NULL;  pred <- NULL; sol <- NULL;
  
  cord1.dec = SpatialPoints(cbind(dat$Bottom_Longitude, dat$Bottom_Latitude), proj4string=CRS("+proj=longlat"))
  cord1.UTM <- spTransform(cord1.dec, CRS("+proj=utm +north +zone=13"))
  dat$Bottom_Longitude <- coordinates(cord1.UTM)[,1]
  dat$Bottom_Latitude <- coordinates(cord1.UTM)[,2]
  
  for(i in 1:k){  
    # Split data into train/test set
    
    test  <- dat[folds$subsets[folds$which==i],]
    train <- dplyr::setdiff(dat, test)
    #####################################################################################################
    # Predict test dataset and calculate mse
    
    lookb=variog(coords=train[,c(9,8)],data=train[,28],trend='1st')
    #lookbc=variog(coords=train[,c(4,3)],data=train[,35],trend='2nd',bin.cloud=TRUE)
    #par(mfrow=c(2,2))
    #plot(lookb, main="binned variogram") 
    #plot(lookbc, bin.cloud=TRUE, main="clouds for binned variogram")  
    covpar<-variofit(lookb,kappa=0.5)
    if(covpar$cov.pars[2]==0) 
    {covpar$cov.pars[2]=0.01}
    model <- Krig(x=train[,c(9,8)],Y=train[,28],theta=covpar$cov.pars[2],m=2) 
    test.pred <- cbind(test[,c(2,28)], Pred=predict(model,as.matrix(test[,c(9,8)]))) 
    
    # Uwi, Target, Pred, Latitude, Longitude
    mse <- c(mse, sum((test.pred[,2]-test.pred[,3])^2)/nrow(test.pred))
    pred <- rbind(pred, test.pred)  # save prediction results for fold i
  }
  # CV results
  rmse <- sqrt(mse)
  sol <- data.frame(K=k, mse=mean(mse), mse.sd=sd(mse), rmse=mean(rmse), rmse.sd=sd(rmse))
  return(list(sol, pred))
}
set.seed(897)
Kri <- runKriCV(dat=newBonespring, k=10)
predKri<- Kri[[2]] 










missing<-function(Z)
{
  sum(is.na(Z))/length(Z)
}












#================================================================================================================================
# Universal Kriging Approach ###
#================================================================================================================================



