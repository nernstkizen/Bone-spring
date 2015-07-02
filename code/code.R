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

#Bonespring<-Bonespring[,c(1,6,59,38,49,53,40,55,29,37,30,56,63,66,52,64,50,66,29,61,60,31,36,94,16,41,10,13,35,2)]
Bonespring<-Bonespring[,c(1,6,7:16,18:41,46:90,94:98,2)]
Bonespring<-as.data.frame(Bonespring)


total<-dim(Bonespring)[1]
cols<-dim(Bonespring)[2]

############Delete unrepresented variables####################
haha<-rep(0,cols-3)
for (i in 3:(cols-1))
{
  haha[i-2]=sum(is.na(Bonespring[,i]))/total
}
Bonespring<-Bonespring[,c(1,2,2+which(haha<0.10),cols)]

############Delete variables having only one value###########
cols<-dim(Bonespring)[2]
lala<-rep(0,cols-3)
for (i in 3:(cols-1))
{
  lala[i-2]=sd(Bonespring[,i],na.rm=TRUE)
}
Bonespring<-Bonespring[,c(1,2,2+which(lala>0),cols)]

#############Imputation for variables#########################

newBonespring<-Bonespring
cols<-dim(newBonespring)[2]
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







#################################################################################
##                          Machine Learning                             ########
#################################################################################



##########Random Forest#####################



cols<-dim(newBonespring)[2]

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  repeats=2)


set.seed(2049)

rfGrid <- expand.grid(mtry=c(20,30,40,50,60))

rfFit <- train(Condensate ~ ., data = newBonespring[,3:cols],
               method = "rf",
               tuneGrid=rfGrid,
               trControl = fitControl,
               ntree=100,
               verbose = FALSE,
               importance=TRUE)


for (k in 1:100)
{
  r<-k*10
  set.seed(9999)
  print(r)
  print(train(Condensate ~ ., data = newBonespring[,3:cols],
        method = "rf",
        tuneGrid=rfGrid,
        trControl = fitControl,
        ntree=r,
        verbose = FALSE))
}





predict(rfFit,newBonespring[,3:(cols-1)])

#####################Boosting##################################

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  repeates=2)

set.seed(1006)
gbmGrid <- expand.grid(interaction.depth=c(5),n.trees =c(500), shrinkage=c(10)*0.01, 
                       n.minobsinnode=10)


gbmFit <- train(Condensate ~ ., data = newBonespring[,3:cols],
                method = "gbm",
                trControl = fitControl,
                tuneGrid=gbmGrid,
                verbose = FALSE)


predict(gbmFit,newBonespring[,3:(cols-1)])

########################Kriging#################################


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
    
    lookb=variog(coords=train[,c(15,14)],data=train[,cols],trend='2nd')
    #lookbc=variog(coords=train[,c(15,14)],data=train[,cols],trend='2nd',bin.cloud=TRUE)
    #par(mfrow=c(2,2))
    #plot(lookb, main="binned variogram") 
    #plot(lookbc, bin.cloud=TRUE, main="clouds for binned variogram")  
    covpar<-variofit(lookb,kappa=0.5)
    if(covpar$cov.pars[2]==0) 
    {covpar$cov.pars[2]=0.01}
    model <- Krig(x=train[,c(15,14)],Y=train[,cols],theta=covpar$cov.pars[2],m=3) 
    test.pred <- cbind(test[,c(1,2,cols)], Pred=predict(model,as.matrix(test[,c(15,14)]))) 
    
    # Uwi, Target, Pred, Latitude, Longitude
    mse <- c(mse, sum((test.pred[,3]-test.pred[,4])^2)/nrow(test.pred))
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

fitControl <- trainControl(## 5-fold CV
  method = "cv",
  number = 5)

newGrid <- expand.grid(C=c(10,20,5,1),sigma=0.2)

################################SVM########################################








missing<-function(Z)
{
  sum(is.na(Z))/length(Z)
}





