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
library(GGally)
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


#ggscatmat(newBonespring,columns=15:22)




####Delete outliers########

for (i in 3:(cols-1))
{
  
    if(is.factor(newBonespring[,i])==FALSE)
    {
      mediann<-median(newBonespring[,i],na.rm=TRUE)
      iqrr<-IQR(newBonespring[,i], na.rm = TRUE, type = 7)
      upper<-quantile(newBonespring[,i], na.rm = TRUE)[4]+1.5*iqrr
      lower<-quantile(newBonespring[,i], na.rm = TRUE)[2]-1.5*iqrr
      index<-((newBonespring[,i]>upper)|(newBonespring[,i]<lower))&(!is.na(newBonespring[,i]))
      if (sum(index,na.rm=TRUE)>0)
        {
        newBonespring[index,i]<-NA
        }
    }  
}


#ggscatmat(newBonespring,columns=15:22)





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
    newBonespring[index,i]<-median(newBonespring[,i],na.rm=TRUE)  
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
  method = "cv",
  number = 10,
  repeats=1)

rfGrid <- expand.grid(mtry=c(50))

rfFit <- train(Condensate ~ ., data = newBonespring[,3:cols],
               method = "rf",
               tuneGrid=rfGrid,
               trControl = fitControl,
               ntree=400,
               verbose = FALSE,
               importance=TRUE)

set.seed(666)
mmm<-rep(0,100)
for (i in 1:100)
{
  
  rfFit <- train(Condensate ~ ., data = newBonespring[,3:cols],
                 method = "rf",
                 tuneGrid=rfGrid,
                 trControl = fitControl,
                 ntree=400,
                 verbose = FALSE,
                 importance=TRUE)
  print(i)
  print(rfFit$results)
  mmm[i]<-rfFit$results[2]
}



#RMSE=22062.43(274.3414) m=50(10), ntree=400




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
set.seed(1876)
rf <- runRFRegCV(dat=newBonespring,  m=10, no.tree=240, k=10)
predRF<- rf[[2]] 

#RMSE=22214.56 m=50(10), ntree=240,seed=1876



##Plot########

plot(predRF[,3]~predRF[,2],xlab='Actual', ylab='Predicted',col='blue',main='Condensate 180 Day Cumulative Production:
Predicted vs. Actual')
abline(a=0,b=1)




#####################Boosting##################################

fitControl <- trainControl(## 10-fold CV
  method = "cv",
  number = 10)

gbmGrid <- expand.grid(interaction.depth=c(10),n.trees =c(5)*100, shrinkage=c(1)*0.01, 
                       n.minobsinnode=10)


gbmFit <- train(Condensate ~ ., data = newBonespring[,3:cols],
                method = "gbm",
                trControl = fitControl,
                tuneGrid=gbmGrid,
                verbose = FALSE)
set.seed(666)
mmm<-rep(0,100)
for (i in 1:100)
{
  
  gbmFit <- train(Condensate ~ ., data = newBonespring[,3:cols],
                  method = "gbm",
                  trControl = fitControl,
                  tuneGrid=gbmGrid,
                  verbose = FALSE)
  print(i)
  print(gbmFit$results)
  mmm[i]<-gbmFit$results[5]
}

#RMSE=22551.17(341.5) interaction=10, ntree=500,shringkage=0.01, seed=666

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
set.seed(2521)
boost <- runboostRegCV(dat=newBonespring,  no.tree=490, shrinkage=0.01,interaction=10,k=10)
predboost<- boost[[2]] 

#RMSE=23308.07 interaction=10, ntree=490,shringkage=0.01, seed=2521

###Plot##########

plot(predboost[,3]~predboost[,2],xlab='Actual', ylab='Predicted',col='blue',main='Condensate 180 Day Cumulative Production:
Predicted vs. Actual')
abline(a=0,b=1)





#######################Kriging###############################

runKriCV <- function(dat, k){
  
  folds <- cvFolds(nrow(dat), K=k)
  mse <- NULL;  pred <- NULL; sol <- NULL;
  
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
    
    lookb=variog(coords=train[,c(12,11)],data=train[,34],trend='1st')
     
    covpar<-variofit(lookb,kappa=0.5)
    if(covpar$cov.pars[2]==0) 
    {covpar$cov.pars[2]=0.01}
    model <- Krig(x=train[,c(12,11)],Y=train[,34],theta=covpar$cov.pars[2],m=2) 
    test.pred <- cbind(test[,c(2,34)], Pred=predict(model,as.matrix(test[,c(12,11)]))) 
    
    # Uwi, Target, Pred, Latitude, Longitude
    mse <- c(mse, sum((test.pred[,2]-test.pred[,3])^2)/nrow(test.pred))
    pred <- rbind(pred, test.pred)  # save prediction results for fold i
  }
  # CV results
  sol <- data.frame(K=k,mse=mean(mse), rmse=sqrt(mean(mse)))
  return(list(sol, pred))
  
}
set.seed(86)
Kri <- runKriCV(dat=newBonespring, k=10)
predKri<- Kri[[2]] 

#RMSE=26975 seed=86


set.seed(666)
mmm<-rep(0,100)
for (i in 1:100)
{
  set.seed(i)
  Kri <- runKriCV(dat=newBonespring, k=10)
  print(i)
  print(Kri[[1]])
  mmm[i]<-Kri[[1]][3]
}

#RMSE=27417.22(414.81) seed=666




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
pred.svm<-dplyr::select(svm,Condensate,svm=Condensate.predict)
pred.Kri<-dplyr::select(predKri,Condensate, Kri=Pred)


jo <- dplyr::left_join(pred.boost, pred.RF, by="Condensate")
jo <- dplyr::left_join(jo,pred.svm,by='Condensate')
jo <- dplyr::left_join(jo,pred.Kri,by='Condensate')
jo <- jo[,-1]  # rm Uwi

q.rec <- qRecCurv(jo) * 100

# Round to integer percentage
index <- ceiling(nrow(q.rec)*seq(0.3,100,0.3)/100)
q.rec <- q.rec[index, ]

q.rec1 <- q.rec %>% dplyr::select(True) %>% dplyr::mutate(RecRate=True, Method="Baseline")
q.rec2 <- q.rec %>% dplyr::select(True, X2) %>% dplyr::rename(RecRate=X2) %>% dplyr::mutate(Method="boost")
q.rec3 <- q.rec %>% dplyr::select(True, X3) %>% dplyr::rename(RecRate=X3) %>% dplyr::mutate(Method="RandomForest")
q.rec4 <- q.rec %>% dplyr::select(True, X4) %>% dplyr::rename(RecRate=X4) %>% dplyr::mutate(Method="SVM")
q.rec5 <- q.rec %>% dplyr::select(True, X5) %>% dplyr::rename(RecRate=X5) %>% dplyr::mutate(Method="Kriging")

q.rec <- dplyr::union(q.rec1, q.rec2)
q.rec <- dplyr::union(q.rec, q.rec3)
q.rec <- dplyr::union(q.rec, q.rec4)
q.rec <- dplyr::union(q.rec, q.rec5)


ggplot(q.rec, aes(x=True, y=RecRate, colour=Method, group=Method)) + 
  geom_line(lwd=1.2) +
  scale_color_manual(values=c("#fe506e", "black", "#228b22","#0099cc",'brown')) +
  xlab("Top Quantile Percentage") + ylab("Recover Rate") + 
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



