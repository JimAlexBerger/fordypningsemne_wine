wineData = read.csv(file.choose(), header = F)
colnames(wineData) = c(
  "fixed acidity",
  "volatile acidity", 
  "citric acid",
  "residual sugar", 
  "chlorides",
  "free sulfur dioxide",
  "total sulfur dioxide", 
  "density",
  "pH",
  "sulphates", 
  "alcohol",
  "quality"
)
wineData$quality = factor(wineData$quality)

library(caret)
library(C50)
library(plyr)
library(doSNOW)
library(data.table)


resultTable <- data.frame(PCA = numeric(),Accuracy = numeric())

for (pcaComponents in c(7:11)){
  train.control <- trainControl(method = "repeatedcv",#Kryssvalidering
                                number = 10,          #Tifoldig
                                repeats = 3,          #3 ganger
                                search = "random",      #Grid eller random
                                verboseIter = T,
                                preProcOptions = list(pcaComp = pcaComponents) #pcaComp = 7 / tresh = 0.85
                                )
  
  #Lager opp 8 threads for kjøring av caret
  cl <- makeCluster(8,type = "SOCK")
  
  #forteller caret at den kan bruke de threadene som ble spunnet opp
  registerDoSNOW(cl)
  
  #Trene på treningsettet
  set.seed(1)
  caret.cv <- train(quality ~ .,
                    data = wineData,
                    method = "C5.0",
                    tuneLength = 35,
                    preProcess = c("center","scale","pca"),
                    trControl = train.control)
  
  #steng ned threadene
  stopCluster(cl)
  
  #se på resulatet
  #caret.cv
  plot(caret.cv, plotType = "line", lwd=3)#plottype (scatter,level,line)
  #summary(caret.cv)
  #caret.cv$preProcess$rotation
  
  resultTable <- rbind(resultTable,data.frame(PCA = pcaComponents,Accuracy = max(caret.cv$results$Accuracy)))
}

plot(resultTable$PCA,resultTable$Accuracy,"o", col="blue", ylim=c(0,1), lwd=3, cex=2)
lines(resultTable$PCA, resultTable$Kappa,"o",col="red", lwd=3, cex=2)
