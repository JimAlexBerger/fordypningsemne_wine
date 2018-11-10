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
library(xgboost)
library(plyr)
library(doSNOW)

train.control <- trainControl(method = "repeatedcv",#Kryssvalidering
                              number = 10,          #Tifoldig
                              repeats = 3,          #3 ganger
                              search = "random")      #Grid eller random

#Lager opp 8 threads for kjøring av caret
cl <- makeCluster(8,type = "SOCK")

#forteller caret at den kan bruke de threadene som ble spunnet opp
registerDoSNOW(cl)

set.seed(1)
#Trene på treningsettet
caret.cv <- train(quality ~ .,
                  data = wineData,
                  method = "xgbTree",
                  tuneLength = 8,
                  trControl = train.control)

#steng ned threadene
stopCluster(cl)

#se på resulatet
res <- caret.cv$results
caret.cv
plot(caret.cv, plotType = "line", lwd=2)#plottype (scatter,level,line)

pdf("output/randomSample.pdf",height=6,width=6)
plot(caret.cv, plotType = "line", lwd=2)#plottype (scatter,level,line)
dev.off()
