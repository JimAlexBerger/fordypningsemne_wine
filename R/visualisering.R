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
library(AppliedPredictiveModeling)

transparentTheme(trans = .4)

#Sammeligne to variabler for å se sammenheng
#plot pairs, ellipse(med ellipse rund hverkvalitet)
pdf("output/pairs.pdf",height=16,width=16)
featurePlot(x = wineData[, 1:11], 
            y = wineData$quality, 
            plot = "pairs",
            
            auto.key = list(columns = 7))
dev.off()


#fordeling av kvalitet for hver variabel
pdf("output/density.pdf",height=16,width=16)
featurePlot(x = wineData[, 1:11], 
            y = wineData$quality,
            plot = "density", 
            
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")), 
            adjust = 1.5, 
            pch = "|", 
            layout = c(6, 2), 
            auto.key = list(columns = 7))
dev.off()

#boxplot for hver variabel
pdf("output/box.pdf",height=16,width=16)
featurePlot(x = wineData[, 1:11], 
            y = wineData$quality,
            plot = "box", 
             
            scales = list(y = list(relation="free"),
                          x = list(rot = 90)),  
            layout = c(6,2 ), 
            auto.key = list(columns = 2))
dev.off()
