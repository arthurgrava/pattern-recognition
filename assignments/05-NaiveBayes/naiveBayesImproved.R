
library('e1071')
data = read.csv('/Users/arthur/wbdc.data', header = FALSE)
data[1:1] <- NULL

generateKFolds <- function (data, kFold, classColumn) {
  size = dim(data)[1]
  sorted = data[sort.list(data[,classColumn]), ]
  numSamples = ceiling(size / kFold)
  folds = array(dim=c(kFold, numSamples, dim(data)[2]))
  fold = 1
  j = 1
  
  for(i in 1:size) {
    if(fold > kFold) {
      fold = 1
      j = j + 1
    }
    for(k in 1:dim(data)[2]) {
      folds[fold,j,k] <- sorted[i,k]
    }
    fold = fold + 1
  }
  return(folds)
}

kfold = 10
folds = generateKFolds(data, kfold, 1)

precision = 0
pcaPrecision = 0
reliefPrecision = 0
recall = 0
pcaRecall = 0
reliefRecall = 0
totalerror = 0
pcaTotalerror = 0
reliefTotalerror = 0

for (i in 1:kfold) {
  train = NULL
  for(j in 1:kfold) {
    if(j != i) {
      train = rbind(train, folds[j,,])
    }
  }
  test = folds[i,,]

  train = data.frame(train)
  test = data.frame(test)
  names(train) <- c("V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12","V13","V14","V15","V16","V17","V18","V19","V20","V21","V22","V23","V24","V25","V26","V27","V28","V29","V30","V31","V32")
  names(test) <- c("V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12","V13","V14","V15","V16","V17","V18","V19","V20","V21","V22","V23","V24","V25","V26","V27","V28","V29","V30","V31","V32")
  train = na.omit(train)
  test = na.omit(test)
  train[,1] <- as.factor(train[,1])
  test[,1] <- as.factor(test[,1])

  pcaMatrix = data.matrix(train[,2:31])
  normalized = scale(pcaMatrix, center=TRUE, scale=TRUE)
  pcaData = princomp(normalized, cor=TRUE, scores=TRUE, covmat=NULL)
  pcaData = pcaData$scores
  pcaData = pcaData[,1:6]

  pcaTest = data.matrix(test[,2:31])
  normalized = scale(pcaTest, center=TRUE, scale=TRUE)
  pcaTest = princomp(pcaTest, cor=TRUE, scores=TRUE, covmat=NULL)
  pcaTest = pcaTest$scores
  pcaTest = pcaTest[,1:6]

  relief = cbind(train[,1], train[,3],train[,6],train[,9],train[,12],train[,15],train[,18],train[,21],train[,24],train[,27],train[,30])
  reliefTest = cbind(test[,1], test[,3],test[,6],test[,9],test[,12],test[,15],test[,18],test[,21],test[,24],test[,27],test[,30])
  relief = data.frame(relief)
  reliefTest = data.frame(reliefTest)
  relief[,1] <- as.factor(relief[,1])
  reliefTest[,1] <- as.factor(reliefTest[,1])

  class <- naiveBayes(train[,2:31], train[,1])
  classPca <- naiveBayes(pcaData, train[,1])
  classRelief <- naiveBayes(relief[,2:11], relief[,1])
  confusion_full <- table(predicted=predict(class, test[,2:31]), true=test[,1])
  confusion_pca <- table(predicted=predict(classPca, pcaTest), true=test[,1])
  confusion_relief <- table(predicted=predict(classRelief, reliefTest[,2:11]), true=reliefTest[,1])

  precision = precision + (confusion_full[1] / (confusion_full[1] + confusion_full[2]))
  recall = recall + (confusion_full[1] / (confusion_full[1] + confusion_full[3]))
  totalerror = totalerror + ((confusion_full[1] + confusion_full[4]) / sum(confusion_full))

  pcaPrecision = pcaPrecision + (confusion_pca[1] / (confusion_pca[1] + confusion_pca[2]))
  pcaRecall = pcaRecall + (confusion_pca[1] / (confusion_pca[1] + confusion_pca[3]))
  pcaTotalerror = pcaTotalerror + ((confusion_pca[1] + confusion_pca[4]) / sum(confusion_pca))

  reliefPrecision = reliefPrecision + (confusion_relief[1] / (confusion_relief[1] + confusion_relief[2]))
  reliefRecall = reliefRecall + (confusion_relief[1] / (confusion_relief[1] + confusion_relief[3]))
  reliefTotalerror = reliefTotalerror + ((confusion_relief[1] + confusion_relief[4]) / sum(confusion_relief))
}

mRecall = recall / kfold
mPrecision = precision / kfold
mTotalerror = totalerror / kfold
mPCARecall = pcaRecall / kfold
mPCAPrecision = pcaPrecision / kfold
mPCATotalerror = pcaTotalerror / kfold
mRELIEFRecall = reliefRecall / kfold
mRELIEFPrecision = reliefPrecision / kfold
mRELIEFTotalerror = reliefTotalerror / kfold

full_str = paste(round(mPrecision, 4), "&", round(mRecall, 4), "&", round((1 - mTotalerror), 4), "\\\\ \n")
pca_str = paste(round(mPCAPrecision, 4), "&", round(mPCARecall, 4), "&", round((1 - mPCATotalerror), 4), "\\\\ \n")
relief_str = paste(round(mRELIEFPrecision, 4), "&", round(mRELIEFRecall, 4), "&", round((1 - mRELIEFTotalerror), 4), "\\\\ \n")