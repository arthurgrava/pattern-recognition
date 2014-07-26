require(nnet)
require(e1071)

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

str = NULL
pca_str = NULL
relief_str = NULL

for(neurons in c(10, 15, 20)) {
    for(decay in c(0.1, 0.01, 0.001)) {
        recall = 0
        precision = 0
        error = 0

        pca_recall = 0
        pca_precision = 0
        pca_error = 0

        relief_recall = 0
        relief_precision = 0
        relief_error = 0

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

            class_full_train <- train$V2
            train[1:1] <- NULL
            class_full_test <- test$V2
            test[1:1] <- NULL

            model_full <- nnet(class_full_train~., data=train, size=neurons, decay=decay)
            confusion = table(predict(model_full, test, type="class"), class_full_test)
            if(length(confusion) < 4) {
                for(i in 1:4) {
                    if(is.na(confusion[i])) {
                        confusion[i] = 0
                    }
                }    
            }
            
            recall = recall + (confusion[1] / (confusion[1] + confusion[3]))
            precision = precision + (confusion[1] / (confusion[1] + confusion[2]))
            error = error + ((confusion[1] + confusion[4]) / sum(confusion))

            model_pca <- nnet(class_full_train~., data=pcaData, size=neurons, decay=decay)
            confusion_pca = table(predict(model_pca, pcaTest, type="class"), class_full_test)
            if(length(confusion_pca) < 4) {
                for(i in 1:4) {
                    if(is.na(confusion_pca[i])) {
                        confusion_pca[i] = 0
                    }
                }    
            }
            pca_recall = pca_recall + (confusion_pca[1] / (confusion_pca[1] + confusion_pca[3]))
            pca_precision = pca_precision + (confusion_pca[1] / (confusion_pca[1] + confusion_pca[2]))
            pca_error = pca_error + ((confusion_pca[1] + confusion_pca[4]) / sum(confusion_pca))

            class_relief_train <- relief$X1
            relief[1:1] <- NULL
            class_relief_test <- reliefTest$X1
            reliefTest[1:1] <- NULL
            model_relief <- nnet(class_relief_train~., data=relief, size=neurons, decay=decay)
            confusion_relief = table(predict(model_relief, reliefTest, type="class"), class_full_test)
            if(length(confusion_relief) < 4) {
                for(i in 1:4) {
                    if(is.na(confusion_relief[i])) {
                        confusion_relief[i] = 0
                    }
                }    
            }
            relief_recall = relief_recall + (confusion_relief[1] / (confusion_relief[1] + confusion_relief[3]))
            relief_precision = relief_precision + (confusion_relief[1] / (confusion_relief[1] + confusion_relief[2]))
            relief_error = relief_error + ((confusion_relief[1] + confusion_relief[4]) / sum(confusion_relief))
        }

        precision = precision /kfold
        recall = recall / kfold
        error = error / kfold

        pca_precision = pca_precision /kfold
        pca_recall = pca_recall / kfold
        pca_error = pca_error / kfold

        relief_precision = relief_precision /kfold
        relief_recall = relief_recall / kfold
        relief_error = relief_error / kfold

        str = paste(str, neurons, "&", decay, "&", precision, "&", recall, "&", (1 - error), "\\\\ \n")
        pca_str = paste(pca_str, neurons, "&", decay, "&", pca_precision, "&", pca_recall, "&", (1 - pca_error), "\\\\ \n")
        relief_str = paste(relief_str, neurons, "&", decay, "&", relief_precision, "&", relief_recall, "&", (1 - relief_error), "\\\\ \n")
    }
}

cat(str)
cat(pca_str)
cat(relief_str)