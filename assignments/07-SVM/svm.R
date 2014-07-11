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

full_str = NULL
pca_str = NULL
relief_str = NULL

for (cost in c(10, 50, 100, 200)) {
  for (kernel in c("linear", "polynomial", "sigmoid")) {
    for (gamma in c(0.1, 0.3, 0.7)) { # all, except by linear
      for (coef in c(0.1, 0.3, 0.7)) { # sigmoid and polynomial
        for (degree in c(1, 5, 15)) { # only polynomial
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

            class_relief_train <- relief$X1
            relief[1:1] <- NULL
            class_relief_test <- reliefTest$X1
            reliefTest[1:1] <- NULL

            confusion_full = NULL
            confusion_pca = NULL
            confusion_relief = NULL

            model_full <- svm(class_full_train~., data=train, kernel=kernel, cost=cost, gamma=gamma, degree=degree, coef0=coef)
            confusion_full <- table(predicted=predict(model_full, test, type="class"), true=class_full_test)
            recall = recall + (confusion_full[1] / (confusion_full[1] + confusion_full[3]))
            precision = precision + (confusion_full[1] / (confusion_full[1] + confusion_full[2]))
            error = error + ((confusion_full[1] + confusion_full[4]) / sum(confusion_full))

            model_pca <- svm(class_full_train~., data=pcaData, kernel=kernel, cost=cost, gamma=gamma, degree=degree, coef0=coef)
            confusion_pca <- table(predicted=predict(model_pca, pcaTest, type="class"), true=class_full_test)
            pca_recall = pca_recall + (confusion_pca[1] / (confusion_pca[1] + confusion_pca[3]))
            pca_precision = pca_precision + (confusion_pca[1] / (confusion_pca[1] + confusion_pca[2]))
            pca_error = pca_error + ((confusion_pca[1] + confusion_pca[4]) / sum(confusion_pca))

            model_relief <- svm(class_relief_train~., data=relief, kernel=kernel, cost=cost, gamma=gamma, degree=degree, coef0=coef)
            confusion_relief <- table(predicted=predict(model_relief, reliefTest, type="class"), true=class_relief_test)
            relief_recall = relief_recall + (confusion_relief[1] / (confusion_relief[1] + confusion_relief[3]))
            relief_precision = relief_precision + (confusion_relief[1] / (confusion_relief[1] + confusion_relief[2]))
            relief_error = relief_error + ((confusion_relief[1] + confusion_relief[4]) / sum(confusion_relief))
          }

          recall = recall / kfold;
          precision = precision / kfold;
          error = error / kfold;

          pca_recall = pca_recall / kfold;
          pca_precision = pca_precision / kfold;
          pca_error = pca_error / kfold;

          relief_recall = relief_recall / kfold;
          relief_precision = relief_precision / kfold;
          relief_error = relief_error / kfold;

          full_str = paste(full_str, cost, "&", kernel, "&", gamma, "&", coef, "&", degree, "&", round(precision, 4), "&", round(recall, 4), "&", round((1 - error), 4), "\\\\ \n")
          pca_str = paste(pca_str, cost, "&", kernel, "&", gamma, "&", coef, "&", degree, "&", round(pca_precision, 4), "&", round(pca_recall, 4), "&", round((1 - pca_error), 4), "\\\\ \n")
          relief_str = paste(relief_str, cost, "&", kernel, "&", gamma, "&", coef, "&", degree, "&", round(relief_precision, 4), "&", round(relief_recall, 4), "&", round((1 - relief_error), 4), "\\\\ \n")

        }
      }
    }
  }
}

cat(full_str)
cat(pca_str)
cat(relief_str)