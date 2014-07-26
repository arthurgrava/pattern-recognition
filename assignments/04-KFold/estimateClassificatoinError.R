# function to run error estimation, but for now it will only run a k-fold cross validation on the data
# data 			- dataset with columns and without the ID column
# kFold 		- number of parts that the dataset will be splitted
# classColumn 	- the column of the dataset that indicates the classification of it
estimateClassificatoinError <- function (data, kFold, classColumn) {
	size = dim(data)[1]
	sorted = data[sort.list(data[,classColumn]), ]
	numSamples = ceiling(size / kFold) # number of samples per instance
	folds = array(dim=c(kFold, numSamples, dim(data)[2])) # folds[fold,line,column]
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