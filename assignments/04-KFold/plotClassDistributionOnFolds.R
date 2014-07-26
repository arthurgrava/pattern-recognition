# Script used to plot the Graphs
data = read.csv('/Users/arthur/wbdc.data', header = FALSE)
data[1:1] <- NULL

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

folds = estimateClassificatoinError(data, 10, 1)
classes = folds[,,1]
clean = classes > 1 # FALSE = B e TRUE = M

proportion = summary(data[,1])
foldProportion = array(dim=c(dim(clean)[1], 2))

for(i in 1:dim(clean)[1]) {
	temp = summary(clean[i,])
	foldProportion[i,1] = temp[2]
	foldProportion[i,2] = temp[3]
}
class(foldProportion) <- 'numeric'

proportion = proportion / sum(proportion)
sumFoldProportion = as.matrix(rowSums(foldProportion))

for(i in 1:dim(sumFoldProportion)[1]) {
	foldProportion[i,1] = foldProportion[i,1] / sumFoldProportion[i]
	foldProportion[i,2] = foldProportion[i,2] / sumFoldProportion[i]
}

plotProportion = NULL
for(i in 1:dim(sumFoldProportion)[1]) {
	plotProportion = c(plotProportion, proportion[1])
}

xCoord = c(1:dim(foldProportion)[1])
plot(plotProportion, type = "l", col = "red", lty = 2, xlab = "Partição", ylab = "Benigno", main = "10 Partiçõess", lwd = 2)
lines(xCoord, foldProportion[,1], type="l", col="black", lty = 1)
#legend("topright", legend=c("Full Dataset Benign Proportion"), col = "red", lty = 2)
