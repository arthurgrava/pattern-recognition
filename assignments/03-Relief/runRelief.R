library('FSelector') # loads the FSelector library that contains relief algorithm

data = read.csv('/Users/arthur/wbdc.data', header = FALSE)
data[1:1] <- list(NULL)
names(data) <- c("class", "Radius_Mean", "Radius_Std.Error", "Radius_Worst/Largest", 
						  "Texture_Mean", "Texture_Std.Error", "Texture_Worst/Largest", 
						  "Perimeter_Mean", "Perimeter_Std.Error", "Perimeter_Worst/Largest", 
						  "Area_Mean", "Area_Std.Error", "Area_Worst/Largest", 
						  "Smoothness_Mean", "Smoothness_Std.Error", "Smoothness_Worst/Largest", 
						  "Compactness_Mean", "Compactness_Std.Error", "Compactness_Worst/Largest", 
						  "Concavity_Mean", "Concavity_Std.Error", "Concavity_Worst/Largest", 
						  "ConcavePoints_Mean", "ConcavePoints_Std.Error", "ConcavePoints_Worst/Largest", 
						  "Symmetry_Mean", "Symmetry_Std.Error", "Symmetry_Worst/Largest", 
						  "FractalDimension_Mean", "FractalDimension_Std.Error", "FractalDimension_Worst/Largest")

weights <- NULL

n <- 100
for (i in 1:n) {
	x <- NULL
	x <- relief(class~., data, 5, 200)
	weights <- c(weights, x$attr_importance)
}

m <- matrix(weights, nrow=n)
mean = colSums(m) / n
names(mean) <- c("Radius_Mean", "Radius_Std.Error", "Radius_Worst/Largest", 
				 "Texture_Mean", "Texture_Std.Error", "Texture_Worst/Largest", 
				 "Perimeter_Mean", "Perimeter_Std.Error", "Perimeter_Worst/Largest", 
				 "Area_Mean", "Area_Std.Error", "Area_Worst/Largest", 
				 "Smoothness_Mean", "Smoothness_Std.Error", "Smoothness_Worst/Largest", 
				 "Compactness_Mean", "Compactness_Std.Error", "Compactness_Worst/Largest", 
				 "Concavity_Mean", "Concavity_Std.Error", "Concavity_Worst/Largest", 
				 "ConcavePoints_Mean", "ConcavePoints_Std.Error", "ConcavePoints_Worst/Largest", 
				 "Symmetry_Mean", "Symmetry_Std.Error", "Symmetry_Worst/Largest", 
				 "FractalDimension_Mean", "FractalDimension_Std.Error", "FractalDimension_Worst/Largest")

sorted = sort(mean, decreasing = TRUE)
threshold = (sorted[1] + sorted[30]) / 2 # mean
result = sorted > threshold
index = sum(result) # TRUE = 1 and FALSE = 0, so the sum will be the number of TRUEs

result[1:index] # Prints the choosen attributes
print(sorted) # Prints the weigth of each attribute 

# Plot of the sorted values and the Threshold
plot(sorted, type = "l", col = "blue", xlab = "Attributes", ylab = "Weights")
xThreshold = c(1:30)
yThreshold <- NULL
for(i in 1:30) { yThreshold <- c(yThreshold, threshold) }
lines(xThreshold, yThreshold, type="l", col="orange", lty = 2)
legend("topright", legend=c("Threshold"), col = "orange", lty = 2)

# Plot of the sorted values and the Threshold
sum <- sum(sorted)
distribution <- sorted[1] / sum
for(i in 2:30) {
	distribution <- c(distribution, ((sorted[i]/sum) + distribution[i-1]) )
}
plot(distribution, type = "l", col = "blue", xlab = "Attributes", ylab = "Distribution")
