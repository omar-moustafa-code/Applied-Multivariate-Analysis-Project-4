# Loading the necessary libraries
library(MASS)
library(robustX)

# Loading the dataset
df = read.csv("Dataset_of_Diabetes.csv")
head(df, 3)

# Displaying the number and rows and columns (dimensions) of the dataset
my_dimensions = dim(df)
cat("Number of Rows:", my_dimensions[1], "\n")
cat("Number of Columns:", my_dimensions[2], "\n")

# Extracting the 10 quantitative variables from the dataset 
# and excluding the categorical ones 
x = df[, c("AGE", "Urea", "Cr", "HbA1c", "Chol", "TG", "HDL", "LDL", "VLDL", "BMI")]
head(x, 3)

x = as.matrix(x)
x = scale(x)

# ------------------------------------------------------------------------------ #

# Part 1: PCA [Based on Lect08 PDF]
pc = princomp(x, cor = TRUE)
summary(pc, loadings = TRUE)

# Scree Plot
plot(pc, main = "Scree Plot [Discrete Plot #1 - PCA]")

# ------------------------------------------------------------------------------ #
# Part 2: MDS [Based on Lect09 PDF]
d = dist(x)
out = cmdscale(d, eig = T)
w = out$points

# Replace negative eigenvalues with 0
eig_vals = pmax(out$eig, 0)

plot(w[,1], w[,2], pch = 19, main = "2D Projection - [Plot #2 - MDS]")
text(w[,1], w[,2], rownames(w), cex = 1.0)

