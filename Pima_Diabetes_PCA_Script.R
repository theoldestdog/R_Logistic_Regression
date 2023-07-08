#### PCA on Pima Diabetes Dataset

### Install/load packages
install.packages("corrr")
install.packages("FactoMineR")

library(corrr)
library("ggcorrplot")
library("factoextra")
library("FactoMineR")

#### load in dataset and have a look at it and its characteristics
pdd_pca <- read.csv("C:\\Users\\alexd\\R_Files_Data\\Pima_Diabetes\\pdd.csv")
head(pdd_pca)
str(pdd_pca)

### check for null values that might bias results of PCA
colSums(is.na(pdd_pca))

### normalize data by removing non-numeric and 'y' cols
pdd_pca_num <- pdd_pca[ , 1:7]
### e.g. pdd_pca_norm <- scale(pdd_pca_num)

### derive correlation matrix
corr_pdd_pca <- cor(pdd_pca_num)
ggcorrplot(corr_pdd_pca)

### apply PCA
pdd_pca_data <- princomp(pdd_pca_num)
summary(pdd_pca_data)

### check loadings since first 2 components have 95% cumulative proportion
pdd_pca_data$loadings[, 1:2]

### visualize each component's importance w/ skree plot
fviz_eig(pdd_pca_data, addlabels = TRUE)

### bi-plot
fviz_pca_var(pdd_pca_data, col.var = "black")

### cos2 low value=not perfect rep of variable, hi value=good rep
fviz_cos2(pdd_pca_data, choice="var", axes = 1:2)

### combine bi-plot and cos2 w/ colours green-orange-black best-worse
fviz_pca_var(pdd_pca_data, col.var = "cos2",
gradient.cols = c("red", "orange", "green"),
repel = TRUE)
