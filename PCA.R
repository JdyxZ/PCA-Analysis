#Install libraries
install.packages("usethis")
install.packages("factoextra")
install.packages("FactoMineR")
install.packages("readxl")
install.packages("rgl")
install.packages("mgcv")
install.packages("nlme")
install.packages("car", dependencies = TRUE)

# Load libraries into memory 
library(factoextra)
library(FactoMineR)
library(stats)
library(carData)
library(car)
library(rgl)
library(readxl)
library(nlme)
library(mgcv)
library("corrplot")

#Càrrega del dataset de les dades del pis de referencia - Parellada (A)
Parellada <- read_excel("CASA_PARELLADAS.xlsx")
datos_nci <- Parellada[2:19]

#Escalat de dades de la matriu per a plotejar correctament el conjunt
scaled_data <- scale(datos_nci)

#Generació del model PCA per plotejat 3D
rotated_data <- prcomp(scaled_data, center=TRUE, scale.=TRUE)$x
pca3d <- PCA(rotated_data,ncp=3,graph=FALSE)

# Set coordinates into auxiliar variables
PCA_x <- pca3d$ind$coord[,1]
PCA_y <- pca3d$ind$coord[,2]
PCA_z <- pca3d$ind$coord[,3]

#Plotejat 3D del model PCA
scatter3d(PCA_x, PCA_y, PCA_z, point.col = "steelblue",
          sphere.size = 1.5, xlab = "PC1", ylab = "PC2", zlab = "PC3", surface = FALSE, ellipsoid = TRUE)

# plot3d(PCA_x, PCA_y, PCA_z, col = "steelblue", 
        # type = "s", size = 0.5, xlab = "PC1", ylab = "PC2", zlab = "PC3")

#Carreguem les dades dels altres pisos
Pompeu<- read_excel(path = "CASA_PADRE_MARC.xlsx")
datos_Pompeu<- Pompeu[,2:19]
scaled_Pompeu <- scale(datos_Pompeu)
rotated_Pompeu <- prcomp(scaled_Pompeu, center=TRUE, scale.=TRUE)$x
Pompeu_coords <- predict(pca3d, rotated_Pompeu)

# Set coordinates into auxiliar variables
Pompeu_x <- Pompeu_coords$coord[, 1]
Pompeu_y <- Pompeu_coords$coord[, 2]
Pompeu_z <- Pompeu_coords$coord[, 3]

# Plot values
scatter3d(PCA_x, PCA_y, PCA_z, point.col = "steelblue",
          sphere.size = 1.5, xlab = "PC1", ylab = "PC2", zlab = "PC3", surface = FALSE, ellipsoid = TRUE)

points3d(Pompeu_x, Pompeu_y, Pompeu_z, col = "red", pch = 19, cex = 1.5, xlab = "Eje X", zlab = "Eje Z")

points3d(Pompeu_x, Pompeu_y, Pompeu_z, col = "red", 
       type = "s", size = 0.5, xlab = "PC1", ylab = "PC2", zlab = "PC3", add=TRUE)

# Add legend
#legend3d("topright", legend = c("PCA Analysis", "Predicted"), col = c("blue", "green"), pch = 16)

#Generació del model PCA en 2D i plotejat de gràfics representatius
pca2d <- PCA(datos_nci,ncp=5,graph=FALSE)
fviz_pca_var(pca2d, col.var = "contrib") # Scree plot
fviz_pca_biplot(pca2d, col.var = "contrib") # Biplot

#Dades i gràfics interessants sobre la variancia i desviació estàndard de les diferents variables
summary(pca3d)
eig.val <- get_eigenvalue(pca3d)
fviz_eig(pca3d, addlabels = TRUE, ylim = c(0, 20))

var <- get_pca_var(pca3d)
corrplot(var$contrib, is.corr=FALSE)

# Contributions of variables to PC1
fviz_contrib(pca3d, choice = "var", axes = 1, top = 10)

cor_matrix <- cor(datos_nci)
plot3d(cor_matrix, type = "s", size = 2)

