#Precàrrega de llibreries i paquets necessaris pel processat de dades i l'estudi PCA
install.packages("factoextra")
install.packages("FactoMineR")
install.packages("readxl")
install.packages("rgl")
install.packages("mgcv")
install.packages("nlme")
install.packages("car", dependencies = TRUE)
library(factoextra)
library(FactoMineR)
library(stats)
library(carData)
library(car)
library(rgl)
library(readxl)
library(nlme)
library(mgcv)

#Càrrega del dataset de les dades del pis de refer?ncia - Parellada (A)
Parellada <- read_excel("CASA_PARELLADAS.xlsx")
datos_nci <- Parellada[2:19]

#Escalat de dades de la matriu per a plotejar correctament el conjunt
scaled_data <- scale(datos_nci)

#Generació del model PCA en 2D i plotejat de gràfics representatius
pca2d <- PCA(datos_nci,ncp=5,graph=FALSE)
fviz_pca_var(pca2d, col.var = "contrib") # Scree plot
fviz_pca_biplot(pca2d, col.var = "contrib") # Biplot

#Generació del model PCA per plotejat 3D
rotated_data <- prcomp(scaled_data, center=TRUE, scale.=TRUE)$x
pca3d <- PCA(rotated_data,ncp=3,graph=FALSE)

#Dades i gràfics interessants sobre la variancia i desviació estàndard de les diferents variables
summary(pca3d)
eig.val <- get_eigenvalue(pca3d)
fviz_eig(pca3d, addlabels = TRUE, ylim = c(0, 20))

var <- get_pca_var(pca3d)
library("corrplot")
corrplot(var$contrib, is.corr=FALSE)

# Contributions of variables to PC1
fviz_contrib(pca3d, choice = "var", axes = 1, top = 10)

#Plotejat 3D del model PCA
scatter3d(pca3d$ind$coord[,1], pca3d$ind$coord[,2], pca3d$ind$coord[,3], point.col = "steelblue",
          sphere.size = 1.5, xlab = "PC1", ylab = "PC2", zlab = "PC3", surface = FALSE, ellipsoid = TRUE)

# plot3d(pca3d$ind$coord[,1], pca3d$ind$coord[,2], pca3d$ind$coord[,3], col = "steelblue", 
        # type = "s", size = 0.5, xlab = "PC1", ylab = "PC2", zlab = "PC3")

cor_matrix <- cor(datos_nci)
plot3d(cor_matrix, type = "s", size = 2)

#Carreguem les dades dels altres pisos
Pompeu<- read_excel(path = "CASA_PADRE_MARC.xlsx")
datos_Pompeu<- Pompeu[,2:19]
scaled_Pompeu <- scale(datos_Pompeu)
rotated_Pompeu <- prcomp(scaled_Pompeu, center=TRUE, scale.=TRUE)$x
Pompeu_coords <- predict(pca3d, rotated_Pompeu)

# Get values lengths
PCA_Length <- nrow(pca3d$ind$coord)
Predicted_Length <- nrow(Pompeu_coords$coord)

# Combine PCA analysis values and new predicted values of the model into an array
x <- c(pca3d$ind$coord[,1], Pompeu_coords$coord[, 1])
y <- c(pca3d$ind$coord[,2], Pompeu_coords$coord[, 2])
z <- c(pca3d$ind$coord[,3], Pompeu_coords$coord[, 3])

# Set a color for each type of values
colors <- c(rep("blue", PCA_Length), rep("green", Predicted_Length))

# Create a grouping variable
groups <- factor(c(rep("PCA Analysis", PCA_Length), rep("Predicted", Predicted_Length)))

# Plot values
scatter3d(x, y, z, groups = groups, point.col = colors, sphere.size = 1.7,
          xlab = "PC1", ylab = "PC2", zlab = "PC3", surface=FALSE)

# Add legend
legend3d("topright", legend = c("PCA Analysis", "Predicted"), col = c("blue", "green"), pch = 16)

