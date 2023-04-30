#Install libraries
install.packages("usethis")
install.packages("factoextra")
install.packages("FactoMineR")
install.packages("readxl")
install.packages("rgl")
install.packages("mgcv")
install.packages("nlme")
install.packages("car", dependencies = TRUE)
install.packages("scatterplot3d")

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
library(corrplot)
library(scatterplot3d)

# Load the dataset of the reference home
Parellada <- read_excel("CASA_PARELLADA_PCA.XLSX")
datos_nci <- Parellada[2:17]

# Scale dataset in order to plot properly
scaled_data <- scale(datos_nci)

# Create the PCA model
rotated_data <- prcomp(scaled_data, center=TRUE, scale.=TRUE)$x
pca3d <- PCA(rotated_data,ncp=3,graph=FALSE)

# Get some useful data from the PCA model
PCA_center <- colMeans(pca3d$ind$coord)
PCA_cov <- cov(pca3d$ind$coord)

# Create an ellipsoid object to represent the conficende region
ellipsoid <- ellipse3d(PCA_cov, centre = PCA_center,level = 0.95)

# Set coordinates of the PCA Analysis into auxiliary variables
PCA_x <- pca3d$ind$coord[,1]
PCA_y <- pca3d$ind$coord[,2]
PCA_z <- pca3d$ind$coord[,3]

# Plot the PCA model
scatter3d(PCA_x, PCA_y, PCA_z, point.col = "steelblue",
          sphere.size = 1.5, xlab = "PC1", ylab = "PC2", zlab = "PC3", surface = FALSE, ellipsoid = TRUE)

# Load the datasets of the others homes
Pompeu<- read_excel(path = "CASA_PADRE_MARC_PCA.xlsx")
datos_Pompeu<- Pompeu[,2:17]

Montigala<- read_excel(path = "CASA_MARC_PCA.xlsx")
datos_Montigala<- Montigala[,2:17]

Nil<- read_excel(path = "CASA_MARTA_PCA.xlsx")
datos_Nil<- Nil[,2:17]

Pitagoras<- read_excel(path = "CASA_NATALIA_PCA.xlsx")
datos_Pitagoras<- Pitagoras[,2:17]

Junta<- read_excel(path = "CASA_JUNTA_COMERÇ_PCA.xlsx")
datos_Junta<- Junta[,2:17]

Erasme<- read_excel(path = "CASA_SANT_ERASME_PCA.xlsx")
datos_Erasme<- Erasme[,2:17]

Manso<- read_excel(path = "GENERAL_MANSO_PCA.xlsx")
datos_Manso<- Manso[,2:17]

Hospi<- read_excel(path = "HOSPITALET_DE_LLOBREGAT_PCA.xlsx")
datos_Hospi<- Hospi[,2:17]

# Scale dataset in order to plot properly
scaled_Pompeu <- scale(datos_Pompeu)
scaled_Montigala <- scale(datos_Montigala)
scaled_Nil <- scale(datos_Nil)
scaled_Pitagoras <- scale(datos_Pitagoras)
scaled_Junta <- scale(datos_Junta)
scaled_Erasme <- scale(datos_Erasme)
scaled_Manso <- scale(datos_Manso)
scaled_Hospi <- scale(datos_Hospi)

# Make a prediction based on the PCA model
rotated_Pompeu <- prcomp(scaled_Pompeu, center=TRUE, scale.=TRUE)$x
Pompeu_coords <- predict(pca3d, rotated_Pompeu)

rotated_Montigala <- prcomp(scaled_Montigala, center=TRUE, scale.=TRUE)$x
Montigala_coords <- predict(pca3d, rotated_Montigala)

rotated_Nil <- prcomp(scaled_Nil, center=TRUE, scale.=TRUE)$x
Nil_coords <- predict(pca3d, rotated_Nil)

rotated_Pitagoras <- prcomp(scaled_Pitagoras, center=TRUE, scale.=TRUE)$x
Pitagoras_coords <- predict(pca3d, rotated_Pitagoras)

rotated_Junta <- prcomp(scaled_Junta, center=TRUE, scale.=TRUE)$x
Junta_coords <- predict(pca3d, rotated_Junta)

rotated_Erasme <- prcomp(scaled_Erasme, center=TRUE, scale.=TRUE)$x
Erasme_coords <- predict(pca3d, rotated_Erasme)

rotated_Manso <- prcomp(scaled_Manso, center=TRUE, scale.=TRUE)$x
Manso_coords <- predict(pca3d, rotated_Manso)

rotated_Hospi <- prcomp(scaled_Hospi, center=TRUE, scale.=TRUE)$x
Hospi_coords <- predict(pca3d, rotated_Hospi)

# Set coordinates of the prediction into auxiliar variables
Pompeu_x <- Pompeu_coords$coord[, 1]
Pompeu_y <- Pompeu_coords$coord[, 2]
Pompeu_z <- Pompeu_coords$coord[, 3]

Montigala_x <- Montigala_coords$coord[, 1]
Montigala_y <- Montigala_coords$coord[, 2]
Montigala_z <- Montigala_coords$coord[, 3]

Nil_x <- Nil_coords$coord[, 1]
Nil_y <- Nil_coords$coord[, 2]
Nil_z <- Nil_coords$coord[, 3]

Pitagoras_x <- Pitagoras_coords$coord[, 1]
Pitagoras_y <- Pitagoras_coords$coord[, 2]
Pitagoras_z <- Pitagoras_coords$coord[, 3]

Junta_x <- Junta_coords$coord[, 1]
Junta_y <- Junta_coords$coord[, 2]
Junta_z <- Junta_coords$coord[, 3]

Erasme_x <- Erasme_coords$coord[, 1]
Erasme_y <- Erasme_coords$coord[, 2]
Erasme_z <- Erasme_coords$coord[, 3]

Manso_x <- Manso_coords$coord[, 1]
Manso_y <- Manso_coords$coord[, 2]
Manso_z <- Manso_coords$coord[, 3]

Hospi_x <- Hospi_coords$coord[, 1]
Hospi_y <- Hospi_coords$coord[, 2]
Hospi_z <- Hospi_coords$coord[, 3]

# Plot PCA Analysis data
plot3d(PCA_x, PCA_y, PCA_z, col = "purple", 
       type = "s", size = 0.5, xlab = "PC1", ylab = "PC2", zlab = "PC3", main="3D Scatter Plot")

# Plot Prediction Model data
plot3d(Pompeu_x, Pompeu_y, Pompeu_z, col = "yellow", 
       type = "s", size = 0.5, add=TRUE)

plot3d(Montigala_x, Montigala_y, Montigala_z, col = "red", 
       type = "s", size = 0.5, add=TRUE)

plot3d(Nil_x, Nil_y, Nil_z, col = "orange", 
       type = "s", size = 0.5, add=TRUE)

plot3d(Pitagoras_x, Pitagoras_y, Pitagoras_z, col = "green", 
       type = "s", size = 0.5, add=TRUE)

plot3d(Junta_x, Junta_y, Junta_z, col = "blue", 
       type = "s", size = 0.5, add=TRUE)

plot3d(Erasme_x, Erasme_y, Erasme_z, col = "black", 
       type = "s", size = 0.5, add=TRUE)

plot3d(Manso_x, Manso_y, Manso_z, col = "pink", 
       type = "s", size = 0.5, add=TRUE)

plot3d(Hospi_x, Hospi_y, Hospi_z, col = "brown", 
       type = "s", size = 0.5, add=TRUE)

# Plot the condicende region
shade3d(ellipsoid, col = "blue", alpha = 0.5)

# Add legend
#legend3d("topright", legend = c("PCA Analysis", "Predicted"), col = c("blue", "green"), pch = 16)

# Generació del model PCA en 2D i plotejat de gràfics representatius
pca2d <- PCA(datos_nci,ncp=5,graph=FALSE)
fviz_pca_var(pca2d, col.var = "contrib") # Scree plot
fviz_pca_biplot(pca2d, col.var = "contrib") # Biplot

# Dades i gràfics interessants sobre la variancia i desviació estàndard de les diferents variables
summary(pca3d)
eig.val <- get_eigenvalue(pca3d)
fviz_eig(pca3d, addlabels = TRUE, ylim = c(0, 20))

var <- get_pca_var(pca3d)
corrplot(var$contrib, is.corr=FALSE)

# Contributions of variables to PC1
fviz_contrib(pca3d, choice = "var", axes = 1, top = 10)

cor_matrix <- cor(datos_nci)
plot3d(cor_matrix, type = "s", size = 2)

