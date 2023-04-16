#Precàrrega de llibreries i paquets necessaris pel processat de dades i l'estudi PCA
install.packages("factoextra")
install.packages("FactoMineR")
install.packages("readxl")
install.packages("rgl")
install.packages("mgcv")
install.packages("nlme")
install.packages("car", dependencies = TRUE)
install.packages("ggplot2")
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
var_prop <- pca3d$sdev^2/sum(pca3d$sdev^2)

#Plotejat 3D del model PCA
scatter3d(pca3d$ind$coord[,1], pca3d$ind$coord[,2], pca3d$ind$coord[,3],color = "blue",
          pch = 19, cex = 1.5, xlab = "PC1", ylab = "PC2", zlab = "PC3")

cor_matrix <- cor(datos_nci)
plot3d(cor_matrix, type = "s", size = 2)

#Carreguem les dades dels altres pisos
Pompeu<- read_excel(path = "CASA_PADRE_MARC.xlsx")
datos_Pompeu<- Pompeu[,2:19]
scaled_Pompeu <- scale(datos_Pompeu)
rotated_Pompeu <- prcomp(scaled_Pompeu, center=TRUE, scale.=TRUE)$x
Pompeu_coords <- predict(pca3d, rotated_Pompeu)
x <- Pompeu_coords$coord[, 1]
y <- Pompeu_coords$coord[, 2]
z <- Pompeu_coords$coord[, 3]

scatter3d(pca3d$ind$coord[,1], pca3d$ind$coord[,2], pca3d$ind$coord[,3],color = "blue",
          pch = 19, cex = 1.5, xlab = "PC1", ylab = "PC2", zlab = "PC3")

points3d(x, y, z, col = "red", pch = 19, cex = 1.5, xlab = "Eje X", zlab = "Eje Z")

#Plotejat de l'el?lipse al voltant del set de dades principal

x <- pca$ind$coord[, 1]
y <- pca$ind$coord[, 2]
z <- pca$ind$coord[, 3]

pcm <- prcomp(pca$ind$coord, center = TRUE, scale. = TRUE)
x1<-pcm$center[1]
y1<-pcm$center[2]
z1<-pcm$center[3]

datos<-data.frame(x,y,z,x1,y1,z1)

# Ajustar l'el?lipse al voltant del conjunt de dades principals
cov_matrix <- cov(datos[, c("x", "y", "z")])
eigen_values <- eigen(cov_matrix)$values
eigen_vectors <- eigen(cov_matrix)$vectors
center <- colMeans(datos[, c("x1", "y1", "z1")])
radius_x <- sqrt(qchisq(0.95, df = 3) * eigen_values[1])
radius_y <- sqrt(qchisq(0.95, df = 3) * eigen_values[2])
radius_z <- sqrt(qchisq(0.95, df = 3) * eigen_values[3])
orientation <- eigen_vectors
position <- center - c(radius_x, radius_y, radius_z) %*% t(orientation)

# Plotejar l'el?lipse alvoltant del conjunt de dades principals
open3d()

# Verificar que x sigui una matriu
if (!is.matrix(x)) {
  stop("Error: x is not a matrix.")
} else {
  
  # Continuar con el resto del c?digo
}
rgl::ellipse3d(center = center, radii = c(radius_x, radius_y, radius_z), 
               orientation = orientation, position = position, color = "blue")
points3d(puntos$x, puntos$y, puntos$z, col = "green")

