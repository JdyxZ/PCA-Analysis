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
datos_nci <- Parellada[-c(1,5,6,8,9,12,13,14,16,17)]
mean.nci <- colMeans(datos_nci)
std.nci <- apply(datos_nci,2,sd)

# Scale dataset in order to plot properly
scaled_data <- scale(datos_nci, center=mean.nci, scale=std.nci)

# Create the PCA model
pca3d <- PCA(scaled_data,ncp=3,graph=TRUE)

# Get some useful data from the PCA model
PCA_center <- colMeans(pca3d$ind$coord)
PCA_cov <- cov(pca3d$ind$coord)

# Create an ellipsoid object to represent the conficende region
ellipsoid <- ellipse3d(PCA_cov, centre = PCA_center,level = 0.9)

# Set coordinates of the PCA Analysis into auxiliary variables
PCA_x <- pca3d$ind$coord[,1]
PCA_y <- pca3d$ind$coord[,2]
PCA_z <- pca3d$ind$coord[,3]

# Plot the PCA model
scatter3d(PCA_x, PCA_y, PCA_z, point.col = "steelblue",
          sphere.size = 1.5, xlab = "PC1", ylab = "PC2", zlab = "PC3", surface = FALSE, ellipsoid = TRUE)

# Load the datasets of the others homes
Pompeu<- read_excel(path = "CASA_PADRE_MARC_PCA.xlsx")
datos_Pompeu<- Pompeu[-c(1,5,6,8,9,12,13,14,16,17)]

Montigala<- read_excel(path = "CASA_MARC_PCA.xlsx")
datos_Montigala<- Montigala[-c(1,5,6,8,9,12,13,14,16,17)]

Nil<- read_excel(path = "CASA_MARTA_PCA.xlsx")
datos_Nil<- Nil[-c(1,5,6,8,9,12,13,14,16,17)]

Pitagoras<- read_excel(path = "CASA_NATALIA_PCA.xlsx")
datos_Pitagoras<- Pitagoras[-c(1,5,6,8,9,12,13,14,16,17)]

Junta<- read_excel(path = "CASA_JUNTA_COMERÇ_PCA.xlsx")
datos_Junta<- Junta[-c(1,5,6,8,9,12,13,14,16,17)]

Erasme<- read_excel(path = "CASA_SANT_ERASME_PCA.xlsx")
datos_Erasme<- Erasme[-c(1,5,6,8,9,12,13,14,16,17)]

Manso<- read_excel(path = "GENERAL_MANSO_PCA.xlsx")
datos_Manso<- Manso[-c(1,5,6,8,9,12,13,14,16,17)]

Hospi<- read_excel(path = "HOSPITALET_DE_LLOBREGAT_PCA.xlsx")
datos_Hospi<- Hospi[-c(1,5,6,8,9,12,13,14,16,17)]

PeudelaCreu<- read_excel(path = "CASA_PEU_DE_LA_CREU_PCA.xlsx")
datos_PeudelaCreu<- PeudelaCreu[-c(1,5,6,8,9,12,13,14,16,17)]

SantBoi<- read_excel(path = "CASA_SANT_BOI_DE_LLOBREGAT_PCA.xlsx")
datos_SantBoi<- SantBoi[-c(1,5,6,8,9,12,13,14,16,17)]

Carretes<- read_excel(path = "CASA_CARRETES_PCA.xlsx")
datos_Carretes<- Carretes[-c(1,5,6,8,9,12,13,14,16,17)]

# Scale dataset in order to plot properly
scaled_Pompeu <- scale(datos_Pompeu,center=mean.nci, scale=std.nci)
scaled_Montigala <- scale(datos_Montigala, center=mean.nci, scale=std.nci)
scaled_Nil <- scale(datos_Nil, center=mean.nci, scale=std.nci)
scaled_Pitagoras <- scale(datos_Pitagoras, center=mean.nci, scale=std.nci)
scaled_Junta <- scale(datos_Junta, center=mean.nci, scale=std.nci)
scaled_Erasme <- scale(datos_Erasme, center=mean.nci, scale=std.nci)
scaled_Manso <- scale(datos_Manso, center=mean.nci, scale=std.nci)
scaled_Hospi <- scale(datos_Hospi, center=mean.nci, scale=std.nci)
scaled_PeudelaCreu <- scale(datos_PeudelaCreu, center=mean.nci, scale=std.nci)
scaled_SantBoi <- scale(datos_SantBoi, center=mean.nci, scale=std.nci)
scaled_Carretes <- scale(datos_Carretes, center=mean.nci, scale=std.nci)

# Make a prediction based on the PCA model
Pompeu_coords <- predict(pca3d, scaled_Pompeu)

Montigala_coords <- predict(pca3d, scaled_Montigala)

Nil_coords <- predict(pca3d, scaled_Nil)

Pitagoras_coords <- predict(pca3d, scaled_Pitagoras)

Junta_coords <- predict(pca3d, scaled_Junta)

Erasme_coords <- predict(pca3d, scaled_Erasme)

Manso_coords <- predict(pca3d, scaled_Manso)

Hospi_coords <- predict(pca3d, scaled_Hospi)

PeudelaCreu_coords <- predict(pca3d, scaled_PeudelaCreu)

SantBoi_coords <- predict(pca3d, scaled_SantBoi)

Carretes_coords <- predict(pca3d, scaled_Carretes)

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

PeudelaCreu_x <- PeudelaCreu_coords$coord[, 1]
PeudelaCreu_y <- PeudelaCreu_coords$coord[, 2]
PeudelaCreu_z <- PeudelaCreu_coords$coord[, 3]

SantBoi_x <- SantBoi_coords$coord[, 1]
SantBoi_y <- SantBoi_coords$coord[, 2]
SantBoi_z <- SantBoi_coords$coord[, 3]

Carretes_x <- Carretes_coords$coord[, 1]
Carretes_y <- Carretes_coords$coord[, 2]
Carretes_z <- Carretes_coords$coord[, 3]

# Plot PCA Analysis data
plot3d(PCA_x, PCA_y, PCA_z, col = "purple", 
       type = "s", size = 0.5, xlab = "PC1", ylab = "PC2", zlab = "PC3", main="3D Scatter Plot",xlim = c(-25, 5), ylim = c(-10, 8),zlim = c(-20,10))

# Plot the condicende region
shade3d(ellipsoid, col = "blue", alpha = 0.5)

# Plot Prediction Model data
plot3d(Pompeu_x, Pompeu_y, Pompeu_z, col = "yellow", 
       type = "s", size = 0.5, add=TRUE)

plot3d(Junta_x, Junta_y, Junta_z, col = "darkblue", 
       type = "s", size = 0.5, add=TRUE)

plot3d(Erasme_x, Erasme_y, Erasme_z, col = "black", 
       type = "s", size = 0.5, add=TRUE)

plot3d(Manso_x, Manso_y, Manso_z, col = "pink", 
       type = "s", size = 0.5, add=TRUE)

plot3d(Montigala_x, Montigala_y, Montigala_z, col = "red", 
       type = "s", size = 0.5, add=TRUE)

plot3d(Nil_x, Nil_y, Nil_z, col = "orange", 
       type = "s", size = 0.5, add=TRUE)

plot3d(Pitagoras_x, Pitagoras_y, Pitagoras_z, col = "darkgreen", 
       type = "s", size = 0.5, add=TRUE)

plot3d(Hospi_x, Hospi_y, Hospi_z, col = "brown", 
       type = "s", size = 0.5, add=TRUE)

plot3d(PeudelaCreu_x, PeudelaCreu_y, PeudelaCreu_z, col = "lightgreen", 
       type = "s", size = 0.5, add=TRUE)

plot3d(SantBoi_x, SantBoi_y, SantBoi_z, col = "cyan", 
       type = "s", size = 0.5, add=TRUE)

plot3d(Carretes_x, Carretes_y, Carretes_z, col = "lightblue", 
       type = "s", size = 0.5, add=TRUE)

# Add legend
legend3d("right", legend = c("Parellada", "Pompeu","Junta","Erasme","Manso","Montigalà","Nil","Pitàgores","Hospi","Peu de la Creu","Sant Boi","Carretes"), col = c("purple", "yellow","darkblue","black","pink","red","orange","darkgreen","brown","lightgreen","cyan","lightblue"), pch = 16)

# 2D PCA model generation and plotting of different representative graphs
pca2d <- PCA(scaled_data,ncp=5,graph=FALSE)
fviz_pca_var(pca2d, col.var = "contrib") # Scree plot
fviz_pca_biplot(pca2d, col.var = "contrib") # Biplot
cor_matrix <- cor(scaled_data)
corrplot(cor_matrix, method = "color") # Correlation variables matrix

# Interesting data about 3D PCA model and graphs abaout variance and standard deviation of the different variables
summary(pca3d)

eig.val <- get_eigenvalue(pca3d)
fviz_eig(pca3d, addlabels = TRUE, ylim = c(0, 50))

var <- get_pca_var(pca3d)
corrplot(var$contrib, is.corr=FALSE)
plot3d(cor_matrix, type = "s", size = 2)

# Contributions of variables to PC1 and PC2
fviz_contrib(pca3d, choice = "var", axes = 1, top = 10)
fviz_contrib(pca3d, choice = "var", axes = 2, top = 10)

# Mahalanobis distance
PCA_mahalanobis <- pca3d$ind$coord
Pompeu_mahalanobis <- Pompeu_coords$coord
Junta_mahalanobis <- Junta_coords$coord
Erasme_mahalanobis <- Erasme_coords$coord
Manso_mahalanobis <- Manso_coords$coord
Montigala_mahalanobis <- Montigala_coords$coord
Nil_mahalanobis <- Nil_coords$coord
Pitagoras_mahalanobis <- Pitagoras_coords$coord
Hospi_mahalanobis <- Hospi_coords$coord
PeudelaCreu_mahalanobis <- PeudelaCreu_coords$coord
SantBoi_mahalanobis <- SantBoi_coords$coord
Carretes_mahalanobis <- Carretes_coords$coord

# Calculate covariance matrix and inverse
cov_mat <- cov(PCA_mahalanobis)
cov_mat_inv <- solve(cov_mat)

# Calculate Mahalanobis distance between the first observation of data1 and data2
mah_dist_Pompeu <- mahalanobis(PCA_mahalanobis[1,], colMeans(Pompeu_mahalanobis), cov_mat_inv)
mah_dist_Pompeu

mah_dist_Junta <- mahalanobis(PCA_mahalanobis[1,], colMeans(Junta_mahalanobis), cov_mat_inv)
mah_dist_Junta

mah_dist_Erasme <- mahalanobis(PCA_mahalanobis[1,], colMeans(Erasme_mahalanobis), cov_mat_inv)
mah_dist_Erasme

mah_dist_Manso <- mahalanobis(PCA_mahalanobis[1,], colMeans(Manso_mahalanobis), cov_mat_inv)
mah_dist_Manso

mah_dist_Montigala <- mahalanobis(PCA_mahalanobis[1,], colMeans(Montigala_mahalanobis), cov_mat_inv)
mah_dist_Montigala

mah_dist_Nil <- mahalanobis(PCA_mahalanobis[1,], colMeans(Nil_mahalanobis), cov_mat_inv)
mah_dist_Nil

mah_dist_Pitagoras <- mahalanobis(PCA_mahalanobis[1,], colMeans(Pitagoras_mahalanobis), cov_mat_inv)
mah_dist_Pitagoras

mah_dist_Hospi <- mahalanobis(PCA_mahalanobis[1,], colMeans(Hospi_mahalanobis), cov_mat_inv)
mah_dist_Hospi

mah_dist_PeudelaCreu <- mahalanobis(PCA_mahalanobis[1,], colMeans(PeudelaCreu_mahalanobis), cov_mat_inv)
mah_dist_PeudelaCreu

mah_dist_SantBoi <- mahalanobis(PCA_mahalanobis[1,], colMeans(SantBoi_mahalanobis), cov_mat_inv)
mah_dist_SantBoi

mah_dist_Carretes <- mahalanobis(PCA_mahalanobis[1,], colMeans(Carretes_mahalanobis), cov_mat_inv)
mah_dist_Carretes

# Volume of data sets
# Calculate the convex hull and volume 
install.packages("geometry")
library(geometry)

ch_Pompeu <- convhulln(Pompeu_coords$coord, output.options = TRUE) 
ch_Junta  <- convhulln(Junta_coords$coord, output.options = TRUE)
ch_Erasme <- convhulln(Erasme_coords$coord, output.options = TRUE) 
ch_Manso <- convhulln(Manso_coords$coord, output.options = TRUE)
ch_Montigala <- convhulln(Montigala_coords$coord, output.options = TRUE)
ch_Nil <- convhulln(Nil_coords$coord, output.options = TRUE)
ch_Pitagoras <- convhulln(Pitagoras_coords$coord, output.options = TRUE)
ch_Hospi <- convhulln(Hospi_coords$coord, output.options = TRUE)
ch_PeudelaCreu <- convhulln(PeudelaCreu_coords$coord, output.options = TRUE)
ch_SantBoi <- convhulln(SantBoi_coords$coord, output.options = TRUE)
ch_Carretes <- convhulln(Carretes_coords$coord, output.options = TRUE)

volume_Pompeu <- ch_Pompeu$vol
volume_Junta <- ch_Junta$vol
volume_Erasme <- ch_Erasme$vol
volume_Manso <- ch_Manso$vol
volume_Montigala <- ch_Montigala$vol
volume_Nil <- ch_Nil$vol
volume_Pitagoras <- ch_Pitagoras$vol
volume_Hospi <- ch_Hospi$vol
volume_PeudelaCreu <- ch_PeudelaCreu$vol
volume_SantBoi <- ch_SantBoi$vol
volume_Carretes <- ch_Carretes$vol

# Print the volume
print(volume_Pompeu)
print(volume_Junta)
print(volume_Erasme)
print(volume_Manso)
print(volume_Montigala)
print(volume_Nil)
print(volume_Pitagoras)
print(volume_Hospi)
print(volume_PeudelaCreu)
print(volume_SantBoi)
print(volume_Carretes)