# --- Instalar y cargar librerías si no están presentes ---
if(!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)

if(!require(gridExtra)) install.packages("gridExtra", dependencies = TRUE)
library(gridExtra)

# --- Cargar dataset ---
data(iris)
X <- iris[, 1:4]

# --- PCA ---
pca <- prcomp(X, center = TRUE, scale. = TRUE)

# PCA 2D
iris_pca_2d <- data.frame(pca$x[, 1:2])
colnames(iris_pca_2d) <- c("PC1", "PC2")

# PCA 3D proyectado en 2D (PC1 vs PC3)
iris_pca_3d_2d <- data.frame(pca$x[, c(1,3)])
colnames(iris_pca_3d_2d) <- c("PC1", "PC3")

# --- Gráficos originales (sin k-means) ---
p1 <- ggplot(iris_pca_2d, aes(x = PC1, y = PC2)) +
  geom_point(color = "gray") +
  ggtitle("PCA 2D (PC1-PC2) sin k-means") +
  theme_minimal()

p2 <- ggplot(iris_pca_3d_2d, aes(x = PC1, y = PC3)) +
  geom_point(color = "gray") +
  ggtitle("PCA 3D proyectado (PC1-PC3) sin k-means") +
  theme_minimal()

# --- Mostrar los gráficos sin color ---
grid.arrange(p1, p2, ncol = 2)

# --- Funciones K-means manual ---
euclidean_distance <- function(p1, p2) sqrt(sum((p1 - p2)^2))

initialize_centroids <- function(data, k) {
  set.seed(42)
  indices <- sample(1:nrow(data), k)
  data[indices, ]
}

assign_clusters <- function(data, centroids) {
  apply(data, 1, function(point) {
    distances <- apply(centroids, 1, function(c) euclidean_distance(point, c))
    which.min(distances)
  })
}

calculate_centroids <- function(data, clusters, k) {
  centroids <- matrix(NA, nrow = k, ncol = ncol(data))
  for (i in 1:k) {
    cluster_points <- data[clusters == i, , drop = FALSE]
    if (nrow(cluster_points) > 0) {
      centroids[i, ] <- colMeans(cluster_points)
    } else {
      centroids[i, ] <- data[sample(1:nrow(data), 1), ]
    }
  }
  centroids
}

kmeans_manual <- function(data, k, max_iters = 100) {
  centroids <- initialize_centroids(data, k)
  for (i in 1:max_iters) {
    clusters <- assign_clusters(data, centroids)
    new_centroids <- calculate_centroids(data, clusters, k)
    if (all(centroids == new_centroids)) break
    centroids <- new_centroids
  }
  list(centroids = centroids, clusters = clusters)
}

# --- Ejecutar K-means manual ---
result <- kmeans_manual(X, k = 3)
clusters <- result$clusters
centroids <- result$centroids

# Asignar nombres a columnas de centroides (para poder proyectar en PCA)
colnames(centroids) <- colnames(X)

# Proyectar centroides al espacio PCA
centroids_pca <- predict(pca, newdata = centroids)
centroids_pca_df <- data.frame(PC1 = centroids_pca[,1],
                               PC2 = centroids_pca[,2],
                               PC3 = centroids_pca[,3])

# Agregar clusters a los dataframes PCA
iris_pca_2d$cluster <- as.factor(clusters)
iris_pca_3d_2d$cluster <- as.factor(clusters)

# --- Gráficos con color (k-means) ---
p3 <- ggplot(iris_pca_2d, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(alpha = 0.7) +
  geom_point(data = centroids_pca_df, aes(x = PC1, y = PC2),
             color = "red", shape = 4, size = 2.5, stroke = 1.5) +
  ggtitle("PCA 2D K-means manual") +
  theme_minimal() +
  theme(legend.position = "none")

p4 <- ggplot(iris_pca_3d_2d, aes(x = PC1, y = PC3, color = cluster)) +
  geom_point(alpha = 0.7) +
  geom_point(data = centroids_pca_df, aes(x = PC1, y = PC3),
             color = "red", shape = 4, size = 2.5, stroke = 1.5) +
  ggtitle("PCA 3D K-means manual") +
  theme_minimal() +
  theme(legend.position = "none")

# --- Mostrar los gráficos con color ---
grid.arrange(p3, p4, ncol = 2)

# ============================================================
# Comparación PCA 2D - K-means manual vs clases reales ---
# ============================================================

iris_pca_2d$Real <- iris$Species

# Gráfico del modelo K-means manual
p_kmeans <- ggplot(iris_pca_2d, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(alpha = 0.8) +
  ggtitle("Clusters (K-means manual)") +
  theme_minimal() +
  theme(legend.position = "none")

# Gráfico con las clases reales del dataset
p_real <- ggplot(iris_pca_2d, aes(x = PC1, y = PC2, color = Real)) +
  geom_point(alpha = 0.8) +
  ggtitle("Clases reales del dataset Iris") +
  theme_minimal() +
  theme(legend.position = "none")

# Mostrar comparación lado a lado
grid.arrange(p_kmeans, p_real, ncol = 2,
             top = "Comparación PCA 2D: K-means manual vs Clases reales")

# ============================================================
# K-Means con librerias
# ============================================================

set.seed(42)
kmeans_lib <- kmeans(X,centers = 3)

clusters_lib <- kmeans_lib$cluster
centroids_lib <- kmeans_lib$centers
colnames(centroids_lib) <- colnames(X)

centroids_pca <- predict(pca, newdata = centroids_lib)
centroids_pca_df <- data.frame(PC1 = centroids_pca[,1],
                               PC2 = centroids_pca[,2],
                               PC3 = centroids_pca[,3])

# Agregar clusters a los dataframes PCA
iris_pca_2d$cluster <- as.factor(clusters_lib)
iris_pca_3d_2d$cluster <- as.factor(clusters_lib)

# --- Gráficos con color (K-means librerías) ---
p3 <- ggplot(iris_pca_2d, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(alpha = 0.7) +
  geom_point(data = centroids_pca_df, aes(x = PC1, y = PC2),
             color = "red", shape = 4, size = 2.5, stroke = 1.5) +
  ggtitle("PCA 2D K-means con librerías") +
  theme_minimal() +
  theme(legend.position = "none")

p4 <- ggplot(iris_pca_3d_2d, aes(x = PC1, y = PC3, color = cluster)) +
  geom_point(alpha = 0.7) +
  geom_point(data = centroids_pca_df, aes(x = PC1, y = PC3),
             color = "red", shape = 4, size = 2.5, stroke = 1.5) +
  ggtitle("PCA 3D K-means con librerías") +
  theme_minimal() +
  theme(legend.position = "none")

# Mostrar los gráficos con color
grid.arrange(p3, p4, ncol = 2)

# ============================================================
# --- Comparación PCA 2D - K-means librerías vs clases reales ---
# ============================================================

iris_pca_2d$Real <- iris$Species

# Gráfico del modelo K-means librerías
p_kmeans <- ggplot(iris_pca_2d, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(alpha = 0.8) +
  ggtitle("Clusters (K-means librerías)") +
  theme_minimal() +
  theme(legend.position = "none")

# Gráfico con las clases reales del dataset
p_real <- ggplot(iris_pca_2d, aes(x = PC1, y = PC2, color = Real)) +
  geom_point(alpha = 0.8) +
  ggtitle("Clases reales del dataset Iris") +
  theme_minimal() +
  theme(legend.position = "none")

# Mostrar comparación lado a lado
grid.arrange(p_kmeans, p_real, ncol = 2,
             top = "Comparación PCA 2D: K-means librerías vs Clases reales")

# ============================================================
# Cálculo de INERCIA y PRECISIÓN (manual y con librerías)
# ============================================================

# --- Función de inercia ---
calcular_inercia <- function(data, centroids, clusters) {
  inercia <- 0
  for (i in 1:nrow(data)) {
    centroide <- centroids[clusters[i], ]
    dist <- sum((data[i, ] - centroide)^2)
    inercia <- inercia + dist
  }
  return(inercia)
}

# --- Función de precisión (comparación con etiquetas reales) ---
precision_kmeans <- function(y, clusters, k) {
  aciertos <- 0
  for (cluster_id in 1:k) {
    indices <- which(clusters == cluster_id)
    if (length(indices) == 0) next
    etiquetas <- y[indices]
    etiqueta_mas_comun <- names(sort(table(etiquetas), decreasing = TRUE))[1]
    aciertos <- aciertos + sum(etiquetas == etiqueta_mas_comun)
  }
  return(aciertos / length(y))
}

# ============================================================
# --- Métricas para K-Means MANUAL ---
# ============================================================

inercia_manual <- calcular_inercia(X, centroids, clusters)
precision_manual <- precision_kmeans(iris$Species, clusters, k = 3)

cat("🔹 Inercia (K-Means manual):", inercia_manual, "\n")
cat("🔹 Precisión (K-Means manual):", round(precision_manual, 4), "\n\n")

# ============================================================
# --- Métricas para K-Means con LIBRERÍAS ---
# ============================================================

inercia_lib <- kmeans_lib$tot.withinss  # la inercia total del modelo de R
precision_lib <- precision_kmeans(iris$Species, clusters_lib, k = 3)

cat("🔸 Inercia (K-Means librerías):", inercia_lib, "\n")
cat("🔸 Precisión (K-Means librerías):", round(precision_lib, 4), "\n")

