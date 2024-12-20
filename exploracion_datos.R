
# Exploración de datos y metadatos desde un archivo .Rda

# 1. Cargar el archivo .Rda
load('datos_y_metadatos.Rda')

# 2. Acceder a los datos
expression_data <- contenedor$expression_data
metadata <- contenedor$metadata

# 3. Exploración de datos de expresión
cat('Resumen de la matriz de expresión:\n')
summary(expression_data)

cat('\nDimensiones de la matriz de expresión:\n')
dim(expression_data)

cat('\nPrimeras filas de la matriz de expresión:\n')
head(expression_data)

# 4. Exploración de metadatos
cat('\nResumen de los metadatos:\n')
summary(metadata)

cat('\nPrimeras filas de los metadatos:\n')
head(metadata)

# 5. Histogramas para verificar la distribución de la expresión
library(ggplot2)
expression_means <- rowMeans(expression_data)

ggplot(data.frame(MeanExpression = expression_means), aes(x = MeanExpression)) +
  geom_histogram(bins = 50, fill = 'blue', color = 'white', alpha = 0.7) +
  labs(
    title = 'Distribución de los niveles de expresión',
    x = 'Nivel medio de expresión',
    y = 'Frecuencia'
  ) +
  theme_minimal()

# 6. PCA para evaluar agrupaciones en las muestras
library(ggplot2)
pca_result <- prcomp(t(expression_data), scale. = TRUE)

# Convertir resultados a data frame
pca_df <- data.frame(
  PC1 = pca_result$x[, 1],
  PC2 = pca_result$x[, 2],
  Group = metadata$agent  # Ajustar según la columna relevante en tus metadatos
)

# Gráfico PCA
ggplot(pca_df, aes(x = PC1, y = PC2, color = Group)) +
  geom_point(size = 3) +
  labs(
    title = 'PCA: Agrupación de muestras',
    x = 'PC1',
    y = 'PC2',
    color = 'Grupo'
  ) +
  theme_minimal()

# 7. Boxplot para evaluar la calidad de los datos
boxplot(expression_data, main = 'Boxplot de niveles de expresión', las = 2, outline = FALSE)

# 8. Guardar resultados básicos
write.csv(expression_means, file = 'expression_means.csv')
write.csv(metadata, file = 'metadata_exploration.csv')

cat('\nExploración completada. Resultados básicos guardados.\n')
