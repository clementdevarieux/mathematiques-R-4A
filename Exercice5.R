# Assurez-vous de spécifier le chemin correct et le délimiteur approprié pour votre fichier .dat
#donnees <- read.table("/Users/Alanboloorian_1/Downloads/Décathlon (données pour l'ACP).dat", header = TRUE, sep = ",")
library(stats)
library(factoextra)




# QUESTION 1: Récupérer les données du fichier "decathlon" et donner la matrice corrélation des variables quantitatives (ne pas prendre COMPET)
# On charge les données 

donnees <- read.table("/Users/Alanboloorian_1/Downloads/Décathlon (données pour l'ACP).dat", header = TRUE, fill = TRUE, sep = " ")
print(donnees)
# Suppression de la colonne "COMPET" car elle est qualitative
donnees_sans_compet <- donnees[, -13]
print(donnees_sans_compet)
# Calcul de la matrice de corrélation
matrice_correlation <- cor(donnees_sans_compet)
# Affiche la matrice de corrélation
print(matrice_correlation)




#QUESTION 2: Quelles sont les couples de variables les plus corrélées, les moins corréelées, les plus opposées ? Justifier.
# Création d'une copie de la matrice de corrélation pour ne pas manipuler l'originale

cor_matrix <- matrice_correlation
# Mettre la diagonale à NA pour éviter de compter la corrélation des variables avec elles-mêmes
diag(cor_matrix) <- NA
# Le couple de variables le plus corrélé
max_correlation_value <- max(cor_matrix, na.rm = TRUE)
max_correlation_indices <- which(cor_matrix == max_correlation_value, arr.ind = TRUE)
most_correlated <- names(donnees_sans_compet)[max_correlation_indices]
# Le couple de variables le moins corrélé (corrélation absolue la plus basse)
min_correlation_value <- min(abs(cor_matrix), na.rm = TRUE)
min_correlation_indices <- which(abs(cor_matrix) == min_correlation_value, arr.ind = TRUE)
least_correlated <- names(donnees_sans_compet)[min_correlation_indices]
# Le couple de variables le plus opposé (corrélation la plus négative)
min_negative_correlation_value <- min(cor_matrix, na.rm = TRUE)
min_negative_correlation_indices <- which(cor_matrix == min_negative_correlation_value, arr.ind = TRUE)
most_opposite <- names(donnees_sans_compet)[min_negative_correlation_indices]
# Affichage des résultats avec les valeurs de corrélation
cat("Les variables les plus corrélées sont :", most_correlated, "avec une corrélation de", max_correlation_value, "\n")
cat("Les variables les moins corrélées (absolument) sont :", least_correlated, "avec une corrélation de", min_correlation_value, "\n")
cat("Les variables les plus opposées sont :", most_opposite, "avec une corrélation de", min_negative_correlation_value, "\n")




#QUESTION 4
# Sélection des colonnes (exclusion de RANG, POINTS et COMPET)
donnees_acp <- donnees[, 1:10]
# Standardisation des données
donnees_acp <- scale(donnees_acp)
# Execution de l'ACP
resultat_acp <- prcomp(donnees_acp, scale. = TRUE)
# Calcul de la matrice de corrélation
correlation_matrix <- cor(donnees_acp)
# Extraction et tri des valeurs propres
eigen_results <- eigen(correlation_matrix)
sorted_eigen_values <- sort(eigen_results$values, decreasing = TRUE)
# Affichage des valeurs propres triées
print(sorted_eigen_values)
# Application de la règle de Kaiser (on prend ceux qui sont supérieurs à 1)
kaiser_rule <- sum(sorted_eigen_values > 1)
cat("Nombre de composantes principales à retenir selon la règle de Kaiser :", kaiser_rule, "\n")
# Calcul et affichage du pourcentage d'inertie expliquée
total_variance_explained <- sum(sorted_eigen_values[1:3]) / sum(sorted_eigen_values) * 100
cat("Pourcentage d'inertie expliquée par les trois premières composantes :", total_variance_explained, "%\n")




#QUESTION 5: Déterminer les trois composantes principales (projection des individus sur les trois vecteurs propres), que l'on note C1,C2,C3 dans l'ordre décroisant d'inertie

# Les scores des trois premières composantes principales
scores <- data.frame(
  C1 = resultat_acp$x[,1], 
  C2 = resultat_acp$x[,2],
  C3 = resultat_acp$x[,3]   
)
# Affichage des premiers scores
head(scores)




#QUESTION 6: Déterminer le tableau des corrélations des variables par rapport à C1,C2,C3 et donner les deux cercles de corrélation des variables par rapport à (C1,C2) et (C2,C3)

# On récupère`resultat_acp` qui est notre résultat de prcomp
loadings <- resultat_acp$rotation  # Les loadings (corrélation entre variables originales et composantes)
# Extraire les loadings pour les trois premières composantes principales
loadings_c1_c2_c3 <- loadings[, 1:3]
# Affichage du tableau des loadings
print(loadings_c1_c2_c3)
# Cercle de corrélation pour C1 et C2
fviz_pca_var(resultat_acp, axes = c(1, 2), 
             col.var = "contrib", # Colorer les variables en fonction de leur contribution
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)  # Pour éviter le chevauchement du texte
title("Cercle de Corrélation (C1, C2)")
# Cercle de corrélation pour C2 et C3
fviz_pca_var(resultat_acp, axes = c(2, 3), 
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
title("Cercle de Corrélation (C2, C3)")




