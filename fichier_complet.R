# EXERCICE 1
# 1)
A = matrix(c(0, 1/2, 1/2, -2/3, 1, 2/3, -1/3, 1/2, 5/6), ncol=3, byrow = T )

valeur_propre = eigen(A)$values
multiplicite_vp = table(valeur_propre)
print(valeur_propre)
print(multiplicite_vp)

# on a 3 valeurs propres distintes de multiplicité 1 sur une matrice 3x3, donc A est diago

# 2)
matrice_random <- function(p) {
  mat = matrix(sample(1:10, p*p, replace = TRUE), p)
  print(mat)
  print('les valeurs propres sont : (multiplicité en dessous)')
  print(table(eigen(mat)$values))
  return(mat)
}

B = matrice_random(4)
# 3)
P = eigen(A)$vector
print(P)

# 4)
D = diag(valeur_propre)
print(D)
P_inv = solve(P)
print(P_inv)

calcul_verif_A = P%*%D%*%P_inv

print(calcul_verif_A)
print(A)

all.equal(A, calcul_verif_A)

# 5)

solve_system<-function(n){
  X0 = c(1, 2, 3)
  D = diag(valeur_propre)
  D_n = D^n
  Xn = P %*% D_n %*% P_inv %*% X0
  return(Xn)
}

print(solve_system(20000))


# on constate que ça tend vers (4, 4, 4) quand n tend vers + l'infini

# _____________________________________________________________________________________________________________


## EXERCICE 2
# 1-

A = matrix(c(-6, -3, 6, 1, -1, 2, 1, -6, 3, 6, 3, -2, 6, -3, 6, -1, 2, -1, 2, 3, -3, 6, 3, 2, -2, -1, 2, -3, 1, 2, 1, 6), ncol=4, byrow = T)

col1 <- A[, 1]
col2 <- A[, 2]
col3 <- A[, 3]
col4 <- A[, 4]

# Calculer le produit scalaire
dot_product <- sum(col1 * col2)

# Afficher le produit scalaire
dot_product

# Calculer le produit scalaire
dot_product <- sum(col1 * col3)

# Afficher le produit scalaire
dot_product

# Calculer le produit scalaire
dot_product <- sum(col1 * col4)

# Afficher le produit scalaire
dot_product


# 2- 

norms <- sqrt(colSums(A^2))

#Afficher les normes 
norms


# on remarque que la norme de chaque colonnes est égale à 10, on divise donc A par 10
U = A/10

print(U)

# 3-
UTU = t(U) %*% U
print(UTU)
# on remarque que la première matrice représente peut être la matrice identité

UUT = U %*% t(U)
print(UUT)
# represente une matrice symétrique 

# 4-

y = c(1, 3, -5, 4, 6, 7, 9, 8)

p = UUT %*% y

print(p)

z = y - p

print(z)

# p appartient à col de A par définition

print(sum(z * p))

# le produit scalaire de z . p est égale à 0, donc z est orthogonal à p

# 5 -

colU1 <- U[, 1]
colU2 <- U[, 2]
colU3 <- U[, 3]
colU4 <- U[, 4]

# Calculer le produit scalaire
dot_product <- sum(z * colU1)

# Afficher le produit scalaire
dot_product

# Calculer le produit scalaire
dot_product <- sum(z * colU2)

# Afficher le produit scalaire
dot_product

# Calculer le produit scalaire
dot_product <- sum(z * colU3)

# Afficher le produit scalaire
dot_product

# Calculer le produit scalaire
dot_product <- sum(z * colU4)

# Afficher le produit scalaire
dot_product

# 6 - Z est orthogonal à p or p appartient à col(A) donc z appartient orthogonal(Col(A))

# 7 - 

y = c(1, 1, 1, 1, 1, 1, 1, 1)

p = UUT %*% y

print(p)

# 8 - 

b <- c(1, 1, 1, 1, -1, -1, -1, -1)
projection_b <- UUT %*% b
distance <- sqrt(sum((b - projection_b)^2))
distance

# _______________________________________________________________________________________________________________

# 1 - fonction permettant de créer un nuage de points Delta
create_delta <- function(n, a, b, c, d) {
  x_points = sort(sample(a:b, n, replace=F), decreasing=F)
  y_points = sample(c:d, n, replace=T)
  
  plot(x = 1,
       type = "n",
       xlim = c(a, b),
       ylim = c(c, d),
       pch = 16,
       xlab = "x values",
       ylab = "y values"
  )
  
  points(x=x_points, y=y_points, pch=16, col="coral2")
  
  x_y_list = list(x_points = x_points, y_points = y_points)
  
  return(x_y_list)
}

create_delta(10, -10, 10, -5, 5)

# 2 - Fonction permettant de tracer une courbe passant par un nuage de points en utilisant la matrice de vandermonde
create_plots_vandermonde <- function(n, a, b, c, d) {
  
  xy_points = create_delta(n, a, b, c, d)
  x_points = xy_points$x_points
  y_points = xy_points$y_points
  
  V = matrix(rep(x_points, each = n)^(seq_len(n)-1), nrow = n, byrow=T)
  print(V)
  
  system_solve = solve(V)%*%y_points
  print(system_solve)
  
  polynomial <- function(x) {
    sum(system_solve * x^(seq_len(n)-1))
  }
  
  x_poly = seq(a, b, length.out = 100)
  y_poly = sapply(x_poly, polynomial)
  
  lines(x_poly, y_poly, col = "blue")
  
  cat("\nPolynôme de Vandermonde :\n")
  cat(sprintf("P(x) = %.4f", system_solve[n]))
  for (i in (n-1):1) {
    if (system_solve[i] >= 0) {
      cat(sprintf(" + %.4f*x^%d", abs(system_solve[i]), i-1))
    } else {
      cat(sprintf(" - %.4f*x^%d", abs(system_solve[i]), i-1))
    }
  }
  cat("\n")
  
}

create_plots_vandermonde(9, -20,20,-10,10)
create_plots_vandermonde(19, -20,20,-10,10)
create_plots_vandermonde(29, -20,20,-10,10)
# jouer avec la valeur de n, plus n est grand, plus c'est dur de trouver une solution

# 3/4 - Fonction permettant de tracer une courbe passant par tous les points du nuage delta grâce à la matrice de Newton et ses différences divisées
create_plots_newton <- function(n, a, b, c, d) {
  
  xy_points = create_delta(n, a, b, c, d)
  x_points = xy_points$x_points
  y_points = xy_points$y_points
  
  divided_differences <- function(x, y) {
    n = length(x)
    coef = matrix(0, n, n)
    coef[,1] = y
    
    for (j in 2:n) {
      for (i in 1:(n-j+1)) {
        coef[i,j] = (coef[i+1,j-1] - coef[i,j-1]) / (x[i+j-1] - x[i])
      }
    }
    return(coef)
  }
  
  newton_matrix = divided_differences(x_points, y_points)
  print("matrice de newton")
  print(newton_matrix)
  newton_coef = newton_matrix[1,]
  print("coefficients de newton")
  print(newton_coef)
  
  newton_polynomial <- function(x, x_points, newton_coef) {
    n = length(newton_coef)
    p = newton_coef[n]
    for (k in (n-1):1) {
      p = newton_coef[k] + (x - x_points[k]) * p
    }
    return(p)
  }
  
  x_poly = seq(a, b, length.out = 100)
  y_poly = sapply(x_poly, function(x) newton_polynomial(x, x_points, newton_coef))
  
  lines(x_poly, y_poly, col = "blue")
  
  cat("\nPolynôme d'interpolation de Newton :\n")
  n <- length(newton_coef)
  cat(sprintf("P(x) = %.4f", newton_coef[n]))
  for (i in (n-1):1) {
    if (newton_coef[i] >= 0) {
      cat(sprintf(" + %.4f*(x^%s)", abs(newton_coef[i]), i-1))
    } else {
      cat(sprintf(" - %.4f*(x^%s)", abs(newton_coef[i]), i-1))
    }
  }
  cat("\n")
}

create_plots_newton(9, 0,28,0,25)
create_plots_newton(19, 0,38,0,45)
create_plots_newton(29, 0,58,0,57)
# fonctionne tout le temps, car on a f(x_i) = y_i pour chaque points d'apres l'interpolation de newton
# mais par contre la forme de la courbe est absurde, passe par tous les points mais ne permet pas de prévoir une tendance

# 5 - Application des fonctions précédentes sur un nuage de points prédéfinit

x_points_defined_10 = c(0,1,2,3,4,5,6,7,8,9)
y_points_defined_10 = c(2,6,12,20,30,42,56,72,90,110)

x_points_defined_15 = c(0,1, 2, 3, 4, 5, 6, 7, 8,  9, 10, 11, 12, 13, 14)
y_points_defined_15 = c(2,6,12,20,30,42,56,72,90,110,132,156,182,210,240)

x_points_defined_20 = c(0,1, 2, 3, 4, 5, 6, 7, 8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19)
y_points_defined_20 = c(2,6,12,20,30,42,56,72,90,110,132,156,182,210,240,272,306,342,380,420)

plot(x = 1,
     type = "n",
     xlim = c(0, 19),
     ylim = c(0, 420),
     pch = 16,
     xlab = "x values",
     ylab = "y values"
)

vandermonde_10 <- function(x_points_defined_10, y_points_defined_10, n=10){
  points(x=x_points_defined_10, y=y_points_defined_10, pch=16, col="coral2")
  V = matrix(rep(x_points_defined_10, each = n)^(seq_len(n)-1), nrow = n, byrow=T)
  print(V)
  
  system_solve = solve(V)%*%y_points_defined_10
  print(system_solve)
  
  polynomial <- function(x) {
    sum(system_solve * x^(seq_len(n)-1))
  }
  
  x_poly = seq(0, 9, length.out = 100)
  y_poly = sapply(x_poly, polynomial)
  
  lines(x_poly, y_poly, col = "blue")
  
  cat("\nPolynôme de Vandermonde :\n")
  cat(sprintf("P(x) = %.4f", system_solve[n]))
  for (i in (n-1):1) {
    if (system_solve[i] >= 0) {
      cat(sprintf(" + %.4f*x^%d", abs(system_solve[i]), i-1))
    } else {
      cat(sprintf(" - %.4f*x^%d", abs(system_solve[i]), i-1))
    }
  }
  cat("\n")
}

newton_10 <- function(x_points_defined_10, y_points_defined_10, n=10){
  points(x=x_points_defined_10, y=y_points_defined_10, pch=16, col="coral2")
  divided_differences <- function(x, y) {
    n = length(x)
    coef = matrix(0, n, n)
    coef[,1] = y
    
    for (j in 2:n) {
      for (i in 1:(n-j+1)) {
        coef[i,j] = (coef[i+1,j-1] - coef[i,j-1]) / (x[i+j-1] - x[i])
      }
    }
    return(coef)
  }
  
  newton_matrix = divided_differences(x_points_defined_10, y_points_defined_10)
  print("matrice de newton")
  print(newton_matrix)
  newton_coef = newton_matrix[1,]
  print("coefficients de newton")
  print(newton_coef)
  
  newton_polynomial <- function(x, x_points_defined_10, newton_coef) {
    n = length(newton_coef)
    p = newton_coef[n]
    for (k in (n-1):1) {
      p = newton_coef[k] + (x - x_points_defined_10[k]) * p
    }
    return(p)
  }
  
  x_poly = seq(0, 9, length.out = 100)
  y_poly = sapply(x_poly, function(x) newton_polynomial(x, x_points_defined_10, newton_coef))
  
  lines(x_poly, y_poly, col = "blue")
  
  cat("\nPolynôme d'interpolation de Newton :\n")
  n <- length(newton_coef)
  cat(sprintf("P(x) = %.4f", newton_coef[n]))
  for (i in (n-1):1) {
    if (newton_coef[i] >= 0) {
      cat(sprintf(" + %.4f*(x^%s)", abs(newton_coef[i]), i-1))
    } else {
      cat(sprintf(" - %.4f*(x^%s)", abs(newton_coef[i]), i-1))
    }
  }
  cat("\n")
  
}

vandermonde_15 <- function(x_points_defined_15, y_points_defined_15, n=15){
  points(x=x_points_defined_15, y=y_points_defined_15, pch=16, col="coral2")
  V = matrix(rep(x_points_defined_15, each = n)^(seq_len(n)-1), nrow = n, byrow=T)
  print(V)
  
  system_solve = solve(V)%*%y_points_defined_15
  print(system_solve)
  
  polynomial <- function(x) {
    sum(system_solve * x^(seq_len(n)-1))
  }
  
  x_poly = seq(0, 14, length.out = 100)
  y_poly = sapply(x_poly, polynomial)
  
  lines(x_poly, y_poly, col = "blue")
  
  cat("\nPolynôme de Vandermonde :\n")
  cat(sprintf("P(x) = %.4f", system_solve[n]))
  for (i in (n-1):1) {
    if (system_solve[i] >= 0) {
      cat(sprintf(" + %.4f*x^%d", abs(system_solve[i]), i-1))
    } else {
      cat(sprintf(" - %.4f*x^%d", abs(system_solve[i]), i-1))
    }
  }
  cat("\n")
  
}

newton_15 <- function(x_points_defined_15, y_points_defined_15, n=15){
  points(x=x_points_defined_15, y=y_points_defined_15, pch=16, col="coral2")
  divided_differences <- function(x, y) {
    n = length(x)
    coef = matrix(0, n, n)
    coef[,1] = y
    
    for (j in 2:n) {
      for (i in 1:(n-j+1)) {
        coef[i,j] = (coef[i+1,j-1] - coef[i,j-1]) / (x[i+j-1] - x[i])
      }
    }
    return(coef)
  }
  
  newton_matrix = divided_differences(x_points_defined_15, y_points_defined_15)
  print("matrice de newton")
  print(newton_matrix)
  newton_coef = newton_matrix[1,]
  print("coefficients de newton")
  print(newton_coef)
  
  newton_polynomial <- function(x, x_points_defined_15, newton_coef) {
    n = length(newton_coef)
    p = newton_coef[n]
    for (k in (n-1):1) {
      p = newton_coef[k] + (x - x_points_defined_15[k]) * p
    }
    return(p)
  }
  
  x_poly = seq(0, 14, length.out = 100)
  y_poly = sapply(x_poly, function(x) newton_polynomial(x, x_points_defined_15, newton_coef))
  
  lines(x_poly, y_poly, col = "blue")
  
  cat("\nPolynôme d'interpolation de Newton :\n")
  n <- length(newton_coef)
  cat(sprintf("P(x) = %.4f", newton_coef[n]))
  for (i in (n-1):1) {
    if (newton_coef[i] >= 0) {
      cat(sprintf(" + %.4f*(x^%s)", abs(newton_coef[i]), i-1))
    } else {
      cat(sprintf(" - %.4f*(x^%s)", abs(newton_coef[i]), i-1))
    }
  }
  cat("\n")
  
}

vandermonde_20 <- function(x_points_defined_20, y_points_defined_20, n=20){
  points(x=x_points_defined_20, y=y_points_defined_20, pch=16, col="coral2")
  V = matrix(rep(x_points_defined_20, each = n)^(seq_len(n)-1), nrow = n, byrow=T)
  print(V)
  
  system_solve = solve(V)%*%y_points_defined_20
  print(system_solve)
  
  polynomial <- function(x) {
    sum(system_solve * x^(seq_len(n)-1))
  }
  
  x_poly = seq(0, 19, length.out = 100)
  y_poly = sapply(x_poly, polynomial)
  
  lines(x_poly, y_poly, col = "blue")
  
  cat("\nPolynôme de Vandermonde :\n")
  cat(sprintf("P(x) = %.4f", system_solve[n]))
  for (i in (n-1):1) {
    if (system_solve[i] >= 0) {
      cat(sprintf(" + %.4f*x^%d", abs(system_solve[i]), i-1))
    } else {
      cat(sprintf(" - %.4f*x^%d", abs(system_solve[i]), i-1))
    }
  }
  cat("\n")
  
}

newton_20 <- function(x_points_defined_20, y_points_defined_20, n=20){
  points(x=x_points_defined_20, y=y_points_defined_20, pch=16, col="coral2")
  divided_differences <- function(x, y) {
    n = length(x)
    coef = matrix(0, n, n)
    coef[,1] = y
    
    for (j in 2:n) {
      for (i in 1:(n-j+1)) {
        coef[i,j] = (coef[i+1,j-1] - coef[i,j-1]) / (x[i+j-1] - x[i])
      }
    }
    return(coef)
  }
  
  newton_matrix = divided_differences(x_points_defined_20, y_points_defined_20)
  print("matrice de newton")
  print(newton_matrix)
  newton_coef = newton_matrix[1,]
  print("coefficients de newton")
  print(newton_coef)
  
  newton_polynomial <- function(x, x_points_defined_20, newton_coef) {
    n = length(newton_coef)
    p = newton_coef[n]
    for (k in (n-1):1) {
      p = newton_coef[k] + (x - x_points_defined_20[k]) * p
    }
    return(p)
  }
  
  x_poly = seq(0, 19, length.out = 100)
  y_poly = sapply(x_poly, function(x) newton_polynomial(x, x_points_defined_20, newton_coef))
  
  lines(x_poly, y_poly, col = "blue")
  
  cat("\nPolynôme d'interpolation de Newton :\n")
  n <- length(newton_coef)
  cat(sprintf("P(x) = %.4f", newton_coef[n]))
  for (i in (n-1):1) {
    if (newton_coef[i] >= 0) {
      cat(sprintf(" + %.4f*(x^%s)", abs(newton_coef[i]), i-1))
    } else {
      cat(sprintf(" - %.4f*(x^%s)", abs(newton_coef[i]), i-1))
    }
  }
  cat("\n")
  
}

vandermonde_10(x_points_defined_10,y_points_defined_10)
newton_10(x_points_defined_10,y_points_defined_10)

vandermonde_15(x_points_defined_15,y_points_defined_15)
newton_15(x_points_defined_15,y_points_defined_15)

vandermonde_20(x_points_defined_20,y_points_defined_20)
newton_20(x_points_defined_20,y_points_defined_20)


plot(x = 1,
     type = "n",
     xlim = c(0, 19),
     ylim = c(0, 420),
     pch = 16,
     xlab = "x values",
     ylab = "y values"
)

# _____________________________________________________________________________________________________
# Exercice 4

library(stringi)

# Étape 1 : Fonction pour identifier les palindromes
is_palindrome <- function(mot) {
  mot_clean <- gsub(" ", "", tolower(mot))
  if (mot_clean == stri_reverse(mot_clean)) {
    return(paste(mot, "est un palindrome"))
  } else {
    return(paste(mot, "n'est pas un palindrome"))
  }
}

# Étape 2 : Appliquer la fonction sur une liste de mots et de phrases
mots <- c("radar", "bonne année", "sept", "kayak", "la mariée ira mal",
          "statistiques", "engage le jeu que je le gagne", "esope reste ici et se repose")
resultats <- sapply(mots, is_palindrome)
print(resultats)

# Étape 3 : Créer un dictionnaire de 8000 mots aléatoires
generate_random_word <- function(length) {
  paste(sample(letters, length, replace = TRUE), collapse = "")
}

dictionnaire <- c()
for (n in 2:9) {
  dictionnaire <- c(dictionnaire, replicate(1000, generate_random_word(n)))
}

# Étape 4 : Retourner tous les mots palindromiques du dictionnaire
palindromes <- dictionnaire[sapply(dictionnaire, function(mot) {
  mot == stri_reverse(mot)
})]

print(palindromes)

# __________________________________________________________________________________________________

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








