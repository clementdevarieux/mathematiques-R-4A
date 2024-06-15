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

