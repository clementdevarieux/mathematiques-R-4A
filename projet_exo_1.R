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








