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

}

# create_plots_vandermonde(9, 0,28,0,25)
# create_plots_vandermonde(19, 0,38,0,35)
# create_plots_vandermonde(29, 0,58,0,55)
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
  
}

# create_plots_newton(9, 0,28,0,25)
# create_plots_newton(19, 0,38,0,45)
# create_plots_newton(29, 0,58,0,57)
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
}

#vandermonde_10(x_points_defined_10,y_points_defined_10)
#newton_10(x_points_defined_10,y_points_defined_10)

#vandermonde_15(x_points_defined_15,y_points_defined_15)
#newton_15(x_points_defined_15,y_points_defined_15)

#vandermonde_20(x_points_defined_20,y_points_defined_20)
newton_20(x_points_defined_20,y_points_defined_20)