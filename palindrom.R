

library(stringi)

# Étape 1 : Fonction pour identifier les palindromes

# Fonction pour normaliser les caractères accentués
normalize_accents <- function(text) {
  stri_trans_general(text, "Latin-ASCII")
}

# Fonction pour identifier les palindromes
is_palindrome <- function(mot) {
  mot_clean <- gsub(" ", "", tolower(normalize_accents(mot)))
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

