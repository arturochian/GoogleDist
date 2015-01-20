# librairies
library(RCurl)
library(RJSONIO)


# import de données
load("data/ville.RData")
dataOrig <- ville
dataDest <- ville

# nombre d'origines et de destination
dOrig <- dim(dataOrig)[1]
dDest <- dim(dataDest)[1]

# creation de la matrice vide
req <- matrix(nrow=dOrig,ncol=dDest,dimnames=list(dataOrig$NOM,dataDest$NOM))

# création de la matrice des requêtes à envoyer
for (i in 1:dOrig){
  for (j in 1:dDest){
    req[i,j] <- paste("http://maps.googleapis.com/maps/api/distancematrix/json?origins=",
                     paste(dataOrig$Y[i], dataOrig$X[i], sep = ","),
                     "&destinations=",
                     paste(dataDest$Y[j], dataDest$X[j], sep = ","),
                     "&mode=driving&language=en&sensor=false",
                     sep = "")
  }
}

# reception des requêtes
result <- apply(X = req, c(1,2), function(x) {try(getURL(URLencode(x)), silent = TRUE)})

# fonction de parsage des réponses 
decode <- function(x){
  # décodage du résultat de la requête
  if (length(fromJSON(x)$rows[[1]]$elements[[1]]) != 1){
    fromJSON(x)$rows[[1]]$elements[[1]]$duration$value
    # fromJSON(x)$rows[[1]]$elements[[1]]$distance$value # Pour avoir la distance en mètres plutôt que le temps en secondes
  } else {
    NA
  }
}
mat <- apply(X = result, c(1,2), decode)

# passage des secondes en minutes
matG <- round(mat/60, 0)
matG[1:5,1:5]