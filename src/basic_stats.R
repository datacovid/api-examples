library(rjson)
library(ggplot2)

# Téléchargement des données
token="<token>"
url=paste0("https://datacovid.org/api/answers/1/age;sortiesEn24h;confinementPieces;confinementNbPersonnes;diplome;lavageMain;gelHydroAlcoolique;preoccupation1;penseInfecte?token=",token)
result = fromJSON(file = url)
results = lapply(result$data, function(x) {
  x[sapply(x, is.null)] <- NA
  x
})

# Catégories d'âge
agecategories=c("18-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64",">=65")

results$age = as.numeric(unlist(results$age))
results$confinementPieces = as.numeric(unlist(results$confinementPieces))
results$sortiesEn24h = as.numeric(unlist(results$sortiesEn24h))
results$confinementNbPersonnes = as.numeric(unlist(results$confinementNbPersonnes))
results$diplome = as.numeric(unlist(results$diplome))
results$lavageMain = as.numeric(unlist(results$lavageMain))
results$gelHydroAlcoolique = as.numeric(unlist(results$gelHydroAlcoolique))
results$penseInfecte = as.numeric(unlist(results$penseInfecte))

results=as.data.frame.list(results)

# Distribution de l'âge
hist(results$age)

# Distribution du nombre de pièces
hist(results$confinementPieces)

# Distribution des diplomes
hist(results$diplome)

# % des personnes qui se pensent infectées
table(results$penseInfecte)/length(results$penseInfecte)

# Nb de pièce par foyer, en catégories
results$confinementPiecesCat=paste(results$confinementPieces)
results$confinementPiecesCat[results$confinementPieces==2 | results$confinementPieces==3] = "2-3"
results$confinementPiecesCat[results$confinementPieces>6] = ">6"

# Nb de personnes par foyer, en catégories
results$confinementNbPersonnesCat = paste(results$confinementNbPersonnes)
results$confinementNbPersonnesCat[results$confinementNbPersonnes==1] = "Seul" 
results$confinementNbPersonnesCat[results$confinementNbPersonnes==2] = "Deux" 
results$confinementNbPersonnesCat[results$confinementNbPersonnes>2] = "Plusieurs" 

# % de personnes dont l'épidémie est leur préoccupation n°1
sum(results$preoccupation1==1)/5000

# Le % de personnes qui se pensent infectés en fonction de la catégorie d'age
table(results$age[results$penseInfecte==1])/length(results$age[results$penseInfecte==1])


