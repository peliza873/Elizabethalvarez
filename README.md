# Elizabethalvarez
# 2020
# Séries temporelles et reproductibilité

Directives de l’évaluation :
Votre travail consiste à:
1. Créer une série temporelle du CO2 à partir des données de hawai.csv
2. Séparer la série en parties d'entraînement (environ 70% des données) et en partie test
3.Créer un modèle prévisionnel sur les données d'entraînement, puis projeter la prévision de  CO2 atmosphérique pour comparer aux données test
4. Effectuer une analyse des résidus
5. Commenter: le modèle est-il fiable? Comment pourrait-il être amélioré?


Fiabilité : Le modèle qui est pour moi le plus fiable est : ARIMA, car il montre une tendance similaire des données correspondant aux années précédentes. La p-valeur est sous le seuil 0.05. 

Amélioration : Une option pour améliorer la méthode est le prétraitement des données,  avec une transformation logarithmique pour éviter les prédictions de débits négatifs. Il est également possible d'utiliser une autre méthode pour évaluer la tendance et l'erreur de chaque modèle. Ils peuvent également être testés avec différentes covariables pour améliorer le modèle. 
