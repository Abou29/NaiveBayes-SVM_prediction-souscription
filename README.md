# Prédire la souscription d'un client à un service bancaire avec naive bayes et avec svm (support vector machine)
Les données sont liées aux campagnes de marketing d'une institution bancaire portugaise. Ces campagnes de marketing étaient basées sur des appels téléphoniques. Souvent, plusieurs contacts avec le même client étaient nécessaires pour savoir si le client va souscrire ou non à un service bancaire. 

L'objectif de la classification est de prédire si le client va souscrire (yes/no) à un service bancaire.

Les attributs sont :

- age : age du client 
- job : type de job du client (categorical: 'admin.','blue-collar','entrepreneur','housemaid','management','retired','self-employed','services','student','technician','unemployed','unknown')
- marital : état matrimonial
- education : niveau d'études (categorical: 'basic.4y','basic.6y','basic.9y','high.school','illiterate','professional.course','university.degree','unknown')
- default : has credit in default? (categorical: 'no','yes','unknown')
- balance : 
- housing : a un prêt au logement ? (categorical: 'no','yes','unknown') 
- loan : a un prêt personnel ? (categorical: 'no','yes','unknown') 
- contact : type de communication de contact (categorical: 'cellular','telephone')
- day : dernier jour de contact de la semaine (categorical: 'mon','tue','wed','thu','fri')   
- month : last contact month of year (categorical: 'jan', 'feb', 'mar', ..., 'nov', 'dec')
- duration : durée du dernier contact, en secondes (numérique) 
- campaign : number of contacts performed during this campaign and for this client (numeric, includes last contact)
- pdays : number of days that passed by after the client was last contacted from a previous campaign (numeric; 999 means client was not previously contacted)
- previous : number of contacts performed before this campaign and for this client (numeric)
- poutcome : outcome of the previous marketing campaign (categorical: 'failure','nonexistent','success')
- **y : le client a-t-il souscrit a un service bancaire**
