
############################################################
#Sp√©cifier un r√©pertoire de travail contenant tous les fichiers de donn√©es n√©cessaires### Load packages####
#setwd("C:/Users/Samuel Provencher-T/Documents")
setwd("/Users/nouveau/Desktop/programmation/Eco_numerique/Travail session")
############################################################
#Packages
#Le script fonctionne avec la version R 3.6.3


library(tidyverse)
#le code fonctionne avec tidyverse 1.3.0
library(ggplot2)
#le code fonctionne avec ggplot 3.2.1
library(ggraph)
#le code fonctionne avec gggraph 1.1.0
library(igraph)
#le code fonctionne avec igraph 1.1.2
library(dplyr)
#le code fonctionne avec dplyr 0.7.4
library(plyr)
#le code fonctionne avec plyr 1.8.4
library(reshape2)
#le code fonctionne avec reshape 1.4.3
library(RSQLite)
#le code fonctionne avec RSQLite 2.0
library(Matrix)
#le code fonctionne avec Matrix 1.2-8

############################################################
# Combiner les donnees des equipes
############################################################
# Table collaboration
rm(list=ls())
c1 <- read.csv("collaborations_Ax_Et_Jo_Va.csv",sep = ";",header = T)
colnames(c1) <- c("etudiant1","etudiant2","cours","date")
c2 <- read.csv("collaborations_beteille.csv",sep = ";",header = T)
c3 <- read.csv("collaborations_gagnon.csv",sep = ";",header = T)
c4 <- read.csv("collaborations_GT_KAC_ETC_AD_RB.csv",sep = ";",header = T)
colnames(c4) <- c("etudiant1","etudiant2","cours","date")
c5 <- read.csv("collaborations_LP_TM_SPT_ELC_VM.csv",sep = ",",header = T)
c6 <- read.csv("collaborations_payette.csv",sep = ",",header = T)
c7 <- read.csv("collaborations_Thiffault.csv",sep = ";",header = T)
colnames(c7) <- c("etudiant1","etudiant2","cours","date")
c8 <- read.csv("collaborations_thirioncharlebois.csv",sep = ";",header = T)
collaborations<-rbind(c1,c2,c3,c4,c5,c6,c7,c8)

# Table cours
u1<-read.csv("cours_Ax_Et_Jo_Va.csv",sep = ";",header = T)
colnames(u1)<-c("sigle","credits","obligatoire","type_travail")
u2<-read.csv("cours_beteille.csv",sep = ";",header = T)
u3<-read.csv("cours_gagnon.csv",sep = ";",header = T)
u3<-u3[,-c(3,4)]
u3<-u3[,c(1,2,4,3)]
colnames(u3)<-c("sigle","credits","obligatoire","type_travail")
u4<-read.csv("cours_GT_AD_RB_ETC_KC.csv",sep = ";",header = T)
colnames(u4)<-c("sigle","credits","obligatoire","type_travail")
u5<-read.csv("cours_LP_TM_SPT_ELC_VM.csv",sep = ",",header = T)
u6<-read.csv("cours_payette.csv",sep = ",",header = T)
u7<-read.csv("cours_Thiffault.csv",sep = ";",header = T)
colnames(u7)<-c("sigle","credits","obligatoire","type_travail")
u8<-read.csv("cours_thirioncharlebois.csv",sep = ";",header = T)
colnames(u8)<-c("sigle","credits","obligatoire","type_travail")
cours<-rbind(u1,u2,u3,u4,u5,u6,u7,u8)

# Etudiants
e1<-read.csv("etudiants_Ax_Et_Jo_Va.csv",sep = ";",header = T)
e1<-e1[,-6]
colnames(e1)<-c("nom_prenom","annee_debut","session_debut","programme","coop")
e2<-read.csv("etudiants_beteille.csv",sep = ";",header = T)
e3<-read.csv("etudiants_gagnon.csv",sep = ";",header = T)
e4<-read.csv("etudiants_GT_KAC_ETC_AD_RB.csv",sep = ";",header = T)
colnames(e4)<-c("nom_prenom","annee_debut","session_debut","programme","coop")
e5<-read.csv("etudiants_LP_TM_SPT_ELC_VM.csv",sep = ",",header = T)
e6<-read.csv("etudiants_payette.csv",sep = ",",header = T)
e7<-read.csv("etudiants_Thiffault.csv",sep = ";",header = T)
colnames(e7)<-c("nom_prenom","annee_debut","session_debut","programme","coop")
e8<-read.csv("etudiants_thirioncharlebois.csv",sep = ";",header = T)
etudiants<-rbind(e1,e2,e3,e4,e5,e6,e7,e8)

# On retire les jeux de donnees fractionnes
rm(c1, c2, c3, c4, c5, c6 ,c7, c8, e1, e2, e3, e4, e5, e6, e7, e8, u1, u2, u3, u4, u5, u6, u7, u8)


############################################################
# Nettoyage des donnees
############################################################

#Correction des noms dans chaque table

# Ici je fais cette operation pour voir le nombre de niveaux differents dans la colone nom_prenom.
# Ensuite, je classe les noms en ordre alphabetique pour voir les typos. 
ok <- etudiants %>%
  group_by(nom_prenom) %>%
  count()
ok$nom_prenom <- as.character(ok$nom_prenom)
ok <- ok[order(ok$nom_prenom),]

# Boucle pour faire les corriger les noms mal ecrits.  
etudiants$nom_prenom <- as.character(etudiants$nom_prenom)
for (i in 1:nrow(etudiants)){
  if (etudiants[i,1] == "barro_nourra" | etudiants[i,1] == "Noura Barro"){
    etudiants[i,1] <- "barro_noura"
  } else if (etudiants[i,1] == "beaulac_ariane"){
    etudiants[i,1] <- "beaulac_arianne"
  } else if (etudiants[i,1] == "beaupre_raphaeljonathan" | etudiants[i,1] == "beaupres_raphael"){
    etudiants[i,1] <- "beaupre_raphael"
  }else if (etudiants[i,1] == "beaulac_ariane"){
    etudiants[i,1] <- "beaulac_arianne"
  }else if (etudiants[i,1] == "beteille_clemene"){
    etudiants[i,1] <- "beteille_clemence"
  }else if (etudiants[i,1]=="choinard_emile"){
    etudiants[i,1]<-"chouinard_emile"
  }else if (etudiants[i,1] == "desroschers_simon"){
    etudiants[i,1] <- "desrochers_simon"
  }else if (etudiants[i,1] == "duchenes_valerie"){
    etudiants[i,1] <- "duchesne_valerie"
  }else if (etudiants[i,1] == "frappierlecompte_juliette"){
    etudiants[i,1] <- "frappierlecomte_juliette"
  }else if (etudiants[i,1] == "gagnon_antony"){
    etudiants[i,1] <- "gagnon_anthony"
  }else if (etudiants[i,1] == "houde_ann-sophie"){
    etudiants[i,1] <- "houde_annsophie"
  }else if (etudiants[i,1] == "Julien Bonneaud-Costa "){
    etudiants[i,1] <- "bonneaudcosta_julien"
  }else if (etudiants[i,1] == "Julien Faure-Levesque"){
    etudiants[i,1] <- "faurelevesque_julien"
  }else if (etudiants[i,1] == "Kathryne Moreau"){
    etudiants[i,1] <- "moreau_kathryne"
  }else if (etudiants[i,1] == "Kenley Joule Pierre"){
    etudiants[i,1] <- "kenleyjoule_pierre"
  }else if (etudiants[i,1] == "nault_lauriane"){
    etudiants[i,1] <- "nault_laurianne"
  }else if (etudiants[i,1] == "plantecardinal_katerine"){
    etudiants[i,1] <- "plantecardinal_katherine"
  }else if (etudiants[i,1] == "rioux_jenny-ann"){
    etudiants[i,1] <- "rioux_jennyann"
  }else if (etudiants[i,1] == "rondeau_saint_jean_camille"){
    etudiants[i,1] <- "rondeausaintjean_camille"
  }else if (etudiants[i,1] == "savoie_cloutier_maude" | etudiants[i,1] == "savoiecloutier_maude"){
    etudiants[i,1] <- "savoiecloutier_kellymaude"
  }else if (etudiants[i,1] == "villleneuve_clara"){
    etudiants[i,1] <- "villeneuve_clara"
  }else if (etudiants[i,1] == "choinard_emile"){
    etudiants[i,1] <- "chouinard_emile"
  }else if (etudiants[i,1] == "M_leopold"){
    etudiants[i,1] <- "martin_leopold"
  }else if(etudiants[i,1]=="laroche_pierre-yves"){
    etudiants[i,1]<-"laroche_pierreyves"
  }else if (etudiants[i,1] == "laporte_simon  "){
    etudiants[i,1] <- "laporte_simon"
  }else if (etudiants[i,1] == "lacroixcarigan_etienne"){
    etudiants[i,1] <- "lacroixcarignan_etienne"
  }else if (etudiants[i,1] == "hinse_pierandrZ"){
    etudiants[i,1] <- "hinse_pierandre"
  }else if (etudiants[i,1] == "gagnon_joannie\n"){
    etudiants[i,1] <- "gagnon_joannie"
  }else if (etudiants[i,1] == "faurelevesque_Julien"){
    etudiants[i,1] <- "faurelevesque_julien"
  }else if (etudiants[i,1] == "carbonneau_alexandre "){
    etudiants[i,1] <- "carbonneau_alexandre"
  }else if (etudiants[i,1] == "boisvert_erika"){
    etudiants[i,1] <- "boisvertvigneault_erika"
  }else if (etudiants[i,1] == "gagon_antony"){
    etudiants[i,1] <- "gagnon_anthony"
  }
}


#Ici on inspecte et corrige les fautes dans la colonne programme de la table etudiants
levels(etudiants$programme)

etudiants$programme <- as.character(etudiants$programme)
summary(etudiants$programme)


etudiants$programme[is.na(etudiants$programme)] <- "autre"

for (i in 1:nrow(etudiants)){
  if (etudiants[i,4] == "microbiolobie" | etudiants[i,4] == "microbiogie"){
    etudiants[i,4] <- "microbiologie"
  } else if (etudiants[i,4] == "ecologie "){
    etudiants[i,4] <- "ecologie"
  } else if (etudiants[i,4] == "biomoleculaire"){
    etudiants[i,4] <- "moleculaire"
  }
}

# Ici, on fait la liste des etudiants avec plusieurs lignes, donc plusieurs infos differentes.
# Ces informations auront besoin d'etre corrigees.
etudiants_distinct <- distinct(etudiants)
doublons_etu<-etudiants_distinct %>%
  group_by(nom_prenom) %>%
  count()
etudiants_prob<-subset(doublons_etu,doublons_etu$n>1)

#Corriger les lignes avec plusieurs infos differentes
for (i in 1:nrow(etudiants_distinct)){
  if(etudiants_distinct[i,1]=="barro_noura"){
    etudiants_distinct[i,3]<-"A" 
    etudiants_distinct[i,5]<-"0"
  }else if(etudiants_distinct[i,1]=="beaulac_arianne"){
    etudiants_distinct[i,2]<-"2016"
    etudiants_distinct[i,5]<-"0"
  }else if(etudiants_distinct[i,1]=="beaupre_raphael"){
    etudiants_distinct[i,2]<-"2018"
    etudiants_distinct[i,3]<-"A"
    etudiants_distinct[i,5]<-"0"
  }else if(etudiants_distinct[i,1]=="bonneaudcosta_julien"){
    etudiants_distinct[i,4]<-"ecologie"
  }else if(etudiants_distinct[i,1]=="boucher_vincent") {
    etudiants_distinct[i,2]<-"2016"
    etudiants_distinct[i,3]<-"A"
    etudiants_distinct[i,4]<-"ecologie"
    etudiants_distinct[i,5]<-"1"
  }else if(etudiants_distinct[i,1]=="boudreau_alice") {
    etudiants_distinct[i,2]<-"2017"
    etudiants_distinct[i,3]<-"A"
  }else if(etudiants_distinct[i,1]=="choiniere_william"){
    etudiants_distinct[i,5]<-"1"
  }else if(etudiants_distinct[i,1]=="chouinard_emile"){
    etudiants_distinct[i,2]<-"2016"
    etudiants_distinct[i,3]<-"A"
    etudiants_distinct[i,4]<-"ecologie"
    etudiants_distinct[i,5]<-"1"
  }else if(etudiants_distinct[i,1]=="cloutier_benjamin"){
    etudiants_distinct[i,2]<-"2018"
    etudiants_distinct[i,4]<-"ecologie"
    etudiants_distinct[i,5]<-"1"
  }else if(etudiants_distinct[i,1]=="courcy_andrea"){
    etudiants_distinct[i,4]<-"moleculaire"
  }else if(etudiants_distinct[i,1]=="coursol_denver") {
    etudiants_distinct[i,2]<-"2018"
    etudiants_distinct[i,3]<-"A"
    etudiants_distinct[i,4]<-"ecologie"
    etudiants_distinct[i,5]<-"1"
  }else if(etudiants_distinct[i,1]=="couture_emma"){
    etudiants_distinct[i,5]<-"1"
  }else if(etudiants_distinct[i,1]=="desormeaux_andreanne"){
    etudiants_distinct[i,2]<-"2016"
    etudiants_distinct[i,4]<-"ecologie"
    etudiants_distinct[i,5]<-"1"
  }else if(etudiants_distinct[i,1]=="duchesne_valerie"){
    etudiants_distinct[i,3]<-"A"
  }else if(etudiants_distinct[i,1]=="dufour_daphne"){
    etudiants_distinct[i,2]<-"2016"
    etudiants_distinct[i,3]<-"A"
    etudiants_distinct[i,4]<-"biologie"
    etudiants_distinct[i,5]<-"0"
  }else if(etudiants_distinct[i,1]=="faurelevesque_julien"){
    etudiants_distinct[i,4]<-"biologie"
  }else if(etudiants_distinct[i,1]=="gagnon_elise"){
    etudiants_distinct[i,2]<-"2017"
    etudiants_distinct[i,3]<-"A"
    etudiants_distinct[i,4]<-"microbiologie"
    etudiants_distinct[i,5]<-"0"
  }else if(etudiants_distinct[i,1]=="guay_laurence"){
    etudiants_distinct[i,4]<-"autre"
    etudiants_distinct[i,5]<-"1"
  }else if(etudiants_distinct[i,1]=="hudonvoyer_rosalie"){
    etudiants_distinct[i,2]<-"2014"
  }else if(etudiants_distinct[i,1]=="juneau_mathilde"){
    etudiants_distinct[i,2]<-"2019"
    etudiants_distinct[i,3]<-"A"
  }else if(etudiants_distinct[i,1]=="lagace_etienne"){
    etudiants_distinct[i,5]<-"0"
  }else if(etudiants_distinct[i,1]=="lamoureux_david"){
    etudiants_distinct[i,2]<-"2016"
    etudiants_distinct[i,5]<-"1"
  }else if(etudiants_distinct[i,1]=="lapalme_dominic"){
    etudiants_distinct[i,3]<-"A"
    etudiants_distinct[i,5]<-"1"
  }else if(etudiants_distinct[i,1]=="lapointe_eve"){
    etudiants_distinct[i,2]<-"2018"
    etudiants_distinct[i,3]<-"A"
    etudiants_distinct[i,4]<-"ecologie"
    etudiants_distinct[i,5]<-"0"
  }else if(etudiants_distinct[i,1]=="legros_jade"){
    etudiants_distinct[i,5]<-"0"
  }else if(etudiants_distinct[i,1]=="mercier_benjamin"){
    etudiants_distinct[i,2]<-"2016"
    etudiants_distinct[i,3]<-"A"
    etudiants_distinct[i,5]<-"0"
  }else if(etudiants_distinct[i,1]=="michaudleblanc_esther"){
    etudiants_distinct[i,2]<-"2018"
    etudiants_distinct[i,5]<-"1"
  }else if(etudiants_distinct[i,1]=="nault_laurianne"){
    etudiants_distinct[i,5]<-"0"
  }else if(etudiants_distinct[i,1]=="pelletier_noemie"){
    etudiants_distinct[i,4]<-"autre"
    etudiants_distinct[i,5]<-"0"
  }else if(etudiants_distinct[i,1]=="plante_louis"){
    etudiants_distinct[i,5]<-"1"
  }else if(etudiants_distinct[i,1]=="plantecardinal_katherine"){
    etudiants_distinct[i,3]<-"A"
    etudiants_distinct[i,4]<-"biologie"
    etudiants_distinct[i,5]<-"0"
  }else if(etudiants_distinct[i,1]=="rondeausaintjean_camille"){
    etudiants_distinct[i,5]<-"1"
  }else if(etudiants_distinct[i,1]=="sabourin_olivier"){
    etudiants_distinct[i,2]<-"2016"
    etudiants_distinct[i,3]<-"A"
    etudiants_distinct[i,5]<-"1"
  }else if(etudiants_distinct[i,1]=="savoiecloutier_kellymaude"){
    etudiants_distinct[i,5]<-"1"
  }else if(etudiants_distinct[i,1]=="thiffault_valerie"){
    etudiants_distinct[i,2]<-"2017"
    etudiants_distinct[i,3]<-"A"
  }else if(etudiants_distinct[i,1]=="thirioncharlebois_erik"){
    etudiants_distinct[i,2]<-"2017"
    etudiants_distinct[i,3]<-"A"
    etudiants_distinct[i,4]<-"autre"
    etudiants_distinct[i,5]<-"1"
  }else if(etudiants_distinct[i,1]=="villeneuve_clara"){
    etudiants_distinct[i,4]<-"biologie"
  }else if(etudiants_distinct[i,1]=="beteille_clemence"){
    etudiants_distinct[i,5] <- 0
  }
}


#Eliminer les nouveaux doublons crees par la correction des lignes
etudiants_distinct<-distinct(etudiants_distinct)


#Verifier quil ne reste plus de doublons
doublons_etu <-etudiants_distinct %>%
  group_by(nom_prenom) %>%
  count()
etudiants_prob<-subset(doublons_etu,doublons_etu$n>1)#devrait etre vide
rm(doublons_etu, etudiants_prob)

#Cleanup table cours####
#V?rifier les erreurs de synthaxe
cours$type_travail <- as.factor(cours$type_travail)
levels(cours$type_travail)
#On voit qu'il existe plusieurs facons d'ecrire ecrit, oral ou labo. Nous allons donc convertir ces abberations de facon a uniformiser cette colonne. 

cours$type_travail <- as.character(cours$type_travail)
for (i in 1:nrow(cours)){
  if (cours[i,4] == "√©crit"){
    cours[i,4] <- "ecrit"
  } else if (cours[i,4] == "laboratoire" | cours[i,4] == "labos"){
    cours[i,4] <- "labo"
  } else if (cours[i, 4] == "ecrit/oral" | cours[i, 4] == "oral " |cours[i, 4] == "oral   "){
    cours[i,4] <- "oral"
  }
}

cours_distinct<-distinct(cours)

#Verifier sil reste des cours en double avec des infos differentes
doublons_cours<-cours_distinct %>%
  group_by(sigle) %>%
  count()
#Ici on trouve que certaines lignes ne coincident pas dans leur information 
cours_prob<-subset(doublons_cours,doublons_cours$n>1)

##etant donne le grand nombre de travaux differents, on priorise selon lordre : oral>ecrit>labo>terrain
#pcq oral est souvent precede dun ecrit, labo et terrain ont souvent comme finalite un travail ecrit et travail preparatoire du terrain = lab
for (i in 1:nrow(cours_distinct)){
  if(cours_distinct[i,1]=="ECL403"){
    cours_distinct[i,4]<-"ecrit"
  }else if(cours_distinct[i,1]=="ECL406"){ 
    cours_distinct[i,4]<-"oral"
  }else if(cours_distinct[i,1]=="ECL406"){
    cours_distinct[i,4]<-"oral"
  }else if(cours_distinct[i,1]=="ECL510"){
    cours_distinct[i,2]<-"3"
    cours_distinct[i,4]<-"ecrit"
  }else if(cours_distinct[i,1]=="ECL515"){
    cours_distinct[i,4]<-"oral"
  }else if(cours_distinct[i,1]=="ECL516"){
    cours_distinct[i,2]<-"3"
  }else if(cours_distinct[i,1]=="ECL522"){
    cours_distinct[i,4]<-"oral"
  }else if(cours_distinct[i,1]=="ECL527"){
    cours_distinct[i,4]<-"oral"
  }else if(cours_distinct[i,1]=="ECL611"){
    cours_distinct[i,4]<-"ecrit"
  }else if(cours_distinct[i,1]=="INS154"){
    cours_distinct[i,4]<-"oral"
  }else if(cours_distinct[i,1]=="ZOO105"){
    cours_distinct[i,4]<-"oral"
  }else if(cours_distinct[i,1]=="ECL603"){
    cours_distinct[i,2]<-"1"
  }else if(cours_distinct[i,1]=="ECL608"){
    cours_distinct[i,4]<-"oral"
  }else if(cours_distinct[i,1]=="ECL616"){
    cours_distinct[i,4]<-"oral"
  }else if(cours_distinct[i,1]=="GMQ106"){
    cours_distinct[i,4]<-"oral"
  }
}
cours_distinct<-distinct(cours_distinct)

#Verifier sil reste des cours en double avec des infos differentes
doublons_cours<-cours_distinct %>%
  group_by(sigle) %>%
  count()

#On retire les objets inutiles
rm(doublons_cours, cours_prob, ok)

# Cleanup collabs
collaborations$etudiant1 <- as.character(collaborations$etudiant1)
collaborations$etudiant2 <- as.character(collaborations$etudiant2)
# Cleanup des noms dans la colone etudiant1
for (i in 1:nrow(collaborations)){
  if (collaborations[i,1] == "barro_nourra" | collaborations[i,1] == "Noura Barro"){
    collaborations[i,1] <- "barro_noura"
  } else if (collaborations[i,1] == "beaulac_ariane"){
    collaborations[i,1] <- "beaulac_arianne"
  } else if (collaborations[i,1] == "beaupre_raphaeljonathan" | collaborations[i,1] == "beaupres_raphael"){
    collaborations[i,1] <- "beaupre_raphael"
  }else if (collaborations[i,1] == "beaulac_ariane"){
    collaborations[i,1] <- "beaulac_arianne"
  }else if (collaborations[i,1] == "beteille_clemene"){
    collaborations[i,1] <- "beteille_clemence"
  }else if (collaborations[i,1] == "desroschers_simon"){
    collaborations[i,1] <- "desrochers_simon"
  }else if (collaborations[i,1] == "duchenes_valerie"){
    collaborations[i,1] <- "duchesne_valerie"
  }else if (collaborations[i,1] == "frappierlecompte_juliette"){
    collaborations[i,1] <- "frappierlecomte_juliette"
  }else if (collaborations[i,1] == "gagnon_antony"){
    collaborations[i,1] <- "gagnon_anthony"
  }else if (collaborations[i,1] == "houde_ann-sophie"){
    collaborations[i,1] <- "houde_annsophie"
  }else if (collaborations[i,1] == "Julien Bonneaud-Costa "){
    collaborations[i,1] <- "bonneaudcosta_julien"
  }else if (collaborations[i,1] == "Julien Faure-Levesque"){
    collaborations[i,1] <- "faurelevesque_julien"
  }else if (collaborations[i,1] == "Kathryne Moreau"){
    collaborations[i,1] <- "moreau_kathryne"
  }else if (collaborations[i,1] == "Kenley Joule Pierre"){
    collaborations[i,1] <- "kenleyjoule_pierre"
  }else if (collaborations[i,1] == "nault_lauriane"){
    collaborations[i,1] <- "nault_laurianne"
  }else if (collaborations[i,1] == "plantecardinal_katerine"){
    collaborations[i,1] <- "plantecardinal_katherine"
  }else if (collaborations[i,1] == "rioux_jenny-ann"){
    collaborations[i,1] <- "rioux_jennyann"
  }else if (collaborations[i,1] == "rondeau_saint_jean_camille"){
    collaborations[i,1] <- "rondeausaintjean_camille"
  }else if (collaborations[i,1] == "savoie_cloutier_maude" | collaborations[i,1] == "savoiecloutier_maude"){
    collaborations[i,1] <- "savoiecloutier_kellymaude"
  }else if (collaborations[i,1] == "villleneuve_clara"){
    collaborations[i,1] <- "villeneuve_clara"
  }else if (collaborations[i,1] == "choinard_emile"){
    collaborations[i,1] <- "chouinard_emile"
  }else if (collaborations[i,1] == "M_leopold"){
    collaborations[i,1] <- "martin_leopold"
  }else if (collaborations[i,1] == "laporte_simon  "){
    collaborations[i,1] <- "laporte_simon"
  }else if (collaborations[i,1] == "lacroixcarigan_etienne"){
    collaborations[i,1] <- "lacroixcarignan_etienne"
  }else if (collaborations[i,1] == "hinse_pierandr≈Ω"|collaborations[i,1]=="hinse_pierandrZ"){
    collaborations[i,1] <- "hinse_pierandre"
  }else if (collaborations[i,1] == "gagnon_joannie\n"){
    collaborations[i,1] <- "gagnon_joannie"
  }else if (collaborations[i,1] == "faurelevesque_Julien"){
    collaborations[i,1] <- "faurelevesque_julien"
  }else if (collaborations[i,1] == "carbonneau_alexandre "){
    collaborations[i,1] <- "carbonneau_alexandre"
  }else if (collaborations[i,1] == "boisvert_erika"){
    collaborations[i,1] <- "boisvertvigneault_erika"
  }else if (collaborations[i,1] == "gagon_antony"){
    collaborations[i,1] <- "gagnon_anthony"
  }else if(collaborations[i,1]=="st-hilaire_pascale"){
    collaborations[i,1]<-"sthilaire_pascale"
  }else if(collaborations[i,1]=="ecliman_jonathan"){
    collaborations[i,1]<-"eschlimann_jonathan"
  }else if(collaborations[i,1]=="aparico_mariaelisa"){
    collaborations[i,1]<-"aparicio_mariaelisa"
  }else if(collaborations[i,1]=="malette_marianne"){
    collaborations[i,1]<-"mallette_marianne"
  }
}

# Cleanup des noms dans la colone etudiant2
for (i in 1:nrow(collaborations)){
  if (collaborations[i,2] == "barro_nourra" | collaborations[i,2] == "Noura Barro"){
    collaborations[i,2] <- "barro_noura"
  } else if (collaborations[i,2] == "beaulac_ariane"){
    collaborations[i,2] <- "beaulac_arianne"
  } else if (collaborations[i,2] == "beaupre_raphaeljonathan" | collaborations[i,2] == "beaupres_raphael"){
    collaborations[i,2] <- "beaupre_raphael"
  }else if (collaborations[i,2] == "beaulac_ariane"){
    collaborations[i,2] <- "beaulac_arianne"
  }else if (collaborations[i,2] == "beteille_clemene"){
    collaborations[i,2] <- "beteille_clemence"
  }else if (collaborations[i,2] == "desroschers_simon"){
    collaborations[i,2] <- "desrochers_simon"
  }else if (collaborations[i,2] == "duchenes_valerie"){
    collaborations[i,2] <- "duchesne_valerie"
  }else if (collaborations[i,2] == "frappierlecompte_juliette"){
    collaborations[i,2] <- "frappierlecomte_juliette"
  }else if (collaborations[i,2] == "gagnon_antony"){
    collaborations[i,2] <- "gagnon_anthony"
  }else if (collaborations[i,2] == "houde_ann-sophie"){
    collaborations[i,2] <- "houde_annsophie"
  }else if (collaborations[i,2] == "Julien Bonneaud-Costa "){
    collaborations[i,2] <- "bonneaudcosta_julien"
  }else if (collaborations[i,2] == "Julien Faure-Levesque"){
    collaborations[i,2] <- "faurelevesque_julien"
  }else if (collaborations[i,2] == "Kathryne Moreau"){
    collaborations[i,2] <- "moreau_kathryne"
  }else if (collaborations[i,2] == "Kenley Joule Pierre"){
    collaborations[i,2] <- "kenleyjoule_pierre"
  }else if (collaborations[i,2] == "nault_lauriane"){
    collaborations[i,2] <- "nault_laurianne"
  }else if (collaborations[i,2] == "plantecardinal_katerine"){
    collaborations[i,2] <- "plantecardinal_katherine"
  }else if (collaborations[i,2] == "rioux_jenny-ann"){
    collaborations[i,2] <- "rioux_jennyann"
  }else if (collaborations[i,2] == "rondeau_saint_jean_camille"){
    collaborations[i,2] <- "rondeausaintjean_camille"
  }else if (collaborations[i,2] == "savoie_cloutier_maude" | collaborations[i,2] == "savoiecloutier_maude"){
    collaborations[i,2] <- "savoiecloutier_kellymaude"
  }else if (collaborations[i,2] == "villleneuve_clara"){
    collaborations[i,2] <- "villeneuve_clara"
  }else if (collaborations[i,2] == "choinard_emile"){
    collaborations[i,2] <- "chouinard_emile"
  }else if (collaborations[i,2] == "M_leopold"){
    collaborations[i,2] <- "martin_leopold"
  }else if (collaborations[i,2] == "laporte_simon  "){
    collaborations[i,2] <- "laporte_simon"
  }else if (collaborations[i,2] == "lacroixcarigan_etienne"){
    collaborations[i,2] <- "lacroixcarignan_etienne"
  }else if (collaborations[i,2] == "hinse_pierandrZ"|collaborations[i,2]=="hinse_pierandrZ"){
    collaborations[i,2] <- "hinse_pierandre"
  }else if (collaborations[i,2] == "gagnon_joannie\n"){
    collaborations[i,2] <- "gagnon_joannie"
  }else if (collaborations[i,2] == "faurelevesque_Julien"){
    collaborations[i,2] <- "faurelevesque_julien"
  }else if (collaborations[i,2] == "carbonneau_alexandre "){
    collaborations[i,2] <- "carbonneau_alexandre"
  }else if (collaborations[i,2] == "boisvert_erika"){
    collaborations[i,2] <- "boisvertvigneault_erika"
  }else if (collaborations[i,2] == "gagon_antony"){
    collaborations[i,2] <- "gagnon_anthony"
  }else if(collaborations[i,2]=="st-hilaire_pascale"){
    collaborations[i,2]<-"sthilaire_pascale"
  }else if(collaborations[i,2]=="ecliman_jonathan"){
    collaborations[i,2]<-"eschlimann_jonathan"
  }else if(collaborations[i,2]=="aparico_mariaelisa"){
    collaborations[i,2]<-"aparicio_mariaelisa"
  }else if(collaborations[i,2]=="malette_marianne"){
    collaborations[i,2]<-"mallette_marianne"
  }
}

# Table de collaboration finale
collab_distinct <- distinct(collaborations)

# Tests pour voir si certains etudiants sont dans une colonne de la table collaborations mais pas dans l'autre
collab_distinct$etudiant1[!(collab_distinct$etudiant1 %in% collab_distinct$etudiant2)]

#Pour comparer dans l'autre direction; autrement avec seulement la ligne du haut les
#√©tudiants2 absents de la liste etudiants 1 ne sont pas releves.
collab_distinct$etudiant2[!(collab_distinct$etudiant2 %in% collab_distinct$etudiant1)]
# les relations impliquant "clement_genevieve"et  "turcotte_annie" doivent √™tre ajout√©es

collab_distinct<-rbind(collab_distinct,list("clement_genevieve","tremblay_audrey","BIO109",2018))
collab_distinct<-rbind(collab_distinct,list("turcotte_annie","tremblay_audrey","BIO109",2018))


# Tests pour voir si certains etudiants sont dans une colonne de la table collaborations mais pas dans la table ?tudiant
collab_distinct$etudiant1[!(collab_distinct$etudiant1 %in% etudiants_distinct$nom_prenom)]
collab_distinct$etudiant2[!(collab_distinct$etudiant2 %in% etudiants_distinct$nom_prenom)]

#Ajouter les etudiants manquants a la table etudiants
#le truc original fonctionne pas pour moi alors j'ai mis ?a
etudiants_distinct <-rbind(etudiants_distinct,list("cote_emma",2017,"A","ecologie",1))
etudiants_distinct <-rbind(etudiants_distinct,list("lajeunesse_gaele",2017,"A","microbiologie",1))

#Voir si il y a le m√™me nombre d'√©tudiant dans la table collaboration que dans la table etudiant
allo<-as.data.frame(sort(unique(collab_distinct$etudiant1)))
etudiantsort<-sort(etudiants_distinct[,1])
testing<- cbind(etudiantsort, allo)
#Il n'y a pas le m√™me nombre! Apr√®s recherche, lalumiere_mariepier n'a aucune collaboration

etudiants_final<-etudiants_distinct[-(which(etudiants_distinct$nom_prenom == 'lalumiere_mariepier')),]

#enlever la ligne incompl?te
collab_distinct<-collab_distinct[complete.cases(collab_distinct), ]

#Test pour voir s'il reste des noms qui sont dans la table collab et non dans celle des etudiants
collab_distinct$etudiant1[!(collab_distinct$etudiant1 %in% etudiants_distinct$nom_prenom)]
collab_distinct$etudiant2[!(collab_distinct$etudiant2 %in% etudiants_distinct$nom_prenom)]


#Corriger la colonne cours
for (i in 1:nrow(collab_distinct)){
  if(collab_distinct[i,3]=="ECL 527"){
    collab_distinct[i,3]<-"ECL527"
  }else if(collab_distinct[i,3]=="BOT 400"){
    collab_distinct[i,3]<-"BOT400"
  }
}

#Verifier sil y a des cours dans collab qui ne sont pas dans cours
collab_distinct$cours[!(collab_distinct$cours %in% cours_distinct$sigle)]

#Corriger les erreurs de synthaxe
for(i in 1:nrow(collab_distinct)){
  if(collab_distinct[i,3]=="TSB300"){
    collab_distinct[i,3]<-"TSB303"
  }else if(collab_distinct[i,3]=="ECL603\n"){
    collab_distinct[i,3]<-"ECL603"
  }else if(collab_distinct[i,3]=="ELC527"){
    collab_distinct[i,3]<-"ECL527"
  }else if(collab_distinct[i,3]=="ZOO105\n"){
    collab_distinct[i,3]<-"ZOO105"
  }else if(collab_distinct[i,3]=="ZOO106\n"){
    collab_distinct[i,3]<-"ZOO106"
  }else if(collab_distinct[i,3]=="ZOO307\n"){
    collab_distinct[i,3]<-"ZOO307"
  }
}

#Reverifier
collab_distinct$cours[!(collab_distinct$cours %in% cours_distinct$sigle)]

#Il reste ECL315 qui existe pour vrai et qui nest pas dans la table cours, donc on lajoute

cours_distinct$sigle <- as.character(cours_distinct$sigle)
cours_distinct <-rbind(cours_distinct,list("ECL315",3,0,"ecrit"))
cours_distinct$sigle <- as.factor(cours_distinct$sigle)



collab_final <- distinct(collab_distinct)
cours_final <- distinct(cours_distinct)

#Cette section est dÈfier ‡ des erreurs qui on Èchapper au processus de nettoyage et que l'on ‡ remarquÈ en fesant la base de donnÈ ou des figure, elle sont corrigÈ ici
#Ici on remplace le noms avec une erreur d'orthographe qui Ètait indÈtectable par le bon nom hinse_pierandre
collab_final[377,1] <- "hinse_pierandre"
collab_final[378,1] <- "hinse_pierandre"
collab_final[375,2] <- "hinse_pierandre"
collab_final[380,2] <- "hinse_pierandre"
#On vÈrifie si le problËme est rÈgler, si on obtient 0 on est correct
collab_final$etudiant1[!(collab_final$etudiant1 %in% etudiants_final$nom_prenom)]
collab_final$etudiant2[!(collab_final$etudiant2 %in% etudiants_final$nom_prenom)]

#Erreur corriger manuellement vue par chance, certaine collaboration n'avait pas la bonne annÈe
collab_final[2112,4] <- 2019
collab_final[2111,4] <- 2019
collab_final[2110,4] <- 2019

#On Èlimine ici des lignes du dataframe collaboration ou le cours n'est pas ‡ la bonne annÈe ECL515 Ètait en 2019 non en 2020, les bonnes lignes existe dÈga il faut juste supprimer celle si
#Pour une raison inconnu la boucle fonctionne pas bien (Modification de derniËre minute), il faut la rÈpÈter 4 fois pour enlever tout les ligne avec ECL515 et annee 2020, aprËs les 4 rÈpÈtition il n'y ‡ plus d'erreur

for(i in 1:nrow(collab_final)){
  if((collab_final[i,3]=="ECL515")&(collab_final[i,4]=="2020")){
    collab_final<-collab_final[-c(i),]
  } else {
    next
  }
}

for(i in 1:nrow(collab_final)){
  if((collab_final[i,3]=="ECL515")&(collab_final[i,4]=="2020")){
    collab_final<-collab_final[-c(i),]
  } else {
    next
  }
}

for(i in 1:nrow(collab_final)){
  if((collab_final[i,3]=="ECL515")&(collab_final[i,4]=="2020")){
    collab_final<-collab_final[-c(i),]
  } else {
    next
  }
}

for(i in 1:nrow(collab_final)){
  if((collab_final[i,3]=="ECL515")&(collab_final[i,4]=="2020")){
    collab_final<-collab_final[-c(i),]
  } else {
    next
  }
}

etudiantsfinal <- etudiants_final[order(etudiants_final$nom_prenom),]
#necessaire d'ordonner pour que les noms dans la matrice d adjacence et la liste des
#programme d etude des etudiants soient bien alignes pour produire les figures

#####################################################
#Graphique liaisons entre etudiants
#####################################################
library(plyr)

library(reshape2)

#avec tidyr: pivot_wider(tmp, names_from = y, values_from = z)
# Creer la matrice d'adjacence qui sera utilisee pour tracer la figure
liens<- as.data.frame(cbind(collab_final$etudiant1,collab_final$etudiant2))
fidelite<-ddply(.data = liens, .(liens[,1],liens[,2]), nrow)

matrix<-acast(fidelite, liens[,1]~liens[,2], value.var="V1")
#Cette matrice contient chaque √©tudiant comme rang√©e et comme colonne, et
#les valeurs dans la matrice sont le nombre de collaborations entre les paires d'√©tudiants
dim(matrix)
#la matrice doit √™tre carr√©e pour pouvoir creer la table d'adjacence

isSymmetric(matrix)#la matrice doit etre symetrique
#la matrice  n'est pas symetrique: il reste des erreurs dans les donnees

library(Matrix)
matrixsym <- as.matrix(forceSymmetric(matrix))
class(matrixsym)
#idealement, on ne devrait pas avoir recours a cette fonction mais 
#corriger toutes les donnees pour rendre la matrice symetrique serait tres long

g <- graph.adjacency(matrixsym, mode="undirected", weighted = T, diag=F,
                     add.colnames=T)
#E(net)$weight donne la valeur du weigth pour chaque paire. Le weigth correspond 
#au nombre de collaborations entre chaque paire d etudiants

net <- simplify(g, remove.multiple = T, remove.loops = T) 
E(net)$width<-E(net)$weight*1.2
######################################################
# Graphique Liens selon le programme

couleur<-etudiantsfinal$programme

for (i in 1:length(couleur)) {
  if (couleur[i]== "ecologie"){
    couleur[i]<-"green"}
  else if (couleur[i]== "autre"){
    couleur[i]<-"black"}
  else if (couleur[i]== "biologie"){
    couleur[i]<-"deepskyblue3"}
  else if (couleur[i]== "microbiologie"){
    couleur[i]<-"coral"}
  else if (couleur[i]== "moleculaire"){
    couleur[i]<-"red3"}
}
V(net)$programme<-couleur

#Graphique avec labels pour verifier si tout est bon
plot(net, vertex.size=5, vertex.color=V(net)$programme, edge.color="gray28", edge.width=E(net)$width, edge.arrow.mode = 0, vertex.label.cex=0.5, vertex.frame.color = NA,
     vertex.label=colnames(matrix))

  
#Graphique sans labels pour le document latex (la figure est trop grosse pour entrer dans le document)
plot(net, vertex.size=5, vertex.color=V(net)$programme, edge.color="gray28", edge.width=E(net)$width, edge.arrow.mode = 0, vertex.label.cex=0.5, vertex.frame.color = NA,
     vertex.label="")
#L√©gende du graphique: 
#noir: Autres, bleu: Biologie, vert: √âcologie, orange: Microbiologie, rouge: Bio mol√©culaire

######################################################
# Graphique Liens selon l annee du debut des etudes
couleur<-etudiantsfinal$annee_debut
couleur[4]
for (i in 1:length(couleur)) {
  if (couleur[i]%in% "2019"){
    couleur[i]<-"green"}
  else if (couleur[i]%in% "2018"){
    couleur[i]<-"black"}
  else if (couleur[i]%in% "2017"){
    couleur[i]<-"deepskyblue3"}
  else if (couleur[i]%in% "2016"){
    couleur[i]<-"coral"}
  else if (couleur[i]%in% "2015"){
    couleur[i]<-"red3"}
  else if (couleur[i]%in% "2014"){
    couleur[i]<-"yellow"}
  else if (couleur[i]%in%"2012"){
    couleur[i]<-"orange4"}
  else {
    couleur[i]<-"gray86"}
}
V(net)$programme<-couleur

#Graphique avec labels pour verifier si tout est bon
plot(net, vertex.size=5, vertex.color=V(net)$programme, edge.color="gray28", edge.width=E(net)$width, edge.arrow.mode = 0, vertex.label.cex=0.5, vertex.frame.color = NA,
     vertex.label=colnames(matrix))
#Graphique sans labels pour le document latex (la figure est trop grosse pour entrer dans le document)
plot(net, vertex.size=5, vertex.color=V(net)$programme, edge.color="gray28", edge.width=E(net)$width, edge.arrow.mode = 0, vertex.label.cex=0.5, vertex.frame.color = NA,
     vertex.label="")
#L√©gende du graphique: 
#annee de debut: noir: 2019, bleu: 2018, vert: 2017, orange: 2016, 
#rouge: 2015, jaune: 2014, orange-brun: 2012, sans information: gris pale

############################################################
# Production du graphique circulaire des collaborations 
############################################################
programmes <- data.frame(from = c("origin","origin","origin","origin","origin"),
                         to = c("ecologie", "microbiologie", "biologie", "moleculaire", "autre"))
etudiants_programmes <- data.frame(from=etudiants_final$programme, 
                                   to=etudiants_final$nom_prenom)

hierarchy <- rbind(programmes, etudiants_programmes)

vertices <- data.frame(name = unique(c(as.character(hierarchy$from), as.character(hierarchy$to))),
                       #√† enlever la ligne suivante si je retourne √† la version de louis)
                       Programme = c("0","0","0","0","0","0", as.character(etudiants_programmes$from)) 
)
#L√©gende du graphique: 
#noir: Autres, bleu: Biologie, vert: √âcologie, orange: Microbiologie, rouge: Bio mol√©culaire
#Collaborations
from <- match(collab_final$etudiant1, vertices$name)
to <- match( collab_final$etudiant2, vertices$name)

#Graphique de liens
mygraph <- graph_from_data_frame( hierarchy, vertices=vertices)
ggraph(mygraph, layout = 'dendrogram', circular = TRUE) + 
  geom_conn_bundle(data = get_con(from = from, to = to), alpha=0.1, colour="gray22", tension = 0.9) + 
  geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05, color= Programme), size=4, alpha=0.75) +
  scale_colour_manual(values=c("black", "deepskyblue3", "darkolivegreen3", "coral", "red3"))+
  theme_void()+
  theme(legend.position="none")
#L√©gende du graphique: 
#noir: Autres, bleu: Biologie, vert: √âcologie, orange: Microbiologie, rouge: Bio mol√©culaire

#####Le graphique des collaborations peut ne pas s'afficher correctement. Si c'est le cas, faire rouler cette derni√®re section 
#de code s√©par√©ment jusqu'√† ce que √ßa fonctionne

############################################################
# Production des barplots 
############################################################
n.etudiant.prog <- etudiants_final %>%
  group_by(etudiants_final$programme) %>%
  count()
n.etudiant.prog$programme<-as.character(n.etudiant.prog$programme)

ggplot(n.etudiant.prog, aes(y=n, x=reorder(programme, -n)))+
  geom_col(aes(fill=programme), size=1)+
  labs(x= "Programme", y="Nombre d'√©tudiants")+
  theme_classic()+
  scale_fill_manual(values=c("grey65", "deepskyblue3", "darkolivegreen3", "coral", "red3"))+
  scale_color_manual(values=c("grey65", "deepskyblue3", "darkolivegreen3", "coral", "red3"))+
  scale_x_discrete(labels=c("√âcologie", "Autre", "Microbiologie", "Biologie", "Mol√©culaire"))+
  scale_y_continuous(limits=c(0, 120), breaks = seq(0,120,20))+
  theme(legend.position = "none",
        axis.text = element_text(size=17, color="black"),
        axis.title = element_text(size=17),
        axis.title.x=element_blank(),
        panel.grid.major.y=element_line(color="black"))

############################################################
#Enregistrement des tables creees dans le script
############################################################
#mon, chemin,vous pouvez mettre vos doc ou vous voulez
#setwd("C:/Users/Samuel Provencher-T/Documents/travail_BIO500")

#write.csv(etudiants_final,"C:/Users/Samuel Provencher-T/Documents/travail_BIO500//etudiants_final.csv", row.names = FALSE)
#write.csv(collab_final,"C:/Users/Samuel Provencher-T/Documents/travail_BIO500//collab_final.csv", row.names = FALSE)
#write.csv(cours_final,"C:/Users/Samuel Provencher-T/Documents/travail_BIO500//cours_final.csv", row.names = FALSE)

write.csv(etudiants_final,file ="etudiants_final.csv", row.names = FALSE)
write.csv(collab_final,file = "collab_final.csv", row.names = FALSE)
write.csv(cours_final,file = "cours_final.csv", row.names = FALSE)

############################################################
#Creation de la base de donnees sql
############################################################
##loader le package n?cessaire
library(RSQLite)
#Fonctionne au moins avec RSQLite 2.0

#Etablir une base de donnee locale parce que on a pas acces a une vraie base de donnee
con <- dbConnect(RSQLite::SQLite(), "my-db.sqlite")
#creer la structure de la table des cours
cours_final_sql <- "
CREATE TABLE cours_final (
  sigle         CHAR(6),
  credits       INTEGER(1),
  obligatoire   BOOLEAN(1),
  type_travail  CHAR(8),
  PRIMARY KEY (sigle)
);"

dbSendQuery(con, cours_final_sql)
#creer la structure de la table des ?tudiants
etudiants_final_sql <- "
CREATE TABLE etudiants_final (
  nom_prenom      VARCHAR(100),
  annee_debut     DATE(4),
  session_debut   CHAR(1),
  programme       VARCHAR(50),
  coop            BOOLEAN(1),
  PRIMARY KEY (nom_prenom)
);"

dbSendQuery(con, etudiants_final_sql)
#Creer la structure de la table des collaboration
collab_final_sql <- "
CREATE TABLE collab_final (
  etudiant1     VARCHAR(100),
  etudiant2     VARCHAR(100),
  cours         CHAR(6),
  date          DATE(4),
  PRIMARY KEY (etudiant1, etudiant2, cours),
  FOREIGN KEY (etudiant1) REFERENCES etudiants_final(nom_prenom),
  FOREIGN KEY (etudiant2) REFERENCES etudiants_final(nom_prenom),
  FOREIGN KEY (cours) REFERENCES cours_final(sigle)
);"
dbSendQuery(con, collab_final_sql)
#Remplir les 3 tables
dbWriteTable(con, append = TRUE, name = "cours_final", value = cours_final, row.names = FALSE)
dbWriteTable(con, append = TRUE, name = "etudiants_final", value = etudiants_final, row.names = FALSE)
#je sais pas pourquois mais on dirait que faire les etapes dans haut mets dequoi de pas correct dans le tableau des collab alors faut delete sont contenu avant de le remplir
dbSendQuery(con,"DROP TABLE collab_final;")
dbWriteTable(con, append = TRUE, name = "collab_final", value = collab_final, row.names = FALSE)


dbReadTable(con, "collab_final")
#pour voir si on a bien nos infos dans le tableau de collab, si elle
#n y sont pas, relancer les 2 dernieres lignes de code

############################################################
#Exemples de requetes
############################################################
#test de requÍte pour voir si cours_final est fonctionnel et contient de donnees
sql_requete <- "
SELECT sigle, credits
  FROM cours_final
;"
req_cours <- dbGetQuery(con, sql_requete)
head(req_cours)
#marche

#test de requÍte pour voir si un inner join est possible entre cours_final et collab_final
sql_requete <- "
SELECT sigle, credits, etudiant1, etudiant2
  FROM cours_final
  INNER JOIN collab_final ON cours_final.sigle = collab_final.cours
;"
req_cours <- dbGetQuery(con, sql_requete)
head(req_cours)
#marche

#test de requÍte pour voir si etudiants_final est fonctionnel et contient de donnees
sql_requete <- "
SELECT nom_prenom, annee_debut, session_debut, programme
  FROM etudiants_final
;"
req_cours <- dbGetQuery(con, sql_requete)
head(req_cours)
#marche 

#requete pour avoir le nombre de collaboration unique
sql_requete <- "
SELECT DISTINCT etudiant1,
count (DISTINCT etudiant2) As nb_collaboration 
  FROM collab_final 
  GROUP BY etudiant1 ORDER BY nb_collaboration DESC  
;"
req_cours <- dbGetQuery(con, sql_requete)
head(req_cours)
req_cours
#pour chaque etudiant 1 compter le nombre d'etudiant 2 distinct
#marche maintenant

#requete pour la nombre de fois ou une paire a travailler ensemble
sql_requete <- "
SELECT COUNT(*) AS nbcollaboration, etudiant1, etudiant2
FROM collab_final
GROUP  BY etudiant1, etudiant2
ORDER BY nbcollaboration DESC
;"
req_cours <- dbGetQuery(con, sql_requete)
head(req_cours)
req_cours

#Verification si le nombre de collab pour la pair Simon Laporte et Axel barry est bonne
sql_requete <- "
SELECT etudiant1,etudiant2
FROM collab_final WHERE etudiant1 LIKE '%barry_axel%' AND etudiant2 LIKE '%laporte_simon%'
;"
req_cours <- dbGetQuery(con, sql_requete)
head(req_cours)
req_cours
#Belle et bien 6 collab

#requete pour voir si j'ai enleve les collab de trop en ECL515 ou on avait un collab en 2019 et en 2020, ce format peut servir pour obtenir
#le nombre de collab pour n'importe quelle paire voulu il faut juste remplac? les x et y par le noms des ?tudiant voulu dans cette sc?cance WHERE etudiant1 LIKE '%x%' AND etudiant2 LIKE '%y%
sql_requete <- "
SELECT etudiant1,etudiant2
FROM collab_final WHERE etudiant1 LIKE '%beteille_clemence%' AND etudiant2 LIKE '%provenchertardif_samuel%'
;"
req_cours <- dbGetQuery(con, sql_requete)
head(req_cours)
req_cours
#Il y en a juste une alors tout est beau

sql_requete <- "
SELECT etudiant1,etudiant2
FROM collab_final WHERE etudiant1 LIKE '%martel_thomas%' AND etudiant2 LIKE '%plante_louis%'
;"
req_cours <- dbGetQuery(con, sql_requete)
head(req_cours)
req_cours

#Utilisation du inner join pour avoir info collab et l'info du cours
sql_requete <- "
SELECT etudiant1,etudiant2, sigle, date, credits, obligatoire, type_travail
FROM cours_final
INNER JOIN collab_final ON cours_final.sigle = collab_final.cours
;"
req_cours <- dbGetQuery(con, sql_requete)
head(req_cours)
req_cours

#Requ?te pour obtenir le nombre de cr?dit obtenue gr?ce a des collaboration
sql_requete <- "
SELECT etudiant1, sum(credits) AS nombre_de_credit_obtenue_collaborations FROM(
SELECT DISTINCT etudiant1,sigle,credits
FROM cours_final
INNER JOIN collab_final ON cours_final.sigle = collab_final.cours
) GROUP BY etudiant1
ORDER BY nombre_de_credit_obtenue_collaborations DESC
;"
req_cours <- dbGetQuery(con, sql_requete)
head(req_cours)
req_cours

#liste des cours avec des collaborations et tout les etudiants qui y ont participer avec le nomre de cr?dit obtenue
sql_requete <- "
SELECT DISTINCT etudiant1,sigle,credits
FROM cours_final
INNER JOIN collab_final ON cours_final.sigle = collab_final.cours
;"
req_cours <- dbGetQuery(con, sql_requete)
head(req_cours)
req_cours

#Nombre de personne dans chaque programme
sql_requete <- "
SELECT COUNT(*) AS nb_personne, programme
FROM etudiants_final
GROUP BY programme
;"
req_cours <- dbGetQuery(con, sql_requete)
head(req_cours)
req_cours

#Nombre de personne coop ou pas coop ou NA
sql_requete <- "
SELECT COUNT(*) AS nb_personne, coop
FROM etudiants_final
GROUP BY coop
;"
req_cours <- dbGetQuery(con, sql_requete)
head(req_cours)
req_cours

#####Le graphique des collaborations peut ne pas s'afficher correctement. Refaire runner le code. Si c'est le cas, faire rouler cette derni√®re section 
#de code s√©par√©ment jusqu'√† ce que √ßa fonctionne
