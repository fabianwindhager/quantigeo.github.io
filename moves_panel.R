library(tidyverse)

getwd()

bi_moves <- read.csv2("C:/Users/fwindhager/001_PROJECTS/002_CURB/001_DATA/OGDEXT_BINNENWAND_1.csv", header = FALSE)

names(bi_moves)

bi_moves <- bi_moves %>%
  rename(jahr = V1, 
         kennz_origin = V2, 
         kennz_dest = V3,
         staat = V4, 
         sex = V5, 
         n = V6) %>% # spalten umbenennen
  mutate(jahr = str_extract(jahr, "[0-9]{4}"), #jahr extrahieren
         kennz_origin = str_extract(kennz_origin, "[0-9]+"), #kennzahl extrahieren
         kennz_dest = str_extract(kennz_dest, "[0-9]+"), #kennzahl extrahieren
         staat = str_replace(staat, "STAAT_DICHOTOM-1", "AT"), #staatszugeh?rigkeit vereinfachen
         staat = str_replace(staat, "STAAT_DICHOTOM-2", "nAT"), #staatszugeh?rigkeit vereinfachen
         sex = str_replace(sex, "C11-1", "m"), #geschlecht vereinfachen
         sex = str_replace(sex, "C11-2", "w")) #geschlecht vereinfachen


bi_moves <- bi_moves %>% 
  group_by(jahr, kennz_origin, kennz_dest) %>% #gruppieren
  mutate(n_sum = sum(n),
         jahr = as.integer(jahr),
         kennz_dest = as.character(kennz_dest),
         kennz_origin = as.character(kennz_origin)) %>% #Wanderungen gesamt berechnen
  select(-c("staat", "sex", "n")) %>% #nicht benötigte Spalten löschen
  distinct() %>% #Duplikate löschen
  filter(!kennz_origin == kennz_dest) %>% 
  ungroup() #Gruppierung aufheben

bi_dta_comp1 <- complete(bi_moves, jahr, kennz_origin, kennz_dest, fill = list(n_sum = 0))

bi_zu_1 <- bi_dta_comp1 %>% 
  group_by(jahr, kennz_dest) %>% 
  summarise(bi_zu = sum(n_sum)) %>%
  rename(gemnr = kennz_dest) %>% 
  ungroup()


bi_weg_1 <- bi_dta_comp1 %>% 
  group_by(jahr, kennz_origin) %>% 
  summarise(bi_weg = sum(n_sum)) %>%
  rename(gemnr = kennz_origin) %>% 
  ungroup()

bi_dta_1 <- left_join(bi_zu_1, bi_weg_1, by = c("gemnr", "jahr"))

bi_dta_1 <- complete(bi_dta_1, jahr, gemnr, fill = list(n_sum = 0))
which(is.na(bi_dta_1), arr.ind = TRUE)

anyNA(bi_dta_1)
#write.csv(bi_dta_1, "data/Binnenwanderungen.csv", row.names = FALSE)

# Aussenwanderungen

au_moves <- read.csv2("C:/Users/fwindhager/001_PROJECTS/002_CURB/001_DATA/OGD_bevwan020_AUSSENWAND_201.csv")
au_moves2 <- read.csv2("C:/Users/fwindhager/001_PROJECTS/002_CURB/001_DATA/OGD_bevwan020_AUSSENWAND_202.csv")

au_moves <- rbind(au_moves, au_moves2)


names(au_moves)

au_moves <- au_moves %>%
  rename(jahr = C.A10.0,
         alter = C.GALT5J100.0,
         staat = C.STAATEN_EU.0,
         gemnr = C.GRGEMAKT.0,
         au_zu = F.ZUZUEGE,
         au_weg = F.WEGZUEGE) %>% # spalten umbenennen
  mutate(jahr = str_extract(jahr, "[0-9]{4}"), #jahr extrahieren
         gemnr = str_extract(gemnr, "[0-9]+")) #kennzahl extrahieren)

agr_au_zu <- au_moves %>% 
  group_by(jahr, gemnr) %>% 
  summarise(au_zu = sum(au_zu),
         au_weg = sum(au_weg)) %>% 
  ungroup()

agr_au_zu <- complete(agr_au_zu, jahr, gemnr, fill = list(n_sum = 0))


anyNA(au_moves)

#write.csv(agr_au_zu, "data/Aussenwanderungen.csv", row.names = FALSE)

moves_new <- read.csv("data/Aussenwanderungen.csv") %>%  filter(jahr != 2024)
bi_dta_1 <- read.csv("data/Binnenwanderungen.csv") 

moves_new <- left_join(bi_dta_1, moves_new, by = c("jahr", "gemnr"))
moves_new[is.na(moves_new)] <- 0

#write.csv(moves_new, "data/Wanderungsdaten.csv", row.names = FALSE)


anyNA(moves_new)
which(is.na(moves_new), arr.ind = TRUE)
colSums(is.na(moves_new))
