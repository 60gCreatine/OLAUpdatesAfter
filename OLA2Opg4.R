#OPG 1

#Hent data for forbrugertillidsundersøgelsen fra januar 1996 til i dag og omregn jeres data til
#kvartaler. Lav en grafisk illustration af jeres omregnede data for DST’s forbrugertillidsindikator og
#kommentér på, hvornår de danske forbrugere er mest og mindst optimistiske.

library(dkstat)
library(ggplot2)

# Finde forbrugertillidsindikatoren
dst_search("Forbrugerforventninger")

# Hent dataen
FORV1 <- dst_meta(table = "FORV1", lang = "da")

FORV1_filter <- list(
  INDIKATOR = "*",
  Tid = "*"
)

FORV1Data <- dst_get_data(table = "FORV1", query = FORV1_filter, lang = "da")

# Loop som gruppere de forskellige spørgsmål i seperate lister
FORV1Data <- as.data.frame(FORV1Data)
unikke_indikatorer <- unique(FORV1Data$INDIKATOR)
indikator_lister <- list()
for (indikator in unikke_indikatorer) {
  indikator_lister[[indikator]] <- FORV1Data[FORV1Data$INDIKATOR == indikator, ]
}

indikator_lister[["Forbrugertillidsindikatoren"]]

samlet_liste <- as.data.frame(indikator_lister)


FTI_DF <- samlet_liste[256:nrow(samlet_liste),]
FTI_DF <- FTI_DF[,c(2,3,6,9,12,15,18,21,24,27,30,33,36,39)]

# Opret et nyt dataframe med hver tredje værdi fra Tid-kolonnen og gennemsnit for hver tredje række
FTI_Q <- data.frame(
  Tid = FTI_DF$Forbrugertillidsindikatoren.TID[seq(1, nrow(FTI_DF), by = 3)]
)

# Loop over the rest of the columns (ignoring "Tid") to calculate quarterly averages
for (col in colnames(FTI_DF)[-1]) {
  FTI_Q[[paste0("Gns_", col)]] <- sapply(seq(1, nrow(FTI_DF), by = 3), function(i) {
    mean(FTI_DF[i:(i+2), col], na.rm = TRUE)
  })
}

############################PLOOOOOOOOT
library(ggplot2)

#HELE LORTET
ggplot() +
  geom_line(aes(x = FTI_Q$Tid, y = FTI_Q[, 2], color = "Forbrugertillidsindikatoren"), size = 0.5) +
  geom_line(aes(x = FTI_Q$Tid, y = FTI_Q[, 3], color = "Fam øko i dag og for et år"), size = 0.5) +
  geom_line(aes(x = FTI_Q$Tid, y = FTI_Q[, 4], color = "Fam øko i dag og om et år"), size = 0.5) +
  geom_line(aes(x = FTI_Q$Tid, y = FTI_Q[, 5], color = "Dan øko i dag og for et år"), size = 0.5) +
  geom_line(aes(x = FTI_Q$Tid, y = FTI_Q[, 6], color = "Dan øko i dag og om et år"), size = 0.5) +
  geom_line(aes(x = FTI_Q$Tid, y = FTI_Q[, 7], color = "Anskaffelse forbrugsgoder nu"), size = 0.5) +
  geom_line(aes(x = FTI_Q$Tid, y = FTI_Q[, 8], color = "Priser i dag vs år siden"), size = 0.5) +
  geom_line(aes(x = FTI_Q$Tid, y = FTI_Q[, 9], color = "priser om et år vs i dag"), size = 0.5) +
  geom_line(aes(x = FTI_Q$Tid, y = FTI_Q[, 10], color = "Arbejdsløshed om et år vs i dag"), size = 0.5) +
  geom_line(aes(x = FTI_Q$Tid, y = FTI_Q[, 11], color = "forbrugsgoder indenfor de næste 12 mdr"), size = 0.5) +
  geom_line(aes(x = FTI_Q$Tid, y = FTI_Q[, 12], color = "Fornuftigt at opspare i dag"), size = 0.5) +
  geom_line(aes(x = FTI_Q$Tid, y = FTI_Q[, 13], color = "Mulighed for opsparing næste 12 mdr"), size = 0.5) +
  geom_line(aes(x = FTI_Q$Tid, y = FTI_Q[, 14], color = "Nuværende opsparingsmulighed"), size = 0.5) +
  #Du kan tilføje flere geom_line for flere kolonner, hvis det er nødvendigt
  labs(title = "Udvikling af værdier over tid", x = "Tid", y = "Værdi") +
  theme_minimal()


#RIGTIGE GRAFER 


#Di's FTI
FTI_Q$Gennemsnit <- rowMeans(FTI_Q[, c(3, 4, 7, 11)], na.rm = TRUE)
ggplot() +
  geom_line(aes(x = FTI_Q$Tid, y = FTI_Q$Gennemsnit, color = "DI's FTI"), size = 0.5,) +
  geom_line(aes(x = FTI_Q$Tid, y = FTI_Q[, 2], color = "DST's FTI"), size = 0.5) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("1996-01-01", "2024-12-31"))) +
  labs(title = "DI's FTI", x = "Tid", y = "Værdi") +
  theme(axis.text.x = element_text(angle = 10, hjust = 1))+
  theme_minimal()






library(ggplot2)

# Sørg for, at 'Tid' er i Date-format
FTI_Q$Tid <- as.Date(FTI_Q$Tid)

# Find maksimum og minimum værdier for DI's FTI og DST's FTI
max_gennemsnit <- which.max(FTI_Q$Gennemsnit)
min_gennemsnit <- which.min(FTI_Q$Gennemsnit)

max_dst <- which.max(FTI_Q[, 2])
min_dst <- which.min(FTI_Q[, 2])

# Opret plottet
ggplot() +
  geom_line(aes(x = FTI_Q$Tid, y = FTI_Q$Gennemsnit, color = "DI's FTI"), size = 0.8) +
  geom_line(aes(x = FTI_Q$Tid, y = FTI_Q[, 2], color = "DST's FTI"), size = 0.8) +
  
  # Tilføj symboler ved maksimums- og minimumsværdierne for begge linjer
  geom_point(aes(x = FTI_Q$Tid[max_gennemsnit], y = FTI_Q$Gennemsnit[max_gennemsnit]), color = "#38025C", size = 3, shape = 1) +
  geom_point(aes(x = FTI_Q$Tid[min_gennemsnit], y = FTI_Q$Gennemsnit[min_gennemsnit]), color = "#38025C", size = 3, shape = 1) +
  
  geom_point(aes(x = FTI_Q$Tid[max_dst], y = FTI_Q[max_dst, 2]), color = "#0E78C8", size = 3, shape = 1) +
  geom_point(aes(x = FTI_Q$Tid[min_dst], y = FTI_Q[min_dst, 2]), color = "#0E78C8", size = 3, shape = 1) +
  
  # Tilføj labels med værdier over maksimums- og minimumspunkterne
  geom_text(aes(x = FTI_Q$Tid[max_gennemsnit], y = FTI_Q$Gennemsnit[max_gennemsnit], 
                label = round(FTI_Q$Gennemsnit[max_gennemsnit], 2)), vjust = -1, color = "#38025C") +
  geom_text(aes(x = FTI_Q$Tid[min_gennemsnit], y = FTI_Q$Gennemsnit[min_gennemsnit], 
                label = round(FTI_Q$Gennemsnit[min_gennemsnit], 2)), vjust = 1.5, color = "#38025C") +
  
  geom_text(aes(x = FTI_Q$Tid[max_dst], y = FTI_Q[max_dst, 2], 
                label = round(FTI_Q[max_dst, 2], 2)), vjust = -1, color = "#0E78C8") +
  geom_text(aes(x = FTI_Q$Tid[min_dst], y = FTI_Q[min_dst, 2], 
                label = round(FTI_Q[min_dst, 2], 2)), vjust = 1.5, color = "#0E78C8") +
  
  scale_color_manual(
    values = c("DI's FTI" = "#38025C", "DST's FTI" = "#0E78C8")
  ) +
  # Vis kun årstal på x-aksen og sæt limits og labels
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("1996-01-01", "2024-12-31"))) +
  
  # Tilføj titler og labels
  labs(title = "DST's indikator er mere volatil end DI's indikator", x = "Tid", y = "Værdi") +
  
  # Tilføj mere tydelige gridlines
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey80", size = 0.5),  # Juster gridlines
    panel.grid.minor = element_line(color = "grey90", size = 0.25),  # Tilføj mindre gridlines
    axis.text.x = element_text(angle = 90, hjust = 1)  # Tilt x-akse labels 90 grader
  )




#OPG 2

#Beregn gennemsnittet for underspørgsmålet ”Set i lyset af den økonomiske situation, mener du, at
#det for øjeblikket er fordelagtigt at anskaffe større forbrugsgoder som fjernsyn, vaskemaskine eller
#lignende, eller er det bedre at vente?” for perioden 1. kvartal 2000 til og med 3. kvartal 2024.
#Vurdér jeres resultat set i forhold til spørgsmålet og svarmulighederne. (Hint: giver resultatet analytisk mening?)

#Set i lyset af den økonomiske situation, mener du, at det for øjeblikket er fordelagtigt at anskaffe
#større forbrugsgoder som fjernsyn, vaskemaskine eller lignende, eller er det bedre at vente?
#Fordelagtigt at købe nu 100, Hverken fordelagtigt eller ufordelagtigt at købe nu 0, Ufordelagtigt at købe nu,
#bedre at vente -100

#https://www.dst.dk/da/Statistik/dokumentation/statistikdokumentation/forbrugerforventninger/indhold

# Beregn gennemsnittet

Forbrugsgoder <- FTI_Q[17:nrow(FTI_Q),c(1,7)]
ForbrugsgoderGNS <- mean(Forbrugsgoder$Gns_Anskaffelse.af.større.forbrugsgoder..fordelagtigt.for.øjeblikket.value, na.rm = TRUE)
print(ForbrugsgoderGNS)
#-10.39899

# Plot data med gennemsnitslinje
ggplot(Forbrugsgoder, aes(x = Tid, y = Gns_Anskaffelse.af.større.forbrugsgoder..fordelagtigt.for.øjeblikket.value)) +
  geom_line(aes(color = "Data"), size = 1) + # Linje for data med legend
  geom_hline(aes(yintercept = ForbrugsgoderGNS, color = "Gennemsnit (-10.44)"), linetype = "dashed", size = 0.8) + # Gennemsnitslinje med legend
  labs(
    title = "Historisk set har spørgsmålet spontane negative udsving",
    x = "Tid",
    y = "Indikator score",
    color = "Forklaring" # Legend label
  ) +
  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_line(color = "grey90"),
    legend.position = "bottom" # Placer legend i bunden
  ) +
  scale_color_manual(
    values = c("Data" = "blue", "Gennemsnit (-10.44)" = "maroon") # Farver for linje og gennemsnit
  ) +
  scale_x_date(
    date_breaks = "5 years", # X-akse bryder hvert 5. år
    date_labels = "%Y", # Viser årstal
    limits = c(min(Forbrugsgoder$Tid), max(Forbrugsgoder$Tid)) # Sætter grænser for x-aksen
  )



#Der er noget juks i måden den får "point" på, i det der ikke er en mellemvej mellem -/100 og 0, bliver det næsten til et ja eller nej spørgsmål
#hvilket ikke er ligeså godt at analysere på, end hvis de havde nogle mellemvariabler som kunne give+/- 50, for at få et mere nuanceret billede af
#hvordan det billedet af folks øjeblikkelige indkøbskræft for forbrugsgoder ser ud.




#OPG 3

#Hent data for de 11 grupper af forbrug blandt husholdningerne. Hvad brugte danskerne flest penge
#på i 2022? Hvilken gruppe af forbruget steg mest fra 2020 til 2024?

#(hint: I kan ikke lægge kvartalerne sammen, når I har kædede værdier)



#################################################################################### Forbrug
# Hent dataen
Forb <- dst_meta(table = "NKHC021", lang = "da") #dækker kun over danskernes forbrug i danmark.

Forb_filter <- list(
  FORMAAAL = "*",
  PRISENHED="2020-priser, kædede værdier",
  SÆSON="Sæsonkorrigeret",
  Tid = "*"
)

Forb <- dst_get_data(table = "NKHC021", query = Forb_filter, lang = "da")        #NKHC021 har 16 spørgsmål og dækker over forbrug i udlandet også

Forb_all <- Forb[,-c(2:3)]

colnames(Forb_all) <- c("Kategori", "TID", "VALUES")

Forb_all <- reshape(Forb_all, 
                    idvar = "Kategori", 
                    timevar = "TID", 
                    direction = "wide")
# Fjern 'VALUES.' fra kolonnenavne
colnames(Forb_all) <- gsub("VALUES.", "", colnames(Forb_all))

#FORB 2000-2024
row.names(Forb_all) <- Forb_all$Kategori
Forb2000 <- Forb_all[-1,-c(2:41)]
Forb2000$Kategori <- NULL
Forb2024 <- Forb2000[,97:99]

Forb2024$ForbrugÅr <- rowMeans(Forb2024, na.rm = TRUE)
library(dplyr)

# Ensure the dataframe is sorted by "ForbrugÅr" in descending order
Forb2024 <- Forb2024 %>%
  arrange(desc(ForbrugÅr))
Forb2024$Kategori <- rownames(Forb2024)

# Create the bar plot
ggplot(Forb2024, aes(x = reorder(Kategori, ForbrugÅr), y = ForbrugÅr, fill = Kategori)) +
  geom_bar(stat = "identity", show.legend = FALSE) + # Bar plot without legend
  scale_fill_manual(values = scales::hue_pal()(nrow(Forb2024))) + # Assign unique colors
  labs(
    title = "Forbrug År Fordelt på Kategorier",
    x = "Kategori",
    y = "Forbrug År"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10, angle = 70, ), 
    axis.text.y = element_text(size = 10, face = "bold"), # Enhance y-axis text
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14) # Centered and bold title
  )

#Danskerne brugte flest penge på Boligbenyttelse i 2022




# 2. Find hvilken gruppe af forbruget steg mest fra 2020 til 2024 (kædede værdier
# Vælg kolonnerne for 2020 og 2024
df_2020_2024 <- Forb2000[, c("TID", "2020-01-01", "2024-01-01")]

# Beregn procentændringen fra 2020 til 2024
df_2020_2024$percent_change <- ((df_2020_2024[, "2024-01-01"] - df_2020_2024[, "2020-01-01"]) / df_2020_2024[, "2020-01-01"]) * 100

# Find den kategori med den største procentuelle stigning
highest_increase <- df_2020_2024[which.max(df_2020_2024$percent_change), "TID"]

cat("Gruppen der steg mest procentuelt fra 2020 til 2024 er:", highest_increase, "\n")
#Gruppen der steg mest procentuelt fra 2020 til 2024 er: Beklædning og fodtøj 



#OPG 4
#Lav 22 simple lineære regressioner mellem hver af de 11 grupper i forbruget (y-variable) og
#henholdsvis forbrugertillidsindikatoren fra DST og DI. I skal gemme summary i 22 lister. I skal
#lave jeres regressioner fra 1. kvartal 2000 til og med 1. kvartal 2024.

#OPRET FTI FOR DI OG DST
DST_FTI_2000 <- FTI_Q[17:nrow(FTI_Q),1:2]
colnames(DST_FTI_2000) <- c("Tid", "DST_FTI")
DI_FTI_2000 <- as.data.frame(DST_FTI_2000$Tid)
DI_FTI_2000$Di_forbrugerTILLID <- rowMeans(FTI_Q[17:nrow(FTI_Q), c(3, 4, 7, 11)], na.rm = TRUE)
colnames(DI_FTI_2000) <- c("Tid", "DI_FTI")



#OPrettelse af forbrug i rigtigt format
DI_FTI_2000 <- DI_FTI_2000[-1,]
DST_FTI_2000 <- DST_FTI_2000[-1,]

Forb2000 <- as.data.frame(t(Forb2000))


#22 Linære regressioner

DI_models <- list()
DST_models <- list()

for (col_name in colnames(Forb2000)) {
  DI_models[[col_name]] <- summary(lm(Forb2000[[col_name]] ~ DI_FTI_2000[[2]]))
  DST_models[[col_name]] <- summary(lm(Forb2000[[col_name]] ~ DST_FTI_2000[[2]]))
}

# Udskriv de resulterende modeller
print(DI_models)
print(DST_models)

#Vores summary er i listeform, og vi kan enten printe hele lortet, eller $ for specifikke


