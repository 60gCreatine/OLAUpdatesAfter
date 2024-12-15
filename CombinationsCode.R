# Hente data
library(dkstat)
library(ggplot2)
FORV1 <- dst_meta(table = "FORV1", lang = "da")

# For at lave et filer, kan vi variables ved FORV1$variables
FORV1_filter <- list(
  INDIKATOR = "*",
  Tid = "*"
)

# Hente tabelens data med ovenstående filter
FORV1_Data <- dst_get_data(table = "FORV1", query = FORV1_filter, lang = "da")
unikke_indikator <- unique(FORV1_Data$INDIKATOR)
indikator_liste <- list()

# Lave nye lister hvor hver unikke indikatornavn
for (indikator in unikke_indikator) {
indikator_liste[[indikator]] <- FORV1_Data[FORV1_Data$INDIKATOR == indikator,]}
FORV1_Total <- data.frame()
FORV1_Total <- as.data.frame(indikator_liste)
  # Fjerne .
  colnames(FORV1_Total) <- gsub("\\."," ",colnames(FORV1_Total))
  
  
  

#Vælger kun tid og værdier for FTI + 12 spørgsmål
FORV1_2000 <- FORV1_Total[304:nrow(FORV1_Total),c(2,3,6,9,12,15,18,21,24,27,30,33,36,39)]
  # Fjerne value
  colnames(FORV1_2000) <- gsub("\\ value","",colnames(FORV1_2000))
  colnames(FORV1_2000)[1] <- "Tid"
  ##### Ændre fortegn #####
  FORV1_2000$`Priser i dag  sammenlignet med for et år siden` <- -FORV1_2000$`Priser i dag  sammenlignet med for et år siden`
  FORV1_2000$`Arbejdsløsheden om et år  sammenlignet med i dag` <- -FORV1_2000$`Arbejdsløsheden om et år  sammenlignet med i dag`
  # Dividere vores tidskolonne med 3, aka Kvartalvis
FORV1_Q <- data.frame(
  Tid <- FORV1_2000$Tid[seq(1, nrow(FORV1_2000), by = 3)])

# Laver om til kvertal, seq = fra 1, til 1, fra hver 3.. Function tager der efter og beregne et gennemsnit af de 3 som input (i er nuværende række)
for (col in 2:ncol(FORV1_2000)) {
  FORV1_Q[[colnames(FORV1_2000)[col]]] <- round(sapply(seq(1, nrow(FORV1_2000), by = 3), function(i) {
    mean(FORV1_2000[i:(i+2), col], na.rm = TRUE)
  }),3)
}

NKHC021 <- dst_meta("NKHC021", lang = "da")
NKHC021_filter <- list(
  FORMAAAL="I alt",
  PRISENHED="2020-priser, kædede værdier",
  SÆSON="Sæsonkorrigeret",
  Tid="*"
)
NKHC021_Data <- dst_get_data(table = "NKHC021", query = NKHC021_filter, lang = "da")

# Fjerne de første 37 rækker, da vi kun skal bruge fra 1999Q1, og og tække 1:3 grundet det ikke er relevant
Forbrugsdata <- NKHC021_Data[37:nrow(NKHC021_Data), 4:5]
colnames(Forbrugsdata)[1] = "Tid"
colnames(Forbrugsdata)[2] = "Dansk_forbrug"

Forbrugsdata$Årlig_vækst <- round(c(rep(NA, 4), diff(Forbrugsdata[,2], lag = 4) / Forbrugsdata[-(1:4),2] * 100),3)
# fjerne tomme rækker før 2000
Forbrugsdata <- na.omit(Forbrugsdata)

forbrug <- Forbrugsdata$Årlig_vækst
# For automatisk at lave vores FORV1 samme længde som i forbrug
forbrug_længde <- as.numeric(length(forbrug))
FTI <- FORV1_Q[1:forbrug_længde, ]
colnames(FTI)[1] = "Tid"
Spørgsmål <- FTI[,3:14]

#### Opgave 1.1 – Kombinationsalgoritme i R ####
# Lav alle kombinationer af de 12 spørgsmål i forbrugerundersøgelsen af DST. I skal bruge data fra 1.
# kvartal 2000 til og med 4. kvartal 2024.

# Først samler vi alle de forskellige FTI Spørgsmål i en vektor

# Alle mulige kombinmationer af x(det sidste tal) spørgsmål
#Kombination1 <- combn(Spørgsmål[1,1:ncol(Spørgsmål)],1)
#Kombination2 <- combn(Spørgsmål[1,1:ncol(Spørgsmål)],2)
#Kombination3 <- combn(Spørgsmål[1,1:ncol(Spørgsmål)],3)
#Kombination4 <- combn(Spørgsmål[1,1:ncol(Spørgsmål)],4)
#Kombination5 <- combn(Spørgsmål[1,1:ncol(Spørgsmål)],5)
#Kombination6 <- combn(Spørgsmål[1,1:ncol(Spørgsmål)],6)
#Kombination7 <- combn(Spørgsmål[1,1:ncol(Spørgsmål)],7)
#Kombination8 <- combn(Spørgsmål[1,1:ncol(Spørgsmål)],8)
#Kombination9 <- combn(Spørgsmål[1,1:ncol(Spørgsmål)],9)
#Kombination10 <- combn(Spørgsmål[1,1:ncol(Spørgsmål)],10)
#Kombination11 <- combn(Spørgsmål[1,1:ncol(Spørgsmål)],11)
#Kombination12 <- combn(Spørgsmål[1,1:ncol(Spørgsmål)],12)

# Lave alle mulige kombinationer af de 12 spørgsmål
  # Laves, fordi kombinationerne skal laves på navne, ikke selve værdierne
Spørgsmål_navne <- names(Spørgsmål)


Kombinationer <- list()
for (i in 1:length(Spørgsmål_navne)) {
  Kombi <- combn(Spørgsmål_navne, i, simplify = FALSE)
  Kombinationer <- c(Kombinationer, Kombi)
}

length(Kombinationer)
# Giver 4095 totalle antal af kombinationer



#### Opgave 1.2 – R2 og forbrugertillidsindikatorer ####
# Hvad er R2 for den indikatorer, der bedst forklarer variationen i den kvartalsvise årlige realvækst i
# husholdningernes forbrugsudgift? Diskutér om denne indikator er bedre end DI’s
# forbrugertillidsindikator. (Hint: se jeres besvarelse af opgave 2 i 2. OLA i flow 1 og opstil en tabel,
#                            der sammenligner de to indikatorer1)

lm_resultater <- list()

for (i in 1:length(Kombinationer)) {
  Kombination <- Kombinationer[[i]]  
  Mean_FTI <- rowMeans(Spørgsmål[, Kombination, drop = FALSE], na.rm = TRUE) 
  
  # Byg data-frame til regression
  variables_df <- data.frame(FTI = Mean_FTI, forbrug = forbrug)
  
  # Kør lineær regression med fejl-håndtering
  lm_resultater[[i]] <- tryCatch({
    summary(lm(forbrug ~ FTI, data = variables_df))
  }, error = function(err) {
    cat(paste0("FEJL: ", err$message, "!!!!!\n"))
    NULL
  })
}

## tjekker med det virker med kombination 666
print(lm_resultater[[666]])
print(Kombinationer[[666]])
# "Danmarks økonomiske situation i dag  sammenlignet med for et år siden"                       
# "Anskaffelse af større forbrugsgoder  inden for de næste 12 mdr "                             
# "Regner med at kunne spare op i de kommende 12 måneder"                                       
# "Familiens økonomiske situation lige nu  kan spare penge slår til  bruger mere end man tjener"

#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.83462    0.42460  -4.321 3.76e-05 ***
#  FTI          0.27828    0.03156   8.818 4.76e-14 ***
# Multiple R-squared:  0.445,	Adjusted R-squared:  0.4392 
# F-statistic: 77.77 on 1 and 97 DF,  p-value: 4.756e-14



# Lav plottet
# Laveste punkt skal omsættes i forbindelse med tid
laveste <- data.frame(
  Tid = c(
    FTI$Tid[which.min(FTI$`Danmarks økonomiske situation i dag  sammenlignet med for et år siden`)],
    FTI$Tid[which.min(FTI$`Anskaffelse af større forbrugsgoder  inden for de næste 12 mdr `)],
    FTI$Tid[which.min(FTI$`Regner med at kunne spare op i de kommende 12 måneder`)],
    FTI$Tid[which.min(FTI$`Familiens økonomiske situation lige nu  kan spare penge slår til  bruger mere end man tjener`)]
  ),
  Value = c(
    min(FTI$`Danmarks økonomiske situation i dag  sammenlignet med for et år siden`),
    min(FTI$`Anskaffelse af større forbrugsgoder  inden for de næste 12 mdr `),
    min(FTI$`Regner med at kunne spare op i de kommende 12 måneder`),
    min(FTI$`Familiens økonomiske situation lige nu  kan spare penge slår til  bruger mere end man tjener`)
  )
)

ggplot() +
  geom_line(data = FTI, aes(x = Tid, y = `Danmarks økonomiske situation i dag  sammenlignet med for et år siden`, color = "Danmarks økonomiske situation i dag")) +
  geom_line(data = FTI, aes(x = Tid, y = `Anskaffelse af større forbrugsgoder  inden for de næste 12 mdr `, color = "Anskaffelse af større forbrugsgoder næste 12 mdr")) +
  geom_line(data = FTI, aes(x = Tid, y = `Regner med at kunne spare op i de kommende 12 måneder`, color = "Regner med at spare op næste 12 mdr")) +
  geom_line(data = FTI, aes(x = Tid, y = `Familiens økonomiske situation lige nu  kan spare penge slår til  bruger mere end man tjener`, color = "Familiens økonomiske situation lige nu, kan spare/slår til/ mere end tjener")) +
  geom_point(data = laveste, aes(x = Tid, y = Value), color = "black") +
  geom_text(
    data = laveste,
    aes(x = Tid, y = Value, label = round(Value, 1)),
    size = 3,                # Større tekst
    color = "black",         # Sort tekst
    nudge_x = 400
  ) +
  labs(
    title = "2 variabler falder aldrig under +15",
    x = "Tid",
    y = "Indikatorscore",
    color = "Indikator"  # Titel for legend
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Kan nu tjekke for alle spm, og lave et nyt nul fra dem der er højt over 0 - se Baums artikel
ggplot() +
  geom_line(data = FTI, aes(x = Tid, y = `Familiens økonomiske situation i dag  sammenlignet med for et år siden`), color = "blue") +
  geom_line(data = FTI, aes(x = Tid, y = `Familiens økonomiske  situation om et år  sammenlignet med i dag`), color = "red") +
  geom_line(data = FTI, aes(x = Tid, y = `Danmarks økonomiske situation i dag  sammenlignet med for et år siden`), color = "green") +
  geom_line(data = FTI, aes(x = Tid, y = `Danmarks økonomiske situation om et år  sammenlignet med i dag`), color = "purple") +
  geom_line(data = FTI, aes(x = Tid, y = `Anskaffelse af større forbrugsgoder  fordelagtigt for øjeblikket`), color = "orange") +
  geom_line(data = FTI, aes(x = Tid, y = `Priser i dag  sammenlignet med for et år siden`), color = "brown") +
  geom_line(data = FTI, aes(x = Tid, y = `Priser om et år  sammenlignet med i dag`), color = "pink") +
  geom_line(data = FTI, aes(x = Tid, y = `Arbejdsløsheden om et år  sammenlignet med i dag`), color = "cyan") +
  geom_line(data = FTI, aes(x = Tid, y = `Anskaffelse af større forbrugsgoder  inden for de næste 12 mdr `), color = "yellow") +
  geom_line(data = FTI, aes(x = Tid, y = `Anser det som fornuftigt at spare op i den nuværende økonomiske situation`), color = "gray") +
  geom_line(data = FTI, aes(x = Tid, y = `Regner med at kunne spare op i de kommende 12 måneder`), color = "darkgreen") +
  geom_line(data = FTI, aes(x = Tid, y = `Familiens økonomiske situation lige nu  kan spare penge slår til  bruger mere end man tjener`), color = "darkred") +
  labs(
    title = "Tidsserieanalyse af økonomiske indikatorer",
    x = "Tid",
    y = "Indikatorscore",
    color = "Indikator"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Meget uoverskueligt, lad os finde dem som kun har min over 1
FTI_MIN <- NULL
for (col in colnames(FTI)[3:14]) {
  min_værdi <- min(FTI[[col]])  # Beregn minimum
  FTI_MIN <- rbind(FTI_MIN, data.frame(Spørgsmål = col, Minimum = min_værdi))
}

ggplot(FTI_MIN, aes(x = reorder(Spørgsmål, Minimum), y = Minimum, fill = Spørgsmål)) +
  geom_bar(stat = "identity") +  
  geom_text(aes(label = round(Minimum, 1)), 
            position = position_stack(vjust = 0.5),  
            size = 3, color = "black") + 
  coord_flip() + 
  labs(
    title = "3 Spørgsmål falder aldrig under 15",
    x = "Spørgsmål",
    y = "Laveste historiske værdi"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),  
    legend.position = "none"              
  )

# Forklare om de forskellige spørgsmål, hvorfor er de altid postive?
# Opret en data frame med de laveste punkter
laveste <- data.frame(
  Tid = c(
    FTI$Tid[which.min(FTI$`Anser det som fornuftigt at spare op i den nuværende økonomiske situation`)],
    FTI$Tid[which.min(FTI$`Regner med at kunne spare op i de kommende 12 måneder`)],
    FTI$Tid[which.min(FTI$`Familiens økonomiske situation lige nu  kan spare penge slår til  bruger mere end man tjener`)]
  ),
  Value = c(
    min(FTI$`Anser det som fornuftigt at spare op i den nuværende økonomiske situation`),
    min(FTI$`Regner med at kunne spare op i de kommende 12 måneder`),
    min(FTI$`Familiens økonomiske situation lige nu  kan spare penge slår til  bruger mere end man tjener`)
  ),
  Label = c(
    "Fornuftigt at spare op nuværende situation",
    "Regner med at spare op næste 12 mdr",
    "Familiens økonomiske situation lige nu, kan spare/slår til/ mere end tjener"
  )
)

# Plot med de laveste punkter vist
ggplot() +
  geom_line(data = FTI, aes(x = Tid, y = `Anser det som fornuftigt at spare op i den nuværende økonomiske situation`, color = "Fornuftigt at spare op nuværende situation")) +
  geom_line(data = FTI, aes(x = Tid, y = `Regner med at kunne spare op i de kommende 12 måneder`, color = "Regner med at spare op næste 12 mdr")) +
  geom_line(data = FTI, aes(x = Tid, y = `Familiens økonomiske situation lige nu  kan spare penge slår til  bruger mere end man tjener`, color = "Familiens økonomiske situation lige nu, kan spare/slår til/ mere end tjener")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Vandret linje ved y = 0
  geom_point(data = laveste, aes(x = Tid, y = Value, color = Label), size = 2) +  # Tilføj laveste punkter
  geom_text(
    data = laveste,
    aes(x = Tid, y = Value, label = round(Value, 1)),
    hjust = 1.4,  # Flytter teksten lidt over punkterne
    size = 3,
    color = "black"
  ) +
  labs(
    title = "3 Spørgsmal falder aldrig under 0",
    x = "Tid",
    y = "Indikatorscore",
    color = "Indikator"  # Label for legend
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




###### Lav nye 0 for 3 det aldrig rammer 0 ######
# Opret nye kolonner, hvor gennemsnittet er justeret til 0
FTI$`Anser det som fornuftigt at spare op i den nuværende økonomiske situation` <- 
  FTI$`Anser det som fornuftigt at spare op i den nuværende økonomiske situation` - 
  mean(FTI$`Anser det som fornuftigt at spare op i den nuværende økonomiske situation`, na.rm = TRUE)

FTI$`Regner med at kunne spare op i de kommende 12 måneder` <- 
  FTI$`Regner med at kunne spare op i de kommende 12 måneder` - 
  mean(FTI$`Regner med at kunne spare op i de kommende 12 måneder`, na.rm = TRUE)

FTI$`Familiens økonomiske situation lige nu  kan spare penge slår til  bruger mere end man tjener` <- 
  FTI$`Familiens økonomiske situation lige nu  kan spare penge slår til  bruger mere end man tjener` - 
  mean(FTI$`Familiens økonomiske situation lige nu  kan spare penge slår til  bruger mere end man tjener`, na.rm = TRUE)

ggplot() +
  geom_line(data = FTI, aes(x = Tid, y = `Anser det som fornuftigt at spare op i den nuværende økonomiske situation`, color = "Fornuftigt at spare op nuværende situation")) +
  geom_line(data = FTI, aes(x = Tid, y = `Regner med at kunne spare op i de kommende 12 måneder`, color = "Regner med at spare op næste 12 mdr")) +
  geom_line(data = FTI, aes(x = Tid, y = `Familiens økonomiske situation lige nu  kan spare penge slår til  bruger mere end man tjener`, color = "Familiens økonomiske situation lige nu, kan spare/slår til/ mere end tjener")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  # Vandret linje ved 0
  labs(
    title = "De justerede kolonner falder nu under 0 (deres nye gennemsnit)",
    x = "Tid",
    y = "Indikatorscore",
    color = "Indikator"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##### Summary, men for nye nul #####
lm_resultater <- list()
Spørgsmål <- FTI[,3:14]

for (i in 1:length(Kombinationer)) {
  Kombination <- Kombinationer[[i]]  
  Mean_FTI <- rowMeans(Spørgsmål[, Kombination, drop = FALSE], na.rm = TRUE) 
  
  # Byg data-frame til regression
  variables_df <- data.frame(FTI = Mean_FTI, forbrug = forbrug)
  
  # Kør lineær regression med fejl-håndtering
  lm_resultater[[i]] <- tryCatch({
    summary(lm(forbrug ~ FTI, data = variables_df))
  }, error = function(err) {
    cat(paste0("FEJL: ", err$message, "!!!!!\n"))
    NULL
  })
}

lm_resultater[[666]]

#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  2.02743    0.22179   9.141 9.58e-15 ***
#  FTI          0.27828    0.03156   8.818 4.76e-14 ***
# Multiple R-squared:  0.445,	Adjusted R-squared:  0.4392 

##### Kun udtrække R^2 #####
r_squared_liste <- numeric(length(lm_resultater))
for (i in 1:length(lm_resultater)) {
  if (!is.null(lm_resultater[[i]])) {
    r_squared_liste[i] <- lm_resultater[[i]]$r.squared
  } else {
    lm_liste[i] <- NA  # Hvis regressionen er NULL, gem NA
  }
}
top_df <- data.frame(kombinations_id = numeric(), R_squared = numeric())
for (i in 1:length(r_squared_liste)) {
    top_df <- rbind(top_df, data.frame(
      kombination_id = i,
      r_squared = r_squared_liste[[i]]
    ))}
top_df <- top_df[order(top_df$r_squared, decreasing = TRUE), ]
head(top_df,5)

###### Lille funktion til top, kan nu bare skrive top_(x), hvor x er hvor mange i toppen vi vil have ######
top_ <- function(top){
for (i in 1:nrow(top_df)){
  if (i <= top) {
    kombi_nr <- top_df$kombination_id[i]
    r2 <- top_df$r_squared[i]
    cat(paste0("Kombination nr: ",i," r2: ",round(r2,2)," har følgende spørgsmål: \n"))
    print(Kombinationer[[kombi_nr]])
  }}}
  
top_(1)
# plot den


###### Function til at finde specifik kombi ######
spm <- c("Familiens økonomiske situation i dag  sammenlignet med for et år siden",
         #"Familiens økonomiske  situation om et år  sammenlignet med i dag",
         "Danmarks økonomiske situation i dag  sammenlignet med for et år siden",
         #"Danmarks økonomiske situation om et år  sammenlignet med i dag",
         "Anskaffelse af større forbrugsgoder  fordelagtigt for øjeblikket", 
         #"Priser i dag  sammenlignet med for et år siden",
         #"Priser om et år  sammenlignet med i dag",
         #"Arbejdsløsheden om et år  sammenlignet med i dag",
         "Anskaffelse af større forbrugsgoder  inden for de næste 12 mdr "
         #"Anser det som fornuftigt at spare op i den nuværende økonomiske situation",
         #"Regner med at kunne spare op i de kommende 12 måneder",
         #"Familiens økonomiske situation lige nu  kan spare penge slår til  bruger mere end man tjener"
         )

find_kombi <- function(spm) {
  # Find kombinationer, der matcher længden og elementerne i spm
  matches <- which(sapply(Kombinationer, function(kombi) {
    length(kombi) == length(spm) && all(spm %in% kombi) && all(kombi %in% spm)
  }))
  
  # Udskriv resultater, hvis findes, altså over 0
  if (length(matches) > 0) {
    for (i in matches) {
      cat(paste0("Match fundet i kombination nr. ", i, ":\n"))
      print(Kombinationer[[i]])
    }
  } else {
    cat("Ingen match fundet.\n")
  }
}
find_kombi(spm)
##### DIs ift. vores bedste kombi #####
#Match fundet i kombination nr. 355:
#[1] "Familiens økonomiske situation i dag  sammenlignet med for et år siden"
#[2] "Danmarks økonomiske situation i dag  sammenlignet med for et år siden" 
#[3] "Anskaffelse af større forbrugsgoder  fordelagtigt for øjeblikket"      
#[4] "Anskaffelse af større forbrugsgoder  inden for de næste 12 mdr " 

#DIs
print(top_df[top_df$kombination_id == 355,])
#kombination_id r_squared
#355            0.3340499

#Vores
print(top_df[top_df$kombination_id ==653,])
#kombination_id r_squared
#653             0.4549612


#### Opgave 1.3 – Spørgsmål i indikatoren ####
# Hvilke spørgsmål indgår i den indikator, 
# som er bedst til at forklare variationen i forbruget? 
# Giver kombinationen af spørgsmål analytisk mening? 
# I bedes overveje alternative indikatorer fra jeres oversigt

# Vi fik den samme som sidste gang


#### Opgave 1.4 – Forudsigelser med afsæt i jeres indikatorer ####
          # Nye nul for totale datasæt
          FORV1_Q$`Anser det som fornuftigt at spare op i den nuværende økonomiske situation` <- 
            FORV1_Q$`Anser det som fornuftigt at spare op i den nuværende økonomiske situation` - 
            mean(FORV1_Q$`Anser det som fornuftigt at spare op i den nuværende økonomiske situation`, na.rm = TRUE)
          
          FORV1_Q$`Regner med at kunne spare op i de kommende 12 måneder` <- 
            FORV1_Q$`Regner med at kunne spare op i de kommende 12 måneder` - 
            mean(FORV1_Q$`Regner med at kunne spare op i de kommende 12 måneder`, na.rm = TRUE)
          
          FORV1_Q$`Familiens økonomiske situation lige nu  kan spare penge slår til  bruger mere end man tjener` <- 
            FORV1_Q$`Familiens økonomiske situation lige nu  kan spare penge slår til  bruger mere end man tjener` - 
            mean(FORV1_Q$`Familiens økonomiske situation lige nu  kan spare penge slår til  bruger mere end man tjener`, na.rm = TRUE)

  # Definer 'Bedste_Kombi' korrekt
  Forbrugsdata$Bedste_Kombi <- round(rowMeans(FTI[, c(
    "Danmarks økonomiske situation i dag  sammenlignet med for et år siden",
    "Priser om et år  sammenlignet med i dag",
    "Anskaffelse af større forbrugsgoder  inden for de næste 12 mdr ",
    "Regner med at kunne spare op i de kommende 12 måneder"
  )], na.rm = TRUE), 2)
  
  # Byg den lineære model
  lm_model <- lm(Årlig_vækst ~ Bedste_Kombi, data = Forbrugsdata)
  summary(lm_model)
  
  # Forudsigelse for det nyeste kvartal
  newestQ <- data.frame(
    Bedste_Kombi = round(rowMeans(FORV1_Q[nrow(FORV1_Q), c(
      "Danmarks økonomiske situation i dag  sammenlignet med for et år siden",
      "Priser om et år  sammenlignet med i dag",
      "Anskaffelse af større forbrugsgoder  inden for de næste 12 mdr ",
      "Regner med at kunne spare op i de kommende 12 måneder"
    )], na.rm = TRUE), 2)
  )
  
  newest_predict <- predict(lm_model, newdata = newestQ)
  print(round(newest_predict,2))
  # 1.65%
  print(round(sd(Forbrugsdata$Årlig_vækst),2))
  #standard afvigelse er på hele 2.8
  
  # Husk vi kun har 2 ud af 3 måneder for kvartalet
  
        # Vi kan evt også lave for DI, og se hvad den forudsiger
  
##### Opgave 1.5 – Sammenlign med en mikroøkonomisk indikator ####
#  # Valg af mikroøkonomiske spørgsmål
#  Spørgsmål <- FTI[, c(
#    "Familiens økonomiske situation i dag  sammenlignet med for et år siden",
#    "Familiens økonomiske  situation om et år  sammenlignet med i dag",
#    "Anskaffelse af større forbrugsgoder  inden for de næste 12 mdr ",
#    "Regner med at kunne spare op i de kommende 12 måneder",
#    "Familiens økonomiske situation lige nu  kan spare penge slår til  bruger mere end man tjener"
#  )]
#  # Tjek rapporten om det er det samme
#  
#  Spørgsmål_navne <- names(Spørgsmål)
#  
#  # Generering af kombinationer
#  Kombinationer <- list()
#  for (i in 1:length(Spørgsmål_navne)) {
#    Kombi <- combn(Spørgsmål_navne, i, simplify = FALSE)
#    Kombinationer <- c(Kombinationer, Kombi)
#  }
#  
#  # Regression og beregning af R^2
#  lm_resultater <- list()
#  r_squared_liste <- numeric(length(Kombinationer))  # Sikrer korrekt initiering
#  
#  for (i in 1:length(Kombinationer)) {
#    Kombination <- Kombinationer[[i]]
#    
#    # Tjek, at kombinationen matcher eksisterende kolonner
#    valid_cols <- Kombination[Kombination %in% colnames(Spørgsmål)]
#    
#    if (length(valid_cols) > 0) {
#      Mean_FTI <- rowMeans(Spørgsmål[, valid_cols, drop = FALSE], na.rm = TRUE)
#      
#      # Byg data-frame til regression
#      variables_df <- data.frame(FTI = Mean_FTI, forbrug = forbrug)
#      
#      # Kør lineær regression
#      lm_resultater[[i]] <- tryCatch({
#        model <- lm(forbrug ~ FTI, data = variables_df)
#        summary(model)$r.squared
#      }, error = function(err) {
#        NA
#      })
#      # Gem R²-værdien
#      r_squared_liste[i] <- lm_resultater[[i]]
#    } else {
#      r_squared_liste[i] <- NA
#    }
#  }
#  
#  # Opret data frame med kombinationer og R^2
#  top_df_mikro <- data.frame(
#    kombination_id = 1:length(Kombinationer),
#    r_squared = r_squared_liste
#  )
#  
#  # Sortér data frame efter R^2
#  top_df_mikro <- top_df_mikro[order(top_df_mikro$r_squared, decreasing = TRUE), ]
#  
#  # Vis de bedste kombinationer
#  head(top_df_mikro, 10)
#
#  
#### Opgave 4 – Stabilitet i jeres forbrugertillidsindikator ####
  ##### Opgave 4.1 – Test af model fra opgave 1 #####
#  Undersøg stabiliteten af jeres fundne indikator fra opgave 1. 
#  Giv en grundig forklaring på opsætning til at undersøge stabiliteten.

  # Antal kvertaler der skal fjernes, fra hver ende at tidsperioden
  max_steps <- 15
  
  # Total antal kvataler
  N <- length(forbrug)
  
  # Funktion til at beregne R^2 for alle kombinationer på et givent datasæt
  beregn_r2 <- function(forbrug_sub, spørgsmål_sub, kombinationer) {
    # Forbered vektor til at gemme R² værdier
    r2_values <- numeric(length(kombinationer))
    
    for (i in seq_along(kombinationer)) {
      spm_navne <- kombinationer[[i]]
      
      # Tjek at alle spørgsmål findes i spørgsmål_sub - hvilket de alle gør, men godt at huske som en 'best practice'
      if (all(spm_navne %in% colnames(spørgsmål_sub))) {
        # Beregn gennemsnits-FTI for denne kombination
        mean_fti <- rowMeans(spørgsmål_sub[, spm_navne, drop = FALSE], na.rm = TRUE)
        
        # Byg data-frame til regression
        df <- data.frame(forbrug = forbrug_sub, FTI = mean_fti)
        
        # Kør lineær regression
        mod <- lm(forbrug ~ FTI, data = df)
        
        # Gem R^2
        r2_values[i] <- summary(mod)$r.squared
      } else {
        r2_values[i] <- NA
      }
    }
    
    return(r2_values)
  }
  
  # Liste til at gemme alle R² resultater for hvert step
  # Efterfølgende vil vi konvertere den til en matrix
  alle_r2_resultater <- vector("list", length = max_steps + 1)
  
  for (step in 0:max_steps) {
    # Definer start- og slutindeks for dette step
    start_ind <- 1 + step
    end_ind <- N - step
    
    # Udsnit af data
    forbrug_sub <- forbrug[start_ind:end_ind]
    spørgsmål_sub <- Spørgsmål[start_ind:end_ind, , drop = FALSE]
    
    # Beregn R² værdierne for alle kombinationer på dette subset
    alle_r2_resultater[[step + 1]] <- beregn_r2(forbrug_sub, spørgsmål_sub, Kombinationer)
  }
  
  # Konverter alle_r2_resultater til en matrix med kombinationer på rækker og steps på kolonner
  r2_matrix <- do.call(cbind, alle_r2_resultater)
  
  # Beregn gennemsnit og standardafvigelse for hver kombination på tværs af steps
  mean_R2 <- rowMeans(r2_matrix, na.rm = TRUE)
  sd_R2 <- apply(r2_matrix, 1, sd, na.rm = TRUE)
  
  # Opret en data frame med resultater
  # Her antager vi, at kombination_id blot er rækkefølgen af kombinationer
  result_df <- data.frame(
    kombination_id = seq_along(Kombinationer),
    Mean_R2 = mean_R2,
    SD_R2 = sd_R2,
    stringsAsFactors = FALSE
  )
  
  # Sortér efter gennemsnits R² hvis ønsket
  result_df <- result_df[order(result_df$Mean_R2, decreasing = TRUE), ]
  
  # Se top-5 mest stabile kombinationer
  head(result_df, 5)
  
  # result_df indeholder nu slutresultatet for alle kombinationer:
  # ID, gennemsnitlig R², samt standardafvigelsen for R² på tværs af de forkortede datasæt
  
   kombi1_id <- 653
   kombi2_id <- 1410
   fjerne_obs_start <- 10
   fjerne_obs_slut <- 15

  

  kombi1_spm <- Kombinationer[[kombi1_id]]
  kombi2_spm <- Kombinationer[[kombi2_id]]
  

  Mean_FTI_kombi1 <- rowMeans(Spørgsmål[, kombi1_spm, drop = FALSE], na.rm = TRUE)
  Mean_FTI_kombi2 <- rowMeans(Spørgsmål[, kombi2_spm, drop = FALSE], na.rm = TRUE)
  

  plot_data <- data.frame(
    Tid = Forbrugsdata$Tid,
    FTI_kombi1 = Mean_FTI_kombi1[],
    FTI_kombi2 = Mean_FTI_kombi2
  )
  

  N <- length(Tid)
  start_cut_index <- 1 + fjerne_obs_start
  end_cut_index <- N - fjerne_obs_slut
  start_cut_time <- Tid[start_cut_index]
  end_cut_time   <- Tid[end_cut_index]
  

  ggplot(plot_data, aes(x = Tid)) +
    geom_line(aes(y = FTI_kombi1, color = "Kombination 653"), size = 1) +
    geom_line(aes(y = FTI_kombi2, color = "Kombination 1410"), size = 1) +
    geom_vline(xintercept = start_cut_time, linetype = "dashed", color = "gray50") +
    geom_vline(xintercept = end_cut_time, linetype = "dashed", color = "gray50") +
    annotate("text", x = start_cut_time, y = Inf, label = "Fjernede obs. før dette punkt", 
             vjust = 2, hjust = 1.1, angle = 90, size = 3, color = "gray20") +
    annotate("text", x = end_cut_time, y = Inf, label = "Fjernede obs. efter dette punkt", 
             vjust = 2, hjust = 1.1, angle = 90, size = 3, color = "gray20") +
    scale_color_manual(values = c("Kombination 653" = "blue", "Kombination 1410" = "red")) +
    labs(title = "En stabiliceringstest på 15 kvartaler, fjerne store udsvingsperioder",
         x = "Tid", y = "Gennemsnitlig FTI", color = "Kombination") +
    theme_minimal(base_size = 12)
  
  
  
  
  
