#install.packages("readr")

library(readr)

####  mest VIRGIN CSV ####
data <- read.csv("regnskaber_industri_transport_byg_5_25000_ansatte_anonym.csv",
                 header = TRUE,
                 sep = ";",
                 fileEncoding = "ISO-8859-1")

data$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål. <- gsub("Dårlige", "Dårlig", data$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.)

freq <- as.data.frame(sort(table(data$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.),decreasing = T))

colnames(freq) <- c("Rating", "frekvens")


#### SÅ PILLER VI ####
data2 <- data
#data2$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål. <- gsub("Dårlig", "Negativ", data2$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.)
#data2$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål. <- gsub("Meget dårlige", "Negativ", data2$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.)
#data2$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål. <- gsub("Gode", "Positiv", data2$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.)
#data2$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål. <- gsub("Meget gode", "Positiv", data2$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.)
#FØR 4484 - EFTER 4433 = 51 NA
# Slet alle rækker, hvor "ved ikke" findes i nogen kolonne
data2 <- data2[!apply(data2, 1, function(row) any(grepl("Ved ikke", row, ignore.case = TRUE))), ]

### så piller vi data3 ###
data3 <- data
data3$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål. <- gsub("Dårlig", "Negativ", data3$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.)
data3$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål. <- gsub("Meget dårlige", "Negativ", data3$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.)
data3$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål. <- gsub("Gode", "Positiv", data3$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.)
data3$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål. <- gsub("Meget gode", "Positiv", data3$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.)
#FØR 4484 - EFTER 4433 = 51 NA
# Slet alle rækker, hvor "ved ikke" findes i nogen kolonne
data3 <- data3[!apply(data3, 1, function(row) any(grepl("Ved ikke", row, ignore.case = TRUE))), ]


freq3 <- as.data.frame(sort(table(data3$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.),decreasing = T))
colnames(freq3) <- c("Rating", "frekvens")


#### SOLIDARITET ####
solidaritet <- data3[,c(1,218)]
solidaritet$Soliditetsgrad.2020.... <- gsub(",", "", solidaritet$Soliditetsgrad.2020....)
solidaritet$Soliditetsgrad.2020.... <- as.numeric(solidaritet$Soliditetsgrad.2020...)
solidaritet <- na.omit(solidaritet)
#FØR 4433 - EFTER 4433 - fjernet 0 linjer
solidaritet$Soliditetsgrad.2020.... <- solidaritet$Soliditetsgrad.2020.... / 100
soldf <- aggregate(solidaritet$Soliditetsgrad.2020...., list(solidaritet$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.), FUN=mean) 
colnames(soldf) <- c("Group", "GNS_SOL")


solidaritet$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål. <- as.factor(solidaritet$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.) 

#GGPLOT#

library(ggplot2)

#Mærkeligt ens resultat - grundet at det er meget samme slags erhverv, hvilket betyder at deres solidaritetsgrad inde for deres sektor er meget ens

# Lav ggplot SOLIDITET
ggplot(soldf, aes(x = Group, y = GNS_SOL, fill = Group)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("darkblue", "gray", "blue")) + # Tilpas farver
  labs(
    x = "Vurdering",
    y = "Gennemsnitlig soliditetsgrad"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5)
  )

























#### alle svar ####
#### alle svar ####
# Beregn gennemsnittet af frekvensen
gennemsnit <- mean(freq$frekvens)

# Lav et søjlediagram med forskellige farver og gennemsnitslinje
ggplot(freq, aes(x = Rating, y = frekvens, fill = Rating)) +
  geom_bar(stat = "identity", color = "black") +
  geom_hline(yintercept = gennemsnit, linetype = "dashed", color = "red", size = 1) +
  labs(
    title = "Langt de fleste virksomheder er positivt stemt når det kommer til muligheden om at låne penge",
    x = "Rating",
    y = "Frekvens",
    subtitle = paste("Gennemsnit:", round(gennemsnit, 2))
  ) +
  scale_fill_brewer(palette = "Set3") + # Farvepalet til søjler
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

#### Fjernet ved ikke da den kan manipulere vores tal - og er totalt ligegyldige ####

freq2 <- freq[-6,]
# Beregn gennemsnittet af frekvensen
gennemsnit2 <- mean(freq2$frekvens)

# Lav et søjlediagram med forskellige farver og gennemsnitslinje
ggplot(freq2, aes(x = Rating, y = frekvens, fill = Rating)) +
  geom_bar(stat = "identity", color = "black") +
  geom_hline(yintercept = gennemsnit2, linetype = "dashed", color = "red", size = 1) +
  labs(
    title = "Langt de fleste virksomheder er positivt stemt når det kommer til muligheden om at låne penge",
    x = "Rating",
    y = "Frekvens",
    subtitle = paste("Gennemsnit:", round(gennemsnit2, 2))
  ) +
  scale_fill_brewer(palette = "Set3") + # Farvepalet til søjler
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )



#### POSITIV NEGATIV NEUTRAL ####


ggplot(freq3, aes(x = Rating, y = frekvens, fill = Rating)) +
  geom_bar(stat = "identity", color = "black") +
  #geom_hline(yintercept = gennemsnit2, linetype = "dashed", color = "red", size = 1) +
  labs(
    title = "Langt de fleste virksomheder er positivt stemt når det kommer til muligheden om at låne penge",
    x = "Rating",
    y = "Frekvens"
    #subtitle = paste("Gennemsnit:", round(gennemsnit2, 2))
  ) +
  scale_fill_brewer(palette = "Set3") + # Farvepalet til søjler
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )


#### uden ved ikke ####
gennemsnit3 <- mean(freq$frekvens)
freq3uvi <- freq3
ggplot(freq3, aes(x = Rating, y = frekvens, fill = Rating)) +
  geom_bar(stat = "identity", color = "black") +
  geom_hline(yintercept = gennemsnit3, linetype = "dashed", color = "red", size = 1) +
  labs(
    title = "Langt de fleste virksomheder er positivt stemt når det kommer til muligheden om at låne penge",
    x = "Rating",
    y = "Frekvens",
    subtitle = paste("Gennemsnit:", round(gennemsnit3, 2))
  ) +
  scale_fill_brewer(palette = "Set3") + # Farvepalet til søjler
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )



# Beregn procentdelen
freq3$procent <- (freq3$frekvens / sum(freq3$frekvens)) * 100

library(scales) # For at bruge procentformatteringen

ggplot(freq3, aes(x = Rating, y = procent, fill = Rating)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(
    aes(label = paste0(round(procent, 1), "%")), # Tilføjer tekst med procent
    vjust = -0.5, # Placering over søjlen
    size = 0 # Juster tekststørrelsen
  ) +
  labs(
    title = "Langt de fleste virksomheder er positivt stemt når det kommer til muligheden om at låne penge",
    x = "Rating",
    y = "Procent"
  ) +
  scale_fill_manual(
    values = c("Positiv" = "blue", "Neutrale" = "gray", "Negativ" = "darkblue") # Specifikke farver
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 10)#, # Trin på 10
    #labels = function(x) paste0(x, "%") # Tilføj "%" efter tallene
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )


### BAUMS GULDKORN ###
#selv forklare de variable
#"Find ud af hvad vi vil have med"
#"start ved colonne a og bare kør derudaf for at finde ud af hvad vi skal bruge"
#" ELLER kig i artiklen og kopier"
#lav modellen
#kør tomodeller, find signifikant og tag kun insignifikant med hvis det skal bruges til analysen (fks geo-placering)
#grafisk forklare variablen i forhold til hvad er er svaret



#### CLM ####
library(ordinal)
DFCLM <- as.data.frame(data2[,1])
colnames(DFCLM) <- "Faktor"
DFCLM$Balance <- data2$Balance.2020..1.000.kr.
DFCLM$Soliditetsgrad <- data2$Soliditetsgrad.2020....
DFCLM$AFKAST <-  data2$Afkastningsgrad.2020....
DFCLM$Ansatte <- data2$Antal.ansatte.Cvr.nr.
DFCLM$ÅRSTAL <- data2$Etableringsdato
DFCLM$Egenkapital <- data2$Egenkapital.2020..1.000.kr.
DFCLM <- na.omit(DFCLM)


## SVARSCORE
DFCLM$svarscore <- factor(DFCLM$Faktor, 
                          levels = c("Meget dårlige", "Dårlig", "Neutrale", "Gode", "Meget gode"), 
                          ordered = TRUE)

## SOLIDIT
DFCLM$Soliditetsgrad <- gsub(",", "", DFCLM$Soliditetsgrad)
DFCLM$Soliditetsgrad <- as.numeric(DFCLM$Soliditetsgrad)
DFCLM$Soliditetsgrad <- DFCLM$Soliditetsgrad / 100

#Afkast
DFCLM$AFKAST <- gsub(",", "", DFCLM$AFKAST)
DFCLM$AFKAST <- as.numeric(DFCLM$AFKAST)
DFCLM$AFKAST <- DFCLM$AFKAST / 100

#BALANCE
DFCLM$Balance <- as.numeric(DFCLM$Balance)

##årstal
DFCLM$ÅRSTAL <- as.Date(DFCLM$ÅRSTAL, format = "%d-%m-%Y")
DFCLM$ÅRSTAL <- as.numeric(difftime(DFCLM$ÅRSTAL, as.Date("2024-12-10"), units = "days"))
DFCLM$ÅRSTAL <- DFCLM$ÅRSTAL *-1


#Egenkapital
DFCLM$Egenkapital <- as.numeric(DFCLM$Egenkapital)


#SCALING
DFCLM$Soliditetsgrad <- scale(DFCLM$Soliditetsgrad)
DFCLM$Balance <- scale(DFCLM$Balance)
DFCLM$AFKAST <- scale(DFCLM$AFKAST +1)
DFCLM$Ansatte <- scale(DFCLM$Ansatte)
DFCLM$Egenkapital <- scale(DFCLM$Egenkapital)
DFCLM$ÅRSTAL <- scale(DFCLM$ÅRSTAL)

CLMBAUM <- clm(svarscore ~ Soliditetsgrad + Balance + AFKAST, data = DFCLM)
summary(CLMBAUM)

### VORES CLM ###
CLM <- clm(svarscore ~ Soliditetsgrad + Balance + AFKAST + Ansatte + ÅRSTAL + Egenkapital, data = DFCLM)
summary(CLM)




### PLOTS ###


#BALANCE
Balansje <- as.data.frame(data3[,1])
Balansje$Balance <- data3$Balance.2020..1.000.kr.
colnames(Balansje) <- c("Factor", "Balance")
Balansje <- na.omit(Balansje)
# Definer dine egne breakpoints
breakpoints <- c(1, 10000, 100000, 1000000, Inf)

# Opdel Balance i grupper baseret på de specifikke breakpoints
Balansje$Balance_Group <- cut(
  Balansje$Balance, 
  breaks = breakpoints, 
  labels = c("1-10.000", "10.000-100.000", "100.000-1.000.000", ">1.000.000"),
  include.lowest = TRUE
)

# Beregn fordelingen af Faktorer inden for hver Balance Group
freq_table <- table(Balansje$Factor, Balansje$Balance_Group)

# Tjek om tabellen er korrekt
print(freq_table)

# Plot data
barplot(
  freq_table, 
  beside = TRUE,               # Søjler side om side
  col = c("#002E6D", "#FAB958", "#A3E1CE"), # Farver for Faktorer
  legend.text = TRUE,          # Tilføj legend med Faktorer
  args.legend = list(x = "topright", bty = "n", inset = c(0, 0)), # Tilpas legendens placering
  main = "Finansieringsmuligheder forbedres med øget balance", # Overskrift
  xlab = "Balance-opdeling",     # X-akse label
  ylab = "Antal svar",         # Y-akse label
  border = "white",            # Fjern kantlinjer om søjler
  space = c(0.2, 1)            # Tilføj mellemrum mellem grupper
)



#AFKASTNIGSGRAD
combined_data <- data.frame(
  Group = rep(c("Negative", "Neutrale", "Positiv"), 2),
  Value = c(14.76852, 15.10715, 13.75527, 40.24975, 41.38015, 43.33976), # Afkastningsgrad og Soliditetsgrad værdier
  Variable = rep(c("Afkastningsgrad", "Soliditetsgrad"), each = 3)
)

# Lav et samlet plot
combined_plot <- ggplot(combined_data, aes(x = Group, y = Value, fill = Group)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = round(Value, 1)), vjust = -0.5, size = 4) + # Tilføj værdier over søjlerne
  scale_fill_manual(values = c("gray70", "gray50", "steelblue")) +
  facet_grid(~ Variable) +
  labs(
    x = "Vurdering",
    y = "Pct.",
    title = "Afkastningsgraden er ikke længere særlig signifikant for om du kan låne penge i 2020¨",
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14, family = "Arial"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.text.x = element_text(angle = 15, hjust = 0, color = "black"),
    axis.text.y = element_text(color = "black"),
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0)
  ) 

# Vis plottet
print(combined_plot)

#### AFKASTNINGSGRAD ####
Afkastning <- data3[,c(1,206)]
Afkastning$Afkastningsgrad.2020.... <- gsub(",","", Afkastning$Afkastningsgrad.2020....)
Afkastning$Afkastningsgrad.2020.... <- as.numeric(Afkastning$Afkastningsgrad.2020....)
Afkastning <- na.omit(Afkastning)
#FØR 4433 - EFTER 4433 - fjernet 0 linjer
Afkastning$Afkastningsgrad.2020.... <- Afkastning$Afkastningsgrad.2020..../100
Afkasdf <- aggregate(Afkastning$Afkastningsgrad.2020...., list(Afkastning$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.), FUN=mean) 
colnames(Afkasdf) <- c("Group", "GNS_AFK")

Afkastning$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål. <- as.factor(Afkastning$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.) 

# Lav ggplot AFKASTNINGSGRAD
ggplot(Afkasdf, aes(x = Group, y = GNS_AFK, fill = Group)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("darkblue", "gray", "blue")) + # Tilpas farver
  labs(
    x = "Vurdering",
    y = "Gennemsnitlig afkastningsgrad"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5)
  )


# Beregn procentdelen af positive vs negative vs neutrale
freq3$procent <- (freq3$frekvens / sum(freq3$frekvens)) * 100


ggplot(freq3, aes(x = Rating, y = procent, fill = Rating)) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = paste0(round(procent, 1), "%")), # Tilføjer tekst med procent
    vjust = -0.5, # Placering over søjlen
    size = 0 # Juster tekststørrelsen
  ) +
  labs(
    title = "Langt de fleste virksomheder er positivt stemt når det kommer til muligheden om at låne penge",
    x = "Rating",
    y = "Procent"
  ) +
  scale_fill_manual(values = c("#A3E1CE", "#FAB958", "#002E6D")) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 10), # Trin på 10
    labels = function(x) paste0(x, "%") # Tilføj "%" efter tallene
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

#### SOLIDARITET ####
solidaritet <- data3[,c(1,218)]
solidaritet$Soliditetsgrad.2020.... <- gsub(",", "", solidaritet$Soliditetsgrad.2020....)
solidaritet$Soliditetsgrad.2020.... <- as.numeric(solidaritet$Soliditetsgrad.2020...)
solidaritet <- na.omit(solidaritet)
#FØR 4433 - EFTER 4433 - fjernet 0 linjer
solidaritet$Soliditetsgrad.2020.... <- solidaritet$Soliditetsgrad.2020.... / 100
soldf <- aggregate(solidaritet$Soliditetsgrad.2020...., list(solidaritet$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.), FUN=mean) 
colnames(soldf) <- c("Group", "GNS_SOL")


solidaritet$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål. <- as.factor(solidaritet$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.) 

#GGPLOT#



#Mærkeligt ens resultat - grundet at det er meget samme slags erhverv, hvilket betyder at deres solidaritetsgrad inde for deres sektor er meget ens
# Beregn procentfordeling
soldf$Percent <- (soldf$GNS_SOL / sum(soldf$GNS_SOL)) * 100

# Lav ggplot SOLIDITET med procentlabels
ggplot(soldf, aes(x = Group, y = GNS_SOL, fill = Group)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#002E6D", "#FAB958", "#A3E1CE")) + # Tilpas farver
  labs(
    title = "Virksomheder med en højere soliditetsgrad ser mere positivt på deres lånemuligheder",
    x = "Vurdering",
    y = "Gennemsnitlig soliditetsgrad"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5) # Centreret og fed overskrift
  )


### EST GGPLOT ###

##årstal
EST <- as.data.frame(data3$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.)
EST$EST <- data3$Etableringsdato
EST$EST <- as.Date(data3$Etableringsdato, format = "%d-%m-%Y")
EST$EST <- as.numeric(difftime(EST$EST, as.Date("2024-12-10"), units = "days"))
EST$EST <- EST$EST *-1

EST <- aggregate(EST$EST, list(EST$`data3$Hvordan.ser.du.mulighederne.for.at.låne.penge.til.din.virksomhed...fiktivt.spørgsmål.`), FUN = mean) 
colnames(EST) <- c("Group", "GNS_EST")


# Beregn procentfordeling
EST$Percent <- (EST$GNS_EST / sum(EST$GNS_EST)) * 100

# Lav ggplot for gennemsnitligt etableringsår med procentfordeling
ggplot(EST, aes(x = Group, y = GNS_EST, fill = Group)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#002E6D", "#FAB958", "#A3E1CE")) + # Tilpas farver
  labs(
    title = "De ældste virksomheder er mere positivt stemte når det kommer til at låne penge", # Overskrift
    x = "Vurdering",
    y = "Dage siden stiftelse"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5) # Centreret og fed overskrift
  )
