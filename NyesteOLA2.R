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


#### Opgave 2.1 – Opdatering af DI’s forbrugertillidsindikator ####
# Opdatér DI’s forbrugertillidsindikator med data frem til og med 2023 fra artiklen ”Forbruget
# fortsætter fremgangen i 2016” (Baum, 2016). Lav vurdering af om forbrugertillidsindikatoren fra DI
# fortsat er bedre end forbrugertillidsindikatoren fra DST. 
FORV1_Q$DI <- round(rowMeans(FORV1_Q[,c(3,5,7,11)]),3)

FTI$DI <- round(rowMeans(FTI[,c(3,5,7,11)]),3)

Indikatordf <- FTI[,c(1,2,15)]
colnames(Indikatordf)[2] <- "DST"
Indikatordf$Forbrug <- Forbrugsdata$Årlig_vækst

# Skalering af DST og DI
Indikatordf$DST_scaled <- Indikatordf$DST / 2.75
Indikatordf$DI_scaled <- Indikatordf$DI / 2.75


ggplot(Indikatordf, aes(x = Tid)) +
  # Barplot for "Forbrug"
  geom_bar(aes(y = Forbrug, fill = "Årlig realvækst pr. kvartal i privatforbruget (højre akse)"), 
           stat = "identity", alpha = 0.6, width = 60) +
  
  # Linjer for skaleret "DST" og "DI"
  geom_line(aes(y = DST_scaled, color = "DST's forbrugertillidsindikator"), size = 1) +
  geom_line(aes(y = DI_scaled, color = "DI's forbrugertillidsindikator"), size = 1) +
  
  # Opsætning af y-akser
  scale_y_continuous(
    name = "Nettotal",                       # Primær y-akse (DST og DI)
    breaks = seq(-20, 20, by = 5),           # Breaks for primær y-akse
    limits = c(-20, 20),                     # Grænser for primær y-akse
    sec.axis = sec_axis(~ .,                 # Sekundær y-akse (Forbrug)
                        name = "Pct.",
                        breaks = seq(-15, 15, by = 2))
   
  ) +
  
  
  # Opsætning af farver
  scale_fill_manual(values = c("lightblue"), name = "") +
  scale_color_manual(values = c("#7D55C7", "#0F78C8"), name = "") +
  
  # Labels og titler
  labs(
    title = "DI's forbrugertillidsindikator holder sig mere negativt over perioden",
    x = "Tid",
    y = NULL
  ) +
  
  # Æstetik og layout
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "black"), # Farve på y-aksens titel
    axis.title.y.right = element_text(color = "black"),
    legend.position = "bottom",                  # Placering af legend
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(size = 14, face = "bold")
  )

lm_DI <- lm(Forbrug~DI, data = Indikatordf)
summary(lm_DI)

lm_DST <- lm(Forbrug~DST, data = Indikatordf)
summary(lm_DST)

# Visualicer R^2
# Visualicer Korrelation - for hver x ift. y (se Baums artikel)
# Ved at køre kombi er DI bedre en FTI

##### Opgave 2.2 – Forudsigelser af forbruget #####
newdataDI <- as.data.frame(FORV1_Q$DI[100])
colnames(newdataDI) <- "DI"
ForbrugPredict_DI <- predict(lm_DI, newdata = newdataDI)
print(ForbrugPredict_DI)
Manuel_DI <- 2.15675 + (0.18122 * newdataDI$DI)
print(Manuel_DI)
# 0.2947145

newdataDST <- as.data.frame(FORV1_Q$Forbrugertillidsindikatoren[100])
colnames(newdataDST) <- "DST"
ForbrugPredict_DST <- predict(lm_DST, newdata = newdataDST)
print(ForbrugPredict_DST)
Manuel_DST <- 1.31708 + (0.17562 * newdataDST$DST)
print(Manuel_DST)
# -0.281062




#### ####
# https://www.danskindustri.dk/arkiv/analyser/2024/5/di-prognose-tilbagegangen-i-beskaftigelsen-er-aflyst/








