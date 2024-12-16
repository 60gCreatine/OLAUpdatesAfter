library(dkstat)
library(ggplot2)
library(pls)
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
Forbrugsdata <- cbind(Forbrugsdata, Spørgsmål)


#### Opgave 2.1 – PCA regression ####
# Lav en PCA regression på jeres data fra opgave 1, hvor Y er jeres årlige realvækst i
# husholdningernes forbrugsudgift og X er alle de 12 spørgsmål fra DST’s
# forbrugerforventningsundersøgelsen.
pcr.fit <- pcr(Årlig_vækst~
                 `Familiens økonomiske situation i dag  sammenlignet med for et år siden` +
                 `Familiens økonomiske  situation om et år  sammenlignet med i dag` +
                 `Danmarks økonomiske situation i dag  sammenlignet med for et år siden` +
                 `Danmarks økonomiske situation om et år  sammenlignet med i dag` +
                 `Anskaffelse af større forbrugsgoder  fordelagtigt for øjeblikket` +
                 `Priser i dag  sammenlignet med for et år siden` +
                 `Priser om et år  sammenlignet med i dag` +
                 `Arbejdsløsheden om et år  sammenlignet med i dag` +
                 `Anskaffelse af større forbrugsgoder  inden for de næste 12 mdr ` +
                 `Anser det som fornuftigt at spare op i den nuværende økonomiske situation` +
                 `Regner med at kunne spare op i de kommende 12 måneder` +
                 `Familiens økonomiske situation lige nu  kan spare penge slår til  bruger mere end man tjener`,
               data = Forbrugsdata, scale = TRUE, validation = "CV")
summary(pcr.fit)

loading.pcr<- pcr.fit$loadings[1:12]^2
print(loading.pcr)
print(sum(loading.pcr)) # tjekker om det går op i 100%

#### Opgave 2.2 – De vigtigste indikatorer ####
# Hvilke 3 spørgsmål i forbrugerforventningsundersøgelsen har de højeste vægte? 
# Diskutér om spørgsmålene giver analytisk mening.
loadings_df <- as.data.frame(round(loading.pcr,3))
loadings_df <- loadings_df[order(loadings_df[, 1], decreasing = TRUE), , drop = FALSE]
rownames(loadings_df) <- colnames(Forbrugsdata[4:15])
colnames(loadings_df) <- "vægtning"
head(loadings_df, 3)
#                                                                        vægtning
# Familiens økonomiske situation i dag  sammenlignet med for et år siden    0.142
# Familiens økonomiske  situation om et år  sammenlignet med i dag          0.140
# Danmarks økonomiske situation i dag  sammenlignet med for et år siden     0.121

# Tja sikkert

##### PCA i hånden #####


#### Opgave 2.3 Forudsig forbruget ####
# Med afsæt i jeres vægte fra opgave 2.1 skal I beregne den årlige realvækst i husholdningernes
# forbrugsudgift for 3. kvartal 2023. Hvad er årsagen til I ikke helt tror på jeres beregnede årlige
# realvækst i husholdningernes forbrugsudgift for 3. kvartal 2023? Hvad skyldes det underlige
# resultat?

newestQ <- FORV1_Q[100, 3:14, drop = FALSE] # Drop 

newest_predict <- predict(pcr.fit, newdata = newestQ, ncomp = 1)

print(newest_predict)
# Årlig_vækst
# -0.1922038

# Ikke underligt
# dobbelt tjek 2.3
pls.fit <- plsr(Årlig_vækst~
                 `Familiens økonomiske situation i dag  sammenlignet med for et år siden` +
                 `Familiens økonomiske  situation om et år  sammenlignet med i dag` +
                 `Danmarks økonomiske situation i dag  sammenlignet med for et år siden` +
                 `Danmarks økonomiske situation om et år  sammenlignet med i dag` +
                 `Anskaffelse af større forbrugsgoder  fordelagtigt for øjeblikket` +
                 `Priser i dag  sammenlignet med for et år siden` +
                 `Priser om et år  sammenlignet med i dag` +
                 `Arbejdsløsheden om et år  sammenlignet med i dag` +
                 `Anskaffelse af større forbrugsgoder  inden for de næste 12 mdr ` +
                 `Anser det som fornuftigt at spare op i den nuværende økonomiske situation` +
                 `Regner med at kunne spare op i de kommende 12 måneder` +
                 `Familiens økonomiske situation lige nu  kan spare penge slår til  bruger mere end man tjener`,
               data = Forbrugsdata, scale = TRUE, validation = "CV")
summary(pls.fit)

loading.pls<- pls.fit$loadings[1:12]^2
loadings_pls_df <- as.data.frame(round(loading.pls,3))
loadings_pls_df <- loadings_pls_df[order(loadings_pls_df[, 1], decreasing = TRUE), , drop = FALSE]
rownames(loadings_pls_df) <- colnames(Forbrugsdata[4:15])
colnames(loadings_pls_df) <- "vægtning"
head(loadings_pls_df, 3)
newest_predict_pls <- predict(pls.fit, newdata = newestQ, ncomp = 1)

print(newest_predict_pls)
# Årlig_vækst
# 0.07934329



#### Opgave 2.4 – Forudsigelser fra DI ####
# Hvad forventede DI den årlige realvækst i 
# husholdningernes forbrugsudgift er i 2016? 
# (se artiklen: ”Forbruget fortsætter fremgangen i 2016”) 
# Hvad endte den årlige realvækst i husholdningernesforbrugsudgift 
# med reelt at være?

# Kan bare tage fra skriftlige




                 
                

