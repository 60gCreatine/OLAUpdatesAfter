library(ggplot2)
library(dkstat)
library(corrplot)
library(dplyr)

#################################################################################################
###Opgave 2 - Forbrugertillidsindikatorer og fremtidig vækst i husholdningernes forbrugsudgift###
#################################################################################################

#################
###Opgave 2.1 ###
#################

############################
###FUNKTIONER TIL DST API###
############################

#dst_search() This function makes it possible to search through the different tables for a word or a phrase.
#dst_tables() This function downloads all the possible tables available.
#dst_meta() This function lets you download the meta data for a specific table, so you can see the description, unit, variables and values you can download data for.
#dst_get_data() lets you download the actual data you wan't.

##############################
####Forbrugerforventninger####
##############################

dst_search(string = "Forbrugerforventning", field = "text")

Tabeller <- (dst_get_tables(lang = "da"))

FORV <- dst_meta(table = "FORV1",lang = "da")


FORV_filter <- list(INDIKATOR ="*",
                    Tid = "*")

FORV1 <- dst_get_data(table = "FORV1", query = FORV_filter, lang = "da")

Forventning <- reshape(FORV1, idvar = "TID", timevar = "INDIKATOR", direction = "wide")

colnames(Forventning) <- gsub("value.", "", colnames(Forventning))

# Opret en ny kolonne med kvartalskoder
Forventning$Kvartal <- paste0(format(Forventning$TID, "%Y"), 
                              "Q", 
                              ceiling(as.numeric(format(Forventning$TID, "%m")) / 3))

# Opret en ny dataframe til kvartalsdata
Kvartalsdata <- data.frame()

# Få listen over unikke kvartaler
unikke_kvartaler <- unique(Forventning$Kvartal)

# Brug et loop til at beregne gennemsnit for hver kvartal
for (kvartal in unikke_kvartaler) {
  # Filtrer data for det aktuelle kvartal
  kvartal_data <- Forventning[Forventning$Kvartal == kvartal, ]
  
  # Beregn gennemsnit for hver kolonne og rund af til 2 decimaler (ekskl. TID og Kvartal)
  gennemsnit <- round(colMeans(kvartal_data[, -c(1, ncol(Forventning))], na.rm = TRUE), 2)
  
  # Opret en midlertidig dataframe til det aktuelle kvartal
  midlertidig_df <- data.frame(Kvartal = kvartal, t(gennemsnit))
  
  # Tilføj til kvartalsdata
  Kvartalsdata <- rbind(Kvartalsdata, midlertidig_df)
}

Forventning <- Kvartalsdata

Forventning_colnames <- c("Kvartal", "Forbrugertillidsindikatoren", 
                          "Familiens økonomiske situation i dag, sammenlignet med for et år siden",
                          "Familiens økonomiske  situation om et år, sammenlignet med i dag", 
                          "Danmarks økonomiske situation i dag, sammenlignet med for et år siden",
                          "Danmarks økonomiske situation om et år, sammenlignet med i dag",
                          "Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket",
                          "Priser i dag, sammenlignet med for et år siden", 
                          "Priser om et år, sammenlignet med i dag",
                          "Arbejdsløsheden om et år, sammenlignet med i dag", 
                          "Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.",
                          "Anser det som fornuftigt at spare op i den nuværende økonomiske situation",
                          "Regner med at kunne spare op i de kommende 12 måneder",
                          "Familiens økonomiske situation lige nu: kan spare/penge slår til/ bruger mere end man tjener")

colnames(Forventning) <- Forventning_colnames

Forventning$DST <- round(rowMeans(Forventning[c("Familiens økonomiske situation i dag, sammenlignet med for et år siden",
                                                "Familiens økonomiske  situation om et år, sammenlignet med i dag",
                                                "Danmarks økonomiske situation i dag, sammenlignet med for et år siden",
                                                "Danmarks økonomiske situation om et år, sammenlignet med i dag",
                                                "Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket")]),1)

Forventning$DI <- round(rowMeans(Forventning[c("Familiens økonomiske situation i dag, sammenlignet med for et år siden",
                                               "Danmarks økonomiske situation i dag, sammenlignet med for et år siden",
                                               "Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket",
                                               "Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.")]),1)

Forbrugertillid <- Forventning[-c(1:101,201),c(15:16)]

rownames(Forbrugertillid) <- NULL

#####################
####Privatforbrug####
#####################

NKHC021 <- dst_meta("NKHC021", lang = "da")

NKHC021_filter <- list(
  FORMAAAL = "I alt",
  PRISENHED = "2020-priser, kædede værdier",
  SÆSON = "Sæsonkorrigeret",
  Tid = "*"
)

Forbrug <- dst_get_data(table = "NKHC021", query = NKHC021_filter, lang = "da")

# Fjern unødvendige kolonner og omdøb
Forbrug <- Forbrug[, -c(1:3)]
colnames(Forbrug) <- c("Kvartal", "Forbrug")

# Beregning af kvartalvis årlig realvækst fx Hvis vi er i Q1 2024, sammenligner den med Q1 2023 eller Q2 2024, sammenligner den med Q2 2023
Forbrug$Realvaekst <- round(c(rep(0, 4), 
                              (Forbrug$Forbrug[5:length(Forbrug$Forbrug)] / 
                                 Forbrug$Forbrug[1:(length(Forbrug$Forbrug) - 4)] - 1) * 100), 2)


Forbrug1 <- Forbrug[-c(1:40), -2]
rownames(Forbrug1) <- NULL

#####################################
####Privatforbrug & Forventninger####
#####################################

Forbrug_endelig <- cbind(Forbrug1, Forbrugertillid)


ggplot(Forbrug_endelig, aes(x = Kvartal)) +
  geom_bar(aes(y = Realvaekst, fill = "Realvaekst"), stat = "identity", position = "dodge") +
  geom_line(aes(y = DST, color = "DST"), linewidth = 1) +
  geom_line(aes(y = DI, color = "DI"), linewidth = 1) +
  geom_point(aes(y = DST, color = "DST"), size = 3, shape = 16) +
  geom_point(aes(y = DI, color = "DI"), size = 3, shape = 16) +
  scale_y_continuous(
    name = "Forbrugertillidsindikator (Nettotal)",
    breaks = seq(-45, 45, by = 8),  # Yderligere udvidet område
    limits = c(-45, 45),            # Yderligere udvidet område
    sec.axis = sec_axis(~ . * (8/45), # Justeret konverteringsfaktor
                        name = "Realvækst (pct.)",
                        breaks = seq(-8, 8, by = 3))
  ) +
  scale_x_datetime(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  scale_fill_manual(values = c("Realvaekst" = "red")) +
  scale_color_manual(values = c("DST" = "blue", "DI" = "green")) +
  labs(
    title = "DI Forbrugertillidsindikator følger tættere realvæksten end DST Forbrugertillidsindikator",
    x = "Kvartal",
    fill = "Privatforbrug",
    color = "Forbrugertillidsindikator"
  ) +
  theme_minimal() +
  theme(
    axis.title.y.right = element_text(color = "black"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(size = 16, face = "bold")  # Større og fed overskrift
  )

##########################
####Korrelationsmatrix####
##########################

# Select kun numeriske kolonner og lav korrelation
Forbrug_corr <- cor(Forbrug_endelig[, c("Realvaekst", "DST", "DI")])

corrplot(Forbrug_corr, method = "circle", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black",
         col = colorRampPalette(c("#6D9EC1", "white", "#E46726"))(200))


##########################
####Lineære regression####
##########################

#1) Realvaekst vs DST
ForbrugRegSimple <- lm(Realvaekst ~ `DST`, data = Forbrug_endelig)
summary(ForbrugRegSimple)

ggplot(ForbrugRegSimple, aes(x = `DST`, y = Realvaekst)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Realvaekst vs. DST",
       x = "DST",
       y = "Realvaekst")

#2) Realvaekst vs DI
ForbrugRegSimple2 <- lm(Realvaekst ~ `DI`, data = Forbrug_endelig)
summary(ForbrugRegSimple2)

ggplot(ForbrugRegSimple2, aes(x = `DI`, y = Realvaekst)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Realvaekst vs. DI",
       x = "DI",
       y = "Realvaekst")

#################
###Opgave 2.2 ###
#################

#Beregn/forudsig den årlige realvækst i husholdningernes forbrugsudgift for 4. kvartal 2024 med
#henholdsvis DI’s forbrugertillidsindikator og forbrugertillidsindikatoren fra DST.

##############################
####Forudsigelse af 2024Q4####
##############################

# Formel for at beregne/forudse y (afhængig variabel) y =𝐵0 +𝐵1⋅𝑋1
# I vores tilfælde er y realvæksten.Lad et summary af den lineære regression for at finde de nødvendige værdier
#𝐵0 er estimated Intercept af y variablen i summary - Teoretisk betyder det Skæringspunktet (intercept), som er værdien af y når X1 er 0.
#𝐵1 er estimated af den uafhængige variabel (x) Teoretisk betyder det hældningen (slope), som angiver, hvordan y ændrer sig med en ændring i X1
#𝑋1 Den uafhængige variabel, som påvirker y. I denne opgave er X1 (den uafhængige variabel) DI og DST forbrugertillidsindikator

## Forudsigelse af realvæksten for 4. kvartal 2024

# DST forudsigelse for Q4 2024
# y = B0 + B1 * X1
DST_2024Q4 <- 1.41983 + 0.17480 * (-9.1)
# Med predict()
FTI2024Q4_DST <- data.frame(DST = -9.1)
Predict_DST <- predict(ForbrugRegSimple, newdata = FTI2024Q4_DST)

# DI forudsigelse for Q4 2024
# y = B0 + B1 * X1
DI_2024Q4 <- 2.25719 + 0.18086 * (-10.3)
# Med predict()
FTI2024Q4_DI <- data.frame(DI = -10.3)
Predict_DI <- predict(ForbrugRegSimple2, newdata = FTI2024Q4_DI)

########################################################################################
###Opgave 3 – Logistik regression, husholdningernes forbrugsudgift og forbrugertillid###
########################################################################################

################
###Opgave 3.1###
################

Forbrugertillid2 <- Forventning[-c(1:101,201),c(3:14)]

Forbrug2 <- Forbrug[-c(1:40),-2]

Forbrug_logi <- cbind(Forbrugertillid2,Forbrug2)

rownames(Forbrug_logi) <- NULL

Forbrug_logi$Realretning <- ifelse(Forbrug_logi$Realvaekst >= 0, "Op", "Ned")

table(Forbrug_logi$Realretning) # Svar: 22 ned og 77 op


# Lav diagrammer for "Op" og "Ned" underspørgsmålene i Forbrugerforventningsundersøgelse.

Forbrug_logi$Realretning <- as.factor(Forbrug_logi$Realretning)

Realvaekst_Op <- subset(Forbrug_logi, Realretning == "Op")
Realvaekst_Ned <- subset(Forbrug_logi, Realretning == "Ned")

Mean_Op <- colMeans(Realvaekst_Op[, sapply(Realvaekst_Op, is.numeric)])

Mean_Ned <- colMeans(Realvaekst_Ned[, sapply(Realvaekst_Ned, is.numeric)])

Mean_combined <- rbind(Op = Mean_Op, Ned = Mean_Ned)

Mean_combined <- as.data.frame(Mean_combined)

Mean_combined$Kategori <- rownames(Mean_combined)

Mean_combined <- Mean_combined[,-13]

Mean_long <- pivot_longer(Mean_combined, 
                          cols = -Kategori, 
                          names_to = "Variabel", 
                          values_to = "Gennemsnit")

ggplot(Mean_long, aes(x = Kategori, y = Gennemsnit, fill = Kategori)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Gennemsnit for spørgsmålene i FTI (Op vs Ned)", 
       x = "Realretning (Op/Ned)", 
       y = "Gennemsnit") +
  facet_wrap(~ Variabel, scales = "free_y") +  # Særskilte diagrammer for hver variabel
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

################
###Opgave 3.2###
################

# OBS! Husk at omdanne dummy variabel til en factor, før der laves en logistisk variabel

ForbrugGlm <- glm(Realretning ~ `Familiens økonomiske situation i dag, sammenlignet med for et år siden` + 
                    `Danmarks økonomiske situation i dag, sammenlignet med for et år siden` + 
                    `Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket` + 
                    `Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.`,
                  data = Forbrug_logi, family = binomial)
summary(ForbrugGlm)


# Lav summary af glm(), find tjek signifikansniveauet af x variabler. De insignifikante variabler fjernes og ny glm().
# Laves med de signifikante variabler. Kør herefter summary igen og repetér processen indtil, at der kan er Signifikante variabler tilbage.

ForbrugGlm2 <- glm(Realretning ~ `Danmarks økonomiske situation i dag, sammenlignet med for et år siden` + 
                     `Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`, data = Forbrug_logi, family = binomial)
summary(ForbrugGlm2)

ForbrugGlm3 <- glm(Realretning ~ `Danmarks økonomiske situation i dag, sammenlignet med for et år siden`,
                   data = Forbrug_logi, family = binomial)

summary(ForbrugGlm3)

Forbrug_corrGlm <- Forbrug_logi[,c(1,3,5,9)]

Kategori_corrGlm <- c("Fam","DK","AnskafNu","Anskaf12")

colnames(Forbrug_corrGlm) <- Kategori_corrGlm

Forbrug_corrGlm <- cor(Forbrug_corrGlm)

corrplot(Forbrug_corrGlm, method = "number",
         tl.cex = 1.0,               
         col = "black",              
         addgrid.col = "black",      
         is.corr = TRUE)


ForbrugGlm_Forudsigelse <- predict(ForbrugGlm3 ,type="response")

###########################################################################################################
GLM2024Q4_DK <- data.frame("Danmarks økonomiske situation i dag, sammenlignet med for et år siden" = -11.15)
colnames(GLM2024Q4_DK) <- "Danmarks økonomiske situation i dag, sammenlignet med for et år siden"
Predict_DK <- predict(ForbrugGlm3, newdata = GLM2024Q4_DK, type = "response")

coef(ForbrugGlm3)
DK_2024Q4 <- 1 / (1 + exp(-(2.03518591 + 0.08398631 * (-11.15)))) 

## Svar: Der er 72,86 % sandsynlighed for at forbruget går op (stiger) i 2024Q4



################
###Opgave 3.3###
################

## Konfusion matrix vha. funktion
# Først binariser forudsigelser baseret på 50 % threshold
# ForbrugGlm_Forudsigelse er forudsigelserne beregnet i opgave 3.2, se ovenfor.

Threshold <- 0.5 # grænseværdi på 50 % for forudsigelsen

ForbrugGlm_ForudsigelseBin <- ifelse(ForbrugGlm_Forudsigelse > Threshold, "Op", "Ned")

Konfusion_matrix <- table(Predicted = ForbrugGlm_ForudsigelseBin, Actual = Forbrug_logi$Realretning)

print(Konfusion_matrix)

## Konfusion matrix manuel udarbejdet

Forbrug_logi$Predicted <- ifelse(ForbrugGlm_Forudsigelse > 0.5, "Op", "Ned")

True_positive <- sum(Forbrug_logi$Predicted == "Op" & Forbrug_logi$Realretning == "Op")
True_negative <- sum(Forbrug_logi$Predicted == "Op" & Forbrug_logi$Realretning == "Ned")
False_negative <- sum(Forbrug_logi$Predicted == "Ned" & Forbrug_logi$Realretning == "Ned")
False_positive <- sum(Forbrug_logi$Predicted == "Ned" & Forbrug_logi$Realretning == "Op")


Konfusion_matrix2 <- matrix(c(True_negative, False_positive,    
                              False_negative, True_positive),     
                            nrow = 2, byrow = TRUE,
                            dimnames = list(Predicted = c('Ned', 'Op'),
                                            Actual = c('Ned', 'Op')))
print(Konfusion_matrix2)

ROC_kurve <- roc(Forbrug_logi$Realretning, ForbrugGlm_Forudsigelse,
                 levels = c("Ned", "Op"), direction = "<")

plot(ROC_kurve, col = "blue", lwd = 2)


################
###Opgave 3.4###
################

## Scenarie 1 - Brug anden signifikant x-variabel som udgangspunkt i glm()

ForbrugGlm3 <- glm(Realretning ~ `Anskaffelse af større forbrugsgoder, inden for de næste 12 mdr.`,
                   data = Forbrug_logi, family = binomial)

summary(ForbrugGlm3)

ForbrugGlm_Forudsigelse2 <- predict(ForbrugGlm3 ,type="response")

ForbrugGlm_ForudsigelseBin2 <- ifelse(ForbrugGlm_Forudsigelse2 > 0.5, "Op", "Ned")

Konfusion_matrix3 <- table(Predicted = ForbrugGlm_ForudsigelseBin2, Actual = Forbrug_logi$Realretning)

print(Konfusion_matrix3)

ROC_kurve2 <- roc(Forbrug_logi$Realretning, ForbrugGlm_Forudsigelse2,
                  levels = c("Ned", "Op"), direction = "<")

plot(ROC_kurve2, col = "blue", lwd = 2)

## Scenarie 2 - ændre threshold fra 50 % til 40 % 

Threshold2 <- 0.40 # grænseværdi på 50 % for forudsigelsen

ForbrugGlm_ForudsigelseBin3 <- ifelse(ForbrugGlm_Forudsigelse > Threshold2, "Op", "Ned")

Konfusion_matrix4 <- table(Predicted = ForbrugGlm_ForudsigelseBin3, Actual = Forbrug_logi$Realretning)

print(Konfusion_matrix4)

##########################################################################################
###Opgave 4 – Forbrug og forbrugertillidsindikatorer fra DST og DI, samt loops i lister###
##########################################################################################

################
###Opgave 4.1###
################

Forbrugertillid3 <- Forventning[-c(1:85),-c(2:14)]
Forbrugertillid3$Kvartal <- as.Date(paste0(substr(Forbrugertillid3$Kvartal, 1, 4), "-",
                                           c("01", "04", "07", "10")[as.integer(substr(Forbrugertillid3$Kvartal, 6, 6))], "-01"))

ggplot(Forbrugertillid3, aes(x = Kvartal)) +
  geom_line(aes(y = DST, color = "DST"), linewidth = 1) + 
  geom_line(aes(y = DI, color = "DI"), linewidth = 1) + 
  geom_point(aes(y = DST, color = "DST"), size = 3, shape = 16) +
  geom_point(aes(y = DI, color = "DI"), size = 3, shape = 16) +
  scale_y_continuous(
    name = "Forbrugertillidsindikator (Nettotal)",
    breaks = seq(-40, 40, by = 10)
  ) +
  scale_x_date(
    date_breaks = "year",
    date_labels = "%Y"
  ) +
  scale_color_manual(values = c("DST" = "blue", "DI" = "green")) +
  labs(
    title = "Forbrugertilliden Rammer Historisk Lavpunkt i Fjerde Kvartal af 2022",
    x = "Kvartal", 
    color = "Forbrugertillidsindikator"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, hjust = 1)
  )

################
###Opgave 4.2###
################

#Beregn gennemsnittet for underspørgsmålet ”Set i lyset af den økonomiske situation, mener du, at
#det for øjeblikket er fordelagtigt at anskaffe større forbrugsgoder som fjernsyn, vaskemaskine eller
#lignende, eller er det bedre at vente?” for perioden 1. kvartal 2000 til og med 3. kvartal 2024.
#Vurdér jeres resultat set i forhold til spørgsmålet og svarmulighederne. (Hint: giver resultatet analytisk mening?)

Anskaf <- Forventning[-c(1:101),c(7,1)]

rownames(Anskaf) <- NULL

mean(Anskaf$`Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`)

Anskaf_sd <- sd(Anskaf$`Anskaffelse af større forbrugsgoder, fordelagtigt for øjeblikket`) #sd() beregner standardafvigelsen

################
###Opgave 4.3###
################

FORB1 <- dst_meta(table = "NKHC021",lang = "da")

FORB_filter1 <- list(FORMAAAL = "*",
                     PRISENHED = "2020-priser, kædede værdier",
                     SÆSON = "Sæsonkorrigeret",
                     Tid = "*")

Hus_Forb <- dst_get_data(table = "NKHC021", query = FORB_filter1, lang = "da")

Hus_Forb <- Hus_Forb[,-c(2:3)]

Hus_Forb <- reshape(Hus_Forb, idvar = "TID", timevar = "FORMAAAL", direction = "wide")

# Fjern 'value.CP' og efterfølgende bogstav fra alle kolonner undtagen 'TID'
colnames(Hus_Forb)[-1] <- gsub("^value\\.CP[A-Z]\\s", "", colnames(Hus_Forb)[-1])

Hus_Forbrug <- Hus_Forb[c(121:139),-2]

rownames(Hus_Forbrug) <- NULL

# Funktionen grepl("2023", TID) returnerer TRUE for alle rækker, hvor '2023' er en del af værdien i kolonnen TID 
# (f.eks. '2023-01-01', '2023-04-01', osv.), og FALSE for alle andre.

Hus_Forbrug2023 <- subset(Hus_Forbrug, grepl("2023", TID))

Hus_Forbrug2023 <- round(colMeans(Hus_Forbrug2023[, 2:16]),0)

Hus_Forbrug2023Max <- names(Hus_Forbrug2023)[which.max(Hus_Forbrug2023)]
Hus_Forbrug2023Value <- max(Hus_Forbrug2023)

cat("Danskerne brugte flest penge på:", Hus_Forbrug2023Max, "med et gennemsnitligt forbrug på", Hus_Forbrug2023Value, "i 2023.")

Hus_Forbrug2 <- round(((Hus_Forbrug[Hus_Forbrug$TID == "2024-07-01", 2:16] - 
                          Hus_Forbrug[Hus_Forbrug$TID == "2020-01-01", 2:16]) /
                         Hus_Forbrug[Hus_Forbrug$TID == "2020-01-01", 2:16]) * 100, 0)

Hus_Forbrug2Max <- names(Hus_Forbrug2)[which.max(Hus_Forbrug2)]
Hus_Forbrug2Value <- max(Hus_Forbrug2)    

cat("Forbruget steg mest for:", Hus_Forbrug2Max, "med en stigning på", Hus_Forbrug2Value, "% fra første kvartal 2020 til 3. kvartal 2024.")

################
###Opgave 4.4###
################

Forbrugertillid4 <- Forventning[-c(1:101,201),c(15:16)]

Hus_Forb1 <- Hus_Forb[-c(1:40),-2]

Hus_Forbrug3 <- cbind(Hus_Forb1, Forbrugertillid4)

rownames(Hus_Forbrug3) <- NULL


HusRegSimple1 <- lm(`Fødevarer mv.` ~ `DST`, data = Hus_Forbrug3)

ggplot(HusRegSimple1, aes(x = `DST`, y = `Fødevarer mv.`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Fødevarer vs. DST",
       x = "DST",
       y = "Fødevarer mv.")

HusRegSimple2 <- lm(`Drikkevarer og tobak mv.` ~ `DST`, data = Hus_Forbrug3)

ggplot(HusRegSimple2, aes(x = `DST`, y = `Drikkevarer og tobak mv.`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Drikkevarer og tobak vs. DST",
       x = "DST",
       y = "Drikkevarer og tobak mv.")

HusRegSimple3 <- lm(`Beklædning og fodtøj` ~ `DST`, data = Hus_Forbrug3)

ggplot(HusRegSimple3, aes(x = `DST`, y = `Beklædning og fodtøj`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Beklædning og fodtøj vs. DST",
       x = "DST",
       y = "Beklædning og fodtøj")

HusRegSimple4 <- lm(`Boligbenyttelse` ~ `DST`, data = Hus_Forbrug3)

ggplot(HusRegSimple4, aes(x = `DST`, y = `Boligbenyttelse`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Boligbenyttelse vs. DST",
       x = "DST",
       y = "Boligbenyttelse")

HusRegSimple5 <- lm(`Elektricitet, fjernvarme og andet brændsel` ~ `DST`, data = Hus_Forbrug3)

ggplot(HusRegSimple5, aes(x = `DST`, y = `Elektricitet, fjernvarme og andet brændsel`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Elektricitet, fjernvarme og andet brændsel vs. DST",
       x = "DST",
       y = "Elektricitet, fjernvarme og andet brændsel")

HusRegSimple6 <- lm(`Boligudstyr, husholdningstjenester mv.` ~ `DST`, data = Hus_Forbrug3)

ggplot(HusRegSimple6, aes(x = `DST`, y = `Boligudstyr, husholdningstjenester mv.`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Boligudstyr, husholdningstjenester mv. vs. DST",
       x = "DST",
       y = "Boligudstyr, husholdningstjenester mv.")

HusRegSimple7 <- lm(`Medicin, lægeudgifter o.l.` ~ `DST`, data = Hus_Forbrug3)

ggplot(HusRegSimple7, aes(x = `DST`, y = `Medicin, lægeudgifter o.l.`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Medicin, lægeudgifter o.l. vs. DST",
       x = "DST",
       y = "Medicin, lægeudgifter o.l.")

HusRegSimple8 <- lm(`Køb af køretøjer` ~ `DST`, data = Hus_Forbrug3)

ggplot(HusRegSimple8, aes(x = `DST`, y = `Køb af køretøjer`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Køb af køretøjer vs. DST",
       x = "DST",
       y = "Køb af køretøjer")

HusRegSimple9 <- lm(`Drift af køretøjer og transporttjenester` ~ `DST`, data = Hus_Forbrug3)

ggplot(HusRegSimple9, aes(x = `DST`, y = `Drift af køretøjer og transporttjenester`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Drift af køretøjer og transporttjenester vs. DST",
       x = "DST",
       y = "Drift af køretøjer og transporttjenester")

HusRegSimple10 <- lm(`Information og kommunikation` ~ `DST`, data = Hus_Forbrug3)

ggplot(HusRegSimple10, aes(x = `DST`, y = `Information og kommunikation`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Information og kommunikation vs. DST",
       x = "DST",
       y = "Information og kommunikation")

HusRegSimple11 <- lm(`Fritid, sport og kultur` ~ `DST`, data = Hus_Forbrug3)

ggplot(HusRegSimple11, aes(x = `DST`, y = `Fritid, sport og kultur`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Fritid, sport og kultur vs. DST",
       x = "DST",
       y = "Fritid, sport og kultur")

HusRegSimple12 <- lm(`Undervisning` ~ `DST`, data = Hus_Forbrug3)

ggplot(HusRegSimple12, aes(x = `DST`, y = `Undervisning`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Undervisning vs. DST",
       x = "DST",
       y = "Undervisning")

HusRegSimple13 <- lm(`Restauranter og hoteller` ~ `DST`, data = Hus_Forbrug3)

ggplot(HusRegSimple13, aes(x = `DST`, y = `Restauranter og hoteller`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Restauranter og hoteller vs. DST",
       x = "DST",
       y = "Restauranter og hoteller")

HusRegSimple14 <- lm(`Forsikring og finansielle tjenester` ~ `DST`, data = Hus_Forbrug3)

ggplot(HusRegSimple14, aes(x = `DST`, y = `Forsikring og finansielle tjenester`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Forsikring og finansielle tjenester vs. DST",
       x = "DST",
       y = "Forsikring og finansielle tjenester")

HusRegSimple15 <- lm(`Andre varer og tjenester` ~ `DST`, data = Hus_Forbrug3)

ggplot(HusRegSimple15, aes(x = `DST`, y = `Andre varer og tjenester`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Andre varer og tjenester vs. DST",
       x = "DST",
       y = "Andre varer og tjenester")

HusRegSimple16 <- lm(`Fødevarer mv.` ~ `DI`, data = Hus_Forbrug3)

ggplot(HusRegSimple16, aes(x = `DI`, y = `Fødevarer mv.`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Fødevarer mv. vs. DI",
       x = "DI",
       y = "Fødevarer mv.")

HusRegSimple17 <- lm(`Drikkevarer og tobak mv.` ~ `DI`, data = Hus_Forbrug3)

ggplot(HusRegSimple17, aes(x = `DI`, y = `Drikkevarer og tobak mv.`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Drikkevarer og tobak mv. vs. DI",
       x = "DI",
       y = "Drikkevarer og tobak mv.")

HusRegSimple18 <- lm(`Beklædning og fodtøj` ~ `DI`, data = Hus_Forbrug3)

ggplot(HusRegSimple18, aes(x = `DI`, y = `Beklædning og fodtøj`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Beklædning og fodtøj vs. DI",
       x = "DI",
       y = "Beklædning og fodtøj")

HusRegSimple19 <- lm(`Boligbenyttelse` ~ `DI`, data = Hus_Forbrug3)

ggplot(HusRegSimple19, aes(x = `DI`, y = `Boligbenyttelse`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Boligbenyttelse vs. DI",
       x = "DI",
       y = "Boligbenyttelse")

HusRegSimple20 <- lm(`Elektricitet, fjernvarme og andet brændsel` ~ `DI`, data = Hus_Forbrug3)

ggplot(HusRegSimple20, aes(x = `DI`, y = `Elektricitet, fjernvarme og andet brændsel`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Elektricitet, fjernvarme og andet brændsel vs. DI",
       x = "DI",
       y = "Elektricitet, fjernvarme og andet brændsel")

HusRegSimple21 <- lm(`Boligudstyr, husholdningstjenester mv.` ~ `DI`, data = Hus_Forbrug3)

ggplot(HusRegSimple21, aes(x = `DI`, y = `Boligudstyr, husholdningstjenester mv.`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Boligudstyr, husholdningstjenester mv. vs. DI",
       x = "DI",
       y = "Boligudstyr, husholdningstjenester mv.")

HusRegSimple22 <- lm(`Medicin, lægeudgifter o.l.` ~ `DI`, data = Hus_Forbrug3)

ggplot(HusRegSimple22, aes(x = `DI`, y = `Medicin, lægeudgifter o.l.`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Medicin, lægeudgifter o.l. vs. DI",
       x = "DI",
       y = "Medicin, lægeudgifter o.l.")

HusRegSimple23 <- lm(`Køb af køretøjer` ~ `DI`, data = Hus_Forbrug3)

ggplot(HusRegSimple23, aes(x = `DI`, y = `Køb af køretøjer`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Køb af køretøjer vs. DI",
       x = "DI",
       y = "Køb af køretøjer")

HusRegSimple24 <- lm(`Drift af køretøjer og transporttjenester` ~ `DI`, data = Hus_Forbrug3)

ggplot(HusRegSimple24, aes(x = `DI`, y = `Drift af køretøjer og transporttjenester`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Drift af køretøjer og transporttjenester vs. DI",
       x = "DI",
       y = "Drift af køretøjer og transporttjenester")

HusRegSimple25 <- lm(`Information og kommunikation` ~ `DI`, data = Hus_Forbrug3)

ggplot(HusRegSimple25, aes(x = `DI`, y = `Information og kommunikation`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Information og kommunikation vs. DI",
       x = "DI",
       y = "Information og kommunikation")

HusRegSimple26 <- lm(`Fritid, sport og kultur` ~ `DI`, data = Hus_Forbrug3)

ggplot(HusRegSimple26, aes(x = `DI`, y = `Fritid, sport og kultur`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Fritid, sport og kultur vs. DI",
       x = "DI",
       y = "Fritid, sport og kultur")

HusRegSimple27 <- lm(`Undervisning` ~ `DI`, data = Hus_Forbrug3)

ggplot(HusRegSimple27, aes(x = `DI`, y = `Undervisning`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Undervisning vs. DI",
       x = "DI",
       y = "Undervisning")

HusRegSimple28 <- lm(`Restauranter og hoteller` ~ `DI`, data = Hus_Forbrug3)

ggplot(HusRegSimple28, aes(x = `DI`, y = `Restauranter og hoteller`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Restauranter og hoteller vs. DI",
       x = "DI",
       y = "Restauranter og hoteller")

HusRegSimple29 <- lm(`Forsikring og finansielle tjenester` ~ `DI`, data = Hus_Forbrug3)

ggplot(HusRegSimple29, aes(x = `DI`, y = `Forsikring og finansielle tjenester`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Forsikring og finansielle tjenester vs. DI",
       x = "DI",
       y = "Forsikring og finansielle tjenester")

HusRegSimple30 <- lm(`Andre varer og tjenester` ~ `DI`, data = Hus_Forbrug3)

ggplot(HusRegSimple30, aes(x = `DI`, y = `Andre varer og tjenester`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Andre varer og tjenester vs. DI",
       x = "DI",
       y = "Andre varer og tjenester")

HusRegSimple_summaries <- list()

for (i in 1:30) {
  HusRegSimple_modeller <- paste0("HusRegSimple", i)
  HusRegSimple_summaries[[i]] <- summary(get(HusRegSimple_modeller))
}

# Navngiv listen for nem adgang
names(HusRegSimple_summaries) <- paste0("HusRegSimple", 1:30)

################################
###Opgave 5 – Eurostat og API###
################################

################
###Opgave 5.1###
################

#Henter alle tabeller i Eurostat
TabellerEU <- get_eurostat_toc()

#Søg efter specifikke datasæt
Expenditure <- search_eurostat_toc("expenditure")

#Hent datasæt
EU_exp <- get_eurostat_data("namq_10_gdp")

# Find Meta data (forklaring på datasættet)
Meta_exp <- get_eurostat_dsd("namq_10_gdp")

# clean_eurostat_cache() skal køres for at hente data på ny. Renser cache mappe.
eurostat::clean_eurostat_cache()

#Filtrering via Eurostat
EU_data <- get_eurostat("namq_10_gdp",
                        filters = list(
                          unit = "CLV15_MEUR",
                          s_adj = "SCA",
                          na_item = "P31_S14",
                          geo = c("BE","BG","CZ","DK","DE","EE","IE","EL","ES","FR",
                                  "HR","IT","CY","LV","LT","LU","HU","MT","NL","AT",
                                  "PL","PT","RO","SI","SK","FI","SE","IS","NO","CH",
                                  "UK","BA","ME","MK","AL","RS","TR")
                        ))

EU_data <- EU_data[,-c(1:4)]

EU_data <- na.omit(EU_data) #fjerner alle rækker, hvor der er NA værdier

# Opret en ny kolonne til realvækst
EU_data$realvaekst <- NA

# for() anvendes til beregning af kvartalsvis årlig realvækst
for (i in 5:nrow(EU_data)) {
  if (EU_data$geo[i] == EU_data$geo[i - 4]) {
    EU_data$realvaekst[i] <- round((EU_data$values[i] - EU_data$values[i - 4]) / EU_data$values[i - 4] * 100,2)
  }
}

Start_dato <- as.Date("2000-01-01")
Slut_dato <- as.Date("2024-06-30")

EU_lande <- c("DK", "BE", "NL", "SE", "AT", "DE", "FR", "IT", "ES")

# Filtrer landekoder og tidsperioden
ForbrugEU <- EU_data[EU_data$geo %in% EU_lande & 
                       EU_data$time >= Start_dato & 
                       EU_data$time <= Slut_dato, ]


Landeliste <- list()

for (land in unique(ForbrugEU$geo)) {
  Landeliste[[land]] <- ggplot(ForbrugEU[ForbrugEU$geo == land, ], aes(x = time, y = realvaekst)) +
    geom_line(size = 1, color = "blue") +
    geom_point(shape = 16, color = "blue") +
    geom_vline(xintercept = as.Date(c("2020-01-01", "2022-06-30")), linetype = "dashed", color = "red", size = 1) +
    geom_vline(xintercept = as.Date("2023-01-01"), linetype = "solid", color = "black", size = 1) +
    labs(
      title = paste("Realvækstens udvikling for husholdningernes forbrug i", land),
      x = "Tid (Kvartal)",
      y = "Realvækst (%)"
    ) +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_x_date(
      breaks = seq(as.Date("2000-01-01"), as.Date("2024-06-30"), by = "1 year"),
      date_labels = "%Y"
    )
}

################
###Opgave 5.2###
################

# Aggregate anvendes til at grupperer efter variablen "geo" (landene) og beregner gennemsnittet af realvækst for hver gruppe
MeanEU <- aggregate(realvaekst ~ geo, data = ForbrugEU, FUN = mean)

ggplot(MeanEU, aes(x = reorder(geo, realvaekst), y = realvaekst, fill = geo, color = geo)) +
  geom_bar(stat = "identity") + 
  labs(title = "Husholdningernes Forbrug Vækster Højst for Svenskerne",
       x = "",
       y = "Gennemsnitlig realvækst(%)") +
  theme_minimal() +
  geom_text(aes(label = round(realvaekst, 2)), vjust = -0.5)

################
###Opgave 5.3###
################

#fra første kvartal 2020 til første kvartal 2023

ForbrugCOVID <- EU_data

ForbrugCOVID <- ForbrugCOVID[!(ForbrugCOVID$time >= as.Date("2020-01-01") & ForbrugCOVID$time <= as.Date("2023-04-01")), ]

ForbrugCOVID$realvaekst <- NA

for (i in 5:nrow(ForbrugCOVID)) {
  if (ForbrugCOVID$geo[i] == ForbrugCOVID$geo[i - 4]) {
    ForbrugCOVID$realvaekst[i] <- round((ForbrugCOVID$values[i] - ForbrugCOVID$values[i - 4]) / ForbrugCOVID$values[i - 4] * 100,2)
  }
}

# Vektorene EU_lande, Start_dato og Slut_dato er defineret tidligere. Se linje 54-57 eller enviroment

ForbrugCOVID <- ForbrugCOVID[ForbrugCOVID$geo %in% EU_lande & 
                               ForbrugCOVID$time >= Start_dato & 
                               ForbrugCOVID$time <= Slut_dato, ]

MeanCOVID <- aggregate(realvaekst ~ geo, data = ForbrugCOVID, FUN = mean)

ggplot(MeanCOVID, aes(x = reorder(geo, realvaekst), y = realvaekst, fill = geo, color = geo)) +
  geom_bar(stat = "identity") + 
  labs(title = "Svenskernes Vækst er Upåvirket af Coronakrisen",
       x = "",
       y = "Gennemsnitlig realvækst(%)") +
  theme_minimal() +
  geom_text(aes(label = round(realvaekst, 2)), vjust = -0.5)

################
###Opgave 5.4###
################

ForbrugCOVID2 <- EU_data

EU_lande2 <- c("BE","BG","CZ","DK","DE","EE","IE","EL","ES","FR",
               "HR","IT","CY","LV","LT","LU","HU","MT","NL","AT",
               "PL","PT","RO","SI","SK","FI","SE","IS","NO","CH")

# Perioden er defineret som værende COVID-19 samt perioden med større volatilitet grundet eftervirkninger fra COVID-19
ForbrugCOVID2 <- ForbrugCOVID2[ForbrugCOVID2$geo %in% EU_lande2 &
                                 ForbrugCOVID2$time >= "2020-01-01" & 
                                 ForbrugCOVID2$time <= "2023-01-01", ]

ForbrugEU2 <- EU_data

# Vektorene Start_dato og Slut_dato er defineret tidligere. Se linje 54-55 eller enviroment
ForbrugEU2 <- ForbrugEU2[ForbrugEU2$geo %in% EU_lande2 & 
                           ForbrugEU2$time >= Start_dato & 
                           ForbrugEU2$time <= Slut_dato, ]

# Beregn gennemsnittet for realvæksten under COVID og hele perioden og derefter beregn differencen
MeanCOVID2 <- aggregate(realvaekst ~ geo, data = ForbrugCOVID2, FUN = mean)
MeanEU2 <- aggregate(realvaekst ~ geo, data = ForbrugEU2, FUN = mean)

MeanCOVID2$realdiff <- round((MeanCOVID2$realvaekst-MeanEU2$realvaekst),2)

Meandiff <- MeanCOVID2[which.min(MeanCOVID2$realdiff), ]

cat("Landet med den største gennemsnitlige kvartalsvise realvækstnedgang er:", Meandiff$geo, "\n")
cat("Den gennemsnitlige realvækst i dette land er:", min(Meandiff$realdiff), "%")

ggplot(MeanCOVID2, aes(x = reorder(geo, realdiff), y = realdiff, fill = realdiff)) +
  geom_bar(stat = "identity") +
  labs(title = "Stærkt opsving i Græsk Økonomi efter Pandemien",
       x = "Land",
       y = "Udvikling i realvækst (%)") +
  theme_minimal() +
  coord_flip() +
  scale_fill_gradient(low = "red", high = "skyblue") +
  geom_text(aes(label = round(realdiff, 2)), vjust = 0.5, hjust = -0.2)
