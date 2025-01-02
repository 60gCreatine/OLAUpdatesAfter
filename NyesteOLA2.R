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


samlet_plot <- ggplot(Indikatordf, aes(x = Tid)) +
  # Barplot for "Forbrug"
  geom_bar(aes(y = Forbrug, fill = "Årlig realvækst pr. kvartal i privatforbruget (højre akse)"), 
           stat = "identity", alpha = 0.6, width = 60) +
  
  # Linjer for skaleret "DST" og "DI"
  geom_line(aes(y = DST_scaled, color = "DST's forbrugertillidsindikator"), size = 1) +
  geom_line(aes(y = DI_scaled, color = "DI's forbrugertillidsindikator"), size = 1) +
  
  scale_y_continuous(
    name = "Nettotal",                       # Primær y-akse (DST og DI)
    breaks = seq(-20, 20, by = 5),           # Breaks for primær y-akse
    limits = c(-20, 20),                     # Grænser for primær y-akse
    sec.axis = sec_axis(~ .,                 # Sekundær y-akse (Forbrug)
                        name = "Pct.",
                        breaks = seq(-15, 15, by = 2))) +
  scale_fill_manual(values = c("lightblue"), name = "") +
  scale_color_manual(values = c("#7D55C7", "#0F78C8"), name = "") +
  labs(
    title = "DI's forbrugertillidsindikator holder sig mere negativt over perioden",
    x = "Tid",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "black"),
    axis.title.y.right = element_text(color = "black"),
    legend.position = "bottom",                  
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(size = 14, face = "bold")
  )
print(samlet_plot)

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
# Uden December tal i FORV1
  # Manuel_DI <- 2.15675 + (0.18122 * newdataDI$DI)
    # 0.2947145
# Med December tal i FORV1
  Manuel_DI <- lm_DI$coefficients[1] + (lm_DI$coefficients[2] * newdataDI$DI)
print(Manuel_DI)
# 0.1320084

newdataDST <- as.data.frame(FORV1_Q$Forbrugertillidsindikatoren[100])
colnames(newdataDST) <- "DST"
ForbrugPredict_DST <- predict(lm_DST, newdata = newdataDST)
print(ForbrugPredict_DST)
    # Manuel_DST <- 1.31708 + (0.17562 * newdataDST$DST)
      # -0.281062
# Med December tal i FORV1
    #                               1 = B_0                   2 = B_1
  Manuel_DST <- lm_DST$coefficients[1] + (lm_DST$coefficients[2] * newdataDST)
print(Manuel_DST)


#### Opgave 
# Nej

#### ####
# https://www.danskindustri.dk/arkiv/analyser/2024/5/di-prognose-tilbagegangen-i-beskaftigelsen-er-aflyst/



#### Opgave 3.1 – Feature Engineering, dummy variable ####
# Dan en dummy variable af den kvartalsvise årlige vækst i husholdningernes forbrugsudgift for
# perioden 1. kvartal 1998 til 2. kvartal 2021. Hvor ofte stiger den kvartalsvise årlige vækst i
# husholdningernes forbrugsudgift? Hvor ofte falder den kvartalsvise årlige vækst i husholdningernes
# forbrugsudgift?

# Her har vi valgt at tage udgangspunkt fra 2000Q1 - 2024Q3, for at denne en sammenhæng mellem de forskellige opgaver



Forbrugsdata$Retning <- as.factor(ifelse(Forbrugsdata$Årlig_vækst > 0, "Op", "Ned"))
table(Forbrugsdata$Retning)
# Ned    Op 
# 22     77 
# 22.22% 77.78%
# Klar 'Op' Bias

# Plot for op og ned over tidsperioden
ggplot(Forbrugsdata, aes(x = Tid, y = Årlig_vækst, fill = Retning)) +
  geom_col() +
  scale_fill_manual(values = c("Op" = "#87bc45", "Ned" = "#ea5545")) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  scale_x_date(date_breaks = "1 year", date_labels = "%y") + # kan bruge %Y for fulde årstal
  labs(
    title = "Der er en stor data-bias for 'Op'",
    x = "Årstal",
    y = "Årlig kvartals realvæksts %",
    fill = "Retning"
  ) +
  theme_minimal(base_size = 14)


#### Opgave 3.2 – Logistisk regression og forudsigelser ####
# Lav en logistik regression med dummy variable fra opgave 1.1 og de fire indikatorer i DI’s
# forbrugertillidsindikator. 
# Hvilken retning forudsiger jeres model, at den årlige vækst i
# husholdningernes forbrugsudgift, vil gå i 3. kvartal 2024?
FTIForbrug <- cbind(Forbrugsdata, Spørgsmål)

glm <- glm(Retning ~
             `Familiens økonomiske situation i dag  sammenlignet med for et år siden` +
             `Danmarks økonomiske situation i dag  sammenlignet med for et år siden` +
             `Anskaffelse af større forbrugsgoder  fordelagtigt for øjeblikket` +
             `Anskaffelse af større forbrugsgoder  inden for de næste 12 mdr `,
           ,data = FTIForbrug, family = binomial)
summary(glm)
# Klart multikollonaritet
library(corrplot)

DI_spm <- FTIForbrug[,c("Familiens økonomiske situation i dag  sammenlignet med for et år siden",
                        "Danmarks økonomiske situation i dag  sammenlignet med for et år siden", 
                        "Anskaffelse af større forbrugsgoder  fordelagtigt for øjeblikket",
                        "Anskaffelse af større forbrugsgoder  inden for de næste 12 mdr ")]
Spm_navn_forkortelse <- c("Fam ift. sidste år", "DK ift. sidste år", "Anskaffelse nu", "Anskaffelse næste 12 mrd.")
DI_cor <- DI_spm
colnames(DI_cor) <- Spm_navn_forkortelse
cor <- cor(DI_cor)
corrplot(cor, 
         method = "square",     # Farvede firkanter
         type = "lower",        # Kun nederste trekant
         diag = FALSE,          # Fjern diagonalen
         tl.col = "black",      # Labels i sort
         number.digits = 2,     # Antal decimaler
         addCoef.col = "black",) # Tilføj koefficienter i sort

# Stor korrelation mellem alle


glm_df <- data.frame()
for (spm in colnames(DI_spm)) {
  formel <- as.formula(paste("Retning ~ `",spm,"`", sep=""))
  tmp_glm <- summary(glm(formel,
                         data = FTIForbrug, family = binomial))
  tmp_df <- as.data.frame(tmp_glm$coefficients)
  tmp_df$`Pr(>|z|)` <- format(tmp_df$`Pr(>|z|)`, scientific = F) 
  glm_df <- rbind(tmp_df,glm_df)
  glm_df_sorted <- glm_df[!grepl("intercept", rownames(glm_df), ignore.case = TRUE), ] # fjerne alle intercepts
  glm_df_sorted <- glm_df_sorted[order(glm_df_sorted$`Pr(>|z|)`),]
}
print(head(glm_df,1))
#                                                                          Estimate Std. Error  z value         Pr(>|z|)
#`Danmarks økonomiske situation i dag  sammenlignet med for et år siden` 0.08397921 0.01877462 4.473018 0.00000771234003

glm_1spm <- glm(Retning~`Danmarks økonomiske situation i dag  sammenlignet med for et år siden`,
                data = FTIForbrug, family = binomial)
glm_1spm_sum <- summary(glm_1spm)
newest_data <- data.frame(FORV1_Q$`Danmarks økonomiske situation i dag  sammenlignet med for et år siden`[100])
colnames(newest_data) <- "Danmarks økonomiske situation i dag  sammenlignet med for et år siden"
Predict_glm <- predict(glm_1spm, newdata = newest_data, type = "response")
Predict_glm_manuelt <- round(as.numeric(1 / (1 + exp(-(glm_1spm_sum$coefficients[1,1] + glm_1spm_sum$coefficients[2,1] * (newest_data))))),3)
# 75% chance for at den stiger
  # 73.4% chance med December FORV1 tal (giver mening eftersom det er negativt)


#### Opgave 3.3 – Simpel validering af model ####
# Hvor ofte forudsiger jeres model i opgave 1.2, at den kvartalsvise årlige realvækst i husholdningernes
# forbrugsugift stiger? Hvor ofte er det så reelt tilfældet, at den kvartalsvise årlige realvækst i
# husholdningernes forbrugsudgift stiger, set i forhold til, hvad jeres model forudsiger?
#  (hint: hvor mange tilfælde af 1 og 1 finder jeres model?)
# Indsætte alle værdier, for at matrix kan virke
Predict_glm_all <- predict(glm_1spm, newdata = FTIForbrug, type = "response") 
threshold <- 0.6 
predicted_classes <- ifelse(Predict_glm_all > threshold,"Op","Ned")
Konfusion_matrix <- table(Predicted = predicted_classes, Actual = FTIForbrug$Retning)
Konfusion_matrix
#          Actual
#Predicted Ned Op
#.     Ned  13  4
#.      Op   9 73
# 0.5 er samme %, men mindre 'ned'

thresholds <- seq(0, 1, by = 0.01)
best_threshold <- NULL
best_accuracy <- 0

for (t in thresholds) {
  predicted_classes <- ifelse(Predict_glm_all > t, "Op", "Ned")
  cm <- table(Predicted = predicted_classes, Actual = FTIForbrug$Retning)
  
  # beregner andelen af korrekt klassificerede observationer i konfusionmatricen.
  accuracy <- sum(diag(cm)) / sum(cm) # Tager den diagonale/korrekte svar, og dividere med total, for at finde bedste
  
  if (accuracy > best_accuracy) {
    best_accuracy <- accuracy
    best_threshold <- t
  }
}
cat("Den bedste threshold ifølge accuracy er:", best_threshold, 
    "med en accuracy på:", best_accuracy, "\n")
# Vi har dog en bias af Op.. Så vær kritisk

library(pROC)
roc <- smooth(roc(Forbrugsdata$Retning, Predict_glm_all))
auc <- auc(roc)
plot(roc, col = "blue", lwd = 2, main = "ROC-kurven har en Area Under the Curve på 0.821", print.auc = TRUE)
# AUC på 0.821

# I hånden
# Omdan faktiske klasser til binært format (Op = 1, Ned = 0)
actual_binary <- ifelse(FTIForbrug$Retning == "Op", 1, 0)

# Forudsagte sandsynligheder fra din model
predicted_probs <- Predict_glm_all

# Sortér efter sandsynligheder (højeste først)
ord <- order(predicted_probs, decreasing = TRUE)
predicted_probs_sorted <- predicted_probs[ord]
actual_sorted <- actual_binary[ord]

# Antal positive og negative
P <- sum(actual_sorted == 1)
N <- sum(actual_sorted == 0)

# Beregn kumulative TPR og FPR
TPR <- cumsum(actual_sorted == 1) / P
FPR <- cumsum(actual_sorted == 0) / N

# Tilføj startpunktet (0,0)
TPR <- c(0, TPR)
FPR <- c(0, FPR)

# Beregn AUC med trapezmetoden
AUC <- sum((FPR[-1] - FPR[-length(FPR)]) * (TPR[-1] + TPR[-length(TPR)])) / 2
cat("AUC (beregnet manuelt):", AUC, "\n")
# 0.8217237 aka det samme som før
  # 0.8266178  efter dec data

# Plot ROC-kurven
plot(FPR, TPR, type = "l", col = "blue", lwd = 2,
     xlab = "False Positive Rate", ylab = "True Positive Rate",
     main = "Manuelt beregnet ROC-kurve")
abline(a=0, b=1, lty=2, col="gray")


#### Opgave 3.4 – Potentielle forbedringer af model ####
# Hvordan I prøve at forbedre jeres model? Lav 2 valgfrie scenarier og test dem i forhold til jeres
# baseline (underspørgsmålene fra DI’s forbrugertillidsindikator).

# Kunne evt forbedres ved brug af hele DIs 
# IDK
test_df <- as.data.frame(FTIForbrug$Retning)
colnames(test_df) <- "Retning"

#test_df$x_variabel <- rowMeans(FTIForbrug[,c(5,7,9,13)]) # For DI
#newest_data <- data.frame(FORV1_Q$DI[100]) # For DI

test_df$x_variabel <- FTI$Forbrugertillidsindikatoren # For DST
newest_data <- data.frame(FORV1_Q$Forbrugertillidsindikatoren[100]) # For DST

colnames(newest_data) <- "x_variabel"

glm_1spm <- glm(Retning~x_variabel,
                data = test_df, family = binomial)
glm_1spm_sum <- summary(glm_1spm)
Predict_glm <- predict(glm_1spm, newdata = newest_data, type = "response")
Predict_glm_manuelt <- round(as.numeric(1 / (1 + exp(-(glm_1spm_sum$coefficients[1,1] + glm_1spm_sum$coefficients[2,1] * (newest_data))))),3)




thresholds <- seq(0, 1, by = 0.01)
best_threshold <- NULL
best_accuracy <- 0

for (t in thresholds) {
  predicted_classes <- ifelse(Predict_glm_all > t, "Op", "Ned")
  cm <- table(Predicted = predicted_classes, Actual = test_df$Retning)
  
  # beregner andelen af korrekt klassificerede observationer i konfusionmatricen.
  accuracy <- sum(diag(cm)) / sum(cm) # Tager den diagonale/korrekte svar, og dividere med total, for at finde bedste
  
  if (accuracy > best_accuracy) {
    best_accuracy <- accuracy
    best_threshold <- t
  }
}
cat("Den bedste threshold ifølge accuracy er:", best_threshold, 
    "med en accuracy på:", best_accuracy, "\n")

Predict_glm_all <- predict(glm_1spm, newdata = test_df, type = "response") 
threshold <- best_threshold
predicted_classes <- ifelse(Predict_glm_all > threshold,"Op","Ned")
Konfusion_matrix <- table(Predicted = predicted_classes, Actual = test_df$Retning)


library(pROC)
roc <- smooth(roc(Forbrugsdata$Retning, Predict_glm_all))
auc <- auc(roc)
plot(roc, col = "blue", lwd = 2, main = "ROC-kurve for modellen", print.auc = TRUE)

# DIs er ikke bedre.... Hvad så med DST? -> gør koden om og se hvad den siger
# DI - Optimale threshold = 0.6 (accuracy = 0.8383838:
#Actual
#Predicted Ned Op
#Ned  10  4
#Op   12 73
# AUC 0.802

# DST - Optimale threshold 0.69 (auc = 0.8181818)
#Actual
#Predicted Ned Op
#Ned  12  8
#Op   10 69
# AUC 0.771, så den er den værste af de 3








