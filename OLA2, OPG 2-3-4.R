library(dkstat)
#######################################################################################################################
#                                                                                                                     #
#                                            FORBRUGERtillidsINDIKATOR                                                #
#######################################################################################################################


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


kolonne3_lister <- lapply(indikator_lister, function(x) x$value)
resultat_data <- do.call(cbind, kolonne3_lister)
colnames(resultat_data) <- names(indikator_lister)
kol3 <- as.data.frame(kolonne3_lister)

#Insæt tid i dataframen med alle values
kol3$Tid <- indikator_lister[[1]][, 2]
# Flyt 'Tid' kolonnen til første position
kol3 <- kol3[, c("Tid", setdiff(names(kol3), "Tid"))]



# Opret en ny tom data frame til kvartalsgennemsnit med alle kolonner undtagen "Tid"
kvartal_data <- data.frame(Tid = character())


# Loop igennem dataene i trin af 3 for hvert kvartal + round til 1 dec
for (i in seq(1, nrow(kol3), by = 3)) {
  ny_tid <- kol3$Tid[i+2]
  gennemsnit_fti <- colMeans(kol3[i:(i+2), -1])
  ny_række <- data.frame(Tid = ny_tid, t(gennemsnit_fti))
  kvartal_data <- rbind(kvartal_data, ny_række)
  kvartal_data[,2:13] <- round(kvartal_data[,2:13],1)
}
kvartal_data <- na.omit(kvartal_data)
# Sammenlign forbrugsværdierne mellem hvert kvartal hinanden følgende år
for (i in 2:nrow(kvartal_data)) {
  if (kvartal_data$Danmarks.økonomiske.situation.om.et.år..sammenlignet.med.i.dag[i] > 0) {
    kvartal_data$Retning[i] <- "op"
  } else if (kvartal_data$Danmarks.økonomiske.situation.om.et.år..sammenlignet.med.i.dag[i] <= 0) {
    kvartal_data$Retning[i] <- "ned"
  }
}


# 'NA' er sat i starten for at matche antallet af rækker, da diff() reducerer antallet af elementer med én
opned <- as.data.frame(kvartal_data$Danmarks.økonomiske.situation.om.et.år..sammenlignet.med.i.dag)
opned$difference <- c(NA, diff(kvartal_data$Danmarks.økonomiske.situation.om.et.år..sammenlignet.med.i.dag))

opned$tid <- kvartal_data$Tid
opned$opned <- for (i in 2:nrow(opned)) {
  if (opned$difference[i] > 0) {
    opned$Retning[i] <- "op"
  } else if (opned$difference[i] <= 0) {
    opned$Retning[i] <- "ned"
  }
}

tillid <- opned[46:143,]

#########################################################################################################################################################
# Søg efter data i DST
dst_search(string = "husholdningernesforbrugsudgifter", field = "text")

# Hent metadata og data for tabellen "NKN3"
forbrug <- dst_meta(table = "NKN3", lang = "da")

# Definer filter for dataudtræk
forbrug_filters <- list(
  TRANSAKT="*",              # P.31 Udgifter til individuelt forbrug
  PRISENHED="*",             # 2020-priser, real værdi, (mia. kr.)
  Tid="*"
)

# Hent data fra DST
forbrugData <- dst_get_data(table = "NKN3", query = forbrug_filters, lang = "da")

# Konverter data til data frame
forbrugData <- as.data.frame(forbrugData)

# Udvælg de relevante rækker
forbrugData <- forbrugData[103:204,]

# Fjern de første to kolonner
forbrugData <- forbrugData[,-c(1,2)]

# Omdøb den anden kolonne til et mere beskrivende navn
colnames(forbrugData)[2] <- "Udgifter.til.individuelt.forbrug"

#realvækst omregning
#forbrugData <- forbrugData$Udgifter.til.individuelt.forbrug
vækstrate <- (exp(diff(log(as.numeric(forbrugData$Udgifter.til.individuelt.forbrug)), lag = 4)) - 1) * 100
#forbrugData <- as.data.frame(realvaekst_rate)
#forbrugData <- as.data.frame(forbrugData$TID)
#forbrugData <- forbrugstid[-c(1,2,3,4),]
#forbrugData$tid <- forbrugstid

forbrugData$vækstrate <- round(c(rep(NA, 4), vækstrate),1)
forbrugData <- na.omit(forbrugData)

# Opret 'Retning' kolonne som en tom kolonne
forbrugData$Retning <- NA

# Sammenlign forbrugsværdierne mellem hvert kvartal hinanden følgende år
for (i in 1:length(forbrugData$vækstrate)) {
  if (forbrugData$vækstrate[i] > 0) {
    forbrugData$Retning[i] <- "op"
  } else if (forbrugData$vækstrate[i] <= 0) {
    forbrugData$Retning[i] <- "ned"
  }
}

forbrugData$FTI <- tillid$`kvartal_data$Danmarks.økonomiske.situation.om.et.år..sammenlignet.med.i.dag`

# Lav en tælling af hvor mange "op", "ned" og "uændret" der er
resultat_tabel <- table(forbrugData$Retning)




# Filtrer rækker, hvor 'Retning' er "op"
op <- subset(forbrugData, Retning == "op")
ned <- subset(forbrugData, Retning=="ned")
meanOP <- mean(op$FTI)
meanNED <- mean(ned$FTI)

hist(ned$FTI)
hist(op$FTI)
hist(forbrugData$FTI)
barplot()


library(ggplot2)

# Data til plot (mean op og ned)
data <- data.frame(
  Retning = c("Op", "Ned"),
  MeanFTI = c(meanOP, meanNED)
)

# Lav barplot med tie-dye lignende farveskift og værdier på hver bar
p <- ggplot(data, aes(x = Retning, y = MeanFTI, fill = Retning)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = round(MeanFTI, 1)), vjust = -0.5, size = 6) + # Viser værdien på hver bar
  scale_fill_manual(values = c("Op" = "#FF6347", "Ned" = "#1E90FF")) + # Tie-dye inspirerede farver
  labs(title = "Når den stiger, stiger den 4.6 gange så meget som når den falder", x = "Retning", y = "Gennemsnitlig FTI") +
  theme_minimal(base_size = 16) + 
  theme(legend.position = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) # Juster y-aksen for at give plads til label

# Vis plot
p

forbrugData$retning_num <- forbrugData$Retning
forbrugData$retning_num <- gsub("ned", "0", forbrugData$retning_num)
forbrugData$retning_num <- gsub("op", "1", forbrugData$retning_num)
forbrugData$retning_num <- as.factor(forbrugData$retning_num)

kvartal2000 <- kvartal_data[102:199,]

glm.fits <- glm(forbrugData$retning_num ~ 
                  #kvartal2000$Forbrugertillidsindikatoren +
                  #kvartal2000$Familiens.økonomiske.situation.i.dag..sammenlignet.med.for.et.år.siden + #pværdi: 0.00250
                  #kvartal2000$Familiens.økonomiske..situation.om.et.år..sammenlignet.med.i.dag+ #pværdi: 0.0279
                  #kvartal2000$Danmarks.økonomiske.situation.i.dag..sammenlignet.med.for.et.år.siden + #pværdi:0.050248
                  kvartal2000$Danmarks.økonomiske.situation.om.et.år..sammenlignet.med.i.dag,# + #0.855837 
                #kvartal2000$Anskaffelse.af.større.forbrugsgoder..fordelagtigt.for.øjeblikket + #0.00357
                #kvartal2000$Priser.i.dag..sammenlignet.med.for.et.år.siden+ #0.010253
                #kvartal2000$Priser.om.et.år..sammenlignet.med.i.dag + #0.00666
                #kvartal2000$Arbejdsløsheden.om.et.år..sammenlignet.med.i.dag + #0.021776
                #kvartal2000$Anskaffelse.af.større.forbrugsgoder..inden.for.de.næste.12.mdr.+ #0.016070
                #kvartal2000$Anser.det.som.fornuftigt.at.spare.op.i.den.nuværende.økonomiske.situation+ #0.00679
                #kvartal2000$Regner.med.at.kunne.spare.op.i.de.kommende.12.måneder + #0.027
                #kvartal2000$Familiens.økonomiske.situation.lige.nu..kan.spare.penge.slår.til..bruger.mere.end.man.tjener, #0.00586
                data = forbrugData, family = binomial)

summary(glm.fits)
# Opret et nyt datasæt med FTI = 15 for flere observationer (hvis du har brug for det)
new_data <- data.frame(FTI = rep(15, 100))  # Hvis du ønsker flere rækker, kan du justere antallet

# Forudsige sandsynligheder for alle rækker i new_data
predicted_prob <- predict(glm.fits, new_data, type = "response")

# Fjern eventuelle NA-værdier
predicted_prob <- na.omit(predicted_prob)

# Beregn gennemsnittet af sandsynlighederne
mean_prob <- mean(predicted_prob)

# Udskriv gennemsnittet som et procenttal
cat("Gennemsnitlig sandsynlighed for at realvæksten stiger når FTI er 15:", round(mean_prob * 100, 2), "%\n")

