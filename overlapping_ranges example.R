source("overlapping_ranges.R")

library(readxl)
testdata <- read_excel("testdata.xlsx")

testdata$Patient_Geneesmiddel <-as.factor(paste(testdata$Patient, testdata$Geneesmiddel))

# Functie aanroepen. Alle regels met het zelfde episode_nr vormen een overlappend geheel.
# De functie is in principe snel omdat hij alleen maar 2x een sortering doet van de volledige lijst, en voor de rest
# alleen maar eenvoudige bewerkingen.
testdata$episode_nr <- group_overlapping_ranges(testdata$Patient_Geneesmiddel, testdata$Startdatum, testdata$Einddatum)

# Je kunt einddatum eventueel nog oprekken zodat er wat tolerantie is. In het onderstaande voorbeeld mogen er 3 dagen zitten
# tussen stop en start van twee regels, dan horen ze nog tot dezelfde episode.
testdata$episode_nr2 <- group_overlapping_ranges(testdata$Patient_Geneesmiddel, testdata$Startdatum, testdata$Einddatum + 3)

# Even een sortering aanbrengen zodat het op het oog min of meer duidelijk is welke regels overlappen
testdata <- testdata[order(testdata$Patient, testdata$Geneesmiddel, testdata$Startdatum), ]

# Vervolgens kun je  allerlei aggregaties doen op episode_nr om begin en eind van episodes te vinden,
# je weet immers welke regels tot dezelfde episode behoren.

