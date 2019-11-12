################################################################
##    Roope Kaaronen   // University of Helsinki
##    Date: 28.10.2019
##    Contact: roope dot kaaronen at helsinki dot fi
##    https://roopekaaronen.com
##    @roopekaaronen
##    GitHub: https://github.com/roopekaaronen
################################################################


# Set working directory

setwd("C:/Users/RoopeOK/Documents/Yliopisto/Sienestys/")

# Install and download packages

# The following line can be used to install packages:
# if(!require(InsertPackage)){install.packages("InsertPackage")}

library(tidyverse)
library(corrplot)
library(Hmisc)
library(likert)
library(wordcloud)
library(tm)
library(qdap)

# Read data

dat = read.csv("C:/Users/RoopeOK/Documents/Yliopisto/Sienestys/mushroom_translate.csv", stringsAsFactors = FALSE, header = T)

# Decriptives
mean(dat$Age) # Mean age
mean(dat$Experience_years) # Mean experience
sum(dat$Experience_years) # Total years of experience
median(dat$Species) # Mean amount of species foraged on one trip
str(dat) # 894 observations

# Gender

Gender <- table(unlist(strsplit(tolower(dat$Gender), " ")))
View(Gender)

################################
##  Demographics and 
##  descriptive analysis
################################


# Plot age and distribution of age of participants.
tiff("age.tiff", units="mm", width=140, height=85, res=300)

p <- ggplot(dat, aes(x=Age)) + 
  geom_density(size = 1, color = "darkgreen", fill="forestgreen", alpha = 0.5) +
  geom_vline(aes(xintercept=mean(Age)),
               color="chocolate4", linetype="dashed", size=1) +
  xlab("Age (mean = 49.6)") +
  ylab("Density")
p
dev.off() # Print data


# Plot experience and distribution of experience of participants.
tiff("xp.tiff", units="mm", width=140, height=85, res=300)

p2 <- ggplot(dat, aes(x=Experience_years)) + 
  geom_density(size = 1, color = "darkgreen", fill="forestgreen", alpha = 0.5) +
  geom_vline(aes(xintercept=mean(Experience_years)),
             color="chocolate4", linetype="dashed", size=1) +
  xlab("Experience (mean = 25)") +
  ylab("Density")
p2
dev.off()


# Plot number of picked species and distribution.

tiff("species.tiff", units="mm", width=140, height=85, res=300)

p3 <- ggplot(dat, aes(x=Species)) + 
  geom_density(size = 1, color = "darkgreen", fill="forestgreen", alpha = 0.5) +
  geom_vline(aes(xintercept=median(Species)),
             color="chocolate4", linetype="dashed", size=1) +
  xlab("Species (median = 4)") +
  ylab("Density")
p3
dev.off() 



################################
##  Foraging habits, Likert-scales
##  and stacked bar charts
################################

# Select relevant variables.
tab <- dat %>% dplyr::select(6:12, 29)
View(tab)

# Rename responses.

tab[tab == "1"] <- "Completely disagree"
tab[tab == "2"] <- "Somewhat disagree"
tab[tab == "3"] <- "Neither agree nor disagree"
tab[tab == "4"] <- "Somewhat agree"
tab[tab == "5"] <- "Completely agree"

# Convert variables of interest into factors.


tab$Experience = factor(tab$Experience,
                   levels = c("Completely disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Completely agree"),
                   ordered = TRUE)

tab$Hunch = factor(tab$Hunch,
                     levels = c("Completely disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Completely agree"),
                     ordered = TRUE)

tab$Glance = factor(tab$Glance,
                     levels = c("Completely disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Completely agree"),
                     ordered = TRUE)

tab$Apaja = factor(tab$Apaja,
                      levels = c("Completely disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Completely agree"),
                      ordered = TRUE)

tab$Familiar = factor(tab$Familiar,
                      levels = c("Completely disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Completely agree"),
                      ordered = TRUE)

tab$Delicious = factor(tab$Delicious,
                      levels = c("Completely disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Completely agree"),
                      ordered = TRUE)

tab$Protect = factor(tab$Protect,
                      levels = c("Completely disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Completely agree"),
                      ordered = TRUE)

tab$Heuristics = factor(tab$Heuristics,
                        levels = c("Completely disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Completely agree"),
                        ordered = TRUE)

# Summary of data.
likert(tab)
summary(tab)

# Create results.

Result = likert(tab)

summary(Result)

# Create stacked bar charts
tiff("barchart.tiff", units="mm", width=200, height=85, res=300)
plot(Result,
     type="bar", legend.position = "right")
dev.off()

# Alternative visualisation
plot(Result,
     type="density",
     facet = TRUE,
     bw = 0.5)

################################
##  Counts of reported heuristics
################################

# Count instances of...

# ...heuristics for safe foraging:
Heuristics1 <- table(unlist(strsplit(tolower(dat$Reported_Heuristics1), " ")))
View(Heuristics1)
# ...heuristics for identifying poisonous/edible mushrooms:
Heuristics2 <- table(unlist(strsplit(tolower(dat$Reported_Heuristics2), " ")))
View(Heuristics2)
# ...heuristics for finding mushrooms:
Heuristics3 <- table(unlist(strsplit(tolower(dat$Reported_Heuristics3), " ")))
View(Heuristics3)

################################
##  Wordclouds and text mining
################################

# Read lines of the mushroom1.txt corpus
art1 <- paste(readLines("mushroom1.txt"), collapse = " ") 

# Tidy the corpus
art1tidy <- gsub(pattern="\\W", replace=" ", art1) %>% gsub(pattern="\\d", replace=" ", art1)
art1tidy <- tolower(art1tidy)
art1tidy <- stripWhitespace(art1tidy)
art1bag <- str_split(art1tidy, pattern="\\s+")
art1bag <- unlist(art1bag)

# Print wordcloud #1
tiff("wordcloud1.tiff", units="mm", width=140, height=85, res=300)
wordcloud(art1bag, min.freq = 5, random.order = F, scale=c(1.5,.5), rot.per = 0)
dev.off()

# Read lines of the mushroom2.txt corpus
art2 <- paste(readLines("mushroom2.txt"), collapse = " ") 

# Tidy the corpus
art2tidy <- gsub(pattern="\\W", replace=" ", art2) %>% gsub(pattern="\\d", replace=" ", art2)
art2tidy <- tolower(art2tidy)
art2tidy <- stripWhitespace(art2tidy)
art2bag <- str_split(art2tidy, pattern="\\s+")
art2bag <- unlist(art2bag)

# Print wordcloud #2
tiff("wordcloud2.tiff", units="mm", width=140, height=85, res=300)
wordcloud(art2bag, min.freq = 5, random.order = F, scale=c(1.5,.5), rot.per = 0)
dev.off()


# View all unique words in the .txt files

# Create function for unique words
uniqueWords = function(d) {
  return(paste(unique(strsplit(d, " ")[[1]]), collapse = ' '))
}

# View unique words of mushroom1.txt
corpus = VCorpus(VectorSource(art1tidy))
tdm1 = as.matrix(TermDocumentMatrix(corpus, control = list(wordLengths = c(1, Inf))))

corpus = tm_map(corpus, content_transformer(uniqueWords))
tdm1 = as.matrix(TermDocumentMatrix(corpus, control = list(wordLengths = c(1, Inf))))
tdm1

# Number of unique words (species and genera) in mushroom1.txt
nrow(tdm1)

# View unique words of mushroom2.txt
corpus = VCorpus(VectorSource(art2tidy))
tdm2 = as.matrix(TermDocumentMatrix(corpus, control = list(wordLengths = c(1, Inf))))

corpus = tm_map(corpus, content_transformer(uniqueWords))
tdm2 = as.matrix(TermDocumentMatrix(corpus, control = list(wordLengths = c(1, Inf))))
tdm2

# Number of unique words (species and genera) in mushroom1.txt
nrow(tdm2)

# View top 10 mentioned mushrooms in mushroom1.txt
corp1 <- Corpus(VectorSource(art1tidy))
(terms <- apply_as_df(corp1, freq_terms, top=10, stopwords=tm::stopwords("en")))


# View top 10 mentioned mushrooms in mushroom2.txt
corp2 <- Corpus(VectorSource(art2tidy))
(terms <- apply_as_df(corp2, freq_terms, top=10, stopwords=tm::stopwords("en")))

# Total number of observations (words) in mushroom1.txt and mushroom2.txt
sapply(strsplit(art1tidy, " "), length) # mushroom1.txt
sapply(strsplit(art2tidy, " "), length) # mushroom2.txt


################################
##  Bar charts of mushroom species
################################

# Collect the top 15 most frequent mushrooms in Picture 1
frequent_terms <- freq_terms(art1tidy, 15)

# Rename the species
frequent_terms$WORD[frequent_terms$WORD == "porcini"] <- "Porcini"
frequent_terms$WORD[frequent_terms$WORD == "sweettooth"] <- "Sweet tooth"
frequent_terms$WORD[frequent_terms$WORD == "russula"] <- "Russula"
frequent_terms$WORD[frequent_terms$WORD == "lactariustrivialis"] <- "Lactarius trivialis"
frequent_terms$WORD[frequent_terms$WORD == "lactariusrufus"] <- "Lactarius rufus"
frequent_terms$WORD[frequent_terms$WORD == "lactarius"] <- "Lactarius"
frequent_terms$WORD[frequent_terms$WORD == "hygrophoruscamarophyllus"] <- "Hygrophorus camarophyllus"
frequent_terms$WORD[frequent_terms$WORD == "hydnum"] <- "Hydnum"
frequent_terms$WORD[frequent_terms$WORD == "hornofplenty"] <- "Horn of plenty"
frequent_terms$WORD[frequent_terms$WORD == "gypsymushroom"] <- "Gypsy mushroom"
frequent_terms$WORD[frequent_terms$WORD == "funnelchanterelle"] <- "Funnel chanterelle"
frequent_terms$WORD[frequent_terms$WORD == "currymilkcap"] <- "Curry milk-cap"
frequent_terms$WORD[frequent_terms$WORD == "chanterelle"] <- "Chanterelle"
frequent_terms$WORD[frequent_terms$WORD == "bolete"] <- "Bolete"
frequent_terms$WORD[frequent_terms$WORD == "albatrellusovinus"] <- "Albatrellus ovinus"

# Plot a bar chart
tiff("species1.tiff", units="mm", width=150, height=100, res=300)
ggplot(frequent_terms, aes(y = FREQ, x = reorder(WORD, FREQ)), colour = "green") +
  geom_bar(stat = "identity", colour = "darkgreen", fill = "forestgreen", alpha = 0.5) +
  ylab("Frequency") +
  xlab("Fungi") +
  coord_flip()
dev.off()

# Repeat the procedures above for Picture 2 species
frequent_terms2 <- freq_terms(art2tidy, 15)

# Rename the species
frequent_terms2$WORD[frequent_terms2$WORD == "porcini"] <- "Porcini"
frequent_terms2$WORD[frequent_terms2$WORD == "woollymilkcap"] <- "Woolly milk-cap"
frequent_terms2$WORD[frequent_terms2$WORD == "russula"] <- "Russula"
frequent_terms2$WORD[frequent_terms2$WORD == "leccinum"] <- "Leccinum"
frequent_terms2$WORD[frequent_terms2$WORD == "agaricus"] <- "Agaricus"
frequent_terms2$WORD[frequent_terms2$WORD == "lactarius"] <- "Lactarius"
frequent_terms2$WORD[frequent_terms2$WORD == "grassgreenrussula"] <- "Grass-green russula"
frequent_terms2$WORD[frequent_terms2$WORD == "orangebirchbolete"] <- "Orange birch bolete"
frequent_terms2$WORD[frequent_terms2$WORD == "hornofplenty"] <- "Horn of plenty"
frequent_terms2$WORD[frequent_terms2$WORD == "parasolmushroom"] <- "Parasol mushroom"
frequent_terms2$WORD[frequent_terms2$WORD == "yellowswamprussula"] <- "Yellow swamp russula"
frequent_terms2$WORD[frequent_terms2$WORD == "uglymilkcap"] <- "Ugly milk-cap"
frequent_terms2$WORD[frequent_terms2$WORD == "chanterelle"] <- "Chanterelle"
frequent_terms2$WORD[frequent_terms2$WORD == "bolete"] <- "Bolete"
frequent_terms2$WORD[frequent_terms2$WORD == "slipperyjack"] <- "Slippery Jack"
frequent_terms2$WORD[frequent_terms2$WORD == "sheathedwoodtuft"] <- "Sheathed woodtuft"

# Plot a bar chart
tiff("species2.tiff", units="mm", width=150, height=100, res=300)
ggplot(frequent_terms2, aes(y = FREQ, x = reorder(WORD, FREQ)), colour = "green") +
  geom_bar(stat = "identity", colour = "yellow4", fill = "yellow4", alpha = 0.5) +
  ylab("Frequency") +
  xlab("Fungi") +
  coord_flip()
dev.off()

################################
##  Exploratory correlation plot
################################

# Select variables of interest

tabCorr <- dat %>% dplyr::select(4:12, 29, 2)

# Print the correlation plot (with Pearson correlation coefficients)
tiff("corrplot.tiff", units="mm", width=150, height=150, res=300)
corrplot(cor(tabCorr, method = "pearson"), method = "color", type = "upper", number.cex = .7,
         addCoef.col = "black")
dev.off()


################################################
##    END OF ANALYSIS
##    Contact: roope dot kaaronen at helsinki dot fi
##    https://roopekaaronen.com
##    @roopekaaronen
##    GitHub: https://github.com/roopekaaronen
################################################

# This is a test