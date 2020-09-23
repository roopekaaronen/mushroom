################################################################
##    Roope Kaaronen   // University of Helsinki
##    Date: 14.8.2020
##    Contact: roope dot kaaronen at helsinki dot fi
##    https://roopekaaronen.com
##    @roopekaaronen
##    GitHub: https://github.com/roopekaaronen
##
##    R code for data-analysis, "Mycological Rationality" paper
################################################################


# Set working directory (optional)...
# ...but only if you want to save figures, it's not used otherwise.

setwd("C:/Users/...")

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
library(plyr)
library(RColorBrewer)
library(reshape2)
library(gridExtra)
library(ggpubr)
library(cowplot)
library(Cairo)

# Read data

dat = read.csv("https://raw.githubusercontent.com/roopekaaronen/mushroom/master/mushroom.csv", stringsAsFactors = FALSE, header = T)

# Decriptives
mean(dat$Age) # Mean age
sd(dat$Age) # SD age
mean(dat$Experience_years) # Mean experience
sd(dat$Experience_years) # SD experience
sum(dat$Experience_years) # Total years of experience
median(dat$Species) # Mean amount of species foraged on one trip
str(dat) # 894 observations

################################
##  Demographics and 
##  descriptive analysis
################################

# Plot age distribution of respondents
p <- ggplot(dat, aes(x=Age)) +
  geom_histogram(color="#e9ecef", fill = "forestgreen",  alpha=0.6, binwidth = 2) + # other colour #404080
  labs(fill="") +
  ylab("Count") +
  geom_vline(aes(xintercept=mean(Age)),
             color="black", linetype="dashed", size=0.5, alpha = 0.5)
p


# Plot experience and distribution of experience of participants.
p2 <- ggplot(dat, aes(x=Experience_years)) +
  geom_histogram(color="#e9ecef", fill = "forestgreen",  alpha=0.6, binwidth = 5) + # other colour #404080
  labs(fill="") +
  ylab("Count") +
  geom_vline(aes(xintercept=mean(Experience_years)),
             color="black", linetype="dashed", size=0.5, alpha = 0.5) +
  xlab("Experience (years)") +
  ylab("Count")
p2


# Plot distribution of picked species.
p3 <- ggplot(dat, aes(x=Species)) +
  geom_histogram(fill = "forestgreen", color = "forestgreen",  alpha=0.6, binwidth = 2, size = 0.5, stat = "count") + # other colour #404080
  labs(fill="") +
  ylab("Count") +
  geom_vline(aes(xintercept=median(Species)),
             color="black", linetype="dashed", size=0.5, alpha = 0.5) +
  xlab("Species picked") +
  ylab("Count")
p3
  
## Combine p, p2, p3 with cowplot

bottom_row <- plot_grid(p3, labels = c('C'), label_size = 12)

top_row <- plot_grid(p, p2, labels = c("A", "B"), ncol = 2)

plot_grid(top_row, bottom_row, nrow = 2)

ggsave("combined.pdf", width = 170, height = 150, units = "mm") # save as PDF
ggsave("combined.tiff", width = 170, height = 150, units = "mm") # save as TIFF



################################
##  Foraging habits, Likert-scales
##  and stacked bar charts
################################

# Select relevant variables.
tab <- dat %>% dplyr::select(6:12, 29)

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

ggsave("barchart.tiff", width = 200, height = 85, units = "mm")
ggsave("barchart.pdf", width = 200, height = 85, units = "mm")


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
#tiff("wordcloud1.tiff", units="mm", width=140, height=85, res=300)
#wordcloud(art1bag, min.freq = 5, random.order = F, scale=c(1.5,.5), rot.per = 0)
#dev.off()

# Read lines of the mushroom2.txt corpus
art2 <- paste(readLines("mushroom2.txt"), collapse = " ") 

# Tidy the corpus
art2tidy <- gsub(pattern="\\W", replace=" ", art2) %>% gsub(pattern="\\d", replace=" ", art2)
art2tidy <- tolower(art2tidy)
art2tidy <- stripWhitespace(art2tidy)
art2bag <- str_split(art2tidy, pattern="\\s+")
art2bag <- unlist(art2bag)

# Print wordcloud #2
#tiff("wordcloud2.tiff", units="mm", width=140, height=85, res=300)
#wordcloud(art2bag, min.freq = 5, random.order = F, scale=c(1.5,.5), rot.per = 0)
#dev.off()


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
#tiff("species1.tiff", units="mm", width=150, height=100, res=300)
ggplot(frequent_terms, aes(y = FREQ, x = reorder(WORD, FREQ))) +
  geom_bar(stat = "identity", colour = "#e9ecef", fill = "forestgreen", alpha = 0.4) +
  ylab("Frequency") +
  xlab("Fungi") +
  coord_flip()
ggsave("species1.pdf", width = 150, height = 100, units = "mm") # save as PDF
ggsave("species1.tiff", width = 150, height = 100, units = "mm") # save as TIFF


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
#tiff("species2.tiff", units="mm", width=150, height=100, res=300)
ggplot(frequent_terms2, aes(y = FREQ, x = reorder(WORD, FREQ)), colour = "green") +
  geom_bar(stat = "identity", colour = "#e9ecef", fill = "goldenrod", alpha = 0.5) +
  ylab("Frequency") +
  xlab("Fungi") +
  coord_flip()

ggsave("species2.pdf", width = 150, height = 100, units = "mm") # save as PDF
ggsave("species2.tiff", width = 150, height = 100, units = "mm") # save as TIFF


################################
##  Exploratory correlation plot
################################

# Select variables of interest

tabCorr <- dat %>% dplyr::select(4:12, 29, 2)

# Print the correlation plot (with Pearson correlation coefficients)
# tiff("corrplot.tiff", units="in", width=7, height=7, res=300)
pdf(file = "corrplot.pdf")

corrplot(cor(tabCorr, method = "pearson"), method = "color", type = "upper", number.cex = .7,
         addCoef.col = "black")
dev.off()

################################
##  Experience vs. mushrooms identified
################################


## Count total number of reported mushroom species for the two pictures
dat$shroomamount <- (str_count(dat$SpeciesPicture1, '\\w+') + str_count(dat$SpeciesPicture2, '\\w+'))

plot(density(dat$shroomamount))# Density distribution of shroomamount
## Linear regression of Experience vs. shroomamount
linear <- lm(shroomamount ~ Experience, data = dat)
summary(linear)

# Convert experience into factor for raincloud plot
dat$Experience <- as.factor(dat$Experience)

## Create raincloud plot (Allen, 2018)
## CODE FOR RAINDCLOUD PLOTS IS ADAPTED FROM ALLEN, 2018: https://micahallen.org/2018/03/15/introducing-raincloud-plots/

## Set source
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

## Set theme
## DEFINE THE THEME
raincloud_theme = theme(
  text = element_text(size = 10),
  axis.title.x = element_text(size = 11),
  axis.title.y = element_text(size = 11),
  axis.text = element_text(size = 9),
  axis.text.x = element_text(angle = 0, vjust = 0.5),
  legend.title=element_text(size=9),
  legend.text=element_text(size=9),
  legend.position = "right",
  plot.title = element_text(lineheight=.8, face="bold", size = 7),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))


## Define functions
lb <- function(x) mean(x) - sd(x)
ub <- function(x) mean(x) + sd(x)

sumld<- ddply(dat, ~Experience, summarise, mean = mean(shroomamount), median = median(shroomamount), lower = lb(shroomamount), upper = ub(shroomamount))
head(sumld)

## Print raincloud plot (with 95% CI, mean, and density distribution)

g <- ggplot(data = dat, aes(y = shroomamount, x = Experience, fill = Experience)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .4, color = "gray23", size = 0.5) +
  geom_point(aes(y = shroomamount, color = Experience), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_point(data = sumld, aes(x = Experience, y = mean), position = position_nudge(x = 0.3), size = 2.5) +
  geom_errorbar(data = sumld, aes(ymin = lower, ymax = upper, y = mean), position = position_nudge(x = 0.3), width = 0) +
  expand_limits(x = 5.25) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  theme_bw() +
  xlab("Experience (self-reported)") +
  ylab("Number of mushroom species mentioned") +
raincloud_theme
g

# Save the raincloud plot using ggsave or Cairo
#ggsave("raincloud.pdf", width = 105, height = 105, units = "mm") # save as PDF
#ggsave("raincloud.tiff", width = 105, height = 105, units = "mm") # save as TIFF

Cairo(width = 125, height = 125, file="Figure14", type="pdf", pointsize=8,
      bg = "transparent", canvas = "white", units = "mm", dpi = "auto")
g
dev.off()
#browseURL('Figure14.pdf')

################################################
##    END OF ANALYSIS
##    Contact: roope dot kaaronen at helsinki dot fi
##    https://roopekaaronen.com
##    @roopekaaronen
##    GitHub: https://github.com/roopekaaronen
################################################