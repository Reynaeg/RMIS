# -------------------------------------------- #
# Code for Analysis of Experiment Data 
# Make sure you have the data installed too! 
# -------------------------------------------- #

# 1. Loading and installing all packages 

# You can un-comment the install.packages() functions if you need to install the packages. 
# But it makes my laptop slow when running this script.

# install.packages("tidyverse")
library(tidyverse)
# install.packages("psych")
library(psych)
# install.packages("rstatix")
library(rstatix)
# install.packages("ggplot2")
library(ggplot2)

# 2. Load & Preprocess the Data 

# This is the name of the .csv file downloaded from the google Forms. 
# You may need to rename it if you were to re-run the experiment
rawdata = read.csv('Experiment research methods (Antwoorden) - Formulierreacties 1.csv')  

# remove timestamps and informed consent columns as they don't require analysis.
dropdata = subset(rawdata, select = -c(1,2))

# Using tidyverse to rename columns for better readability 
data = dropdata %>% rename(Age = Wat.is.uw.leeftijd..in.jaren..,
                           Gender = Wat.is.uw.geslacht., 
                           ControlCondition = Conditie.A.resultaat,
                           BetaCondition =  Conditie.B.resultaat,
                           GammaCondition = Conditie.C.resultaat,
                           MostPleasantNoise = Welk.geluid.vond.u.het.prettigst.om.naar.te.luisteren.,
                           LeastPleasantNoise = Welk.geluid.vond.u.het.minst.prettig.om.naar.te.luisteren.,
                           Disorder =  Heeft.u.een.gediagnosticeerde.gehooraandoening.of.een.vorm.van.dyslexie....Als.u.hierbij..ja..wil.antwoorden..voer.dat.dan.a.u.b..in.bij..Anders..en.specifieer.a.u.b..welke.stoornis..als.u.dat.wil.delen.,
                           Extra = Als.u.nog.iets.kwijt.wil..kunt.u.dat.hier.benoemen.)

# Now rename values for easier analysis
data[data=="Het tweede geluid"]<-"Beta"   # this changes the item for all values in the df. Doesnt matter in our case tho
data[data=="Het eerste geluid"]<-"Control"
data[data=="Het derde geluid"]<-"Gamma"
data[data=="Nee"]<-NaN
data[data==""]<- NaN

# Remove the unneeded variables from the environment (purely because I don't like having all those variables there)
rm(dropdata, rawdata) 
data$ID = seq.int(nrow(data)) # Add an ID row, for ease of use as I'm not familiar with R and can't find good sources to help me loop over index 

# -- Descriptive statistics of participants -- #
# Using describe() with the psych package
control = data$ControlCondition  # shortening them for ease of use
beta = data$BetaCondition
gamma = data$GammaCondition

describe(data)  # contains lots of useful descriptive statistics, however we only need Age and the 3 conditions.
summary(data)  # for IQR, median and Range
t.test(control)  # for CI, check CI for mean
t.test(beta)
t.test(gamma)

# Now we need counts and descriptive statistics for the categorical variables, such as Gender, Most/Least pleasant noise and Disorders.
count(data, Gender, sort = TRUE)
count(data, Disorder, sort = TRUE)  # Disorders will be counted, although (due to lack of constraints on open ended questions) the specific disorders will be counted by hand
pleasant = count(data, MostPleasantNoise, sort = TRUE)
unpleasant = count(data, LeastPleasantNoise, sort = TRUE)

# The Column Extra will be interpreted by the researchers, also due to lack of constraints on open ended questions

# -- Result Analysis -- #



# -- Normality -- #
# Shapiro-Wilk test to check normal distribution for 3 condition results
# The null-hypothesis for Shapiro-Wilk test assumes that the data IS normally distributed. Thus, we do not want p < 0.05 
# So we can also check for normality of differences between conditions? I think this is the way to go.
shapiro.test(beta - control)
shapiro.test(gamma - control)
shapiro.test(gamma - beta)

# -- Repeated Measures Anova -- #
# We also need to do Mauchly's tets of sphericity aka equal variance in conditions i think, but that is done with the function anova_test()
# You do NOT want to violate this, aka, finding p > 0.05 is what we want
# The table needs to be pivotted for this


longTable = data %>% pivot_longer(cols = c(ControlCondition, BetaCondition, GammaCondition),
                                  names_to = "Condition",
                                  values_to = "Score"
                                  )

anova_scores = anova_test(
  data = longTable,
  dv = Score,
  wid = ID,
  within = Condition
)

# GG only if Mauchly IS significant, and thus violates. Otherwise no correction is needed
anova_scores  # computes anova, mauchly AND we can check greenhouse geisser corrected annova like this if needed:
get_anova_table(anova_scores, correction = "GG")


# -- Post hoc paired w/ bonferroni -- #
posthocpaired = longTable %>%
  pairwise_t_test(
    Score ~ Condition,
    paired = TRUE,
    p.adjust.method = "bonferroni"
  )

posthocpaired


# -- Visualisation -- #
# Visualisations not only for the research document but also the poster perhaps?

boxplot(Score ~ Condition, data = longTable) # just normal visualisation, but normal boxplots are a little boring.

ggplot(longTable, aes(x = Condition, y = Score)) +
  geom_boxplot() +
  geom_jitter(width = 0.1)   # boxplot with data points visualised!

ggplot(longTable, aes(x = Condition, y = Score)) +  
  geom_violin() +
  geom_boxplot(width = 0.1)  # violin plots

# and then these for poster, as they help us visualize the participants demographics 

ggplot(data, aes(x = 2, fill =  Gender)) +
  geom_bar(width = 1) +
  geom_text(stat = "count", aes(label = after_stat(count)),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  xlim(c(0.5, 2.5)) +
  theme_void() 

ggplot(data, aes(x = "", fill = factor(Age))) +
  geom_bar(width = 1) +
  geom_text(stat = "count", aes(label = after_stat(count)),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  theme_void() # THIS IS SOO ugly im sorry
  
  # I MISS PYTHON I MISS PANDAS I HATE THISSSS 

ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = 18:28) +
  theme_test()   

# ranking pleasant and unpleasant noises
ggplot(pleasant, aes(x= MostPleasantNoise, y=n)) +
       geom_col() + 
       geom_text(aes(label=n), vjust = -0.5) + 
       theme_test()
ggplot(unpleasant, aes(x= LeastPleasantNoise, y=n)) +
       geom_col() + 
       geom_text(aes(label=n), vjust = -0.5) + 
       theme_test()
