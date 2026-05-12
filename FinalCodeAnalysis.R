# --- Code for Analysis of Experiment Data --- #
# Make sure you have the data installed too #

# -- Load & Preprocess the Data -- #
rawdata = read.csv('TestData.csv')  # CHANGE THE NAME TO THE DEFAULT NAME OF THE FILE FOR THE FINAL VERSION! this is just to know which is(n't) the final data
dropdata = subset(rawdata, select = -c(1,2))  # remove timestamps and informed consent columns as they don't require analysis.

# Load the tidyverse library to rename columns for better readability 
install.packages("tidyverse")
library(tidyverse)
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
install.packages("psych")
library(psych)
describe(data)  # contains lots of useful descriptive statistics, however we only need Age and the 3 conditions.
summary(data)  # for IQR, median and Range

# Now we need counts and descriptive statistics for the categorical variables, such as Gender, Most/Least pleasant noise and Disorders.
count(data, Gender, sort = TRUE)
count(data, Disorder, sort = TRUE)  # Disorders will be counted, although (due to lack of constraints on open ended questions) the specific disorders will be counted by hand
count(data, MostPleasantNoise, sort = TRUE)
count(data, LeastPleasantNoise, sort = TRUE)

# The Column Extra will be interpreted by the researchers, also due to lack of constraints on open ended questions

# -- Result Analysis -- #

control = data$ControlCondition  # shortening them for ease of use
beta = data$BetaCondition
gamma = data$GammaCondition

# -- Normality -- #
# Shapiro-Wilk test to check normal distribution for 3 condition results
# The null-hypothesis for Shapiro-Wilk test assumes that the data IS normally distributed. Thus, we do not want p < 0.05 
shapiro.test(control)
shapiro.test(beta)
shapiro.test(gamma)
# although this checks if all conditions are normal, I don't know if that is what ANOVA assumes. 
# so we can also check for normality of differences between conditions?
shapiro.test(beta - control)
shapiro.test(gamma - control)
shapiro.test(gamma - beta)

# -- Repeated Measures Anova -- #
# We also need to do Mauchly's tets of sphericity, but that is done with the function anova_test()
# The table needs to be pivotted for this
# But we still need some packages 
install.packages("rstatix")
library(rstatix)

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

anova_scores  # computes anova AND mauchly

# -- Post hoc paired w/ bonferroni -- #
posthocpaired = longTable %>%
  pairwise_t_test(
    Score ~ Condition,
    paired = TRUE,
    p.adjust.method = "bonferroni"
  )

posthocpaired


# -- Visualisation? -- #
library(ggplot2)
boxplot(Score ~ Condition, data = longTable) # just normal visualisation, but boxplot is a little iffy for within participants, no?

ggplot(longTable, aes(x = Condition, y = Score)) +
  geom_boxplot() +
  geom_jitter(width = 0.1)   # boxplot with data points visualised!

ggplot(longTable, aes(x = Condition, y = Score)) +  
  geom_violin() +
  geom_boxplot(width = 0.1)  # violin plots

