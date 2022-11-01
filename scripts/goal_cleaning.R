## Load and install packages, control options ####
## First specify the packages of interest
options(pillar.sigfig = 5)
packages = c("tidyverse","ruler","stats","broom","syllabifyr","devtools")

## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

source("scripts/plotnik_functions.R")

data_speakers <- read.csv("data/data_goal_norm.csv",stringsAsFactors = TRUE) %>% 
  mutate(vowelLexSet="OW") %>% 
  mutate(vowelLexSet=factor(vowelLexSet)) %>% 
  mutate(stress=1)

data_social = read_csv("../CoRP-master.csv"
                       #,stringsAsFactors=TRUE
                       #,fileEncoding = "UTF-8-BOM"
) %>% 
  mutate_if(is.character, factor) %>%
  mutate(region = recode(region,"North-East"="NE","South-East"="SE")) %>% 
  mutate(corpus = factor(paste(corpus, region, sep = "-")))


data_lexSets = read.delim("../../DataExtraction/LexicalSet_referenceList.txt", stringsAsFactors = TRUE) %>%
  droplevels() %>% 
  filter(lexicalSet_broad != "") %>% 
  dplyr::rename(vowelLexSet=vowel) %>%
  filter(vowelLexSet=="OW") %>% 
  distinct() %>% 
  droplevels()

data_SUBTLEX <- read.delim("../SUBTLEX-UK.txt") %>%
  dplyr::select(Spelling,FreqCount,BNC_freq, LogFreq.Zipf., LogFreqBNC.Zipf.) %>% 
  mutate(word = Spelling) %>%
  mutate_if(is.character, str_to_upper)

# Outlier Functions
Q1.fn <- function(x){nth(fivenum(x,na.rm=TRUE), 2, order_by = NULL)}
Q3.fn <- function(x){nth(fivenum(x,na.rm=TRUE), 4, order_by = NULL)}
between.IQR.fn <- function(x){between(x, Q1.fn(x) - (1.5 * IQR(x, na.rm=TRUE)), 
                                      Q3.fn(x) + (1.5 * IQR(x, na.rm=TRUE)))}

## Merge Data ------------------------
data_goal_wide = data_speakers %>%
  inner_join(data_social,by="fileNameGoal") %>%
  inner_join(data_lexSets) %>% 
  mutate(fileName = factor(fileName)) %>%
  mutate(word = factor(word)) %>%
  mutate(vowel = factor(vowel)) %>%
  droplevels() %>%
  filter(lexicalSet_broad != "") %>%
  filter(!lexicalSet_narrow %in% c("","hoe","hope")) %>% 
  mutate(word=(recode(word,"COAL-"="COAL"))) %>% 
  droplevels() %>% 
  filter(!str_detect(word,regex("^XX"))) %>%
  filter(!str_detect(word,regex("\\w+\\*"))) %>%
  filter(!str_detect(word,"-+")) %>% 
  filter(!str_detect(word,"-+")) %>% 
  group_by(corpus) %>% 
  group_by(lexicalSet_narrow) %>%
  filter(between.IQR.fn(F1.30.)) %>%
  filter(between.IQR.fn(F2.30.)) %>% 
  mutate(corpus_ord = factor(corpus, levels = c("CoRP-NE","CoRP-SE"), ordered = TRUE)) %>%
  dplyr::select(ageGroup,ageRecording,corpus,corpus_ord,dateRecording,DOB,duration,edLevel,F1.11.,F1.10.,F1.20.,F1.30.,F1.40.,F1.50.      ,F1.60.,F1.70.,F1.80.,F1.90.,F1.99.,F2.11.,F2.10.,F2.20.,F2.30.,F2.40.,F2.50.,F2.60.,F2.70.,F2.80.,F2.90.,F2.99.,fatherEd,fatherEdLevel,fatherOccupation,fatherRegion,fileName,fileNameGoal,firstName,fol_seg,id,juniorEd,lastName,lexicalSet_broad,lexicalSet_narrow,motherEd,motherEdLevel,motherOccupation,motherRegion,occClass,occupation,parentEdLevelHigh,parentOccClass,position,prec_seg,privateTotal,region,rowid_fac,SecondEd,sex,sixthFEd,speakerNumber,stress,style,traj,vowel,vowelLexSet,word,yearRecording,YOB )

## Pivot -----------------------------
data_goal_long <- data_goal_wide %>%
  pivot_longer(cols = contains("."),
               names_to = c(".value", "measurement.no"), 
               names_pattern = "(F\\d).(\\d\\d).") %>%
  mutate(measurement.no = recode(measurement.no,"11"="00","99"="100"#,.default=levels(measurement.no)
                                 ))

data_goal <- data_goal_long %>% 
  mutate(measurement.no = as.numeric(measurement.no)) %>%
  mutate(traj = as.numeric(traj)) %>%
  dplyr::select(traj,position,vowel,stress,word,duration,speakerNumber,firstName,sex,YOB,ageGroup,corpus,id,lexicalSet_narrow,lexicalSet_broad,word,vowel,style,measurement.no,F1,F2)

write_csv(data_goal,"data/data_goal.csv")

  
