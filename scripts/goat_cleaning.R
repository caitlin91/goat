## Load and install packages, control options ####
## First specify the packages of interest
options(pillar.sigfig = 5)
packages = c("tidyverse","ruler","stats","broom","syllabifyr")

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
## If a package is installed, it will be loaded. If any are not, the missing package(s) will be installed from CRAN and then loaded.

source("scripts/plotnik_functions.R")

# read in data -------------
#data_proforma <- read.delim("data/001_MO_20180709_traj_norm.txt",stringsAsFactors = TRUE) %>% rowid_to_column("position")
data_path <- "../speakers"

read.fn <- function(x) (read.delim(file.path(data_path, x),stringsAsFactors = TRUE) %>% rowid_to_column("position"))

FAVEfiles <- dir(data_path, pattern = "*_traj_norm.txt")


data_nested <- tibble(fileName = FAVEfiles) %>% # create a data frame holding the file names
  mutate(file_contents = map(fileName, read.fn)  # read files into a new data column 
  )
data_speakers <- unnest(data_nested,cols=c(file_contents)) %>% 
  dplyr::select(position,vowel,stress,word,style,norm_F1,norm_F2,dur,fm,fp,fv,ps,fs,fileName,F1.0.,F2.0.,F1.10.,F2.10.,F1.20.,F2.20.,F1.30.,F2.30.,F1.40.,F2.40.,F1.50.,F2.50.,F1.60.,F2.60.,F1.70.,F2.70.,F1.80.,F2.80.,F1.90.,F2.90.,F1.100.,F2.100.) %>%
  mutate(rowid_fac=factor(position)) %>%
  mutate(fileName=factor(fileName)) %>% 
  rowid_to_column("traj") %>%
  mutate(traj = factor(traj)) %>%
  rename(F1.11. = F1.0.) %>%
  rename(F1.99. = F1.100.) %>%
  rename(F2.11. = F2.0.) %>%
  rename(F2.99. = F2.100.) %>% 
  droplevels() %>% 
  as_tibble()

data_social = read_csv("../CoRP-master.csv"
                       #,stringsAsFactors=TRUE
                       #,fileEncoding = "UTF-8-BOM"
) %>% 
  mutate_if(is.character, factor) %>%
  mutate(region = recode(region,"North-East"="NE","South-East"="SE")) %>% 
  mutate(corpus = factor(paste(corpus, region, sep = "-")))

data_lexSets = read_delim("../../DataExtraction/LexicalSet_referenceList.txt"
                          # , stringsAsFactors = TRUE
                          , delim="\t"
) %>%
  droplevels() %>% 
  mutate_if(is.character, factor)

data_SUBTLEX <- read.delim("../SUBTLEX-UK.txt") %>%
  dplyr::select(Spelling,FreqCount,BNC_freq, LogFreq.Zipf., LogFreqBNC.Zipf.) %>% 
  mutate(word = Spelling) %>%
  mutate_if(is.character, str_to_upper)

data_lexSets = read.delim("../../DataExtraction/LexicalSet_referenceList.txt", stringsAsFactors = TRUE) %>%
  droplevels() %>% 
  filter(lexicalSet_broad != "") %>% 
  distinct()


## Merge Data ------------------------
data_all = data_speakers %>%
  inner_join(data_social) %>%
  inner_join(data_lexSets) %>% 
  mutate(fileName = factor(fileName)) %>%
  mutate(word = factor(word)) %>%
  mutate(vowel = factor(vowel)) %>%
  droplevels() %>%
  filter(lexicalSet_broad != "") %>%
  filter(lexicalSet_narrow != "") %>% 
  droplevels()


# Data Cleaning -----------------------------------------------------------------
#Recoding
data_clean = data_all %>%
  mutate(folMan = plt_manner.fn(fm)) %>%
  mutate(folPlace = plt_place.fn(fp)) %>%
  mutate(folVc = plt_voice.fn(fv)) %>%
  mutate(preSeg = plt_preseg.fn(ps)) %>%
  mutate(folSeq = plt_folseq.fn(fs))

data_goat_wide = data_clean %>%
  filter(lexicalSet_broad == "GOAT") %>%
  droplevels()

# pivot ---------------------------------------------------------------------
### pivot
data_goat_long = data_goat_wide %>%
  pivot_longer(cols = contains("."),
               names_to = c(".value", "measurement.no"), 
               names_pattern = "(F\\d).(\\d\\d).") %>%
  mutate(measurement.no = recode(measurement.no,"11"="00","99"="100",.default=levels(measurement.no))) %>%
  mutate(measurement.no = as.numeric(measurement.no)) %>%
  mutate(traj = as.numeric(traj)) %>%
  dplyr::select(traj,position,vowel,stress,word,norm_F1,norm_F2,dur,speakerNumber,firstName,sex,YOB,ageGroup,corpus,id,lexicalSet_narrow,lexicalSet_broad,word,vowel,style,folMan,folPlace,folVc,preSeg,folSeq,measurement.no,F1,F2)
  


## Outliers -----------------
# Outlier Functions
Q1.fn <- function(x){nth(fivenum(x,na.rm=TRUE), 2, order_by = NULL)}
Q3.fn <- function(x){nth(fivenum(x,na.rm=TRUE), 4, order_by = NULL)}
between.IQR.fn <- function(x){between(x, Q1.fn(x) - (1.5 * IQR(x, na.rm=TRUE)), 
                                      Q3.fn(x) + (1.5 * IQR(x, na.rm=TRUE)))}

data_goat = data_goat_long %>%
  filter(stress != "0") %>%
  filter(!word %in% c("DON'T","DONT","O","OH","OM","SO","Y'KNOW","JOB","OK","OKAY","WON'T","NO")) %>%
  filter(!str_detect(word,regex("^XX"))) %>%
  filter(!str_detect(word,regex("\\w+\\*"))) %>%
  filter(!str_detect(word,"-+")) %>% 
  group_by(corpus) %>% 
  group_by(lexicalSet_narrow) %>%
  filter(between.IQR.fn(norm_F1)) %>%
  filter(between.IQR.fn(norm_F2)) %>% 
  mutate(corpus_ord = factor(corpus, levels = c("CoRP-NE","CoRP-SE"), ordered = TRUE))

write_csv(data_goat,"data/data_goat.csv")
