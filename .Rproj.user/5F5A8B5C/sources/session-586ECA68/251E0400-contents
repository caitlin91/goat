## First specify the packages of interest
packages = c("tidyverse","ruler","broom","syllabifyr")

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
data_path <- "../speakers"
FAVEfiles <- dir(data_path, pattern = "*_traj_norm.txt")

read.fn <- function(x) (read.delim(file.path(data_path, x),stringsAsFactors = TRUE,fileEncoding = "UTF-8-BOM") %>% rowid_to_column())
data_nested <- tibble(fileName = FAVEfiles) %>% # create a data frame holding the file names
  mutate(file_contents = map(fileName, read.fn)  # read files into a new data column 
  )
data_speakers <- unnest(data_nested,cols=c(file_contents)) %>%
  dplyr::select(rowid,vowel,stress,word,t,style,norm_F1,norm_F2, dur,fm,fp,fv,ps,fs,fileName) %>%
  rowid_to_column("rowNumber_all") %>%
  mutate(time = t) %>% 
  mutate(rowid_fac=factor(rowid)) %>%
  mutate(rowNumber_all_fac = factor(rowNumber_all)) %>%
  mutate(fileName=factor(fileName)) %>% 
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



data_all <-  data_speakers %>%
  inner_join(data_social) %>%
  inner_join(data_lexSets) %>% 
  left_join(data_SUBTLEX) %>% 
  mutate(fileName = factor(fileName)) %>%
  mutate(word = factor(word)) %>%
  mutate(vowel = factor(vowel)) %>%
  droplevels() %>%
  filter(lexicalSet_broad != "")

# data_SDoutliers = data_all %>%
#   group_by(lexicalSet_broad) %>%
#   filter(between(norm_F1, mean(norm_F1, na.rm=TRUE) - (2.5 * sd(norm_F1, na.rm=TRUE)), 
#                  mean(norm_F1, na.rm=TRUE) + (2.5 * sd(norm_F1, na.rm=TRUE)))) %>%
#   filter(between(norm_F2, mean(norm_F2, na.rm=TRUE) - (2.5 * sd(norm_F2, na.rm=TRUE)), 
#                  mean(norm_F2, na.rm=TRUE) + (2.5 * sd(norm_F2, na.rm=TRUE))))

Q1.fn <- function(x){nth(fivenum(x,na.rm=TRUE), 2, order_by = NULL)}
Q3.fn <- function(x){nth(fivenum(x,na.rm=TRUE), 4, order_by = NULL)}
between.IQR.fn <- function(x){between(x, Q1.fn(x) - (1.5 * IQR(x, na.rm=TRUE)), 
                                      Q3.fn(x) + (1.5 * IQR(x, na.rm=TRUE)))}


# filter(between(norm_F1,  - (1.5*IQR(norm_F1, na.rm=TRUE)),
#                norm_F1, nth(fivenum(data_all$norm_F1,na.rm=TRUE), 2, order_by = NULL, default = default_missing(x)) + (1.5*IQR(norm_F1, na.rm=TRUE))))

data_IQRoutliers = data_all %>%
  filter(stress == "1") %>%
  filter(!word %in% c("ABOUT", "AND", "BUT", "FOR", "HE", "HE'S", "HUH", "I", "I'LL", "I'M", "IS", "IT", "IT'S", "ITS", "MY", "OF", "OH", "SHE", "SHE'S", "THAT", "THE", "THEM", "THEN", "THERE", "THEY", "THIS", "UH", "UM", "UP", "WAS", "WE", "WERE", "WHAT", "YEAH", "YOU", "AH", "ARE", "LA","CAUSE","ON","COS","CA-","HAHAHA","AN","A","DOS","OU","EW","GOTTA","GONNA","OKAY", "A", "AN", "BY", "BE", "GOT", "BUT", "ARE", "AH", "'CAUSE", "DID", "DIDN'T", "DO", "DUNNO", "GOTTA", "LOT", "EC", "EE", "HUH", "G", "ING", "UM", "LG}UM", "UH", "UM", "O","JUST","US","SOMEWHAT" )) %>%
  filter(!str_detect(word,regex("^XX"))) %>%
  filter(!str_detect(word,regex("\\w+\\*"))) %>%
  droplevels() %>% 
  group_by(lexicalSet_broad,corpus) %>%
  # filter(between(norm_F1, Q1.fn(norm_F1) - (1.5 * IQR(norm_F1, na.rm=TRUE)), 
  # Q3.fn(norm_F1) + (1.5 * IQR(norm_F1, na.rm=TRUE)))) %>%
  filter(between.IQR.fn(norm_F1)) %>%
  filter(between.IQR.fn(norm_F2))


# data_outliers = inner_join(data_all,data_tbl)
data_clean = data_IQRoutliers %>%
  mutate(folMan = plt_manner.fn(fm)) %>%
  mutate(folPlace = plt_place.fn(fp)) %>%
  mutate(folVc = plt_voice.fn(fv)) %>%
  mutate(preSeg = plt_preseg.fn(ps)) %>%
  mutate(folSeq = plt_folseq.fn(fs)) %>% 
  dplyr::select(rowNumber_all,rowid,word,style,time,norm_F1,norm_F2, dur, rowid_fac, rowNumber_all_fac, lastName, sex, YOB, ageGroup, region, speakerNumber, id, juniorEd, SecondEd, sixthFEd, privateTotal, occupation, occClass, edLevel, fatherRegion, motherRegion, fatherEd, motherEd, motherEdLevel,fatherEdLevel,parentEdLevelHigh,motherOccupation,fatherOccupation,parentOccClass,corpus,lexicalSet_broad,lexicalSet_narrow,folMan,folPlace,folVc,preSeg,folSeq, FreqCount,BNC_freq, LogFreq.Zipf., LogFreqBNC.Zipf.) %>% 
  mutate(folSeq = recode(folSeq, "none" = "none", "oneSyll" = "oneSyll", "twoSyll" = "twoSyll", "complxcoda" = "complxcoda", "compcoda_onesyll" = "compcoda_sylls", "compcoda_twosyll" = "compcoda_sylls")) %>%
  mutate(folSeq_small = recode(folSeq, "none" = "none", "oneSyll" = "Syll", "twoSyll" = "Syll", "complxcoda" = "complxcoda", "compcoda_sylls" = "Syll")) %>% 
  ungroup() %>% 
  mutate(id = paste(id, corpus, sep = "_"))

data_FSTG <- data_clean %>%
  filter(lexicalSet_broad %in% c("FOOT","STRUT","THOUGHT","GOOSE")) %>% 
  mutate(lexSet = lexicalSet_broad) %>% 
  mutate(id = paste(id, corpus, sep = "_"))

data_FS <- data_FSTG %>% 
  filter(lexSet %in% c("FOOT","STRUT"))

data_T <- data_FSTG %>% 
  filter(lexSet == "THOUGHT")
  
data_G <- data_FSTG %>% 
    filter(lexSet == "GOOSE")


# numbers of levels #### remove if 3 or more orders of magnitude smaller than the largest

## folMan ####
table(data_FS$folMan) # lose "none"
table((data_FS %>% filter(lexSet=="STRUT"))$folMan) # lose "none"
table((data_FS %>% filter(lexSet=="FOOT"))$folMan) # lose central, nasal, none
table((data_FS %>% filter(corpus=="CoRP-SE"))$folMan)
table((data_FS %>% filter(corpus=="CoRP-NE"))$folMan)


## folPlace ####
table(data_FS$folPlace) #lose none #conflate to labial/coronal/dorsal
table((data_FS %>% filter(lexSet == "STRUT"))$folPlace) # lose "none"
table((data_FS %>% filter(lexSet == "FOOT"))$folPlace) # lose "labial" "none"
table((data_FS %>% filter(corpus=="CoRP-SE"))$folMan)
table((data_FS %>% filter(corpus=="CoRP-NE"))$folMan)

## folVc ####
table(data_FS$folVc)
table((data_FS %>% filter(lexSet =="STRUT"))$folVc)
table((data_FS %>% filter(lexSet =="FOOT"))$folVc)
table((data_FS %>% filter(corpus=="CoRP-SE"))$folMan)
table((data_FS %>% filter(corpus=="CoRP-NE"))$folMan)

## preSeg ####
table(data_FS$preSeg)
table((data_FS %>% filter(lexSet=="STRUT"))$preSeg)
table((data_FS %>% filter(lexSet=="FOOT"))$preSeg)
table((data_FS %>% filter(corpus=="CoRP-SE"))$folMan)
table((data_FS %>% filter(corpus=="CoRP-NE"))$folMan)


## folSeq_small ####
table(data_FS$folSeq_small)
table((data_FS %>% filter(lexSet == "STRUT"))$folSeq_small)
table((data_FS %>% filter(lexSet == "FOOT"))$folSeq_small) # lose two_fol_syl
table((data_FS %>% filter(corpus=="CoRP-SE"))$folMan)
table((data_FS %>% filter(corpus=="CoRP-NE"))$folMan)


#### filtering out small levels ####

data_FS <- data_FS %>%
  droplevels() %>% 
  mutate(folPlace = recode_factor(folPlace, "apical" = "coronal", "interdental" = "coronal", "labiodental" = "labial", "palatal"="coronal", "velar"="dorsal")) %>% 
  filter(!folMan %in% c("central", "nasal", "none")) %>% 
  filter(!folPlace %in% c("labial","none")) %>% 
  filter(folSeq_small != "two_fol_syl") %>% 
  filter(preSeg !="w/y") %>% #removed because most tokens are "WOULD"  
  mutate(preSeg_small = recode_factor(preSeg, "liquid" = "liquid", "nasal_apical" = "stop","nasal_labial" = "stop","obstruent_liquid" = "obstruent-liquid","oral_apical" = "stop","oral_labial" = "stop","palatal"="SH/JH","velar"="stop"))

data_T <- data_T %>% 
  droplevels() %>% 
  mutate(folPlace = recode_factor(folPlace, "apical" = "coronal", "interdental" = "coronal", "labiodental" = "labial", "palatal"="coronal", "velar"="dorsal")) %>% 
  filter(!folMan %in% c("central", "nasal", "none")) %>% 
  filter(!folPlace %in% c("labial","none")) %>% 
  filter(folSeq_small != "two_fol_syl") %>% 
  filter(preSeg !="w/y") %>% #removed because most tokens are "WOULD"  
  mutate(preSeg_small = recode_factor(preSeg, "liquid" = "liquid", "nasal_apical" = "stop","nasal_labial" = "stop","obstruent_liquid" = "obstruent-liquid","oral_apical" = "stop","oral_labial" = "stop","palatal"="SH/JH","velar"="stop"))

data_G <- data_G %>% 
  mutate(folPlace = recode_factor(folPlace, "apical" = "coronal", "interdental" = "coronal", "labiodental" = "labial", "palatal"="coronal", "velar"="dorsal")) %>% 
  filter(!folMan %in% c("central", "nasal", "none")) %>% 
  filter(!folPlace %in% c("labial","none")) %>% 
  filter(folSeq_small != "two_fol_syl") %>% 
  filter(preSeg !="w/y") %>% #removed because most tokens are "WOULD"  
  mutate(preSeg_small = recode_factor(preSeg, "liquid" = "liquid", "nasal_apical" = "stop","nasal_labial" = "stop","obstruent_liquid" = "obstruent-liquid","oral_apical" = "stop","oral_labial" = "stop","palatal"="SH/JH","velar"="stop"))

data_schwa <- data_all %>% 
  filter(lexicalSet_broad == "schwa") %>% 
  filter(!word %in% c("ABOUT", "AND", "BUT", "FOR", "HE", "HE'S", "HUH", "I", "I'LL", "I'M", "IS", "IT", "IT'S", "ITS", "MY", "OF", "OH", "SHE", "SHE'S", "THAT", "THE", "THEM", "THEN", "THERE", "THEY", "THIS", "UH", "UM", "UP", "WAS", "WE", "WERE", "WHAT", "YEAH", "YOU", "AH", "ARE", "LA","CAUSE","ON","COS","CA-","HAHAHA","AN","A","DOS","OU","EW","GOTTA","GONNA","OKAY", "A", "AN", "BY", "BE", "GOT", "BUT", "ARE", "AH", "'CAUSE", "DID", "DIDN'T", "DO", "DUNNO", "GOTTA", "LOT", "EC", "EE", "HUH", "G", "ING", "UM", "LG}UM", "UH", "UM", "O","JUST","US","SOMEWHAT" )) %>%
  filter(!str_detect(word,regex("^XX"))) %>%
  filter(!str_detect(word,regex("\\w+\\*"))) %>%
  droplevels() %>% 
  group_by(lexicalSet_broad,corpus) %>%
  # filter(between(norm_F1, Q1.fn(norm_F1) - (1.5 * IQR(norm_F1, na.rm=TRUE)), 
  # Q3.fn(norm_F1) + (1.5 * IQR(norm_F1, na.rm=TRUE)))) %>%
  filter(between.IQR.fn(norm_F1)) %>%
  filter(between.IQR.fn(norm_F2)) %>% 
  mutate(folMan = plt_manner.fn(fm)) %>%
  mutate(folPlace = plt_place.fn(fp)) %>%
  mutate(folVc = plt_voice.fn(fv)) %>%
  mutate(preSeg = plt_preseg.fn(ps)) %>%
  mutate(folSeq = plt_folseq.fn(fs)) %>% 
  dplyr::select(rowNumber_all,rowid,word,style,time,norm_F1,norm_F2,dur, rowid_fac, rowNumber_all_fac, lastName, sex, YOB, ageGroup, region, speakerNumber, id, juniorEd, SecondEd, sixthFEd, privateTotal, occupation, occClass, edLevel, fatherRegion, motherRegion, fatherEd, motherEd, motherEdLevel,fatherEdLevel,parentEdLevelHigh,motherOccupation,fatherOccupation,parentOccClass,corpus,lexicalSet_broad,lexicalSet_narrow,folMan,folPlace,folVc,preSeg,folSeq, FreqCount,BNC_freq, LogFreq.Zipf., LogFreqBNC.Zipf.) %>% 
  mutate(folSeq = recode(folSeq, "none" = "none", "oneSyll" = "oneSyll", "twoSyll" = "twoSyll", "complxcoda" = "complxcoda", "compcoda_onesyll" = "compcoda_sylls", "compcoda_twosyll" = "compcoda_sylls")) %>%
  mutate(folSeq_small = recode(folSeq, "none" = "none", "oneSyll" = "Syll", "twoSyll" = "Syll", "complxcoda" = "complxcoda", "compcoda_sylls" = "Syll")) %>% 
  ungroup() %>% 
  mutate(lexSet = lexicalSet_broad) %>% 
  droplevels() %>% 
  mutate(id = paste(id, corpus, sep = "_")) %>% 
  mutate(folPlace = recode_factor(folPlace, "apical" = "coronal", "interdental" = "coronal", "labiodental" = "labial", "palatal"="coronal", "velar"="dorsal")) %>% 
  filter(!folMan %in% c("central", "nasal", "none")) %>% 
  filter(!folPlace %in% c("labial","none")) %>% 
  filter(folSeq_small != "two_fol_syl") %>% 
  filter(preSeg !="w/y") %>% #removed because most tokens are "WOULD"  
  mutate(preSeg_small = recode_factor(preSeg, "liquid" = "liquid", "nasal_apical" = "stop","nasal_labial" = "stop","obstruent_liquid" = "obstruent-liquid","oral_apical" = "stop","oral_labial" = "stop","palatal"="SH/JH","velar"="stop"))

data_FST <- rbind(data_FS,data_T, data_schwa)

data_ST <- data_FST %>% 
  filter(lexSet != "FOOT")