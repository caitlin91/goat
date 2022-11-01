## Load and install packages, control options ####
## First specify the packages of interest
options(pillar.sigfig = 5)
packages = c("ggplot2","tibble","ruler","stats","broom","syllabifyr","vowels","readr","dplyr")

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


# Data ####
data_1 <- read.csv("data/data_goal-nonnorm-allrun1.csv")
data_2 <- read.csv("data/data_goal-nonnorm-allrun2.csv")

data_goal_nonnorm <- data_1 %>% 
  rbind(data_2) %>% 
  mutate(fileNameGoal=factor(fileNameGoal)) %>% 
  rowid_to_column("traj") %>%
  mutate(traj = factor(traj)) %>%
  dplyr::rename(F1.11.nn = F1.0.,
         F1.99.nn = F1.100.,
         F2.11.nn = F2.0.,
         F2.99.nn = F2.100.) %>%
  dplyr::rename(F1.10.nn = F1.10.,
         F1.20.nn=F1.20.,
         F1.30.nn=F1.30.,
         F1.40.nn=F1.40.,
         F1.50.nn=F1.50.,
         F1.50.nn=F1.50.,
         F1.60.nn=F1.60.,
         F1.70.nn=F1.70.,
         F1.80.nn=F1.80.,
         F1.90.nn=F1.90.) %>% 
  dplyr::rename(F2.10.nn = F2.10.,
         F2.20.nn=F2.20.,
         F2.30.nn=F2.30.,
         F2.40.nn=F2.40.,
         F2.50.nn=F2.50.,
         F2.50.nn=F2.50.,
         F2.60.nn=F2.60.,
         F2.70.nn=F2.70.,
         F2.80.nn=F2.80.,
         F2.90.nn=F2.90.) %>%
  rowid_to_column("position") %>% 
  mutate(rowid_fac=factor(position)) %>%
  mutate(word = str_to_upper(word)) %>% 
  droplevels()

# data_social = read_csv("../CoRP-master.csv"
#                        #,stringsAsFactors=TRUE
#                        #,fileEncoding = "UTF-8-BOM"
# ) %>% 
#   mutate_if(is.character, factor) %>%
#   mutate(region = recode(region,"North-East"="NE","South-East"="SE")) %>% 
#   mutate(corpus = factor(paste(corpus, region, sep = "-")))
# 
# data_SUBTLEX <- read.delim("../SUBTLEX-UK.txt") %>%
#   dplyr::select(Spelling,FreqCount,BNC_freq, LogFreq.Zipf., LogFreqBNC.Zipf.) %>% 
#   mutate(word = Spelling) %>%
#   mutate_if(is.character, str_to_upper)
# 
# data_lexSets = read.delim("../../DataExtraction/LexicalSet_referenceList.txt", stringsAsFactors = TRUE) %>%
#   droplevels() %>% 
#   filter(lexicalSet_broad != "") %>% 
#   distinct()

# Normalising ####
## *  point 0 (11) ####
data_11 <- data_goal_nonnorm %>% 
  select(fileNameGoal,vowel,F1.11.nn,F2.11.nn) %>%
  # rename(speaker_id=fileNameGoal,vowel_id=vowel,F1=F1.11.,F2=F2.11.) %>%
  # mutate(F1=as.numeric(F1),F2=as.numeric(F2)) %>%
  add_column(context=NA,.after="vowel") %>%
  add_column(F3=NA,F1_glide=NA,F2_glide=NA,F3_glide=NA)
  
data_11_norm <- norm.lobanov(data_11) %>% 
  scalevowels() %>% 
  select("Speaker", "Vowel", "F*1","F*2") %>%
  dplyr::rename(fileNameGoal = Speaker,
         vowel = Vowel) %>% 
  dplyr::rename(F1.11.="F*1",
         F2.11.="F*2") %>% 
  rowid_to_column("position")

## * points 10 & 60 ####
data_1060 <- data_goal_nonnorm %>% 
  select(fileNameGoal,vowel,F1.10.nn,F2.10.nn,F1.60.nn,F2.60.nn) %>%
  add_column(context="L",.after="vowel") %>%
  add_column(F3=NA,.after="F2.10.nn") %>% 
  add_column(F3_glide=NA,.after="F2.60.nn")

data_1060_norm <- norm.lobanov(data_1060) %>% 
  scalevowels() %>% 
  select("Speaker", "Vowel", "F*1","F*2","F*1 gl","F*2 gl") %>%
  dplyr::rename(fileNameGoal = Speaker,
         vowel = Vowel) %>% 
  dplyr::rename(F1.10. = "F*1",
         F2.10. = "F*2",
         F1.60. = "F*1 gl",
         F2.60. = "F*2 gl") %>% 
  rowid_to_column("position")

## * points 20 & 70 ####
data_2070 <- data_goal_nonnorm %>% 
  select(fileNameGoal,vowel,F1.20.nn,F2.20.nn,F1.70.nn,F2.70.nn) %>%
  add_column(context="L",.after="vowel") %>%
  add_column(F3=NA,.after="F2.20.nn") %>% 
  add_column(F3_glide=NA,.after="F2.70.nn")

data_2070_norm <- norm.lobanov(data_2070) %>% 
  scalevowels() %>% 
  select("Speaker", "Vowel", "F*1","F*2","F*1 gl","F*2 gl") %>%
  dplyr::rename(fileNameGoal = Speaker,
         vowel = Vowel) %>% 
  dplyr::rename(F1.20. = "F*1",
         F2.20. = "F*2",
         F1.70. = "F*1 gl",
         F2.70. = "F*2 gl") %>% 
  rowid_to_column("position")

## * points 30 & 80 ####
data_3080 <- data_goal_nonnorm %>% 
  select(fileNameGoal,vowel,F1.30.nn,F2.30.nn,F1.80.nn,F2.80.nn) %>%
  add_column(context="L",.after="vowel") %>%
  add_column(F3=NA,.after="F2.30.nn") %>% 
  add_column(F3_glide=NA,.after="F2.80.nn")

data_3080_norm <- norm.lobanov(data_3080) %>% 
  scalevowels() %>% 
  select("Speaker", "Vowel", "F*1","F*2","F*1 gl","F*2 gl") %>%
  dplyr::rename(fileNameGoal = Speaker,
         vowel = Vowel) %>% 
  dplyr::rename(F1.30. = "F*1",
         F2.30. = "F*2",
         F1.80. = "F*1 gl",
         F2.80. = "F*2 gl") %>% 
  rowid_to_column("position")

## * points 40 & 99 ####
data_4090 <- data_goal_nonnorm %>% 
  select(fileNameGoal,vowel,F1.40.nn,F2.40.nn,F1.90.nn,F2.90.nn) %>%
  add_column(context="L",.after="vowel") %>%
  add_column(F3=NA,.after="F2.40.nn") %>% 
  add_column(F3_glide=NA,.after="F2.90.nn")

data_4090_norm <- norm.lobanov(data_4090) %>% 
  scalevowels() %>% 
  select("Speaker", "Vowel", "F*1","F*2","F*1 gl","F*2 gl") %>%
  dplyr::rename(fileNameGoal = Speaker,
         vowel = Vowel) %>% 
  dplyr::rename(F1.40. = "F*1",
         F2.40. = "F*2",
         F1.90. = "F*1 gl",
         F2.90. = "F*2 gl") %>% 
  rowid_to_column("position")

## * points 50 & 100 (99) ####
data_5099 <- data_goal_nonnorm %>% 
  select(fileNameGoal,vowel,F1.50.nn,F2.50.nn,F1.99.nn,F2.99.nn) %>%
  add_column(context="L",.after="vowel") %>%
  add_column(F3=NA,.after="F2.50.nn") %>% 
  add_column(F3_glide=NA,.after="F2.99.nn")

data_5099_norm <- norm.lobanov(data_5099) %>% 
  scalevowels() %>% 
  select("Speaker", "Vowel", "F*1","F*2","F*1 gl","F*2 gl") %>%
  dplyr::rename(fileNameGoal = Speaker,
         vowel = Vowel) %>% 
  dplyr::rename(F1.50. = "F*1",
         F2.50. = "F*2",
         F1.99. = "F*1 gl",
         F2.99. = "F*2 gl") %>% 
  rowid_to_column("position")


data_goal_norm <- data_11_norm %>% 
  inner_join(data_1060_norm,by=c("position","vowel","fileNameGoal")) %>% 
  inner_join(data_2070_norm,by=c("position","vowel","fileNameGoal"))%>%
  inner_join(data_3080_norm,by=c("position","vowel","fileNameGoal")) %>% 
  inner_join(data_4090_norm,by=c("position","vowel","fileNameGoal"))%>% 
  inner_join(data_5099_norm,by=c("position","vowel","fileNameGoal")) %>% 
  inner_join(data_goal_nonnorm) %>% 
  dplyr::select(-F1.11.nn,-F1.10.nn,-F1.20.nn,-F1.30.nn,-F1.40.nn,-F1.50.nn,-F1.60.nn,-F1.70.nn,-F1.80.nn,-F1.90.nn,-F1.99.nn,
                -F2.11.nn,-F2.10.nn,-F2.20.nn,-F2.30.nn,-F2.40.nn,-F2.50.nn,-F2.60.nn,-F2.70.nn,-F2.80.nn,-F2.90.nn,-F2.99.nn)

write.csv(data_goal_norm,"data/data_goal_norm.csv",row.names = FALSE)
