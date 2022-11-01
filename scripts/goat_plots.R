## Load and install packages, control options ####
## First specify the packages of interest
packages = c("tidyverse","patchwork","plyr","svglite","tidymv","beepr")
  
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
# Data ####
data_goat <- read.csv("data/data_goat.csv",stringsAsFactors = TRUE) %>% 
  mutate(lexSet=recode_factor(lexicalSet_narrow, "hoe"="hope","hold"="hole","holder"="hole","hole"="hole","holey"="holey","holy"="holy","hope"="hope","wholly"="wholly")) %>% 
  filter(preSeg != "w/y") %>% 
  filter(lexSet != "wholly") %>%
  # mutate(corpus_ord = factor(corpus, ordered = TRUE)) %>% 
  mutate(traj= factor(traj)) %>%
  mutate(firstName = factor(firstName)) %>%
  mutate(id_ord = factor(id, ordered = TRUE)) %>%
  mutate(lexSet_ord = ordered(lexSet)) %>%
  mutate(sex_ord = ordered(sex)) %>%
  mutate(ageGroup_ord = ordered(ageGroup)) %>%
  mutate(style_ord = ordered(style)) %>%
  mutate(word_ord = ordered(word)) %>%
  mutate(folMan_ord = ordered(folMan)) %>%
  mutate(folPlace_ord = ordered(folPlace)) %>%
  mutate(folVc_ord = ordered(folVc)) %>%
  mutate(preSeg_ord = ordered(preSeg)) %>%
  mutate(folSeq_ord = ordered(folSeq)) %>%
  filter(style %in% c("interview","minimalpair","wordlist")) %>%
  droplevels() %>%
  ungroup() %>% 
  mutate(age_sex = ordered(interaction(ageGroup, sex))) %>% 
  mutate(age_corp = ordered(interaction(ageGroup, corpus))) %>% 
  mutate(age_lexSet = ordered(interaction(ageGroup, lexSet))) %>% 
  mutate(age_sex_corp = ordered(interaction(ageGroup, sex, corpus))) %>% 
  mutate(age_sex_lexSet = ordered(interaction(ageGroup, sex, lexSet))) %>%
  mutate(age_sex_corp_lexSet = ordered(interaction(ageGroup, sex, corpus, lexSet))) %>% 
  mutate(sex_corp = ordered(interaction(sex, corpus))) %>% 
  mutate(sex_lexSet = ordered(interaction(sex, lexSet))) %>%
  mutate(sex_corp_lexSet = ordered(interaction(sex, corpus, lexSet))) %>%
  mutate(corpus_lexSet = ordered(interaction(corpus, lexSet)))

hope.F1.gamm.p <- read.csv("data/hope-F1-gamm-p.csv",stringsAsFactors=TRUE)
hope.F2.gamm.p <- read.csv("data/hope-F2-gamm-p.csv",stringsAsFactors=TRUE)

mono.SE.F1.gamm.p <- read.csv("data/mono-SE-F1-gamm-p.csv",stringsAsFactors = TRUE)
mono.SE.F2.gamm.p <- read.csv("data/mono-SE-F2-gamm-p.csv",stringsAsFactors = TRUE)
mono.DE.F1.gamm.p <- read.csv("data/mono-DE-F1-gamm-p.csv",stringsAsFactors = TRUE)
mono.DE.F2.gamm.p <- read.csv("data/mono-DE-F2-gamm-p.csv",stringsAsFactors = TRUE)
mono.NE.F1.gamm.p <- read.csv("data/mono-NE-F1-gamm-p.csv",stringsAsFactors = TRUE)
mono.NE.F2.gamm.p <- read.csv("data/mono-NE-F2-gamm-p.csv",stringsAsFactors = TRUE)

goal.SE.F1.gamm.p <- read.csv("data/goal-SE-F1-gamm-p.csv",stringsAsFactors = TRUE)
goal.SE.F2.gamm.p <- read.csv("data/goal-SE-F2-gamm-p.csv",stringsAsFactors = TRUE)
goal.SE.F2.gamm.less.p <- read.csv("data/goal-SE-F2-gamm-less-p.csv",stringsAsFactors = TRUE)
goal.DE.F1.gamm.p <- read.csv("data/goal-DE-F1-gamm-p.csv",stringsAsFactors = TRUE)
goal.DE.F1.gamm.less.p <- read.csv("data/goal-DE-F1-gamm-less-p.csv",stringsAsFactors = TRUE)
goal.DE.F2.gamm.p <- read.csv("data/goal-DE-F2-gamm-p.csv",stringsAsFactors = TRUE)
goal.NE.F1.gamm.p <- read.csv("data/goal-NE-F1-gamm-p.csv",stringsAsFactors = TRUE)
goal.NE.F2.gamm.p <- read.csv("data/goal-NE-F2-gamm-p.csv",stringsAsFactors = TRUE)

data_goal = data_goat %>%
  filter(!lexicalSet_narrow %in%  c("hoe","hope")) %>% 
  mutate(lexSet=recode_factor(lexicalSet_narrow, "hold"="hole","holder"="hole","hole"="hole","holey"="holey","holy"="holy","hope"="hope","wholly"="wholly")) %>% 
  filter(lexSet != "wholly") %>%
  mutate(traj= factor(traj)) %>%
  mutate(id_ord = factor(id, ordered = TRUE)) %>%
  mutate(lexSet_ord = ordered(lexSet)) %>%
  mutate(sex_ord = ordered(sex)) %>%
  mutate(ageGroup_ord = ordered(ageGroup)) %>%
  mutate(style_ord = ordered(style)) %>%
  mutate(word_ord = ordered(word)) %>%
  droplevels() %>%
  ungroup() %>% 
  mutate(age_sex = ordered(interaction(ageGroup, sex))) %>% 
  mutate(age_corp = ordered(interaction(ageGroup, corpus))) %>% 
  mutate(age_lexSet = ordered(interaction(ageGroup, lexSet))) %>% 
  mutate(age_sex_corp = ordered(interaction(ageGroup, sex, corpus))) %>% 
  mutate(age_sex_lexSet = ordered(interaction(ageGroup, sex, lexSet))) %>%
  mutate(age_sex_corp_lexSet = ordered(interaction(ageGroup, sex, corpus, lexSet))) %>% 
  mutate(sex_corp = ordered(interaction(sex, corpus))) %>% 
  mutate(sex_lexSet = ordered(interaction(sex, lexSet))) %>%
  mutate(sex_corp_lexSet = ordered(interaction(sex, corpus, lexSet))) %>%
  mutate(corpus_lexSet = ordered(interaction(corpus, lexSet)))

  
# Colours --------------------
# goatcolours
hopecol <- "#FF3200"
holecol <- "#172869"
holycol <- "#E9A17C"
holeycol <- "#1BB6AF"
  
goatColours <- c("#172869"
                 , "#1BB6AF"
                 ,"#de8c62"
                 #,"#0076BB"
                 , "#FF3200"
)

names(goatColours) = levels(data_goat$lexSet_ord)
goatFill <- scale_fill_manual(name = "lexSet_ord",values = goatColours)
goatCol <- scale_colour_manual(name = "lexSet_ord",values=goatColours)

# regioncolours
regionColours <- c("#77b144","#524c95","#95524C")
NEcol <- "#77b144"
SEcol <- "#524c95"
DEcol <- "#95524c"
names(regionColours) = levels(data_goat$corpus)
regionFillScale <- scale_fill_manual(name = "corpus",values = regionColours)
regionColScale <- scale_colour_manual(name = "corpus",values=regionColours)

# lines
oldline <- "dotdash"
youngline <- "dashed"

femaleline <- "twodash"
maleline <- "longdash"

hopeline <- "solid"
holeline <- "dotted"
holeyline <- "dotdash"
holyline <- "dashed"
  
  
  
# Themes ####
theme_Caitlin_present <- function() {theme_bw(base_size = 22) %+replace%
    theme(plot.background  = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill="gray90", colour=NA),
          legend.background = element_rect(fill="transparent", colour=NA),
          legend.key = element_rect(fill="transparent", colour=NA),
          panel.grid.major = element_line(colour = "white", size = 0.2), 
          panel.grid.minor = element_line(colour = "grey98", size = 0.5))}

theme_Caitlin <- function() {theme_bw(base_size = 12) %+replace%
    theme(plot.background  = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill="transparent", colour=NA),
          legend.background = element_rect(fill="transparent", colour=NA),
          legend.key = element_rect(fill="transparent", colour=NA),
          panel.grid.major = element_line(colour = "grey80", size = 0.2), 
          panel.grid.minor = element_line(colour = "grey90", size = 0.5)
    )}
theme_Caitlin_gamm <- function() {theme_bw(base_size = 11) %+replace%
    theme(plot.background  = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill="gray90", colour=NA),
          legend.background = element_rect(fill="transparent", colour=NA),
          legend.key = element_rect(fill="transparent", colour=NA),
          panel.grid.major = element_line(colour = "white", size = 0.2), 
          panel.grid.minor = element_line(colour = "grey98", size = 0.5),
          # legend.key.size = unit(0.25, 'cm'), #change legend key size
          # legend.key.height = unit(1, 'cm'), #change legend key height
          # legend.key.width = unit(1, 'cm'), #change legend key width
          legend.title = element_text(size=11), #change legend title font size
          legend.text = element_text(size=7.5) #change legend text font size
          )}

theme_Caitlin_gammpresent <- function() {theme_bw(base_size = 18) %+replace%
    theme(plot.background  = element_rect(fill = "transparent", colour = NA),
          panel.background = element_rect(fill="gray90", colour=NA),
          legend.background = element_rect(fill="transparent", colour=NA),
          legend.key = element_rect(fill="transparent", colour=NA),
          panel.grid.major = element_line(colour = "white", size = 0.2), 
          panel.grid.minor = element_line(colour = "grey98", size = 0.5))}

yaxisF1 <- scale_y_continuous(limits = c(300,800), breaks = seq(0,1200,100))
yaxisF2 <- scale_y_continuous(limits = c(300,2000), breaks = seq(0,2500,250))
xaxisall <-  scale_x_continuous(breaks = seq(0,100,50))



# hope -------------------------------------------------

# *F1 --------------------------
hope.F1.gamm.p <- hope.F1.gamm.p %>% 
  mutate(speaker.group=recode(sex_corp, "Female.CoRP-NE" = "CoRP-NE",
                              "Female.CoRP-SE"="CoRP-SE",
                              "Female.DECTE-NE"="DECTE",
                              "Male.CoRP-NE" = "CoRP-NE",
                              "Male.CoRP-SE"="CoRP-SE",
                              "Male.DECTE-NE"="DECTE"))
hope.F1.gamm.p$speaker.group <- factor(hope.F1.gamm.p$speaker.group, levels = c("CoRP-SE","CoRP-NE","DECTE"))
hope.F1.gamm.p$sex_corp <- factor(hope.F1.gamm.p$sex_corp, levels = c("Female.CoRP-SE","Male.CoRP-SE", "Female.CoRP-NE","Male.CoRP-NE","Female.DECTE-NE","Male.DECTE-NE"))


hope.F1.gamm.plot <- ggplot(data = hope.F1.gamm.p,
                            aes(
                              x=measurement.no,
                              y=fit
                              # ,linecolour = lexSet_ord
                              # ,fill=lexSet
                              # ,colour = lexSet_ord
                              )
                            )+
  theme_Caitlin_gamm()+
  geom_smooth_ci(sex_corp)+
  scale_linetype_manual(values=c(femaleline, maleline, femaleline,maleline,femaleline,maleline), name = "Corpus, Sex", labels = c("CoRP-SE, Female","CoRP-SE, Male", "CoRP-NE, Female","CoRP-NE, Male","DECTE, Female","DECTE-NE, Male")
                        )+
  scale_color_manual(values=c(SEcol,SEcol,NEcol,NEcol,DEcol,DEcol),name="Corpus, Sex", labels = c("CoRP-SE, Female","CoRP-SE, Male", "CoRP-NE, Female","CoRP-NE, Male","DECTE, Female","DECTE-NE, Male")
                     )+
  xaxisall +   xlab("Percentage duration through the vowel")+
  yaxisF1 +   ylab("Model fit F1 (Hz)") +   ylab("Model fit F1 (Hz)")+
  facet_wrap(~speaker.group,ncol = 3)+
  # scale_fill_discrete(breaks=legend_ord) +
  NULL
hope.F1.gamm.plot
ggsave("figures/hope-F1-gamm-plot.svg",hope.F1.gamm.plot,height=4,width=6,units="in")


# *F2 --------------------------
hope.F2.gamm.p <- hope.F2.gamm.p %>% 
  mutate(corpus=recode(age_sex_corp,
                       "Old.Female.CoRP-NE" = "CoRP-NE",
                       "Old.Female.CoRP-SE"="CoRP-SE",
                       "Old.Female.DECTE-NE"="DECTE",
                       "Old.Male.CoRP-NE"="CoRP-NE",
                       "Old.Male.CoRP-SE"="CoRP-SE",
                       "Old.Male.DECTE-NE"="DECTE",
                       "Young.Female.CoRP-NE"="CoRP-NE",
                       "Young.Female.CoRP-SE"="CoRP-SE"
                       ,"Young.Male.CoRP-NE"="CoRP-NE",
                       "Young.Male.CoRP-SE"="CoRP-SE",
                       "Young.Male.DECTE-NE"="DECTE")) %>% 
  mutate(ageGroup=recode(age_sex_corp,
                         "Old.Female.CoRP-NE" = "Old",
                         "Old.Female.CoRP-SE"="Old",
                         "Old.Female.DECTE-NE"="Old",
                         "Old.Male.CoRP-NE"="Old",
                         "Old.Male.CoRP-SE"="Old",
                         "Old.Male.DECTE-NE"="Old",
                         "Young.Female.CoRP-NE"="Young",
                         "Young.Female.CoRP-SE"="Young",
                         "Young.Male.CoRP-NE"="Young",
                         "Young.Male.CoRP-SE"="Young",
                         "Young.Male.DECTE-NE"="Young")) %>% 
  mutate(sex =recode(age_sex_corp, 
                     "Old.Female.CoRP-NE" = "Female",
                     "Old.Female.CoRP-SE"="Female",
                     "Old.Female.DECTE-NE"="Female",
                     "Old.Male.CoRP-NE"="Male",
                     "Old.Male.CoRP-SE"="Male",
                     "Old.Male.DECTE-NE"="Male",
                     "Young.Female.CoRP-NE"="Female",
                     "Young.Female.CoRP-SE"="Female",
                     "Young.Male.CoRP-NE"="Male",
                     "Young.Male.CoRP-SE"="Male",
                     "Young.Male.DECTE-NE"="Male"))

hope.F2.gamm.p$corpus <- factor(hope.F2.gamm.p$corpus, levels = c("CoRP-SE", "CoRP-NE","DECTE"))
hope.F2.gamm.p$age_sex_corp <- factor(hope.F2.gamm.p$age_sex_corp, levels = c("Old.Female.CoRP-SE","Old.Male.CoRP-SE","Young.Female.CoRP-SE","Young.Male.CoRP-SE","Old.Female.CoRP-NE","Old.Male.CoRP-NE","Young.Female.CoRP-NE","Young.Male.CoRP-NE","Old.Female.DECTE-NE","Old.Male.DECTE-NE","Young.Male.DECTE-NE"))

 


hope.F2.gamm.plot <- ggplot(data = hope.F2.gamm.p,
                            aes(
                              x=measurement.no,
                              y=fit
                              # ,linecolour = lexSet_ord
                              # ,fill=lexSet
                              # ,colour = lexSet_ord
                            )
)+
  theme_Caitlin_gamm()+
  geom_smooth_ci(age_sex_corp)+
  scale_linetype_manual(values=c(femaleline, maleline, femaleline, maleline,femaleline, maleline, femaleline, maleline,femaleline, maleline, maleline),
                        labels=c("Old, Female, CoRP-SE","Old, Male, CoRP-SE","Young, Female, CoRP-SE","Young, Male, CoRP-SE","Old, Female, CoRP-NE","Old, Male, CoRP-NE","Young, Female, CoRP-NE","Young, Male, CoRP-NE","Old, Female, DECTE-NE","Old, Male, DECTE-NE","Young, Male, DECTE-NE"),
                        name = "Speaker Group")+
  scale_color_manual(values=c(SEcol,SEcol,SEcol,SEcol,NEcol,NEcol,NEcol,NEcol,DEcol,DEcol,DEcol),
                     labels=c("Old, Female, CoRP-SE","Old, Male, CoRP-SE","Young, Female, CoRP-SE","Young, Male, CoRP-SE","Old, Female, CoRP-NE","Old, Male, CoRP-NE","Young, Female, CoRP-NE","Young, Male, CoRP-NE","Old, Female, DECTE-NE","Old, Male, DECTE-NE","Young, Male, DECTE-NE"),
                     name="Speaker Group")+
  facet_wrap(ageGroup~corpus)+
  xaxisall +   xlab("Percentage duration through the vowel")+
  yaxisF2 +   ylab("Model fit F2 (Hz)")+
  NULL
hope.F2.gamm.plot
ggsave("figures/hope-F2-gamm-plot.svg",hope.F2.gamm.plot,height=4,width=6,units="in")





# mono -------------------------------------------------

# *SE ####
# **F1 ####
mono.SE.F1.gamm.p <- mono.SE.F1.gamm.p %>%
  mutate(sex=recode(sex_lexSet, "Female.hole" = "Female","Female.hope"="Female","Male.hole" = "Male","Male.hope"="Male"))
  
mono.SE.F1.gamm.plot <- ggplot(data = mono.SE.F1.gamm.p,
                               aes(
                                 x=measurement.no,
                                 y=fit
                                 # linetype = sex_lexSet,
                                 # ,linecolour = sex_lexSet
                                 # fill = sex_lexSet
                                 # ,colour = sex_lexSet
                                 )
                               )+
  theme_Caitlin_gamm()+
  geom_smooth_ci(sex_lexSet)+
  scale_color_manual(values = c(holecol,hopecol,holecol,hopecol), name = "legend", labels = c("Female, HOLE", "Female, HOPE", "Male, HOLE", "Male, HOPE"))+
  scale_linetype_manual(values=c(holeline,hopeline,holeline,hopeline), name = "legend", labels = c("Female, HOLE", "Female, HOPE", "Male, HOLE", "Male, HOPE"))+
  xaxisall +   xlab("Percentage duration through the vowel")+
  yaxisF1 +   ylab("Model fit F1 (Hz)") +   ylab("Model fit F1 (Hz)")+
  facet_wrap(~sex)+
    NULL
mono.SE.F1.gamm.plot
ggsave("figures/mono-SE-F1-gamm-plot.svg",mono.SE.F1.gamm.plot,height=4,width=6,units="in")
  
# **F2 ####
mono.SE.F2.gamm.p <- mono.SE.F2.gamm.p %>% 
  mutate(ageGroup=recode(age_sex,"Old.Female" = "Old","Old.Male"="Old","Young.Female"="Young","Young.Male"="Young")) %>% 
  mutate(sex=recode(age_sex,"Old.Female" = "Female","Old.Male"="Male","Young.Female"="Female","Young.Male"="Male")) %>% 
  mutate(group = paste(age_sex, lexSet_ord, sep = "."))
  
mono.SE.F2.gamm.plot <- ggplot(data = mono.SE.F2.gamm.p,
                               aes(
                                 x=measurement.no,
                                 y=fit
                                 # ,linecolour = sex_lexSet
                                 # fill = sex_lexSet
                                 # ,colour = sex_lexSet
                                 )
                               )+
  theme_Caitlin_gamm()+
  geom_smooth_ci(lexSet_ord)+
  scale_linetype_manual(values=c(holeline,hopeline), name = "Lexical Set", labels = c("HOLE","HOPE"))+
  scale_color_manual(values=c(holecol,hopecol),name="Lexical Set", labels = c("HOLE","HOPE"))+
  facet_wrap(sex~ageGroup,ncol=2)+
  xaxisall +   xlab("Percentage duration through the vowel")+
  yaxisF2 +   ylab("Model fit F2 (Hz)")+
  NULL
mono.SE.F2.gamm.plot
ggsave("figures/mono-SE-F2-gamm-plot.svg",mono.SE.F2.gamm.plot,height=4,width=6,units="in")

mono.SE.F2.gamm.present <- ggplot(data = mono.SE.F2.gamm.p,
                             aes(
                               x=measurement.no,
                               y=fit
                               # ,linetype = age_sex
                               # fill = sex_lexSet
                               # ,colour = sex_lexSet
                             )
)+
  theme_Caitlin_gammpresent()+
  theme(legend.position = "none")+
  geom_smooth_ci(group)+
  scale_linetype_manual(values=c(holeline,hopeline,holeline,hopeline,holeline,hopeline,holeline,hopeline), name = "Lexical Set"
                        # , labels = c("HOLE","HOPE")
                        )+
  scale_color_manual(values=c(holecol,hopecol,holecol,hopecol,holecol,hopecol,holecol,hopecol),name="Lexical Set", 
                     # labels = c("HOLE","HOPE")
                     )+
  # facet_wrap(sex~ageGroup,ncol=2)+
  xaxisall +   xlab("Percentage duration through the vowel")+
  yaxisF2 +   ylab("Model fit F2 (Hz)")+
  NULL
mono.SE.F2.gamm.present
ggsave("figures/mono-SE-F2-gamm-present.png",mono.SE.F2.gamm.present,height=4,width=6,units="in")



  

# *DE ####
# **F1 ####
mono.DE.F1.gamm.p <- mono.DE.F1.gamm.p %>% 
  mutate(ageGroup=recode(age_lexSet, "Old.hole"="Old","Old.hope"="Old","Young.hope"="Young","Young.hole"="Young"))

mono.DE.F1.gamm.plot <- ggplot(data = mono.DE.F1.gamm.p,
                               aes(
                                 x=measurement.no,
                                 y=fit
                                 # linetype = sex_lexSet,
                                 # ,linecolour = sex_lexSet
                                 # fill = sex_lexSet
                                 # ,colour = sex_lexSet
                                 )
                               )+
  theme_Caitlin_gamm()+
  geom_smooth_ci(age_lexSet)+
  scale_color_manual(values = c(holecol,hopecol,holecol,hopecol), name = "legend",labels = c("HOLE","HOPE","HOLE","HOPE"))+
  scale_linetype_manual(values=c(holeline,hopeline,holeline,hopeline), name = "legend", labels = c("HOLE","HOPE","HOLE","HOPE"))+
  yaxisF1 +   ylab("Model fit F1 (Hz)") +   ylab("Model fit F1 (Hz)")+
  xaxisall +   xlab("Percentage duration through the vowel")+
  facet_wrap(~ageGroup)+
  NULL
mono.DE.F1.gamm.plot
ggsave("figures/mono-DE-F1-gamm-plot.svg",mono.DE.F1.gamm.plot,height=4,width=6,units="in")

# **F2 -----------------------------------------------
mono.DE.F2.gamm.p <- mono.DE.F2.gamm.p %>% 
  mutate(ageGroup=recode(age_sex_lexSet, "Old.Female.hope" = "Old","Old.Female.hole" = "Old","Old.Male.hope"="Old","Old.Male.hole"="Old","Young.Male.hope"="Young","Young.Male.hole"="Young")) %>% 
  mutate(sex=recode(age_sex_lexSet, "Old.Female.hope" = "Female","Old.Female.hole" = "Female","Old.Male.hope"="Male","Old.Male.hole"="Male","Young.Male.hope"="Male","Young.Male.hole"="Male"))


mono.DE.F2.gamm.plot <- ggplot(data = mono.DE.F2.gamm.p,
                               aes(
                                 x=measurement.no,
                                 y=fit
                                 # ,linecolour = sex_lexSet
                                 # fill = sex_lexSet
                                 # ,colour = sex_lexSet
                               )
)+
  theme_Caitlin_gamm()+
  geom_smooth_ci(age_sex_lexSet)+
  scale_linetype_manual(values=c(holeline, hopeline, holeline, hopeline, holeline,hopeline),name="Lexical Set", labels = c("HOLE","HOPE","HOLE","HOPE","HOLE","HOPE"))+
  scale_color_manual(values=c(holecol, hopecol, holecol, hopecol, holecol,hopecol),name="Lexical Set", labels = c("HOLE","HOPE","HOLE","HOPE","HOLE","HOPE"))+
  # facet_grid(col=vars(ageGroup),row=vars(sex))+
  facet_wrap(sex~ageGroup,ncol=2)+
  yaxisF2 +   ylab("Model fit F2 (Hz)")+
  xaxisall +   xlab("Percentage duration through the vowel")+
  NULL
mono.DE.F2.gamm.plot
ggsave("figures/mono-DE-F2-gamm-plot.svg",mono.DE.F2.gamm.plot,height=4,width=6,units="in")

mono.DE.F2.gamm.present <- ggplot(data = mono.DE.F2.gamm.p,
                             aes(
                               x=measurement.no,
                               y=fit
                               # ,linecolour = sex_lexSet
                               # fill = sex_lexSet
                               # ,colour = sex_lexSet
                             )
)+
  theme_Caitlin_gammpresent()+
  theme(legend.position = "none") +
  geom_smooth_ci(age_sex_lexSet)+
  scale_linetype_manual(values=c(holeline, hopeline, holeline, hopeline, holeline,hopeline),name="Lexical Set", labels = c("HOLE","HOPE","HOLE","HOPE","HOLE","HOPE"))+
  scale_color_manual(values=c(holecol, hopecol, holecol, hopecol, holecol,hopecol),name="Lexical Set", labels = c("HOLE","HOPE","HOLE","HOPE","HOLE","HOPE"))+
  # facet_grid(col=vars(ageGroup),row=vars(sex))+
  # facet_wrap(sex~ageGroup,ncol=2)+
  yaxisF2 +   ylab("Model fit F2 (Hz)")+
  xaxisall +   xlab("Percentage duration through the vowel")+
  NULL
mono.DE.F2.gamm.present
ggsave("figures/mono-DE-F2-gamm-present.png",mono.DE.F2.gamm.present,height=4,width=6,units="in")

# *NE -----------------------------------
# **F1 ####
mono.NE.F1.gamm.plot <- ggplot(data = mono.NE.F1.gamm.p,
                            aes(
                              x=measurement.no,
                              y=fit
                              )
)+
  theme_Caitlin_gamm()+
  geom_smooth_ci(lexSet_ord)+
  scale_linetype_manual(values=c(holeline,hopeline), name = "Lexical Set", labels = c("HOLE","HOPE"))+
  scale_color_manual(values=c(holecol,hopecol),name="Lexical Set", labels = c("HOLE","HOPE"))+
  yaxisF1 +   ylab("Model fit F1 (Hz)") +   ylab("Model fit F1 (Hz)")+
  xaxisall +   xlab("Percentage duration through the vowel")+
  NULL
mono.NE.F1.gamm.plot

ggsave("figures/mono-NE-F1-gamm-plot.svg",mono.NE.F1.gamm.plot,height=4,width=6,units="in")

# **F2
mono.NE.F2.gamm.p <- mono.NE.F2.gamm.p%>% 
  mutate(group = paste(ageGroup, sex, lexSet_ord, sep = "."))
mono.NE.F2.gamm.plot <- ggplot(data = mono.NE.F2.gamm.p,
                               aes(
                                 x=measurement.no,
                                 y=fit
                                 # ,linecolour = sex_lexSet
                                 # fill = sex_lexSet
                                 # ,colour = sex_lexSet
                               )
)+
  theme_Caitlin_gamm()+
  geom_smooth_ci(lexSet_ord)+
  scale_linetype_manual(values=c(holeline,hopeline), name = "Lexical Set", labels = c("HOLE","HOPE"))+
  scale_color_manual(values=c(holecol,hopecol),name="Lexical Set", labels = c("HOLE","HOPE"))+
  facet_wrap(sex~ageGroup,ncol=2)+
  yaxisF2 +   ylab("Model fit F2 (Hz)")+
  xaxisall +   xlab("Percentage duration through the vowel")+
  NULL
mono.NE.F2.gamm.plot

ggsave("figures/mono-NE-F2-gamm-plot.svg",mono.NE.F2.gamm.plot,height=4,width=6,units="in")

mono.NE.F2.gamm.present <- ggplot(data = mono.NE.F2.gamm.p,
                             aes(
                               x=measurement.no,
                               y=fit
                               # ,linecolour = sex_lexSet
                               # fill = sex_lexSet
                               # ,colour = sex_lexSet
                             )
)+
  theme_Caitlin_gammpresent()+
  theme(legend.position="none") +
  geom_smooth_ci(group)+
  scale_linetype_manual(values=c(holeline,hopeline,holeline,hopeline,holeline,hopeline,holeline,hopeline), name = "Lexical Set", labels = c("HOLE","HOPE"))+
  scale_color_manual(values=c(holecol,hopecol,holecol,hopecol,holecol,hopecol,holecol,hopecol),name="Lexical Set", labels = c("HOLE","HOPE"))+
  # facet_wrap(sex~ageGroup,ncol=2)+
  yaxisF2 +   ylab("Model fit F2 (Hz)")+
  xaxisall +   xlab("Percentage duration through the vowel")+
  NULL
mono.NE.F2.gamm.present
ggsave("figures/mono-NE-F2-gamm-present.png",mono.NE.F2.gamm.present,height=4,width=6,units="in")

mono.F2.gamm.present <- mono.SE.F2.gamm.present + mono.NE.F2.gamm.present + mono.DE.F2.gamm.present
ggsave("figures/mono-F2-gamm-present.png",mono.F2.gamm.present,height=4,width=6,units="in")


# *mono All -----------------------
# **F1
mono.F1.gamm.plot <- (mono.SE.F1.gamm.plot+theme(legend.position ="none")+ggtitle("CoRP-SE") + xaxisall +   xlab("Percentage duration through the vowel")) +
  (mono.NE.F1.gamm.plot+theme(legend.position ="none")+ggtitle("CoRP-NE") + xaxisall +   xlab("Percentage duration through the vowel"))+
  (mono.DE.F1.gamm.plot+ggtitle("DECTE") + xaxisall +   xlab("Percentage duration through the vowel")) +
  plot_layout(ncol = 3, widths = c(2,1,2))
mono.F1.gamm.plot
ggsave("figures/mono-F1-gamm-plot.svg",mono.F1.gamm.plot,height=8,width=12,units="in")

mono.F2.gamm.plot <- (mono.SE.F2.gamm.plot+theme(legend.position ="none")+ggtitle("CoRP-SE") + xaxisall +   xlab("Percentage duration through the vowel")) +
  (mono.NE.F2.gamm.plot+theme(legend.position ="none")+ggtitle("CoRP-NE") + xaxisall +   xlab("Percentage duration through the vowel")) +
  (mono.DE.F2.gamm.plot+ggtitle("DECTE") + xaxisall +   xlab("Percentage duration through the vowel"))
mono.F2.gamm.plot
ggsave("figures/mono-F2-gamm-plot.svg",mono.F2.gamm.plot,height=4,width=8,units="in")

# GOAL ----------------
# *SE---------------------
# ** F1 ####
goal.SE.F1.gamm.p <- goal.SE.F1.gamm.p %>%
  mutate(ageGroup=recode(age_sex_lexSet, "Old.Female.hole" = "Old", "Old.Female.holey" = "Old", "Old.Female.holy" = "Old","Old.Male.hole" = "Old", "Old.Male.holey" = "Old", "Old.Male.holy" = "Old","Young.Female.hole" = "Young", "Young.Female.holey" = "Young", "Young.Female.holy" = "Young","Young.Male.hole" = "Young", "Young.Male.holey" = "Young", "Young.Male.holy" = "Young")) %>% 
  mutate(sex=recode(age_sex_lexSet, "Old.Female.hole" = "Female", "Old.Female.holey" = "Female", "Old.Female.holy" = "Female","Old.Male.hole" = "Male", "Old.Male.holey" = "Male", "Old.Male.holy" = "Male","Young.Female.hole" = "Female", "Young.Female.holey" = "Female", "Young.Female.holy" = "Female","Young.Male.hole" = "Male", "Young.Male.holey" = "Male", "Young.Male.holy" = "Male")) %>% 
  mutate(lexical.set=recode(age_sex_lexSet, "Old.Female.hole" = "hole", "Old.Female.holey" = "holey", "Old.Female.holy" = "holy","Old.Male.hole" = "hole", "Old.Male.holey" = "holey", "Old.Male.holy" = "holy","Young.Female.hole" = "hole", "Young.Female.holey" = "holey", "Young.Female.holy" = "holy","Young.Male.hole" = "hole", "Young.Male.holey" = "holey", "Young.Male.holy" = "holy"))

goal.SE.F1.gamm.plot <- ggplot(data = goal.SE.F1.gamm.p,
                               aes(
                                 x=measurement.no,
                                 y=fit
                                 # linetype = sex_lexSet,
                                 # ,linecolour = sex_lexSet
                                 # fill = sex_lexSet
                                 # ,colour = sex_lexSet
                               )
)+
  theme_Caitlin_gamm()+
  geom_smooth_ci(age_sex_lexSet)+
  scale_colour_manual(name = "Lexical Set", labels = c("hole","holey","holy","hole","holey","holy","hole","holey","holy","hole","holey","holy")
                      ,values = c(holecol,holeycol,holycol,holecol,holeycol,holycol,holecol,holeycol,holycol,holecol,holeycol,holycol))+
  scale_linetype_manual(name = "Lexical Set", labels = c("hole","holey","holy","hole","holey","holy","hole","holey","holy","hole","holey","holy")
                        ,values=c(holeline,holeyline,holyline,holeline,holeyline,holyline,holeline,holeyline,holyline,holeline,holeyline,holyline))+
  yaxisF1 +   ylab("Model fit F1 (Hz)")+
  xaxisall +   xlab("Percentage duration through the vowel")+
  facet_wrap(sex~ageGroup)+
  NULL
goal.SE.F1.gamm.plot
ggsave("figures/goal-SE-F1-gamm-plot.svg",goal.SE.F1.gamm.plot,height=4,width=6,units="in")



# ** F2 ####
goal.SE.F2.gamm.plot <- ggplot(data = goal.SE.F2.gamm.less.p,
                               aes(
                                 x=measurement.no,
                                 y=fit
                               )
)+
  theme_Caitlin_gamm()+
  geom_smooth_ci(lexSet_ord)+
  scale_colour_manual(name = "Lexical Set",
                      values = c(holecol, holeycol, holycol))+
  scale_linetype_manual(name = "Lexical Set"
                        ,
                      values = c(holeline,holeyline,holyline)
                      )+
  yaxisF2 +   ylab("Model fit F2 (Hz)")+
  xaxisall +   xlab("Percentage duration through the vowel")+
  NULL
goal.SE.F2.gamm.plot
ggsave("figures/goal-SE-F2-gamm-plot.svg",goal.SE.F2.gamm.plot,height=4,width=6,units="in")


goal.SE.F2.gamm.present <- ggplot(data = goal.SE.F2.gamm.less.p,
                             aes(
                               x=measurement.no,
                               y=fit
                             )
)+
  theme_Caitlin_gammpresent()+
  # theme(legend.position = "none") +
  geom_smooth_ci(lexSet_ord) +
  scale_colour_manual(name = "Context",
                      values = c(holecol,holeycol,holycol))+
  scale_linetype_manual(name = "Context",values = c(holeline,holeyline,holyline))+
  yaxisF2 +   ylab("Model fit F2 (Hz)")+
  xaxisall +   xlab("Percentage duration through the vowel")+
  NULL
goal.SE.F2.gamm.present
ggsave("figures/goal-SE-F2-gamm-present.png",goal.SE.F2.gamm.present,height=4,width=6,units="in")


goal.SE.F2.boxplot <- ggplot(data_goat %>% filter(measurement.no=="50",corpus == "CoRP-SE"),aes(x=lexSet_ord,y=F2,fill=lexSet_ord))+
  # geom_boxplot()+
  geom_violin()+
  geom_count()+
  # geom_jitter(aes(colour=lexSet_ord)) +
  theme_Caitlin()+
  theme(legend.position = "none")+
  scale_fill_manual(values = c(hopecol,holecol,holeycol,holycol))+
  ylab("F2 (Hz)") +
  xlab("Lexical Set")+
  NULL
goal.SE.F2.boxplot
ggsave("figures/goal-SE-F2-boxplot.svg",goal.SE.F2.boxplot,height=4,width=6,units="in")

goal.SE.F2.boxplot.present <- goal.SE.F2.boxplot + geom_violin()+ theme_Caitlin_present()+ theme(legend.position = "none")
ggsave("figures/goal-SE-F2-boxplot-present.png",goal.SE.F2.boxplot.present,height=4,width=6,units="in")

# * DE ----------------
# **F1 ####
goal.DE.F1.gamm.plot <- ggplot(data = goal.DE.F1.gamm.less.p,
                               aes(
                                 x=measurement.no,
                                 y=fit
                                 )
)+
  theme_Caitlin_gamm()+
  geom_smooth_ci(lexSet_ord)+
  scale_colour_manual(name = "Lexical Set",
                      values = c(holecol,holycol))+
  scale_linetype_manual(name= "Lexical Set",
                        values = c(holeline,holyline))+
  yaxisF1 +   ylab("Model fit F1 (Hz)")+
  xaxisall +   xlab("Percentage duration through the vowel")+
  # facet_grid(rows=vars(ageGroup),cols=vars(sex))+
  NULL
goal.DE.F1.gamm.plot

ggsave("figures/goal-DE-F1-gamm-plot.svg",goal.DE.F1.gamm.plot,height=4,width=6,units="in")

# **F2 ####
goal.DE.F2.gamm.p <- goal.DE.F2.gamm.p %>% 
  mutate(group = paste(ageGroup, sex, lexSet_ord, sep = "."))
  
goal.DE.F2.gamm.plot <- ggplot(data = goal.DE.F2.gamm.p,
                               aes(
                                 x=measurement.no,
                                 y=fit
                               )
)+
  theme_Caitlin_gamm()+
  geom_smooth_ci(lexSet_ord)+
  scale_colour_manual(name = "Lexical Set",
                      values = c(holecol,holycol))+
  scale_linetype_manual(name= "Lexical Set",
                        values = c(holeline,holyline))+
  yaxisF2 +   ylab("Model fit F2 (Hz)")+
  xaxisall +   xlab("Percentage duration through the vowel")+
  facet_wrap(sex~ageGroup,ncol=2)+
  NULL
goal.DE.F2.gamm.plot
ggsave("figures/goal-DE-F2-gamm-plot.svg",goal.DE.F2.gamm.plot,height=4,width=6,units="in")

goal.DE.F2.gamm.present <- ggplot(data = goal.DE.F2.gamm.p,
                                  aes(
                                    x=measurement.no,
                                    y=fit
                                  )
)+
  theme_Caitlin_gammpresent()+
  theme(legend.position = "none")+
  geom_smooth_ci(group)+
  scale_colour_manual(name = "Context",
                      values = c(holecol,holycol,holecol,holycol,holecol,holycol,holecol,holycol))+
  scale_linetype_manual(name= "Context",
                        values = c(holeline,holyline,holeline,holyline,holeline,holyline,holeline,holyline))+
  yaxisF2 +   ylab("Model fit F2 (Hz)")+
  xaxisall +   xlab("Percentage duration through the vowel")+
  # facet_wrap(sex~ageGroup,ncol=2)+
  NULL
goal.DE.F2.gamm.present
ggsave("figures/goal-DE-F2-gamm-present.png",goal.DE.F2.gamm.present,height=4,width=6,units="in")

goal.DE.F2.boxplot <- ggplot(data_goat %>% filter(measurement.no=="50",corpus == "DECTE-NE"),aes(x=lexSet,y=F2,fill=lexSet))+
  geom_boxplot()+
  # geom_violin()+
  # geom_count()+
  geom_jitter() +
  # theme_Caitlin()+
  theme(legend.position = "none")+
  scale_fill_manual(values = c(hopecol,holecol,holycol))+
  # yaxisF2 +   ylab("Model fit F2 (Hz)")+
  # xlab("Lexical Set")+
  NULL
goal.DE.F2.boxplot
# ggsave("figures/goal-DE-F2-boxplot.svg",goal.DE.F2.boxplot,height=4,width=6,units="in")

# *NE -------------------------------
# **F1 ####
goal.NE.F1.gamm.p <- goal.NE.F1.gamm.p %>% 
  mutate(ageGroup=recode(age_sex_lexSet, "Old.Female.hole" = "Old", "Old.Female.holey" = "Old", "Old.Female.holy" = "Old","Old.Male.hole" = "Old", "Old.Male.holey" = "Old", "Old.Male.holy" = "Old","Young.Female.hole" = "Young", "Young.Female.holey" = "Young", "Young.Female.holy" = "Young","Young.Male.hole" = "Young", "Young.Male.holey" = "Young", "Young.Male.holy" = "Young")) %>% 
  mutate(sex=recode(age_sex_lexSet, "Old.Female.hole" = "Female", "Old.Female.holey" = "Female", "Old.Female.holy" = "Female","Old.Male.hole" = "Male", "Old.Male.holey" = "Male", "Old.Male.holy" = "Male","Young.Female.hole" = "Female", "Young.Female.holey" = "Female", "Young.Female.holy" = "Female","Young.Male.hole" = "Male", "Young.Male.holey" = "Male", "Young.Male.holy" = "Male")) %>% 
  mutate(lexical.set=recode(age_sex_lexSet, "Old.Female.hole" = "hole", "Old.Female.holey" = "holey", "Old.Female.holy" = "holy","Old.Male.hole" = "hole", "Old.Male.holey" = "holey", "Old.Male.holy" = "holy","Young.Female.hole" = "hole", "Young.Female.holey" = "holey", "Young.Female.holy" = "holy","Young.Male.hole" = "hole", "Young.Male.holey" = "holey", "Young.Male.holy" = "holy"))

goal.NE.F1.gamm.plot <- ggplot(data = goal.NE.F1.gamm.p,
                               aes(
                                 x=measurement.no,
                                 y=fit
                               )
)+
  theme_Caitlin_gamm()+
  geom_smooth_ci(age_sex_lexSet)+
  scale_colour_manual(name = "Lexical Set", labels = c("hole","holey","holy","hole","hole","holey","holy","hole","holey","holy")
                      ,values = c(holecol,holeycol,holycol,holecol,holecol,holeycol,holycol,holecol,holeycol,holycol))+
  scale_linetype_manual(name = "Lexical Set", labels = c("hole","holey","holy","hole","hole","holey","holy","hole","holey","holy")
                        ,values = c(holeline,holeyline,holyline,holeline,holeline,holeyline,holyline,holeline,holeyline,holyline))+
  yaxisF1 +   ylab("Model fit F1 (Hz)")+
  xaxisall +   xlab("Percentage duration through the vowel")+
  facet_wrap(sex~ageGroup,ncol=2)+
  NULL
goal.NE.F1.gamm.plot

ggsave("figures/goal-NE-F1-gamm-plot.svg",goal.NE.F1.gamm.plot,height=4,width=6,units="in")



# ** F2 ####
goal.NE.F2.gamm.p <- goal.NE.F2.gamm.p %>% 
  mutate(ageGroup=recode(age_sex_lexSet, "Old.Female.hole" = "Old", "Old.Female.holey" = "Old", "Old.Female.holy" = "Old","Old.Male.hole" = "Old", "Old.Male.holey" = "Old", "Old.Male.holy" = "Old","Young.Female.hole" = "Young", "Young.Female.holey" = "Young", "Young.Female.holy" = "Young","Young.Male.hole" = "Young", "Young.Male.holey" = "Young", "Young.Male.holy" = "Young")) %>% 
  mutate(sex=recode(age_sex_lexSet, "Old.Female.hole" = "Female", "Old.Female.holey" = "Female", "Old.Female.holy" = "Female","Old.Male.hole" = "Male", "Old.Male.holey" = "Male", "Old.Male.holy" = "Male","Young.Female.hole" = "Female", "Young.Female.holey" = "Female", "Young.Female.holy" = "Female","Young.Male.hole" = "Male", "Young.Male.holey" = "Male", "Young.Male.holy" = "Male")) %>% 
  mutate(lexical.set=recode(age_sex_lexSet, "Old.Female.hole" = "hole", "Old.Female.holey" = "holey", "Old.Female.holy" = "holy","Old.Male.hole" = "hole", "Old.Male.holey" = "holey", "Old.Male.holy" = "holy","Young.Female.hole" = "hole", "Young.Female.holey" = "holey", "Young.Female.holy" = "holy","Young.Male.hole" = "hole", "Young.Male.holey" = "holey", "Young.Male.holy" = "holy"))


goal.NE.F2.gamm.plot <- ggplot(data = goal.NE.F2.gamm.p,
                               aes(
                                 x=measurement.no,
                                 y=fit
                                 # linetype = sex_lexSet,
                                 # ,linecolour = sex_lexSet
                                 # fill = sex_lexSet
                                 # ,colour = sex_lexSet
                               )
)+
  theme_Caitlin_gamm()+
  geom_smooth_ci(age_sex_lexSet)+
  scale_colour_manual(name = "Lexical Set",
  values = c(holecol,holeycol,holycol, holecol,holecol, holeycol,holycol,holecol,holeycol,holycol))+
    scale_linetype_manual(name = "Lexical Set",
                          values = c(holeline,holeyline,holyline, holeline,holeline, holeyline,holyline,holeline,holeyline,holyline))+
  yaxisF2 +   ylab("Model fit F2 (Hz)")+
  xaxisall +   xlab("Percentage duration through the vowel")+
  facet_wrap(sex~ageGroup,ncol=2)+
  NULL
goal.NE.F2.gamm.plot
ggsave("figures/goal-NE-F2-gamm-plot.svg",goal.NE.F2.gamm.plot,height=4,width=6,units="in")

goal.NE.F2.gamm.present <- ggplot(data = goal.NE.F2.gamm.p,
                                  aes(
                                    x=measurement.no,
                                    y=fit
                                    # linetype = sex_lexSet,
                                    # ,linecolour = sex_lexSet
                                    # fill = sex_lexSet
                                    # ,colour = sex_lexSet
                                  )
)+
  theme_Caitlin_gammpresent() +
  theme(legend.position = "none") +
  geom_smooth_ci(age_sex_lexSet)+
  scale_colour_manual(name = "Lexical Set",
                      values = c(holecol,holeycol,holycol, holecol,holecol, holeycol,holycol,holecol,holeycol,holycol))+
  scale_linetype_manual(name = "Lexical Set",
                        values = c(holeline,holeyline,holyline, holeline,holeline, holeyline,holyline,holeline,holeyline,holyline))+
  yaxisF2 +   ylab("Model fit F2 (Hz)")+
  xaxisall +   xlab("Percentage duration through the vowel")+
  # facet_wrap(sex~ageGroup,ncol=2)+
  NULL
goal.NE.F2.gamm.present
ggsave("figures/goal-NE-F2-gamm-present.png",goal.NE.F2.gamm.present,height=4,width=6,units="in")


goal.NE.F2.boxplot <- ggplot(data_goat %>% filter(measurement.no=="50",corpus == "CoRP-NE"),aes(x=lexSet_ord,y=F2,fill=lexSet_ord))+
  # geom_boxplot()+
  geom_violin()+
  geom_count()+
  # geom_jitter() +
  theme_Caitlin()+
  theme(legend.position = "none")+
  scale_fill_manual(values = c(hopecol,holecol,holeycol,holycol))+
  yaxisF2 +   ylab("Model fit F2 (Hz)")+
  xlab("Lexical Set")+
  NULL
goal.NE.F2.boxplot
ggsave("figures/goal-NE-F2-boxplot.svg",goal.NE.F2.boxplot,height=4,width=6,units="in")

goal.F2.gamm.present <- goal.SE.F2.gamm.present + goal.NE.F2.gamm.present + goal.DE.F2.gamm.present
ggsave("figures/goal-F2-gamm-present.png",goal.F2.gamm.present,height=4,width=6,units="in")

# *All -------------------
# **F1 ####
goal.F1.gamm.plot <- (goal.SE.F1.gamm.plot+theme(legend.position ="left")+ggtitle("CoRP-SE")) +
  (goal.NE.F1.gamm.plot+theme(legend.position ="none")+ggtitle("CoRP-NE"))+
  ((goal.DE.F1.gamm.plot+ggtitle("DECTE \n(repeated for comparison)")+theme(legend.position="none",axis.text.x = element_blank(),axis.ticks.x = element_blank())+xlab(""))/goal.DE.F1.gamm.plot+theme(legend.position="none")) +
  plot_layout(design="
                      11223
                      11223
                          ")
goal.F1.gamm.plot
ggsave("figures/goal-F1-gamm-plot.svg",goal.F1.gamm.plot,height=8,width=12,units="in")

# **F2 ####
goal.F2.gamm.plot <-((goal.SE.F2.gamm.plot+ theme(legend.position ="left",axis.text.x = element_blank(),axis.ticks.x = element_blank())+
    ggtitle("CoRP-SE \n(repeated for comparison)")+xlab(""))/
  (goal.SE.F2.gamm.plot+theme(legend.position="none")))+
  (goal.NE.F2.gamm.plot+theme(legend.position ="none")+
    ggtitle("CoRP-NE"))+
  (goal.DE.F2.gamm.plot+
    ggtitle("DECTE")+theme(legend.position="none"))+
  plot_layout(design="
                      13344
                      23344
                          ")
goal.F2.gamm.plot
ggsave("figures/goal-F2-gamm-plot.svg",goal.F2.gamm.plot,height=8,width=12,units="in")
