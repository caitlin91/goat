# Taken from Joe Fruehwald's Favertools repository

# Functions to make processing data from FAVE easier.  Then can run a mutate in dplyr 

plt_manner.fn <- function(x){
  manner.codes = c(
    `0` = "none",
    `1` = "stop",
    `2` = "affricate",
    `3` = "fricative",
    `4` = "nasal",
    `5` = "lateral",
    `6` = "central"
  )
  manner <- manner.codes[as.character(x)]
  return(manner)
}

plt_place.fn <- function(x){
  place.codes = c(
    `0` = "none",
    `1` = "labial",
    `2` = "labiodental",
    `3` = "interdental",
    `4` = "apical",
    `5` = "palatal",
    `6` = "velar"
  )
  place <- place.codes[as.character(x)]
  return(place)
}

plt_preseg.fn <- function(x){
  preseg.codes = c(
    `0` = "none",
    `1` = "oral_labial",
    `2` = "nasal_labial",
    `3` = "oral_apical",
    `4` = "nasal_apical",
    `5` = "palatal",
    `6` = "velar",
    `7` = "liquid",
    `8` = "obstruent_liquid",
    `9` = "w/y"
  )  
  preseg <- preseg.codes[as.character(x)]
  return(preseg)
}

plt_voice.fn <- function(x){
  voice.codes = c(
    `1` = "voiceless",
    `2` = "voiced"
  )
  
  voice <- voice.codes[as.character(x)]
  return(voice)
}

plt_folseq.fn <- function(x){
  folseq.codes = c(
    `0` = "none",
    `1` = "one_fol_syll",
    `2` = "two_fol_syl",
    `3` = "complex_coda",
    `4` = "complex_one_syl",
    `5` = "complex_two_syl"
  )
  
  folseq <- folseq.codes[as.character(x)]
  return(folseq)
}

plt_vowels.fn <- function(x){
  vclass.codes = c(
    `1` = "i",
    `2` = "e",
    `3` = "ae",
    `5` = "o",
    `6` = "uh",
    `7` = "u",
    `*` = "*",
    `11` = "iy",
    `12` = "iyF",
    `21` = "ey",
    `22` = "eyF",
    `41` = "ay",
    `47` = "ay0",
    `61` = "oy",
    `42` = "aw",
    `62` = "ow",
    `63` = "owF",
    `72` = "uw",
    `73` = "Tuw",
    `82` = "iw",
    `33` = "aeh",
    `39` = "aeBR",
    `43` = "ah",
    `53` = "oh",
    `14` = "iyr",
    `24` = "eyr",
    `44` = "ahr",
    `54` = "ohr",
    `64` = "owr",
    `74` = "uwr",
    `94` = "*hr"
  )
  if(is.numeric(x)){
    x <- floor(x)
  }
  vowels <- vclass.codes[as.character(x)]
  return(vowels)
}
