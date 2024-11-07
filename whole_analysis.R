library(ggplot2)
library(haven)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(MatchIt)
library(psych)

##### GLOBAL ###########
# set upper and lower cutoffs to dichotimize paraphilia variables
cutoff1=4
cutoff2=2
paraphilias_6 = c("stalk_immobilize_rape","nonconsensual_sadomasochism",
              "pedophilia", "hebephilia",
              "exhibitionism", "frotteurism")
paraphilias_3 = c("chronophilia","courtship_disorder","agonistic_continuum")
sexes=c("males","females")

##### CLEAN DATA ##########
check_numeric<-function(d, column_names){
  for (this_column in column_names){
    val = d[[this_column]]
    if (!is.numeric(val)) {
      stop(sprintf("%s is not numeric, but %s",this_column,as.character(class(val))))
    }
  }
}
## WARNING: standardize_missing IS PROBLEM IF SOMEONE REPORTED DESIRED SEXUAL ACTIVITY, NUMBER OF SEX PARTNERS ETC. AS 997 OR 998
standardize_missing<-function(d){
  d <- d %>%
    mutate(across(where(is.numeric), ~na_if(.x, -1))) %>%#question has not been asked to that person or person chose a response that made the question obsolete
    mutate(across(where(is.numeric), ~na_if(.x, 997))) %>%#"do not know" option for religion item
    mutate(across(where(is.numeric), ~na_if(.x, 998))) #take out 998, to check if missings are missing because of "prefer not to answer"
  return(d)
}
Czechsex <- read.csv("/Users/ellen/PAPERS/SaraCzechSex/DATA_ALL.csv")
# check type of Age, birth SEX, honesty, education
check_numeric(Czechsex,c("AGE","A1","P22","EDU")) #will check others when we rename variables
# standardize missing
Czechsex <- standardize_missing(Czechsex)
# filter people who have not responded honestly
Czechsex<-subset(Czechsex,P22==1 )
## get BIRTH_SEX
Czechsex$BIRTH_SEX<-Czechsex$A1 #1 prefer not to say, 2 women, 3 man
Czechsex$BIRTH_SEX <-ifelse(Czechsex$BIRTH_SEX ==2, 2,
                            ifelse(Czechsex$BIRTH_SEX==3, 1, NA))
# exclude missing BIRTH_SEX
#Czechsex = Czechsex[!is.na(Czechsex$BIRTH_SEX),]
## height
Czechsex$height<-Czechsex$C26
check_numeric(Czechsex, "height")
Czechsex <- Czechsex %>%
  mutate(height = case_when(
    height == 0.158 ~  158,
    height == 0.163 ~  163,
    height == 0.178 ~ 178,
    height == 0.183 ~ 183,
    height == 0.18 ~ 180,
    height == 0.16 ~ 160,
    TRUE ~ height  # retain other values WARNING THERE ARE STILL SOME HEIGHT VALUES
  ))
if (any(Czechsex$height<1)){
  stop("Improbable height values still exist")
}
## education
Czechsex$EDU_bin<-ifelse(Czechsex$EDU>1,1,
                         ifelse(Czechsex$EDU==1,0,NA))
##### get paraphilias ##########
# 6 paraphilias 
Czechsex$stalk_immobilize_rape<-Czechsex$R1_E43A
Czechsex$nonconsensual_sadomasochism<-Czechsex$R2_E43A
Czechsex$pedophilia<-Czechsex$R3_E43A
Czechsex$hebephilia<-Czechsex$R4_E43A
Czechsex$exhibitionism<-Czechsex$R5_E43A
Czechsex$frotteurism<-Czechsex$R6_E43A
# check type
check_numeric(Czechsex, paraphilias_6)
# get the 3 types of paraphilias
Czechsex$chronophilia <- pmax(Czechsex$pedophilia, Czechsex$hebephilia)
Czechsex$courtship_disorder <- pmax(Czechsex$exhibitionism, Czechsex$frotteurism)
Czechsex$agonistic_continuum <- pmax(Czechsex$nonconsensual_sadomasochism, Czechsex$stalk_immobilize_rape)
## get dicotomous scores (chronophilia.2, courtship_disorder.2, agonistic_continuum.2)
for (p in paraphilias_3){
  Czechsex[[paste0(p,".2")]]<- ifelse(Czechsex[[p]] >=cutoff1, 1, 
                                      ifelse(Czechsex[[p]] <=cutoff2, 0, NA))
}; rm("p")
##### Sexual outcomes ##########
sexual_outcomes = c("sex_partners_lifetime","desired_frequency","masturbation_frequency",
                    "sexual_debut_masturbation","sexual_debut",
                    "menarche_semenarche")
check_numeric(Czechsex, "E1")

# THERE IS ERROR HERE WITH sex_partners_lifetime
Czechsex$sex_partners_lifetime<-ifelse(Czechsex$E1==1,0, Czechsex$E1_2_TEXT)
Czechsex$sex_partners_lifetime = as.double(Czechsex$sex_partners_lifetime)
Czechsex <- standardize_missing(Czechsex)

Czechsex$desired_frequency<-ifelse(Czechsex$E40_EXCLUSIVE == 1,0,Czechsex$E40) #only for non-virgins
Czechsex$masturbation_frequency<-ifelse(Czechsex$F3==1,0,Czechsex$F2)
Czechsex$sexual_debut_masturbation<-Czechsex$F1
Czechsex$sexual_debut<-Czechsex$E7
Czechsex$sexual_debut_penetration<-Czechsex$R3_E42A
# menarche/semenarche
Czechsex$menarche <- Czechsex$C29 #delete people who have not responded to question
Czechsex <- Czechsex %>%
  mutate(menarche = case_when(
    menarche == 0.12 ~  12,
    TRUE ~ menarche  # retain other values
  ))
Czechsex$semenarche <-Czechsex$C28B
Czechsex$menarche_semenarche<-ifelse(Czechsex$BIRTH_SEX == 1, Czechsex$semenarche,
                                     ifelse(Czechsex$BIRTH_SEX == 2, Czechsex$menarche, NA))
# check type
check_numeric(Czechsex, sexual_outcomes)

## CHILD SEXUAL ABUSE ##############
CSA_cutoff = 18 # sexual abuse below this age is considered CSA

# FORCE/VIOLENCE (K2, K3)
# K2 have you ever been forced by threats or violence to engage in sexual activity 1=yes, 2=yes, multiple times, 3=no never
SA_violence = as.logical(rep(NA,nrow(Czechsex)))
SA_violence[Czechsex$K2==1 | Czechsex$K2==2] = TRUE
SA_violence[Czechsex$K2==3] = FALSE # dont remember or choose to not to answer stays NA

CSA_violence = as.logical(rep(NA,nrow(Czechsex)))
CSA_violence[SA_violence == TRUE & Czechsex$K3 < CSA_cutoff] = TRUE
CSA_violence[SA_violence == TRUE & Czechsex$K3 >= CSA_cutoff] = FALSE 
CSA_violence[SA_violence == FALSE] = FALSE # chose not to answer earlier question or if dont remember age of abuse stays as NA

# NON VIOLENT (K24, K25)
#In addition to what you said in the previous questions, has it ever happened to you that a person has tried to touch you with sexual intent against your will or tried to get you to touch them? 
#This can be cuddling, pinching or grabbing in a sexual way, or touching on or around intimate parts of the body.
# 1 = yes, 2= multiple times, 3=no (CHECK OPTION SOUNDS WEIRD), 4=DONT REMEMBER (not coded as NA)
SA_nonviolence = as.logical(rep(NA,nrow(Czechsex)))
SA_nonviolence[Czechsex$K24==1 | Czechsex$K24==2] = TRUE
SA_nonviolence[Czechsex$K24==3] = FALSE 
SA_nonviolence[Czechsex$K24==4] = NA # choose to not to answer stays NA

CSA_nonviolence = as.logical(rep(NA,nrow(Czechsex)))
CSA_nonviolence[SA_nonviolence == TRUE & Czechsex$K25 < CSA_cutoff] = TRUE
CSA_nonviolence[SA_nonviolence == TRUE & Czechsex$K25 >= CSA_cutoff] = FALSE 
CSA_nonviolence[SA_nonviolence == FALSE] = FALSE # if dont remember age stays as NA

### CSA 
# FALSE | NA = NA, BUT TRUE | NA = TRUE! (R IS STUPID)
CSA = as.logical(rep(NA,nrow(Czechsex)))
CSA[CSA_violence | SA_nonviolence] = TRUE
CSA[CSA_violence == FALSE & SA_nonviolence == FALSE]=FALSE

Czechsex$CSA = CSA; rm(list=c("CSA"))


##### split by gender ########
males<-subset(Czechsex,BIRTH_SEX==1)
females<-subset(Czechsex,BIRTH_SEX==2)


##### MATCHING #########

#function optimizing sample size while making sure that the matched groups are prefectly balanced for Age 
optimal_ratio <- function(data, variable, max_diff) {
  best_ratio <- NULL
  max_sample_size <- 0
  
  # Filtrer ut profilerte data før matching
  data_before <- data[data[[variable]] %in% c(0, 1) & complete.cases(data[c(variable, "AGE")]), , drop = FALSE]
  
  for (ratio in seq(10, 100, by = 10)) {  # Start med ratio 10, øke med 10.
    m.out <- matchit(as.formula(paste(variable, "~ AGE")), 
                     data = data_before,
                     method = "nearest", 
                     distance = "glm", 
                     ratio = ratio)
    
    m.data <- match.data(m.out)
    
    subset_matched_v1 <- m.data[m.data[[variable]] == 0, ]
    subset_matched_v2 <- m.data[m.data[[variable]] == 1, ]
    
    age_mean_matched_v1 <- mean(subset_matched_v1$AGE, na.rm = TRUE)
    age_mean_matched_v2 <- mean(subset_matched_v2$AGE, na.rm = TRUE)
    age_diff <- abs(age_mean_matched_v1 - age_mean_matched_v2) * 12
    
    sample_size <- nrow(m.data)
    
    if (age_diff <= max_diff && sample_size > max_sample_size) {
      max_sample_size <- sample_size
      best_ratio <- ratio
    } else if (age_diff > max_diff) {
      # Finjustering av ratioen
      for (step_ratio in seq(ratio - 9, ratio + 10, by = 1)) {
        if (step_ratio <= 0) next  # Sørg for at vi ikke har negative eller null ratioer
        
        m.out <- matchit(as.formula(paste(variable, "~ AGE")), 
                         data = data_before,
                         method = "nearest", 
                         distance = "glm", 
                         ratio = step_ratio)
        
        m.data <- match.data(m.out)
        
        subset_matched_v1 <- m.data[m.data[[variable]] == 0, ]
        subset_matched_v2 <- m.data[m.data[[variable]] == 1, ]
        
        age_mean_matched_v1 <- mean(subset_matched_v1$AGE, na.rm = TRUE)
        age_mean_matched_v2 <- mean(subset_matched_v2$AGE, na.rm = TRUE)
        age_diff <- abs(age_mean_matched_v1 - age_mean_matched_v2) * 12
        
        sample_size <- nrow(m.data)
        
        if (age_diff <= max_diff && sample_size > max_sample_size) {
          max_sample_size <- sample_size
          best_ratio <- step_ratio
        }
      }
      break  # Avslutt hovedsløyfen
    }
  }
  return(best_ratio)
}

# alternative function for getting ratios (NOT USED)
make_Ratios_data_frame<-function(){
  NRatios = length(paraphilias_3)*length(sexes)
  Ratios = data.frame(
    "sex" = rep("",NRatios),
    "paraphilia_3" = rep("",NRatios),
    "ratio" = rep(NA,NRatios)
  )
  r = 0
  for (this_sex in sexes){
    for (this_paraphilia in paraphilias_3) {
      r = r+1
      Ratios$sex[r] = this_sex
      Ratios$paraphilia_3[r] = this_paraphilia
      val = optimal_ratio(get(this_sex), paste0(this_paraphilia,".2"), 0)
      Ratios$ratio[r] = val
    }
  }
  return(Ratios)
  ## alternative 
  # for (this_sex in sexes){
  #   for (this_paraphilia in paraphilias_3) {
  #     eval_this = paste0(
  #       sprintf('%s.ratio.%s', this_sex, this_paraphilia),
  #       sprintf('= optimal_ratio(%s, "%s.2", 0)',this_sex, this_paraphilia)
  #     )
  #     eval(parse(text=eval_this)); rm("eval_this")
  #   }
  # }
}

# get ratios
males.ratio.chronophilia<-optimal_ratio(males, "chronophilia.2", 0)#6
males.ratio.courtship_disorder<-optimal_ratio(males, "courtship_disorder.2", 0)#4
males.ratio.agonistic_continuum<-optimal_ratio(males, "agonistic_continuum.2", 0)#2

females.ratio.chronophilia<-optimal_ratio(females, "chronophilia.2", 0)#19
females.ratio.courtship_disorder<-optimal_ratio(females, "courtship_disorder.2", 0)#16
females.ratio.agonistic_continuum<-optimal_ratio(females, "agonistic_continuum.2", 0)#5
# get matched data
get_matched_data<-function(males_or_females, cutoff1, cutoff2, paraphilia){
  data = get(males_or_females)
  # Filter the data based on the cutoff values for the paraphilia
  data$paraphilia_status <- ifelse(data[[paraphilia]] >= cutoff1, 1,              
                                   ifelse(data[[paraphilia]] <= cutoff2, 0, NA))
  # Identify the optimal ratio
  ratio <- get(sprintf("%s.ratio.%s",males_or_females, paraphilia))
  # Remove NAs
  data <- data[complete.cases(data[c("paraphilia_status", "AGE")]), ] 
  # Perform propensity score matching
  match_model <- matchit(paraphilia_status ~ AGE, data = data, method = "nearest", ratio = ratio)
  matched_data <- match.data(match_model)
  return(matched_data)
}

males.chronophilia.matched <-get_matched_data("males", cutoff1, cutoff2, "chronophilia")
males.courtship_disorder.matched <-get_matched_data("males", cutoff1, cutoff2, "courtship_disorder")
males.agonistic_continuum.matched <-get_matched_data("males", cutoff1, cutoff2, "agonistic_continuum")

females.chronophilia.matched <-get_matched_data("females", cutoff1, cutoff2, "chronophilia")
females.courtship_disorder.matched <-get_matched_data("females", cutoff1, cutoff2, "courtship_disorder")
females.agonistic_continuum.matched <-get_matched_data("females", cutoff1, cutoff2, "agonistic_continuum")

get_data_for_sex_paraphilia_outcome<-function(males_or_females,paraphilia,outcome,write_to_matlab){
  d<-get(sprintf('%s.%s.matched',males_or_females,paraphilia))
  # remove rows were outcome is missing
  d<-d[!is.na(d[[outcome]]),unique(c("ID",outcome,"paraphilia_status","AGE"))] #unique output is not sorted
  if (write_to_matlab){
    floc = sprintf('/Users/ellen/Documents/MATLAB/SaraEtiology/%s_%s_%s.csv',
                   males_or_females, paraphilia, outcome);
    if (file.exists(floc)){file.remove(floc)}
    write.table(d,
                file = floc,
                append = FALSE,
                row.names = FALSE)}
  
  return(d)
}


### MATCHING: GET RESULTS ############
results_row <- data.frame(
  "sex" = "",#males or females
  "paraphilia" = "",
  "n_yes" = NA,
  "n_no" = NA,
  "outcome" ="",
  'M_SD_yes'="",
  'M_SD_no'="",
  'Med_IQR_yes'="",
  'Med_IQR_no'="",
  "type" = "", # "parametric", "nonparametric", "binary"
  "statistic" = NA,
  "df" = NA,
  "p" = NA, # raw p
  "holm_p" = NA,
  "effect_size" = NA,
  "wendt_r"=NA, # calculate point-biserial using R
  "DIF"=NA
)

get_M_SD<-function(y){
  chr = sprintf('%.2f (%.2f)', mean(y), sd(y))
  return(chr)
}
get_Med_IQR<-function(y){
  chr = sprintf('%.2f (%.2f)', median(y), IQR(y))
  return(chr)
}
get_n_percentage<-function(bin_y){
  # get number of 1's and percentage of scores that are 1 
  # e.g. number of percentage of yes group that has higher education
  n1 = sum(as.double(bin_y==1))
  chr = sprintf('%.2f (%.2f%%)', n1, n1/length(bin_y)*100)
}
get_results<-function(males_or_females,paraphilia,outcome){
  # get matched data set
  #get matched d males.chronophilia.matched
  write_to_matlab = FALSE
  d = get_data_for_sex_paraphilia_outcome(males_or_females,paraphilia,outcome,write_to_matlab)
  #d should not have missing data for outcome
  if (any(is.na(d[[outcome]]))){stop(sprintf('%s %s has missing data for %s',males_or_females,paraphilia,outcome))}
  # get groups
  yes_group=subset(d,paraphilia_status==1)
  no_group=subset(d,paraphilia_status==0)
  
  # complete observations
  results_out<- results_row
  results_out$sex = males_or_females
  results_out$paraphilia = paraphilia
  results_out$outcome = outcome
  
  results_out$n_yes = nrow(yes_group)
  results_out$n_no = nrow(no_group)
  
  parametric = outcome %in% c("AGE","height")
  is_binary = outcome %in% c("EDU_bin","CSA")
  
  if (is_binary ){
    results_out$M_SD_yes = get_n_percentage(yes_group[[outcome]])
    results_out$M_SD_no = get_n_percentage(no_group[[outcome]])
    
  }else{ #for parametric and nonparameitrc
    results_out$M_SD_yes <- get_M_SD(yes_group[[outcome]])
    results_out$M_SD_no <- get_M_SD(no_group[[outcome]])
    
    results_out$Med_IQR_yes <- get_Med_IQR(yes_group[[outcome]])
    results_out$Med_IQR_no <- get_Med_IQR(no_group[[outcome]])
  }
  
  

  if (parametric){
    results_out$type = "parametric"
    ### do welch's t-test ####
    t_out = t.test(yes_group[[outcome]], no_group[[outcome]], var.equal = FALSE, paired = FALSE)
    results_out$statistic = t_out$statistic
    results_out$df = t_out$parameter 
    results_out$p = t_out$p.value
    rm("t_out")
    ### get Hedges's ######
    my <- mean(yes_group[[outcome]]) # na.rm = TRUE should be unnecessary
    mn <- mean(no_group[[outcome]]) # na.rm = TRUE should be unnecessary
    # Calculate pooled standard deviation
    sy <- sd(yes_group[[outcome]]) # na.rm = TRUE should be unnecessary
    sn <- sd(no_group[[outcome]]) # na.rm = TRUE should be unnecessary
    ny <- results_out$n_yes
    nn <- results_out$n_no
    sp = sqrt(
      ((ny-1)*sy^2 + (nn-1)*sn^2)/
        (ny + nn - 2)
    )
    # get hedge's g
    g = (my - mn)/sp
    g_correction <- 1 - (3 / (4 * (ny + nn - 2) - 1))  # Hedges' correction
    results_out$effect_size <- g * g_correction
    rm(list=c("my","mn","sy","sn","ny","nn","sp","g","g_correction"))
    
  }else if (is_binary){
    results_out$type = "binary"
    # fisher's test
    table <- table(d[[outcome]], d$paraphilia_status)
    f_out <- fisher.test(table)
    results_out$effect_size <- f_out$estimate  # Get odds ratio
    results_out$p <- f_out$p.value  # Get p-value
  }
  else{# use non parametric
    # Wilcoxon ranksum test
    results_out$type ="nonparametric"
    w_out<-wilcox.test(yes_group[[outcome]],no_group[[outcome]],conf.int=TRUE,paired=FALSE)
    z_value <- qnorm(w_out$p.value / 2, lower.tail = FALSE)
    r <- z_value / sqrt(z_value^2 + nrow(d) - 1)
    results_out$statistic = z_value
    results_out$p = w_out$p.value
    results_out$df = NA
    results_out$DIF=w_out$estimate
    if (results_out$DIF<0){r = r*-1}
    results_out$effect_size = r
    
  }

  return(results_out)
}



all_outcomes<-c("CSA","height","EDU_bin",sexual_outcomes)

rout = data.frame()
for (this_sex in sexes){
  for (this_paraphilia in paraphilias_3) {
    for (this_outcome in all_outcomes) {
      rout_new_row = get_results(this_sex,this_paraphilia,this_outcome)
      rout = rbind(rout, rout_new_row); rm("rout_new_row")
    }
  }
}
rm(list=c("this_sex","this_paraphilia","this_outcome"))
### MATCHING: WRITE RESULTS #####
floc = '/Users/ellen/PAPERS/SaraCzechSex/WRITE_HERE3_CSA.csv'
if (file.exists(floc)){file.remove(floc)}
write.table(rout,file=floc,append=FALSE,row.names=FALSE,sep=",")



# 
# ##### SCRAP
# library(coin)
# library(rstatix)
# alt_wilcox_effsize<-function(males_or_females='males',paraphilia='chronophilia',outcome='sexual_debut'){
#   d<-get(sprintf('%s.%s.matched',males_or_females,paraphilia))
#   d$Y = d[[outcome]]
#   w=wilcox_effsize(d,Y~paraphilia_status,paired=FALSE)
# }
# 
# rout$ALT_WILCOX_EFF = rep(NA,nrow(rout))
# nonparametric =
# r = 0
# for (this_sex in sexes){
#   for (this_paraphilia in paraphilias_3) {
#     for (this_outcome in all_outcomes) {
#       r = r+1
#       if (this_outcome %in% sexual_outcomes){ # do for nonparamtric
#         w = alt_wilcox_effsize(rout$sex[r], rout$paraphilia[r], rout$outcome[r])
#         rout$ALT_WILCOX_EFF[r] = w$effsize; rm("w")
#       }
#       
#     }
#   }
# }
# rm(list=c("r","this_sex","this_paraphilia","this_outcome"))
































##### OVERLAP ##########

get_percent_A_in_B<-function(males_or_females, paraphilia_type_A, paraphilia_type_B, paraphilia_status_A=1, paraphilia_status_B=1){
  # paraphilia_status_A = 1, paraphilia_status_B = 1  get percentage of people in paraphilia A (yes group) also in paraphilia B (yes group)
  # paraphilia_status_A = 0, paraphilia_status_B = 0  get percentage of people in paraphilia A (no/control group) also in paraphilia B (no/control group)
  # paraphilia_status_A = 1, paraphilia_status_B = 0  get percentage of people in paraphilia A (yes group) also in paraphilia B (no/control group)
  # get_percent_A_in_B('males', "chronophilia", "agonistic_continuum", 1, 1) get percentage of people with chronophilia who are also positive for agonistic_continum
  
  get_ids<-function(paraphilia_type, paraphilia_status){
    # get array of IDs of people for a particular paraphilia type and status
    d <-get(sprintf('%s.%s.matched',males_or_females, paraphilia_type))
    ids = d$ID[d$paraphilia_status == paraphilia_status]
    return(ids)
  }
  idsA = get_ids(paraphilia_type_A,paraphilia_status_A)
  idsB = get_ids(paraphilia_type_B,paraphilia_status_B)
  
  A_in_B_prcnt = sum(idsA %in% idsB)/length(idsA)
  return(A_in_B_prcnt)
}

overlap_row = data.frame(
  "sex"="",
  "group_A"="",
  "chronophilia_0"=NA,
  "chronophilia_1"=NA,
  "courtship_disorder_0"=NA,
  "courtship_disorder_1"=NA,
  "agonistic_continuum_0"=NA,
  "agonistic_continuum_1"=NA
)

get_overlap_results<-function(males_or_females, paraphilia_type_A, paraphilia_status_A){
  new_row = overlap_row
  new_row$sex = males_or_females
  new_row$group_A = sprintf('%s_%d',paraphilia_type_A, paraphilia_status_A)
  for (paraphilia_type_B in paraphilias_3){
    for (paraphilia_status_B in c(0,1)) {
      A_in_B_prcnt = get_percent_A_in_B(males_or_females, paraphilia_type_A, paraphilia_type_B, paraphilia_status_A, paraphilia_status_B)
      new_row[[sprintf("%s_%d",paraphilia_type_B,paraphilia_status_B)]] = A_in_B_prcnt
    }
  }
  return(new_row)
}

overlap=data.frame()
for (this_sex in sexes){
  for (paraphilia_type_A in paraphilias_3) {
    for (paraphilia_status_A in c(0,1)) {
      overlap_new_row = get_overlap_results(this_sex,paraphilia_type_A,paraphilia_status_A)
      overlap = rbind(overlap, overlap_new_row); rm("overlap_new_row")
    }
  }
}
rm(list=c("this_sex","paraphilia_type_A","paraphilia_status_A"))
floc = '/Users/ellen/PAPERS/SaraCzechSex/overlap.csv'
if (file.exists(floc)){file.remove(floc)}
write.table(overlap,file=floc,append=FALSE,row.names=FALSE,sep=",")
