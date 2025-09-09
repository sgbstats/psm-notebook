source("R/get_variables.R")

library(MASS)

`%notin%`=Negate(`%in%`)
interaction.nb=function(data, outcome, treatment,interaction=NULL,  covariates=NULL, offset=NULL )
{
  if(is.null(covariates))
  {
    covariates_f=""
  }else
  {
    covariates_f=paste0("+",paste(covariates, collapse = "+", sep=""))
  }
  if(is.null(offset))
  {
    offset_f=""
  }else
  {
    offset_f=paste0("+offset(", offset,")")
  }
  if(is.na(interaction))
  {
    interaction_f=""
  }else
  {
    interaction_f=paste0("*",interaction)
  }
  
  formula=as.formula(paste0(outcome, "~", treatment,interaction_f,covariates_f,offset_f ))
  m=MASS::glm.nb(formula = formula, data=data)
  m
}

library(tidyverse)
library(survival)
interaction.cox=function(data, outcome_time,cnsr_time, treatment,interaction,  covariates=NULL )
{
  
  
  if(is.null(covariates))
  {
    covariates_f=""
  }else
  {
    covariates_f=paste0("+",paste(covariates, collapse = "+", sep=""))
  }
  if(is.na(interaction))
  {
    data2=data %>%
      rename("outcome_time"=outcome_time,
             "cnsr_time"=cnsr_time) %>% 
      dplyr::select(all_of(c("outcome_time", "cnsr_time")), all_of(treatment),  all_of(covariates)) %>% 
      mutate(event=!is.na(outcome_time),
             time=if_else(is.na(outcome_time), cnsr_time, outcome_time))
    interaction_f=""
  }else
  {
    data2=data %>%
      rename("outcome_time"=outcome_time,
             "cnsr_time"=cnsr_time) %>% 
      dplyr::select(all_of(c("outcome_time", "cnsr_time")), all_of(treatment),all_of(interaction),  all_of(covariates)) %>% 
      mutate(event=!is.na(outcome_time),
             time=if_else(is.na(outcome_time), cnsr_time, outcome_time))
    interaction_f=paste0("*",interaction)
  }
  formula=as.formula(paste0("survival::Surv(time, event)~",treatment,interaction_f,covariates_f))
  m=survival::coxph(formula = formula, data=data2)
  m
}

interaction.lm=function(data,outcome,treatment,interaction=NULL,  covariates=NULL )
{
  
  
  if(is.null(covariates))
  {
    covariates_f=""
  }else
  {
    covariates_f=paste0("+",paste(covariates, collapse = "+", sep=""))
  }
  if(is.na(interaction))
  {
    interaction_f=""
  }else
  {
    interaction_f=paste0("*",interaction)
  }
  base=str_split(outcome, pattern="_")[[1]][1] %>% str_to_lower() %>% paste0("_bl")
  formula=as.formula(paste0(outcome,"~", treatment,interaction_f,covariates_f, "+",base ))
  m=lm(formula = formula, data=data)
  m
}


is_real_date=function(date_str)
{
  tryCatch({
    as.Date(date_str)
    T
  },
  error=function(e)
  {
    F
  })
  
}

date_impute=function(date_str, direction='down')
{
  if(is.na(date_str))
  {
    return(NA_character_)
  }
  if(date_str=="")
  {
    return(NA_character_)
  }
  
  if(is_real_date(date_str)) {return(date_str)}
  date=stringr::str_split(date_str, "-")
  
  year=as.numeric(date[[1]][1])
  month=as.numeric(date[[1]][2])
  day=as.numeric(date[[1]][3])
  
  if(direction=='down')
  {
    if(is.na(day))
    {
      day=1
    }
    if(is.na(month))
    {
      month=1
    }
    
  }else if(direction=='up')
  {
    if(is.na(month))
    {
      month=12
    }
    if(is.na(day))
    {
      day=case_when(month %in% c(1,3,5,7,8,10,12)~31,
                    month %in% c(4,6,9,11)~30,
                    year%%400==0~29,
                    year%%100==0~28,
                    year%%4==0~29,
                    T~28)
    }
  }
  imputed_date=paste(year, sprintf("%02d", month), sprintf("%02d", day), sep="-")
  return(imputed_date)
}

clean_usubjid=function(data, study_name="RECODE",digits=4, subjid="subjid"){
  data %>% clean_names() %>% 
    rename("subjid"=subjid) %>% 
    mutate(studyid=study_name,
           subjid=as.numeric(parse_number(as.character(subjid))),
           usubjid=paste0(study_name,"_", sprintf(paste0('%0',digits, 'd'), subjid))) %>% 
    arrange(usubjid)
}

dataset_load=function(adam, raw, adam_path, raw_path,study_name, study_prefix, dataset_suffix="", digits=4, subjid="subjid"){
  datasets=c(adam, raw)
  path=c(rep(adam_path, length(adam)), rep(raw_path, length(raw)))
  for(i in 1:length(datasets))
  {
    cat(paste(datasets[i], '\n'))
    
    if(exists("overwrite", inherits = T))
    {
      
    }else if(paste0(datasets[i]) %in% ls(envir = .GlobalEnv)) 
    {
      next
    }
    
    
    load(paste0(path[i],study_prefix,datasets[i],dataset_suffix,'.RDa'))
    assign(datasets[i],
           eval(parse(text=paste0(study_prefix,datasets[i], dataset_suffix))) %>%
             janitor::clean_names() %>% 
             rename("subjid"=subjid) %>% 
             mutate(studyid=study_name,
                    subjid=as.numeric(gsub("0352-2046-|V|T", "", as.character(subjid))),
                    usubjid=paste0(study_name,"_", sprintf(paste0('%0',digits, 'd'), subjid)))%>% 
             arrange(usubjid),
           envir = .GlobalEnv) 
    rm(list=paste0(study_prefix,datasets[i], dataset_suffix))
  }
  return(list("adam"=adam, "raw"=raw))
}


time_of_day_fix=function(time_of_day)
{
  if(str_length(time_of_day)==4)
  {
    time_of_day=paste0("0", time_of_day)
  }
  return(time_of_day)
}

bin_lorry=function(class_keep="function", specific_keep=c()){
  l=ls(pos=1L)
  bin=c()
  
  for(i in l)
  {
    if(i %in% c("i", "bin"))
    {
      bin=c(bin, T)
    }
    else if(i %in% specific_keep)
    {
      bin=c(bin, F)
    } 
    else if(sum(class(get(i)) %in% class_keep)>0)
    {
      bin=c(bin, F)
    }
    else
    {
      bin=c(bin, T)
    }
  }
  rm(list=l[bin], pos=1L)
  gc()
}
labalamaics=list()
labalamaics$ics='Budes|Beclo|Fluti|Momet|Triam|Cicle|Fluni|hydrocort|methypred|prednis|cortisone|dexame|betameth|fostair|seretide|trimbow|trelegy|trixeo|breztri'
labalamaics$laba='Albuterol|Salmeterol|arform|formo|oldaterol|vilan|advair|symbi|brexo|seretide|fostair|trimbow|trelegy|trixeo|breztri|spiolto|stiolto|utibron|ultibron|anoro|bevespi'
labalamaics$lama='tiotro|aclidi|glyco|umeclid|trimbow|trelegy|trixeo|breztri|spiolto|stiolto|utibron|ultibron|anoro|bevespi'
labalamaics$betab="acebutolol|atenolol|betaxolol|bisoprolol|carteolol|carvedilol|celiprolol|esmolol|labetalol|metoprolol|nadolol|nebivolol|penbutolol|pindolol|propranolol|sotalol|timolol"
labalamaics$acearb=	"benazepril|captopril|enalapril|fosinopril|lisinopril|perindopril|quinapril|ramipril|trandolapril|Moexipril|candesartan|eprosartan|irbesartan|losartan|olmesartan|telmisartan|valsartan|Azilsartan"
labalamaics$statins=	"atorvastatin|fluvastatin|lovastatin|pitavastatin|pravastatin|rosuvastatin|simvastatin"
labalamaics$antiplatelet=	"aspirin|clopidogrel|ticagrelor|prasugrel|cilostazol|dipyridamole|tirofiban|eptifibatide|cangrelor|vorapaxar|Abciximab"
labalamaics$anticoag=	"warfarin|apixaban|edoxaban|fondaparinux|rivaroxaban|heparin|dalteparin|enoxaparin|argatroban|bivalirudin|dabigatran|desirudin|tinzaparin|Betrixaban"
labalamaics$antidepressants=	"isocarboxazid|phenelzine|tranylcypromine|bupropion|citalopram|escitalopram|fluoxetine|fluvoxamine|paroxetine|sertraline|vilazodone|duloxetine|desvenlafaxine|levomilnacipran|venlafaxine|nefazodone|trazodone|amitriptyline|clomipramine|desipramine|doxepin|imipramine|nortriptyline|protriptyline|trimipramine|amoxapine|maprotiline|mirtazapine|vortioxetine"
labalamaics$anxiolytics=	"citalopram|escitalopram|fluoxetine|fluvoxamine|paroxetine|sertraline|vilazodone|venlafaxine|duloxetine|desvenlafaxine|amitriptyline|clomipramine|doxepin|imipramine|desipramine|nortriptyline|clonazepam|diazepam|lorazepam|alprazolam|oxazepam|pregabalin|hydroxyzine|buspirone|Chlordiazepoxide"
labalamaics$ppigord="dexlansoprazole|esomeprazole|lansoprazole|omeprazole|pantoprazole|rabeprazole|Cimetidine|Famotidine|Nizatidine|Aluminum hydroxide/Magnesium hydroxide|Calcium carbonate|Sodium bicarbonate|Magnesium hydroxide|Aluminum hydroxide|Magnesium trisilicate|Alginic acid|Ranitidine"


derived_vars=function(data){
  data %>% mutate(reversible_bl=if_else(reversibility_bl>20, "Reversible", "Non-reversible")) %>% 
    mutate(gold_grp=factor(case_when(mmrc_bl<=1&exac_bl>1~"C",
                                     cat_bl<10&exac_bl>1~"C",
                                     mmrc_bl>1&exac_bl>1~"D",
                                     cat_bl>=10&exac_bl>1~"D",
                                     mmrc_bl<=1&exac_sev_bl>0~"C",
                                     cat_bl<10&exac_sev_bl>0~"C",
                                     mmrc_bl>1&exac_sev_bl>0~"D",
                                     cat_bl>=10&exac_sev_bl>0~"D",
                                     mmrc_bl<=1&exac_bl<=1~"A",
                                     cat_bl<10&exac_bl<=1~"A",
                                     mmrc_bl>1&exac_bl<=1~"B",
                                     cat_bl>=10&exac_bl<=1~"B"), levels=c("A","B", "C", "D")),
           gold_stage=factor(case_when(fev1percent_bl<30~"Very severe airflow limitation",
                                       fev1percent_bl<50~"Severe airflow limitation",
                                       fev1percent_bl<80~"Moderate airflow limitation",
                                       fev1percent_bl>=80~"Mild airflow limitation"), 
                             levels=c("Mild airflow limitation", "Moderate airflow limitation", "Severe airflow limitation", "Very severe airflow limitation"))) %>% 
    mutate(eos_2pc=if_else(eos_pc_bl>2, ">2%", "<2%"),
           eos_200=eos_bl>0.2,
           eos_cat=factor(case_when(eos_bl<0.2~"0-0.2",
                                    eos_bl<0.3~"0.2-0.3",
                                    eos_bl<0.4~"0.3-0.4",
                                    eos_bl>=0.4~">0.4"), levels=c("0-0.2","0.2-0.3", "0.3-0.4",">0.4") ))%>% 
    return()
  
}

make_subgroups=function(data, vars){
  # some of the worst code every written
  # takes each of the subgroup vars and makes new ones for each of the levels, true and false. 
  # there is some wastage becuase the functions that analyse the data only like true values
  subgroup=c()
  for(i in vars){
    if(class(data[[i]])=="logical"){
      x=c(T,F)
    }else{
    x=levels(data[[i]])
    }
    for(j in x){
      lookup=c("foo", "foo3")
      names(lookup)=c(i, paste(i,j,sep="_"))
      data=data %>% 
        rename("foo"=eval(i)) %>% 
        mutate(foo2=foo) %>%
        mutate(foo3=foo2==j) %>% 
        rename(all_of(lookup)) 
      add=paste(i,j,sep="_")
      names(add)=j
      subgroup=c(subgroup,  add)
    }
  }
  subgroup=unname(subgroup)
  return(list("data"=data,
            "subgroup"=subgroup))
}
# test=make_subgroups(IMPACT_ad, c("smoking_bl", "sex"))
