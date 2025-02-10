library(tidyverse)

load("data/study_data.RDa")
source("R/prediction/prediction.R")
# 
# model="exac_modsev_n ~ arm_ipd*eos_bl+age_imp+exac_bl+sex+smoking_bl+fev1_bl+offset(trt_dur)"
# data=study_data$EFFECT$ad
# 
# data=data %>% dplyr::mutate_at(c("exac_bl", "fev1_bl"),~ifelse(sample(c(TRUE, FALSE), size = length(.), replace = TRUE, prob = c(0.8, 0.2)),., NA)) 
# data$train=rbernoulli(nrow(data), 0.8)
# 

# test.data=data.frame(arm_ipd="ICS", eos_bl=0.2, age_imp=60, exac_bl=2, sex="M",smoking_bl="Former", fev1_bl=1, trt_dur=1)
# # 
# dat <- read.csv(file = "prediction/MissingDataAndPrediction/support2.csv")
# dd <- datadist(dat)
# options(datadist='dd')
# ex.fit <- "sps~pafi+meanbp+wblc+alb+resp+temp+hrt+bili+crea+sod"
# 
# out2=submodels(dat, ex.fit, glm, family="gaussian")



model="exac_modsev_n ~ arm_ipd*eos_bl+arm_ipd*age_imp+arm_ipd*exac_bl+arm_ipd*sex+arm_ipd*smoking_bl+arm_ipd*fev1_bl+offset(trt_dur)"
model=as.formula(model)
# model=as.character(model)
data=study_data$EFFECT$ad
model_fn=MASS::glm.nb
force_variables="trt_dur"
contrasts=NULL
remove_model_data = c()
submodel_type="psm"
threshold_multiplier=2

data=data %>% dplyr::mutate_at(c("exac_bl", "fev1_bl"),~ifelse(sample(c(TRUE, FALSE), size = length(.), replace = TRUE, prob = c(0.8, 0.2)),., NA)) 
data$train=as.logical(rbinom(nrow(data),1, 0.8))
submodels.object=submodels(data %>% filter(train), model, MASS::glm.nb,force_variables="trt_dur", contrasts=NULL, remove_model_data = c())


newdata=data %>% filter(!train)
test=predict.submodels(newdata=newdata, submodels.object = submodels.object, se.fit=T,retain_lhs = T,include_mp=T, type='response')
auc(test, "exac_modsev_n", "fit")
# 
# x=predict(submodels.object[[1]][["mod"]], newdata = prediction.data, type="response")
# y=cbind.data.frame("act"=prediction.data$exac_modsev_n, x) %>% na.omit()
# auc(y, "act", "x")

test %>% 
  # mutate(fit=log(fit), exac_modsev_n=log(exac_modsev_n)) %>% 
  ggplot(aes(x=fit, y=exac_modsev_n, col=mp))+
  geom_point()+
  geom_smooth(se=F)


test2=predict.submodels(prediction.data = data %>% filter(train), submodels.object = submodels.object,retain_lhs = T, se.fit=T, type='response')
test2 %>% ggplot(aes(x=fit, y=exac_modsev_n))+
  geom_point()+
  geom_smooth()

lm(exac_modsev_n~fit, data=test2)
coefs=submodel.print(submodels.object = submodels.object, long_wide = "wide")

test3=tribble(~"arm_ipd", ~"eos_bl",~"age_imp",~"exac_bl",~"sex",~"smoking_bl",~"trt_dur",~"fev1_bl",
              "ICS", 0.2, 65, 1, "M", "Former",  1,0.8,
              "Control", 0.2, 65, 1, "M", "Former",  1, 0.8)



predict.submodels(test3, submodels.object_modsev, type="response")
# submodels.object[1]
# library(emmeans)
# emmeans(submodels.object[[1]][["mod"]], "arm_ipd", by=c("eos_bl","age_imp","exac_bl","sex","smoking_bl","trt_dur"))

model_modsev="exac_modsev_n ~ arm_ipd*eos_bl+age_imp+exac_bl+sex+smoking_bl+fev1_bl+offset(trt_dur)"
model_sev="exac_severe_n ~ arm_ipd*eos_bl+age_imp+exac_bl+sex+smoking_bl+fev1_bl+offset(trt_dur)"
model_pneum="pneum_n ~ arm_ipd*eos_bl+age_imp+exac_bl+sex+smoking_bl+fev1_bl+offset(trt_dur)"
data=study_data$EFFECT$ad

# data$train=as.logical(rbinom(nrow(data),1, 0.8))
submodels.object_modsev=submodels(data, model_modsev, MASS::glm.nb,force_variables="trt_dur", contrasts=NULL, remove_model_data = c())
submodels.object_sev=submodels(data, model_sev, MASS::glm.nb,force_variables="trt_dur", contrasts=NULL, remove_model_data = c())
submodels.object_pneum=submodels(data, model_pneum, MASS::glm.nb,force_variables="trt_dur", contrasts=NULL, remove_model_data = c())

save(submodels.object_pneum, submodels.object_sev, submodels.object_modsev, file="RECODE_meta/Data/submodels.RDa")

# caret::varImp(submodels.object[[1]][["mod"]])


model2=exac_modsev_n ~ arm_ipd*eos_bl+age_imp+ arm_ipd*exac_bl+sex+smoking_bl+fev1_bl+offset(trt_dur)
model_breakdown(model2)
