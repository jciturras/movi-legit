#Movilidad social y justificación de desigualdad

#cargar base de datos
library(haven)
library(sjlabelled)
library("here")
library(dplyr)
library(sjmisc)
library(lme4)
library(texreg)
library(ggplot2)

load(here::here("input/data/proc/df_study1.RData"))

# Estimaciones ------------------------------------------------------------

#modelo nulo
r01 <- lmer(lngap_just ~ 1 + (1 | pais), data = df_study1, REML = FALSE)

screenreg(r01)
#ICC
varcomp=as.data.frame(VarCorr(r01))
tau00=varcomp[1,4]
sigma2=varcomp[2,4]
icc=tau00/(tau00+sigma2)
icc
#0.1994494

#modelo within
r02 <- lmer(lngap_just ~ 1 + lngap_perc_cwc+ mov_obj_cwc + mov_subj_cwc +
              edad_cwc + hombre_cwc + educ_cwc + sindicato_cwc+
              q2_cwc +q3_cwc +q4_cwc +q5_cwc + (1 | pais), data = df_study1, REML = FALSE)

#modelo between
r03 <- lmer(lngap_just ~ 1 + lngap_perc_cgm+ mov_obj_cgm + mov_subj_cgm +
              edad_cgm + hombre_cgm + educ_cgm + sindicato_cgm+
              q2_cgm +q3_cgm +q4_cgm +q5_cgm + (1 | pais), data = df_study1, REML = FALSE)

#modelo full
r04 <- lmer(lngap_just ~ 1 + lngap_perc_cwc+ mov_obj_cwc + mov_subj_cwc +
              edad_cwc + hombre_cwc + educ_cwc + sindicato_cwc+
              q2_cwc +q3_cwc +q4_cwc +q5_cwc +
              lngap_perc_cgm+ mov_obj_cgm + mov_subj_cgm +
              edad_cgm + hombre_cgm + educ_cgm + sindicato_cgm+
              q2_cgm +q3_cgm +q4_cgm +q5_cgm + (1 | pais), data = df_study1, REML = FALSE)
screenreg(l=list(r01,r02,r03,r04))


#Modelos con variable movilidad subjetiva categorica

#modelo within
r05 <- lmer(lngap_just ~ 1 + lngap_perc_cwc+ mov_obj_cwc + sub_asc_cwc+sub_des_cwc +
              edad_cwc + hombre_cwc + educ_cwc + sindicato_cwc+
              q2_cwc +q3_cwc +q4_cwc +q5_cwc + (1 | pais), data = df_study1, REML = FALSE)

#modelo between
r06 <- lmer(lngap_just ~ 1 + lngap_perc_cgm+ mov_obj_cgm + sub_asc_cgm+sub_des_cgm +
              edad_cgm + hombre_cgm + educ_cgm + sindicato_cgm+
              q2_cgm +q3_cgm +q4_cgm +q5_cgm + (1 | pais), data = df_study1, REML = FALSE)

#modelo full
r07 <- lmer(lngap_just ~ 1 + lngap_perc_cwc+ mov_obj_cwc + sub_asc_cwc+sub_des_cwc +
              edad_cwc + hombre_cwc + educ_cwc + sindicato_cwc+
              q2_cwc +q3_cwc +q4_cwc +q5_cwc +
              lngap_perc_cgm+ mov_obj_cgm + sub_asc_cgm+sub_des_cgm +
              edad_cgm + hombre_cgm + educ_cgm + sindicato_cgm+
              q2_cgm +q3_cgm +q4_cgm +q5_cgm + (1 | pais), data = df_study1, REML = FALSE)

summary(r07)

omit <- "((Intercept))|(hombre_cwc)|(educ_cwc)|(sindicato_cwc)|(q2_cwc)|(q3_cwc)|(q4_cwc)|(q5_cwc)|(lngap_perc_cgm)|(mov_obj_cgm)|(sub_asc_cgm)|(sub_des_cgm)|(edad_cgm)|(hombre_cgm)|(educ_cgm)|(sindicato_cgm)|(q2_cgm)|(q3_cgm)|(q4_cgm)|(q5_cgm)|(edad_cwc)"

omit <-
c(
  "(Intercept)"   ,
  "lngap_perc_cwc",
  # "mov_obj_cwc"   ,
  # "sub_asc_cwc"   ,
  # "sub_des_cwc"    ,
  "edad_cwc"      ,
  "hombre_cwc"    ,
  "educ_cwc"     ,
  "sindicato_cwc"  ,
  "q2_cwc"        ,
  "q3_cwc"        ,
  "q4_cwc"        ,
  "q5_cwc"        ,
  "lngap_perc_cgm",
  "mov_obj_cgm"    ,
  "sub_asc_cgm"   ,
  "sub_des_cgm"    ,
  "edad_cgm"       ,
  "hombre_cgm"     ,
  "educ_cgm"       ,
  "sindicato_cgm"  ,
  "q2_cgm"         ,
  "q3_cgm"        ,
  "q4_cgm"         ,
  "q5_cgm"
)

custom.coef <- c("Movilidad Objetiva (cwc)",
                 "Movilidad Subjetiva Ascendente (cwc)",
                 "Movilidad Subjetiva Descendente (cwc)")

screenreg(l=list(r07),
          # omit.coef = omit,
          # custom.coef.names = custom.coef
          )


r07_plot <-
jtools::plot_summs(r07,
                   coefs =  c("mov_obj_cwc","sub_asc_cwc","sub_des_cwc")) +
  labs(title = "Modelo Multinivel comparado Países - ISSP 2009",
       caption = paste0("Individuos = ",nobs(r07),
                        ", Paises = ",length(r07@u),
                        ", Incluye controles")) +
  scale_y_discrete(labels=rev(custom.coef))
r07_plot

ggsave(plot = r07_plot,
       filename = here::here("output/images/model_issp.png"),
       device = "png",
       width = 20,height = 15,
       units = "cm")





