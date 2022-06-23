#Movilidad social y justificación de desigualdad

#cargar base de datos
library(haven)
library(sjlabelled)
library("here")
library(dplyr)
library(sjmisc)
library(lme4)
library(texreg)
issp<- haven::read_sav(here::here("input/data/original/ZA5400_v4-0-0.sav"))




# Recodificación de variables relevantes ----------------------------------

# Educacion ---------------------------------------------------------------


table(issp$DEGREE)

issp$EDUCYRS[issp$EDUCYRS %in% c(95,96,97,98,99)] <- NA # year of education
issp$DEGREE[issp$DEGREE   %in% c(8,9)]            <- NA # formal qualification level (comparative)

summary(issp$EDUCYRS)
class(issp$EDUCYRS)
issp$EDUCYRS <- as.numeric(issp$EDUCYRS)
table(issp$DEGREE)

issp$DEGREE <- as_factor(issp$DEGREE)



# nombre sustantivo educacion ----------------------------------------------#

issp <- rename(issp,educ=EDUCYRS,educat=DEGREE)

table(issp$educat) # OK, (CORRER TODO PARA QUE QUEDE BIEN!)

# Educación: Educacion terciaria

issp$edter <- ifelse(test = (issp$educat ==5),yes = 1,no = 0) # Complete & Incomplete Terciary =1; Less than Terciary =0
table(issp$edter)


# Ingresos ----------------------------------------------------------------

# - Ingresos en Deciles
# - Ingresos en log
# - Si voy a usar la variable ingreso como control, ¿debería usarla en zscore para mantener validez inter-temporal?

# Year 2009---------------------------------------------------------------------------------#


income <- c("AR_INC","AT_INC","AU_INC","BE_INC","BG_INC","CH_INC","CL_INC","CN_INC","CY_INC","CZ_INC","DE_INC","DK_INC",
            "EE_INC","ES_INC","FI_INC","FR_INC","GB_INC","HR_INC","HU_INC","IL_INC","IS_INC","IT_INC","JP_INC","KR_INC","LT_INC",
            "LV_INC","NO_INC","NZ_INC","PH_INC","PL_INC","PT_INC","RU_INC","SE_INC","SI_INC","SK_INC","TR_INC","TW_INC","UA_INC",
            "US_INC","VE_INC","ZA_INC")

lapply(issp[,income], table)

for (i in income) {
  issp[[i]][issp[[i]] %in% c(999990,999997,999998,999999,9999990,9999998,9999999,9999997,99999990,99999999,99999990,99999999,99999998)] <- NA
  issp[[i]][issp[[i]] %in% c(999990,999997,999998,999999,9999990,9999998,9999999,9999997,99999990,99999999,99999990,99999999,99999998)] <- NA
}

lapply(issp[,income], table)  # Ok el recode de ingresos familiares

# Crear variable ingresos unica

psum2 <- function(...,na.rm=FALSE) {
  dat <- do.call(cbind,list(...))
  res <- rowSums(dat, na.rm=na.rm)
  idx_na <- !rowSums(!is.na(dat))
  res[idx_na] <- NA
  res
}

issp$INCOME <- psum2(x = issp[,income],na.rm = TRUE)
class(issp$INCOME)

table(issp$HOMPOP)
issp$HOMPOP[issp$HOMPOP %in%c(0,99) ] <- NA

issp$incomepcap <- as.numeric(issp$INCOME/issp$HOMPOP) # Ingreso per capita del hogar
table(issp$incomepcap)

issp <- rename(issp,country=V4)

issp <- issp %>%group_by(country)  %>% mutate(D10=ntile(incomepcap,10),
                                              Q05=ntile(incomepcap,5),
                                              zinc=scale(incomepcap,center = TRUE),
                                              loginc=log(incomepcap)) %>% ungroup()
deciles <- issp[,c("country","INCOME","HOMPOP","incomepcap","zinc","D10")]
table(issp$Q05)



# Ocupación del entrevistado ----------------------------------------------


#     - 9996 Not classif; inadeq described
#     - 9997 Refused
#     - 9998 Dont know
#     - 9999 Na
#     - 0000 NAP,NAV
table(issp$ISCO88)

500+170+600+1451+6820 #check de missing. Debería tener 9541 missing en ISEI
# Crear ISEI con recode de Ganzeboom & Treiman-----------------------------#
issp$isei<- issp$ISCO88
issp$isei[issp$isei %in% c(0,9996,9997,9998,9999) ] <- NA


{
  issp$isei[issp$ISCO88==1000]=55
  issp$isei[issp$ISCO88==1100]=70
  issp$isei[issp$ISCO88==1110]=77
  issp$isei[issp$ISCO88==1120]=77
  issp$isei[issp$ISCO88==1130]=66
  issp$isei[issp$ISCO88==1140]=58
  issp$isei[issp$ISCO88==1141]=58
  issp$isei[issp$ISCO88==1142]=58
  issp$isei[issp$ISCO88==1143]=58
  issp$isei[issp$ISCO88==1200]=68
  issp$isei[issp$ISCO88==1210]=70
  issp$isei[issp$ISCO88==1220]=67
  issp$isei[issp$ISCO88==1221]=67
  issp$isei[issp$ISCO88==1222]=67
  issp$isei[issp$ISCO88==1223]=67
  issp$isei[issp$ISCO88==1224]=59
  issp$isei[issp$ISCO88==1225]=59
  issp$isei[issp$ISCO88==1226]=59
  issp$isei[issp$ISCO88==1227]=87
  issp$isei[issp$ISCO88==1228]=59
  issp$isei[issp$ISCO88==1229]=67
  issp$isei[issp$ISCO88==1230]=61
  issp$isei[issp$ISCO88==1231]=69
  issp$isei[issp$ISCO88==1232]=69
  issp$isei[issp$ISCO88==1233]=56
  issp$isei[issp$ISCO88==1234]=69
  issp$isei[issp$ISCO88==1235]=69
  issp$isei[issp$ISCO88==1236]=69
  issp$isei[issp$ISCO88==1237]=69
  issp$isei[issp$ISCO88==1239]=69
  issp$isei[issp$ISCO88==1240]=58
  issp$isei[issp$ISCO88==1250]=64
  issp$isei[issp$ISCO88==1251]=70
  issp$isei[issp$ISCO88==1252]=60
  issp$isei[issp$ISCO88==1300]=51
  issp$isei[issp$ISCO88==1310]=51
  issp$isei[issp$ISCO88==1311]=43
  issp$isei[issp$ISCO88==1312]=56
  issp$isei[issp$ISCO88==1313]=51
  issp$isei[issp$ISCO88==1314]=49
  issp$isei[issp$ISCO88==1315]=44
  issp$isei[issp$ISCO88==1316]=51
  issp$isei[issp$ISCO88==1317]=51
  issp$isei[issp$ISCO88==1318]=51
  issp$isei[issp$ISCO88==1319]=51
  issp$isei[issp$ISCO88==2000]=70
  issp$isei[issp$ISCO88==2100]=69
  issp$isei[issp$ISCO88==2110]=74
  issp$isei[issp$ISCO88==2111]=74
  issp$isei[issp$ISCO88==2112]=74
  issp$isei[issp$ISCO88==2113]=74
  issp$isei[issp$ISCO88==2114]=74
  issp$isei[issp$ISCO88==2120]=71
  issp$isei[issp$ISCO88==2121]=71
  issp$isei[issp$ISCO88==2122]=71
  issp$isei[issp$ISCO88==2130]=71
  issp$isei[issp$ISCO88==2131]=71
  issp$isei[issp$ISCO88==2132]=71
  issp$isei[issp$ISCO88==2139]=71
  issp$isei[issp$ISCO88==2140]=73
  issp$isei[issp$ISCO88==2141]=69
  issp$isei[issp$ISCO88==2142]=69
  issp$isei[issp$ISCO88==2143]=68
  issp$isei[issp$ISCO88==2144]=68
  issp$isei[issp$ISCO88==2145]=67
  issp$isei[issp$ISCO88==2146]=71
  issp$isei[issp$ISCO88==2147]=67
  issp$isei[issp$ISCO88==2148]=56
  issp$isei[issp$ISCO88==2149]=69
  issp$isei[issp$ISCO88==2200]=80
  issp$isei[issp$ISCO88==2210]=78
  issp$isei[issp$ISCO88==2211]=77
  issp$isei[issp$ISCO88==2212]=77
  issp$isei[issp$ISCO88==2213]=79
  issp$isei[issp$ISCO88==2220]=85
  issp$isei[issp$ISCO88==2221]=88
  issp$isei[issp$ISCO88==2222]=85
  issp$isei[issp$ISCO88==2223]=83
  issp$isei[issp$ISCO88==2224]=74
  issp$isei[issp$ISCO88==2229]=85
  issp$isei[issp$ISCO88==2230]=43
  issp$isei[issp$ISCO88==2300]=69
  issp$isei[issp$ISCO88==2310]=77
  issp$isei[issp$ISCO88==2320]=69
  issp$isei[issp$ISCO88==2321]=70
  issp$isei[issp$ISCO88==2322]=66
  issp$isei[issp$ISCO88==2330]=66
  issp$isei[issp$ISCO88==2331]=66
  issp$isei[issp$ISCO88==2332]=43
  issp$isei[issp$ISCO88==2340]=66
  issp$isei[issp$ISCO88==2350]=66
  issp$isei[issp$ISCO88==2351]=70
  issp$isei[issp$ISCO88==2352]=70
  issp$isei[issp$ISCO88==2359]=65
  issp$isei[issp$ISCO88==2400]=68
  issp$isei[issp$ISCO88==2410]=69
  issp$isei[issp$ISCO88==2411]=69
  issp$isei[issp$ISCO88==2412]=69
  issp$isei[issp$ISCO88==2419]=69
  issp$isei[issp$ISCO88==2420]=85
  issp$isei[issp$ISCO88==2421]=85
  issp$isei[issp$ISCO88==2422]=90
  issp$isei[issp$ISCO88==2429]=82
  issp$isei[issp$ISCO88==2430]=65
  issp$isei[issp$ISCO88==2431]=65
  issp$isei[issp$ISCO88==2432]=65
  issp$isei[issp$ISCO88==2440]=65
  issp$isei[issp$ISCO88==2441]=78
  issp$isei[issp$ISCO88==2442]=71
  issp$isei[issp$ISCO88==2443]=71
  issp$isei[issp$ISCO88==2444]=65
  issp$isei[issp$ISCO88==2445]=71
  issp$isei[issp$ISCO88==2446]=51
  issp$isei[issp$ISCO88==2450]=61
  issp$isei[issp$ISCO88==2451]=65
  issp$isei[issp$ISCO88==2452]=54
  issp$isei[issp$ISCO88==2453]=64
  issp$isei[issp$ISCO88==2454]=64
  issp$isei[issp$ISCO88==2455]=64
  issp$isei[issp$ISCO88==2460]=53
  issp$isei[issp$ISCO88==3000]=54
  issp$isei[issp$ISCO88==3100]=50
  issp$isei[issp$ISCO88==3110]=49
  issp$isei[issp$ISCO88==3111]=45
  issp$isei[issp$ISCO88==3112]=45
  issp$isei[issp$ISCO88==3113]=46
  issp$isei[issp$ISCO88==3114]=46
  issp$isei[issp$ISCO88==3115]=54
  issp$isei[issp$ISCO88==3116]=54
  issp$isei[issp$ISCO88==3117]=54
  issp$isei[issp$ISCO88==3118]=51
  issp$isei[issp$ISCO88==3119]=53
  issp$isei[issp$ISCO88==3120]=52
  issp$isei[issp$ISCO88==3121]=52
  issp$isei[issp$ISCO88==3122]=52
  issp$isei[issp$ISCO88==3123]=52
  issp$isei[issp$ISCO88==3130]=52
  issp$isei[issp$ISCO88==3131]=48
  issp$isei[issp$ISCO88==3132]=57
  issp$isei[issp$ISCO88==3133]=57
  issp$isei[issp$ISCO88==3139]=52
  issp$isei[issp$ISCO88==3140]=57
  issp$isei[issp$ISCO88==3141]=52
  issp$isei[issp$ISCO88==3142]=52
  issp$isei[issp$ISCO88==3143]=69
  issp$isei[issp$ISCO88==3144]=69
  issp$isei[issp$ISCO88==3145]=50
  issp$isei[issp$ISCO88==3150]=50
  issp$isei[issp$ISCO88==3151]=50
  issp$isei[issp$ISCO88==3152]=50
  issp$isei[issp$ISCO88==3200]=48
  issp$isei[issp$ISCO88==3210]=50
  issp$isei[issp$ISCO88==3211]=50
  issp$isei[issp$ISCO88==3212]=50
  issp$isei[issp$ISCO88==3213]=50
  issp$isei[issp$ISCO88==3220]=55
  issp$isei[issp$ISCO88==3221]=51
  issp$isei[issp$ISCO88==3222]=51
  issp$isei[issp$ISCO88==3223]=51
  issp$isei[issp$ISCO88==3224]=60
  issp$isei[issp$ISCO88==3225]=51
  issp$isei[issp$ISCO88==3226]=60
  issp$isei[issp$ISCO88==3227]=51
  issp$isei[issp$ISCO88==3228]=51
  issp$isei[issp$ISCO88==3229]=51
  issp$isei[issp$ISCO88==3230]=38
  issp$isei[issp$ISCO88==3231]=38
  issp$isei[issp$ISCO88==3232]=38
  issp$isei[issp$ISCO88==3240]=49
  issp$isei[issp$ISCO88==3241]=51
  issp$isei[issp$ISCO88==3242]=38
  issp$isei[issp$ISCO88==3300]=38
  issp$isei[issp$ISCO88==3310]=38
  issp$isei[issp$ISCO88==3320]=38
  issp$isei[issp$ISCO88==3330]=38
  issp$isei[issp$ISCO88==3340]=38
  issp$isei[issp$ISCO88==3400]=55
  issp$isei[issp$ISCO88==3410]=55
  issp$isei[issp$ISCO88==3411]=61
  issp$isei[issp$ISCO88==3412]=54
  issp$isei[issp$ISCO88==3413]=59
  issp$isei[issp$ISCO88==3414]=56
  issp$isei[issp$ISCO88==3415]=56
  issp$isei[issp$ISCO88==3416]=50
  issp$isei[issp$ISCO88==3417]=56
  issp$isei[issp$ISCO88==3419]=55
  issp$isei[issp$ISCO88==3420]=55
  issp$isei[issp$ISCO88==3421]=55
  issp$isei[issp$ISCO88==3422]=55
  issp$isei[issp$ISCO88==3423]=55
  issp$isei[issp$ISCO88==3429]=55
  issp$isei[issp$ISCO88==3430]=54
  issp$isei[issp$ISCO88==3431]=54
  issp$isei[issp$ISCO88==3432]=59
  issp$isei[issp$ISCO88==3433]=51
  issp$isei[issp$ISCO88==3434]=61
  issp$isei[issp$ISCO88==3439]=54
  issp$isei[issp$ISCO88==3440]=56
  issp$isei[issp$ISCO88==3441]=56
  issp$isei[issp$ISCO88==3442]=57
  issp$isei[issp$ISCO88==3443]=56
  issp$isei[issp$ISCO88==3444]=46
  issp$isei[issp$ISCO88==3449]=56
  issp$isei[issp$ISCO88==3450]=56
  issp$isei[issp$ISCO88==3451]=55
  issp$isei[issp$ISCO88==3452]=56
  issp$isei[issp$ISCO88==3460]=43
  issp$isei[issp$ISCO88==3470]=52
  issp$isei[issp$ISCO88==3471]=53
  issp$isei[issp$ISCO88==3472]=64
  issp$isei[issp$ISCO88==3473]=50
  issp$isei[issp$ISCO88==3474]=50
  issp$isei[issp$ISCO88==3475]=54
  issp$isei[issp$ISCO88==3480]=38
  issp$isei[issp$ISCO88==4000]=45
  issp$isei[issp$ISCO88==4100]=45
  issp$isei[issp$ISCO88==4110]=51
  issp$isei[issp$ISCO88==4111]=51
  issp$isei[issp$ISCO88==4112]=50
  issp$isei[issp$ISCO88==4113]=50
  issp$isei[issp$ISCO88==4114]=51
  issp$isei[issp$ISCO88==4115]=53
  issp$isei[issp$ISCO88==4120]=51
  issp$isei[issp$ISCO88==4121]=51
  issp$isei[issp$ISCO88==4122]=51
  issp$isei[issp$ISCO88==4130]=36
  issp$isei[issp$ISCO88==4131]=32
  issp$isei[issp$ISCO88==4132]=43
  issp$isei[issp$ISCO88==4133]=45
  issp$isei[issp$ISCO88==4140]=39
  issp$isei[issp$ISCO88==4141]=39
  issp$isei[issp$ISCO88==4142]=39
  issp$isei[issp$ISCO88==4143]=39
  issp$isei[issp$ISCO88==4144]=39
  issp$isei[issp$ISCO88==4190]=39
  issp$isei[issp$ISCO88==4200]=49
  issp$isei[issp$ISCO88==4210]=48
  issp$isei[issp$ISCO88==4211]=53
  issp$isei[issp$ISCO88==4212]=46
  issp$isei[issp$ISCO88==4213]=40
  issp$isei[issp$ISCO88==4214]=40
  issp$isei[issp$ISCO88==4215]=40
  issp$isei[issp$ISCO88==4220]=52
  issp$isei[issp$ISCO88==4221]=52
  issp$isei[issp$ISCO88==4222]=52
  issp$isei[issp$ISCO88==4223]=52
  issp$isei[issp$ISCO88==5000]=40
  issp$isei[issp$ISCO88==5100]=38
  issp$isei[issp$ISCO88==5110]=34
  issp$isei[issp$ISCO88==5111]=34
  issp$isei[issp$ISCO88==5112]=34
  issp$isei[issp$ISCO88==5113]=34
  issp$isei[issp$ISCO88==5120]=32
  issp$isei[issp$ISCO88==5121]=30
  issp$isei[issp$ISCO88==5122]=30
  issp$isei[issp$ISCO88==5123]=34
  issp$isei[issp$ISCO88==5130]=25
  issp$isei[issp$ISCO88==5131]=25
  issp$isei[issp$ISCO88==5132]=25
  issp$isei[issp$ISCO88==5133]=25
  issp$isei[issp$ISCO88==5139]=25
  issp$isei[issp$ISCO88==5140]=30
  issp$isei[issp$ISCO88==5141]=29
  issp$isei[issp$ISCO88==5142]=19
  issp$isei[issp$ISCO88==5143]=54
  issp$isei[issp$ISCO88==5149]=19
  issp$isei[issp$ISCO88==5150]=43
  issp$isei[issp$ISCO88==5151]=43
  issp$isei[issp$ISCO88==5152]=43
  issp$isei[issp$ISCO88==5160]=47
  issp$isei[issp$ISCO88==5161]=42
  issp$isei[issp$ISCO88==5162]=50
  issp$isei[issp$ISCO88==5163]=40
  issp$isei[issp$ISCO88==5164]=40
  issp$isei[issp$ISCO88==5169]=40
  issp$isei[issp$ISCO88==5200]=43
  issp$isei[issp$ISCO88==5210]=43
  issp$isei[issp$ISCO88==5220]=43
  issp$isei[issp$ISCO88==5230]=37
  issp$isei[issp$ISCO88==6000]=23
  issp$isei[issp$ISCO88==6100]=23
  issp$isei[issp$ISCO88==6110]=23
  issp$isei[issp$ISCO88==6111]=23
  issp$isei[issp$ISCO88==6112]=23
  issp$isei[issp$ISCO88==6113]=23
  issp$isei[issp$ISCO88==6114]=23
  issp$isei[issp$ISCO88==6120]=23
  issp$isei[issp$ISCO88==6121]=23
  issp$isei[issp$ISCO88==6122]=23
  issp$isei[issp$ISCO88==6123]=23
  issp$isei[issp$ISCO88==6124]=23
  issp$isei[issp$ISCO88==6129]=23
  issp$isei[issp$ISCO88==6130]=23
  issp$isei[issp$ISCO88==6131]=23
  issp$isei[issp$ISCO88==6132]=27
  issp$isei[issp$ISCO88==6133]=28
  issp$isei[issp$ISCO88==6134]=28
  issp$isei[issp$ISCO88==6140]=22
  issp$isei[issp$ISCO88==6141]=22
  issp$isei[issp$ISCO88==6142]=22
  issp$isei[issp$ISCO88==6150]=28
  issp$isei[issp$ISCO88==6151]=28
  issp$isei[issp$ISCO88==6152]=28
  issp$isei[issp$ISCO88==6153]=28
  issp$isei[issp$ISCO88==6154]=28
  issp$isei[issp$ISCO88==6200]=16
  issp$isei[issp$ISCO88==6210]=16
  issp$isei[issp$ISCO88==7000]=34
  issp$isei[issp$ISCO88==7100]=31
  issp$isei[issp$ISCO88==7110]=30
  issp$isei[issp$ISCO88==7111]=30
  issp$isei[issp$ISCO88==7112]=30
  issp$isei[issp$ISCO88==7113]=27
  issp$isei[issp$ISCO88==7120]=30
  issp$isei[issp$ISCO88==7121]=29
  issp$isei[issp$ISCO88==7122]=29
  issp$isei[issp$ISCO88==7123]=26
  issp$isei[issp$ISCO88==7124]=29
  issp$isei[issp$ISCO88==7129]=30
  issp$isei[issp$ISCO88==7130]=34
  issp$isei[issp$ISCO88==7131]=19
  issp$isei[issp$ISCO88==7132]=30
  issp$isei[issp$ISCO88==7133]=31
  issp$isei[issp$ISCO88==7134]=34
  issp$isei[issp$ISCO88==7135]=26
  issp$isei[issp$ISCO88==7136]=33
  issp$isei[issp$ISCO88==7137]=37
  issp$isei[issp$ISCO88==7140]=29
  issp$isei[issp$ISCO88==7141]=29
  issp$isei[issp$ISCO88==7142]=32
  issp$isei[issp$ISCO88==7143]=29
  issp$isei[issp$ISCO88==7200]=34
  issp$isei[issp$ISCO88==7210]=31
  issp$isei[issp$ISCO88==7211]=29
  issp$isei[issp$ISCO88==7212]=30
  issp$isei[issp$ISCO88==7213]=33
  issp$isei[issp$ISCO88==7214]=30
  issp$isei[issp$ISCO88==7215]=30
  issp$isei[issp$ISCO88==7216]=30
  issp$isei[issp$ISCO88==7220]=35
  issp$isei[issp$ISCO88==7221]=33
  issp$isei[issp$ISCO88==7222]=40
  issp$isei[issp$ISCO88==7223]=34
  issp$isei[issp$ISCO88==7224]=24
  issp$isei[issp$ISCO88==7230]=34
  issp$isei[issp$ISCO88==7231]=34
  issp$isei[issp$ISCO88==7232]=42
  issp$isei[issp$ISCO88==7233]=33
  issp$isei[issp$ISCO88==7234]=23
  issp$isei[issp$ISCO88==7240]=40
  issp$isei[issp$ISCO88==7241]=40
  issp$isei[issp$ISCO88==7242]=39
  issp$isei[issp$ISCO88==7243]=41
  issp$isei[issp$ISCO88==7244]=40
  issp$isei[issp$ISCO88==7245]=38
  issp$isei[issp$ISCO88==7300]=34
  issp$isei[issp$ISCO88==7310]=38
  issp$isei[issp$ISCO88==7311]=38
  issp$isei[issp$ISCO88==7312]=38
  issp$isei[issp$ISCO88==7313]=38
  issp$isei[issp$ISCO88==7320]=28
  issp$isei[issp$ISCO88==7321]=27
  issp$isei[issp$ISCO88==7322]=29
  issp$isei[issp$ISCO88==7323]=29
  issp$isei[issp$ISCO88==7324]=29
  issp$isei[issp$ISCO88==7330]=29
  issp$isei[issp$ISCO88==7331]=29
  issp$isei[issp$ISCO88==7332]=29
  issp$isei[issp$ISCO88==7340]=40
  issp$isei[issp$ISCO88==7341]=40
  issp$isei[issp$ISCO88==7342]=40
  issp$isei[issp$ISCO88==7343]=42
  issp$isei[issp$ISCO88==7344]=40
  issp$isei[issp$ISCO88==7345]=37
  issp$isei[issp$ISCO88==7346]=38
  issp$isei[issp$ISCO88==7400]=33
  issp$isei[issp$ISCO88==7410]=30
  issp$isei[issp$ISCO88==7411]=30
  issp$isei[issp$ISCO88==7412]=31
  issp$isei[issp$ISCO88==7413]=30
  issp$isei[issp$ISCO88==7414]=30
  issp$isei[issp$ISCO88==7415]=30
  issp$isei[issp$ISCO88==7416]=30
  issp$isei[issp$ISCO88==7420]=33
  issp$isei[issp$ISCO88==7421]=33
  issp$isei[issp$ISCO88==7422]=33
  issp$isei[issp$ISCO88==7423]=33
  issp$isei[issp$ISCO88==7424]=33
  issp$isei[issp$ISCO88==7430]=36
  issp$isei[issp$ISCO88==7431]=29
  issp$isei[issp$ISCO88==7432]=29
  issp$isei[issp$ISCO88==7433]=45
  issp$isei[issp$ISCO88==7434]=36
  issp$isei[issp$ISCO88==7435]=36
  issp$isei[issp$ISCO88==7436]=33
  issp$isei[issp$ISCO88==7437]=28
  issp$isei[issp$ISCO88==7440]=31
  issp$isei[issp$ISCO88==7441]=31
  issp$isei[issp$ISCO88==7442]=31
  issp$isei[issp$ISCO88==7500]=42
  issp$isei[issp$ISCO88==7510]=42
  issp$isei[issp$ISCO88==7520]=39
  issp$isei[issp$ISCO88==7530]=26
  issp$isei[issp$ISCO88==8000]=31
  issp$isei[issp$ISCO88==8100]=30
  issp$isei[issp$ISCO88==8110]=35
  issp$isei[issp$ISCO88==8111]=35
  issp$isei[issp$ISCO88==8112]=35
  issp$isei[issp$ISCO88==8113]=35
  issp$isei[issp$ISCO88==8120]=30
  issp$isei[issp$ISCO88==8121]=31
  issp$isei[issp$ISCO88==8122]=30
  issp$isei[issp$ISCO88==8123]=28
  issp$isei[issp$ISCO88==8124]=30
  issp$isei[issp$ISCO88==8130]=22
  issp$isei[issp$ISCO88==8131]=22
  issp$isei[issp$ISCO88==8139]=22
  issp$isei[issp$ISCO88==8140]=27
  issp$isei[issp$ISCO88==8141]=27
  issp$isei[issp$ISCO88==8142]=27
  issp$isei[issp$ISCO88==8143]=27
  issp$isei[issp$ISCO88==8150]=35
  issp$isei[issp$ISCO88==8151]=35
  issp$isei[issp$ISCO88==8152]=35
  issp$isei[issp$ISCO88==8153]=35
  issp$isei[issp$ISCO88==8154]=35
  issp$isei[issp$ISCO88==8155]=35
  issp$isei[issp$ISCO88==8159]=35
  issp$isei[issp$ISCO88==8160]=32
  issp$isei[issp$ISCO88==8161]=33
  issp$isei[issp$ISCO88==8162]=27
  issp$isei[issp$ISCO88==8163]=33
  issp$isei[issp$ISCO88==8170]=26
  issp$isei[issp$ISCO88==8171]=26
  issp$isei[issp$ISCO88==8172]=26
  issp$isei[issp$ISCO88==8200]=32
  issp$isei[issp$ISCO88==8210]=36
  issp$isei[issp$ISCO88==8211]=36
  issp$isei[issp$ISCO88==8212]=30
  issp$isei[issp$ISCO88==8220]=30
  issp$isei[issp$ISCO88==8221]=30
  issp$isei[issp$ISCO88==8222]=30
  issp$isei[issp$ISCO88==8223]=30
  issp$isei[issp$ISCO88==8224]=30
  issp$isei[issp$ISCO88==8229]=30
  issp$isei[issp$ISCO88==8230]=30
  issp$isei[issp$ISCO88==8231]=30
  issp$isei[issp$ISCO88==8232]=30
  issp$isei[issp$ISCO88==8240]=29
  issp$isei[issp$ISCO88==8250]=38
  issp$isei[issp$ISCO88==8251]=38
  issp$isei[issp$ISCO88==8252]=38
  issp$isei[issp$ISCO88==8253]=38
  issp$isei[issp$ISCO88==8260]=30
  issp$isei[issp$ISCO88==8261]=29
  issp$isei[issp$ISCO88==8262]=29
  issp$isei[issp$ISCO88==8263]=32
  issp$isei[issp$ISCO88==8264]=24
  issp$isei[issp$ISCO88==8265]=32
  issp$isei[issp$ISCO88==8266]=32
  issp$isei[issp$ISCO88==8269]=32
  issp$isei[issp$ISCO88==8270]=29
  issp$isei[issp$ISCO88==8271]=29
  issp$isei[issp$ISCO88==8272]=29
  issp$isei[issp$ISCO88==8273]=29
  issp$isei[issp$ISCO88==8274]=29
  issp$isei[issp$ISCO88==8275]=29
  issp$isei[issp$ISCO88==8276]=29
  issp$isei[issp$ISCO88==8277]=29
  issp$isei[issp$ISCO88==8278]=29
  issp$isei[issp$ISCO88==8279]=29
  issp$isei[issp$ISCO88==8280]=31
  issp$isei[issp$ISCO88==8281]=30
  issp$isei[issp$ISCO88==8282]=34
  issp$isei[issp$ISCO88==8283]=34
  issp$isei[issp$ISCO88==8284]=30
  issp$isei[issp$ISCO88==8285]=30
  issp$isei[issp$ISCO88==8286]=30
  issp$isei[issp$ISCO88==8290]=26
  issp$isei[issp$ISCO88==8300]=32
  issp$isei[issp$ISCO88==8310]=36
  issp$isei[issp$ISCO88==8311]=41
  issp$isei[issp$ISCO88==8312]=32
  issp$isei[issp$ISCO88==8320]=34
  issp$isei[issp$ISCO88==8321]=30
  issp$isei[issp$ISCO88==8322]=30
  issp$isei[issp$ISCO88==8323]=30
  issp$isei[issp$ISCO88==8324]=34
  issp$isei[issp$ISCO88==8330]=26
  issp$isei[issp$ISCO88==8331]=26
  issp$isei[issp$ISCO88==8332]=26
  issp$isei[issp$ISCO88==8333]=28
  issp$isei[issp$ISCO88==8334]=28
  issp$isei[issp$ISCO88==8340]=32
  issp$isei[issp$ISCO88==8400]=24
  issp$isei[issp$ISCO88==9000]=20
  issp$isei[issp$ISCO88==9100]=25
  issp$isei[issp$ISCO88==9110]=29
  issp$isei[issp$ISCO88==9111]=29
  issp$isei[issp$ISCO88==9112]=28
  issp$isei[issp$ISCO88==9113]=29
  issp$isei[issp$ISCO88==9120]=28
  issp$isei[issp$ISCO88==9130]=16
  issp$isei[issp$ISCO88==9131]=16
  issp$isei[issp$ISCO88==9132]=16
  issp$isei[issp$ISCO88==9133]=16
  issp$isei[issp$ISCO88==9140]=23
  issp$isei[issp$ISCO88==9141]=23
  issp$isei[issp$ISCO88==9142]=23
  issp$isei[issp$ISCO88==9150]=27
  issp$isei[issp$ISCO88==9151]=25
  issp$isei[issp$ISCO88==9152]=27
  issp$isei[issp$ISCO88==9153]=27
  issp$isei[issp$ISCO88==9160]=23
  issp$isei[issp$ISCO88==9161]=23
  issp$isei[issp$ISCO88==9162]=23
  issp$isei[issp$ISCO88==9200]=16
  issp$isei[issp$ISCO88==9210]=16
  issp$isei[issp$ISCO88==9211]=16
  issp$isei[issp$ISCO88==9212]=16
  issp$isei[issp$ISCO88==9213]=16
  issp$isei[issp$ISCO88==9300]=23
  issp$isei[issp$ISCO88==9310]=21
  issp$isei[issp$ISCO88==9311]=21
  issp$isei[issp$ISCO88==9312]=21
  issp$isei[issp$ISCO88==9313]=21
  issp$isei[issp$ISCO88==9320]=20
  issp$isei[issp$ISCO88==9321]=20
  issp$isei[issp$ISCO88==9322]=24
  issp$isei[issp$ISCO88==9330]=29
  issp$isei[issp$ISCO88==9331]=22
  issp$isei[issp$ISCO88==9332]=22
  issp$isei[issp$ISCO88==9333]=30
  issp$isei[issp$ISCO88==110 ]=70}
issp$isei <- as.numeric(issp$isei)
summary(issp$isei)
table(issp$isei)

View(issp[,c("ISCO88","isei")])




# Ocupación del padre -----------------------------------------------------

table(issp$V57)

500+170+600+1451+6820 #check de missing. Debería tener 9541 missing en isei_f
# Crear isei_f con recode de Ganzeboom & Treiman-----------------------------#
issp$isei_f<- issp$V57
issp$isei_f[issp$isei_f %in% c(0,9996,9997,9998,9999, 1) ] <- NA


{
  issp$isei_f[issp$V57==1000]=55
  issp$isei_f[issp$V57==1100]=70
  issp$isei_f[issp$V57==1110]=77
  issp$isei_f[issp$V57==1120]=77
  issp$isei_f[issp$V57==1130]=66
  issp$isei_f[issp$V57==1140]=58
  issp$isei_f[issp$V57==1141]=58
  issp$isei_f[issp$V57==1142]=58
  issp$isei_f[issp$V57==1143]=58
  issp$isei_f[issp$V57==1200]=68
  issp$isei_f[issp$V57==1210]=70
  issp$isei_f[issp$V57==1220]=67
  issp$isei_f[issp$V57==1221]=67
  issp$isei_f[issp$V57==1222]=67
  issp$isei_f[issp$V57==1223]=67
  issp$isei_f[issp$V57==1224]=59
  issp$isei_f[issp$V57==1225]=59
  issp$isei_f[issp$V57==1226]=59
  issp$isei_f[issp$V57==1227]=87
  issp$isei_f[issp$V57==1228]=59
  issp$isei_f[issp$V57==1229]=67
  issp$isei_f[issp$V57==1230]=61
  issp$isei_f[issp$V57==1231]=69
  issp$isei_f[issp$V57==1232]=69
  issp$isei_f[issp$V57==1233]=56
  issp$isei_f[issp$V57==1234]=69
  issp$isei_f[issp$V57==1235]=69
  issp$isei_f[issp$V57==1236]=69
  issp$isei_f[issp$V57==1237]=69
  issp$isei_f[issp$V57==1238]=69 #AGREGADA
  issp$isei_f[issp$V57==1239]=69
  issp$isei_f[issp$V57==1240]=58
  issp$isei_f[issp$V57==1250]=64
  issp$isei_f[issp$V57==1251]=70
  issp$isei_f[issp$V57==1252]=60
  issp$isei_f[issp$V57==1300]=51
  issp$isei_f[issp$V57==1310]=51
  issp$isei_f[issp$V57==1311]=43
  issp$isei_f[issp$V57==1312]=56
  issp$isei_f[issp$V57==1313]=51
  issp$isei_f[issp$V57==1314]=49
  issp$isei_f[issp$V57==1315]=44
  issp$isei_f[issp$V57==1316]=51
  issp$isei_f[issp$V57==1317]=51
  issp$isei_f[issp$V57==1318]=51
  issp$isei_f[issp$V57==1319]=51
  issp$isei_f[issp$V57==2000]=70
  issp$isei_f[issp$V57==2100]=69
  issp$isei_f[issp$V57==2110]=74
  issp$isei_f[issp$V57==2111]=74
  issp$isei_f[issp$V57==2112]=74
  issp$isei_f[issp$V57==2113]=74
  issp$isei_f[issp$V57==2114]=74
  issp$isei_f[issp$V57==2120]=71
  issp$isei_f[issp$V57==2121]=71
  issp$isei_f[issp$V57==2122]=71
  issp$isei_f[issp$V57==2130]=71
  issp$isei_f[issp$V57==2131]=71
  issp$isei_f[issp$V57==2132]=71
  issp$isei_f[issp$V57==2139]=71
  issp$isei_f[issp$V57==2140]=73
  issp$isei_f[issp$V57==2141]=69
  issp$isei_f[issp$V57==2142]=69
  issp$isei_f[issp$V57==2143]=68
  issp$isei_f[issp$V57==2144]=68
  issp$isei_f[issp$V57==2145]=67
  issp$isei_f[issp$V57==2146]=71
  issp$isei_f[issp$V57==2147]=67
  issp$isei_f[issp$V57==2148]=56
  issp$isei_f[issp$V57==2149]=69
  issp$isei_f[issp$V57==2200]=80
  issp$isei_f[issp$V57==2210]=78
  issp$isei_f[issp$V57==2211]=77
  issp$isei_f[issp$V57==2212]=77
  issp$isei_f[issp$V57==2213]=79
  issp$isei_f[issp$V57==2220]=85
  issp$isei_f[issp$V57==2221]=88
  issp$isei_f[issp$V57==2222]=85
  issp$isei_f[issp$V57==2223]=83
  issp$isei_f[issp$V57==2224]=74
  issp$isei_f[issp$V57==2229]=85
  issp$isei_f[issp$V57==2230]=43
  issp$isei_f[issp$V57==2300]=69
  issp$isei_f[issp$V57==2310]=77
  issp$isei_f[issp$V57==2320]=69
  issp$isei_f[issp$V57==2321]=70
  issp$isei_f[issp$V57==2322]=66
  issp$isei_f[issp$V57==2330]=66
  issp$isei_f[issp$V57==2331]=66
  issp$isei_f[issp$V57==2332]=43
  issp$isei_f[issp$V57==2340]=66
  issp$isei_f[issp$V57==2350]=66
  issp$isei_f[issp$V57==2351]=70
  issp$isei_f[issp$V57==2352]=70
  issp$isei_f[issp$V57==2359]=65
  issp$isei_f[issp$V57==2400]=68
  issp$isei_f[issp$V57==2410]=69
  issp$isei_f[issp$V57==2411]=69
  issp$isei_f[issp$V57==2412]=69
  issp$isei_f[issp$V57==2419]=69
  issp$isei_f[issp$V57==2420]=85
  issp$isei_f[issp$V57==2421]=85
  issp$isei_f[issp$V57==2422]=90
  issp$isei_f[issp$V57==2429]=82
  issp$isei_f[issp$V57==2430]=65
  issp$isei_f[issp$V57==2431]=65
  issp$isei_f[issp$V57==2432]=65
  issp$isei_f[issp$V57==2440]=65
  issp$isei_f[issp$V57==2441]=78
  issp$isei_f[issp$V57==2442]=71
  issp$isei_f[issp$V57==2443]=71
  issp$isei_f[issp$V57==2444]=65
  issp$isei_f[issp$V57==2445]=71
  issp$isei_f[issp$V57==2446]=51
  issp$isei_f[issp$V57==2450]=61
  issp$isei_f[issp$V57==2451]=65
  issp$isei_f[issp$V57==2452]=54
  issp$isei_f[issp$V57==2453]=64
  issp$isei_f[issp$V57==2454]=64
  issp$isei_f[issp$V57==2455]=64
  issp$isei_f[issp$V57==2460]=53
  issp$isei_f[issp$V57==3000]=54
  issp$isei_f[issp$V57==3100]=50
  issp$isei_f[issp$V57==3110]=49
  issp$isei_f[issp$V57==3111]=45
  issp$isei_f[issp$V57==3112]=45
  issp$isei_f[issp$V57==3113]=46
  issp$isei_f[issp$V57==3114]=46
  issp$isei_f[issp$V57==3115]=54
  issp$isei_f[issp$V57==3116]=54
  issp$isei_f[issp$V57==3117]=54
  issp$isei_f[issp$V57==3118]=51
  issp$isei_f[issp$V57==3119]=53
  issp$isei_f[issp$V57==3120]=52
  issp$isei_f[issp$V57==3121]=52
  issp$isei_f[issp$V57==3122]=52
  issp$isei_f[issp$V57==3123]=52
  issp$isei_f[issp$V57==3130]=52
  issp$isei_f[issp$V57==3131]=48
  issp$isei_f[issp$V57==3132]=57
  issp$isei_f[issp$V57==3133]=57
  issp$isei_f[issp$V57==3139]=52
  issp$isei_f[issp$V57==3140]=57
  issp$isei_f[issp$V57==3141]=52
  issp$isei_f[issp$V57==3142]=52
  issp$isei_f[issp$V57==3143]=69
  issp$isei_f[issp$V57==3144]=69
  issp$isei_f[issp$V57==3145]=50
  issp$isei_f[issp$V57==3150]=50
  issp$isei_f[issp$V57==3151]=50
  issp$isei_f[issp$V57==3152]=50
  issp$isei_f[issp$V57==3200]=48
  issp$isei_f[issp$V57==3210]=50
  issp$isei_f[issp$V57==3211]=50
  issp$isei_f[issp$V57==3212]=50
  issp$isei_f[issp$V57==3213]=50
  issp$isei_f[issp$V57==3220]=55
  issp$isei_f[issp$V57==3221]=51
  issp$isei_f[issp$V57==3222]=51
  issp$isei_f[issp$V57==3223]=51
  issp$isei_f[issp$V57==3224]=60
  issp$isei_f[issp$V57==3225]=51
  issp$isei_f[issp$V57==3226]=60
  issp$isei_f[issp$V57==3227]=51
  issp$isei_f[issp$V57==3228]=51
  issp$isei_f[issp$V57==3229]=51
  issp$isei_f[issp$V57==3230]=38
  issp$isei_f[issp$V57==3231]=38
  issp$isei_f[issp$V57==3232]=38
  issp$isei_f[issp$V57==3240]=49
  issp$isei_f[issp$V57==3241]=51
  issp$isei_f[issp$V57==3242]=38
  issp$isei_f[issp$V57==3300]=38
  issp$isei_f[issp$V57==3310]=38
  issp$isei_f[issp$V57==3320]=38
  issp$isei_f[issp$V57==3330]=38
  issp$isei_f[issp$V57==3340]=38
  issp$isei_f[issp$V57==3400]=55
  issp$isei_f[issp$V57==3410]=55
  issp$isei_f[issp$V57==3411]=61
  issp$isei_f[issp$V57==3412]=54
  issp$isei_f[issp$V57==3413]=59
  issp$isei_f[issp$V57==3414]=56
  issp$isei_f[issp$V57==3415]=56
  issp$isei_f[issp$V57==3416]=50
  issp$isei_f[issp$V57==3417]=56
  issp$isei_f[issp$V57==3419]=55
  issp$isei_f[issp$V57==3420]=55
  issp$isei_f[issp$V57==3421]=55
  issp$isei_f[issp$V57==3422]=55
  issp$isei_f[issp$V57==3423]=55
  issp$isei_f[issp$V57==3429]=55
  issp$isei_f[issp$V57==3430]=54
  issp$isei_f[issp$V57==3431]=54
  issp$isei_f[issp$V57==3432]=59
  issp$isei_f[issp$V57==3433]=51
  issp$isei_f[issp$V57==3434]=61
  issp$isei_f[issp$V57==3439]=54
  issp$isei_f[issp$V57==3440]=56
  issp$isei_f[issp$V57==3441]=56
  issp$isei_f[issp$V57==3442]=57
  issp$isei_f[issp$V57==3443]=56
  issp$isei_f[issp$V57==3444]=46
  issp$isei_f[issp$V57==3449]=56
  issp$isei_f[issp$V57==3450]=56
  issp$isei_f[issp$V57==3451]=55
  issp$isei_f[issp$V57==3452]=56
  issp$isei_f[issp$V57==3460]=43
  issp$isei_f[issp$V57==3470]=52
  issp$isei_f[issp$V57==3471]=53
  issp$isei_f[issp$V57==3472]=64
  issp$isei_f[issp$V57==3473]=50
  issp$isei_f[issp$V57==3474]=50
  issp$isei_f[issp$V57==3475]=54
  issp$isei_f[issp$V57==3480]=38
  issp$isei_f[issp$V57==4000]=45
  issp$isei_f[issp$V57==4100]=45
  issp$isei_f[issp$V57==4110]=51
  issp$isei_f[issp$V57==4111]=51
  issp$isei_f[issp$V57==4112]=50
  issp$isei_f[issp$V57==4113]=50
  issp$isei_f[issp$V57==4114]=51
  issp$isei_f[issp$V57==4115]=53
  issp$isei_f[issp$V57==4120]=51
  issp$isei_f[issp$V57==4121]=51
  issp$isei_f[issp$V57==4122]=51
  issp$isei_f[issp$V57==4130]=36
  issp$isei_f[issp$V57==4131]=32
  issp$isei_f[issp$V57==4132]=43
  issp$isei_f[issp$V57==4133]=45
  issp$isei_f[issp$V57==4140]=39
  issp$isei_f[issp$V57==4141]=39
  issp$isei_f[issp$V57==4142]=39
  issp$isei_f[issp$V57==4143]=39
  issp$isei_f[issp$V57==4144]=39
  issp$isei_f[issp$V57==4190]=39
  issp$isei_f[issp$V57==4200]=49
  issp$isei_f[issp$V57==4210]=48
  issp$isei_f[issp$V57==4211]=53
  issp$isei_f[issp$V57==4212]=46
  issp$isei_f[issp$V57==4213]=40
  issp$isei_f[issp$V57==4214]=40
  issp$isei_f[issp$V57==4215]=40
  issp$isei_f[issp$V57==4220]=52
  issp$isei_f[issp$V57==4221]=52
  issp$isei_f[issp$V57==4222]=52
  issp$isei_f[issp$V57==4223]=52
  issp$isei_f[issp$V57==5000]=40
  issp$isei_f[issp$V57==5100]=38
  issp$isei_f[issp$V57==5110]=34
  issp$isei_f[issp$V57==5111]=34
  issp$isei_f[issp$V57==5112]=34
  issp$isei_f[issp$V57==5113]=34
  issp$isei_f[issp$V57==5120]=32
  issp$isei_f[issp$V57==5121]=30
  issp$isei_f[issp$V57==5122]=30
  issp$isei_f[issp$V57==5123]=34
  issp$isei_f[issp$V57==5130]=25
  issp$isei_f[issp$V57==5131]=25
  issp$isei_f[issp$V57==5132]=25
  issp$isei_f[issp$V57==5133]=25
  issp$isei_f[issp$V57==5139]=25
  issp$isei_f[issp$V57==5140]=30
  issp$isei_f[issp$V57==5141]=29
  issp$isei_f[issp$V57==5142]=19
  issp$isei_f[issp$V57==5143]=54
  issp$isei_f[issp$V57==5149]=19
  issp$isei_f[issp$V57==5150]=43
  issp$isei_f[issp$V57==5151]=43
  issp$isei_f[issp$V57==5152]=43
  issp$isei_f[issp$V57==5160]=47
  issp$isei_f[issp$V57==5161]=42
  issp$isei_f[issp$V57==5162]=50
  issp$isei_f[issp$V57==5163]=40
  issp$isei_f[issp$V57==5164]=40
  issp$isei_f[issp$V57==5169]=40
  issp$isei_f[issp$V57==5200]=43
  issp$isei_f[issp$V57==5210]=43
  issp$isei_f[issp$V57==5220]=43
  issp$isei_f[issp$V57==5230]=37
  issp$isei_f[issp$V57==6000]=23
  issp$isei_f[issp$V57==6100]=23
  issp$isei_f[issp$V57==6110]=23
  issp$isei_f[issp$V57==6111]=23
  issp$isei_f[issp$V57==6112]=23
  issp$isei_f[issp$V57==6113]=23
  issp$isei_f[issp$V57==6114]=23
  issp$isei_f[issp$V57==6120]=23
  issp$isei_f[issp$V57==6121]=23
  issp$isei_f[issp$V57==6122]=23
  issp$isei_f[issp$V57==6123]=23
  issp$isei_f[issp$V57==6124]=23
  issp$isei_f[issp$V57==6129]=23
  issp$isei_f[issp$V57==6130]=23
  issp$isei_f[issp$V57==6131]=23
  issp$isei_f[issp$V57==6132]=27
  issp$isei_f[issp$V57==6133]=28
  issp$isei_f[issp$V57==6134]=28
  issp$isei_f[issp$V57==6140]=22
  issp$isei_f[issp$V57==6141]=22
  issp$isei_f[issp$V57==6142]=22
  issp$isei_f[issp$V57==6150]=28
  issp$isei_f[issp$V57==6151]=28
  issp$isei_f[issp$V57==6152]=28
  issp$isei_f[issp$V57==6153]=28
  issp$isei_f[issp$V57==6154]=28
  issp$isei_f[issp$V57==6200]=16
  issp$isei_f[issp$V57==6210]=16
  issp$isei_f[issp$V57==7000]=34
  issp$isei_f[issp$V57==7100]=31
  issp$isei_f[issp$V57==7110]=30
  issp$isei_f[issp$V57==7111]=30
  issp$isei_f[issp$V57==7112]=30
  issp$isei_f[issp$V57==7113]=27
  issp$isei_f[issp$V57==7120]=30
  issp$isei_f[issp$V57==7121]=29
  issp$isei_f[issp$V57==7122]=29
  issp$isei_f[issp$V57==7123]=26
  issp$isei_f[issp$V57==7124]=29
  issp$isei_f[issp$V57==7129]=30
  issp$isei_f[issp$V57==7130]=34
  issp$isei_f[issp$V57==7131]=19
  issp$isei_f[issp$V57==7132]=30
  issp$isei_f[issp$V57==7133]=31
  issp$isei_f[issp$V57==7134]=34
  issp$isei_f[issp$V57==7135]=26
  issp$isei_f[issp$V57==7136]=33
  issp$isei_f[issp$V57==7137]=37
  issp$isei_f[issp$V57==7140]=29
  issp$isei_f[issp$V57==7141]=29
  issp$isei_f[issp$V57==7142]=32
  issp$isei_f[issp$V57==7143]=29
  issp$isei_f[issp$V57==7200]=34
  issp$isei_f[issp$V57==7210]=31
  issp$isei_f[issp$V57==7211]=29
  issp$isei_f[issp$V57==7212]=30
  issp$isei_f[issp$V57==7213]=33
  issp$isei_f[issp$V57==7214]=30
  issp$isei_f[issp$V57==7215]=30
  issp$isei_f[issp$V57==7216]=30
  issp$isei_f[issp$V57==7220]=35
  issp$isei_f[issp$V57==7221]=33
  issp$isei_f[issp$V57==7222]=40
  issp$isei_f[issp$V57==7223]=34
  issp$isei_f[issp$V57==7224]=24
  issp$isei_f[issp$V57==7230]=34
  issp$isei_f[issp$V57==7231]=34
  issp$isei_f[issp$V57==7232]=42
  issp$isei_f[issp$V57==7233]=33
  issp$isei_f[issp$V57==7234]=23
  issp$isei_f[issp$V57==7240]=40
  issp$isei_f[issp$V57==7241]=40
  issp$isei_f[issp$V57==7242]=39
  issp$isei_f[issp$V57==7243]=41
  issp$isei_f[issp$V57==7244]=40
  issp$isei_f[issp$V57==7245]=38
  issp$isei_f[issp$V57==7300]=34
  issp$isei_f[issp$V57==7310]=38
  issp$isei_f[issp$V57==7311]=38
  issp$isei_f[issp$V57==7312]=38
  issp$isei_f[issp$V57==7313]=38
  issp$isei_f[issp$V57==7320]=28
  issp$isei_f[issp$V57==7321]=27
  issp$isei_f[issp$V57==7322]=29
  issp$isei_f[issp$V57==7323]=29
  issp$isei_f[issp$V57==7324]=29
  issp$isei_f[issp$V57==7330]=29
  issp$isei_f[issp$V57==7331]=29
  issp$isei_f[issp$V57==7332]=29
  issp$isei_f[issp$V57==7340]=40
  issp$isei_f[issp$V57==7341]=40
  issp$isei_f[issp$V57==7342]=40
  issp$isei_f[issp$V57==7343]=42
  issp$isei_f[issp$V57==7344]=40
  issp$isei_f[issp$V57==7345]=37
  issp$isei_f[issp$V57==7346]=38
  issp$isei_f[issp$V57==7400]=33
  issp$isei_f[issp$V57==7410]=30
  issp$isei_f[issp$V57==7411]=30
  issp$isei_f[issp$V57==7412]=31
  issp$isei_f[issp$V57==7413]=30
  issp$isei_f[issp$V57==7414]=30
  issp$isei_f[issp$V57==7415]=30
  issp$isei_f[issp$V57==7416]=30
  issp$isei_f[issp$V57==7420]=33
  issp$isei_f[issp$V57==7421]=33
  issp$isei_f[issp$V57==7422]=33
  issp$isei_f[issp$V57==7423]=33
  issp$isei_f[issp$V57==7424]=33
  issp$isei_f[issp$V57==7430]=36
  issp$isei_f[issp$V57==7431]=29
  issp$isei_f[issp$V57==7432]=29
  issp$isei_f[issp$V57==7433]=45
  issp$isei_f[issp$V57==7434]=36
  issp$isei_f[issp$V57==7435]=36
  issp$isei_f[issp$V57==7436]=33
  issp$isei_f[issp$V57==7437]=28
  issp$isei_f[issp$V57==7440]=31
  issp$isei_f[issp$V57==7441]=31
  issp$isei_f[issp$V57==7442]=31
  issp$isei_f[issp$V57==7500]=42
  issp$isei_f[issp$V57==7510]=42
  issp$isei_f[issp$V57==7520]=39
  issp$isei_f[issp$V57==7530]=26
  issp$isei_f[issp$V57==8000]=31
  issp$isei_f[issp$V57==8100]=30
  issp$isei_f[issp$V57==8110]=35
  issp$isei_f[issp$V57==8111]=35
  issp$isei_f[issp$V57==8112]=35
  issp$isei_f[issp$V57==8113]=35
  issp$isei_f[issp$V57==8120]=30
  issp$isei_f[issp$V57==8121]=31
  issp$isei_f[issp$V57==8122]=30
  issp$isei_f[issp$V57==8123]=28
  issp$isei_f[issp$V57==8124]=30
  issp$isei_f[issp$V57==8130]=22
  issp$isei_f[issp$V57==8131]=22
  issp$isei_f[issp$V57==8139]=22
  issp$isei_f[issp$V57==8140]=27
  issp$isei_f[issp$V57==8141]=27
  issp$isei_f[issp$V57==8142]=27
  issp$isei_f[issp$V57==8143]=27
  issp$isei_f[issp$V57==8150]=35
  issp$isei_f[issp$V57==8151]=35
  issp$isei_f[issp$V57==8152]=35
  issp$isei_f[issp$V57==8153]=35
  issp$isei_f[issp$V57==8154]=35
  issp$isei_f[issp$V57==8155]=35
  issp$isei_f[issp$V57==8159]=35
  issp$isei_f[issp$V57==8160]=32
  issp$isei_f[issp$V57==8161]=33
  issp$isei_f[issp$V57==8162]=27
  issp$isei_f[issp$V57==8163]=33
  issp$isei_f[issp$V57==8170]=26
  issp$isei_f[issp$V57==8171]=26
  issp$isei_f[issp$V57==8172]=26
  issp$isei_f[issp$V57==8200]=32
  issp$isei_f[issp$V57==8210]=36
  issp$isei_f[issp$V57==8211]=36
  issp$isei_f[issp$V57==8212]=30
  issp$isei_f[issp$V57==8220]=30
  issp$isei_f[issp$V57==8221]=30
  issp$isei_f[issp$V57==8222]=30
  issp$isei_f[issp$V57==8223]=30
  issp$isei_f[issp$V57==8224]=30
  issp$isei_f[issp$V57==8229]=30
  issp$isei_f[issp$V57==8230]=30
  issp$isei_f[issp$V57==8231]=30
  issp$isei_f[issp$V57==8232]=30
  issp$isei_f[issp$V57==8240]=29
  issp$isei_f[issp$V57==8250]=38
  issp$isei_f[issp$V57==8251]=38
  issp$isei_f[issp$V57==8252]=38
  issp$isei_f[issp$V57==8253]=38
  issp$isei_f[issp$V57==8260]=30
  issp$isei_f[issp$V57==8261]=29
  issp$isei_f[issp$V57==8262]=29
  issp$isei_f[issp$V57==8263]=32
  issp$isei_f[issp$V57==8264]=24
  issp$isei_f[issp$V57==8265]=32
  issp$isei_f[issp$V57==8266]=32
  issp$isei_f[issp$V57==8269]=32
  issp$isei_f[issp$V57==8270]=29
  issp$isei_f[issp$V57==8271]=29
  issp$isei_f[issp$V57==8272]=29
  issp$isei_f[issp$V57==8273]=29
  issp$isei_f[issp$V57==8274]=29
  issp$isei_f[issp$V57==8275]=29
  issp$isei_f[issp$V57==8276]=29
  issp$isei_f[issp$V57==8277]=29
  issp$isei_f[issp$V57==8278]=29
  issp$isei_f[issp$V57==8279]=29
  issp$isei_f[issp$V57==8280]=31
  issp$isei_f[issp$V57==8281]=30
  issp$isei_f[issp$V57==8282]=34
  issp$isei_f[issp$V57==8283]=34
  issp$isei_f[issp$V57==8284]=30
  issp$isei_f[issp$V57==8285]=30
  issp$isei_f[issp$V57==8286]=30
  issp$isei_f[issp$V57==8290]=26
  issp$isei_f[issp$V57==8300]=32
  issp$isei_f[issp$V57==8310]=36
  issp$isei_f[issp$V57==8311]=41
  issp$isei_f[issp$V57==8312]=32
  issp$isei_f[issp$V57==8320]=34
  issp$isei_f[issp$V57==8321]=30
  issp$isei_f[issp$V57==8322]=30
  issp$isei_f[issp$V57==8323]=30
  issp$isei_f[issp$V57==8324]=34
  issp$isei_f[issp$V57==8330]=26
  issp$isei_f[issp$V57==8331]=26
  issp$isei_f[issp$V57==8332]=26
  issp$isei_f[issp$V57==8333]=28
  issp$isei_f[issp$V57==8334]=28
  issp$isei_f[issp$V57==8340]=32
  issp$isei_f[issp$V57==8342]=32
  issp$isei_f[issp$V57==8400]=24
  issp$isei_f[issp$V57==9000]=20
  issp$isei_f[issp$V57==9100]=25
  issp$isei_f[issp$V57==9110]=29
  issp$isei_f[issp$V57==9111]=29
  issp$isei_f[issp$V57==9112]=28
  issp$isei_f[issp$V57==9113]=29
  issp$isei_f[issp$V57==9120]=28
  issp$isei_f[issp$V57==9130]=16
  issp$isei_f[issp$V57==9131]=16
  issp$isei_f[issp$V57==9132]=16
  issp$isei_f[issp$V57==9133]=16
  issp$isei_f[issp$V57==9140]=23
  issp$isei_f[issp$V57==9141]=23
  issp$isei_f[issp$V57==9142]=23
  issp$isei_f[issp$V57==9150]=27
  issp$isei_f[issp$V57==9151]=25
  issp$isei_f[issp$V57==9152]=27
  issp$isei_f[issp$V57==9153]=27
  issp$isei_f[issp$V57==9160]=23
  issp$isei_f[issp$V57==9161]=23
  issp$isei_f[issp$V57==9162]=23
  issp$isei_f[issp$V57==9200]=16
  issp$isei_f[issp$V57==9210]=16
  issp$isei_f[issp$V57==9211]=16
  issp$isei_f[issp$V57==9212]=16
  issp$isei_f[issp$V57==9213]=16
  issp$isei_f[issp$V57==9300]=23
  issp$isei_f[issp$V57==9310]=21
  issp$isei_f[issp$V57==9311]=21
  issp$isei_f[issp$V57==9312]=21
  issp$isei_f[issp$V57==9313]=21
  issp$isei_f[issp$V57==9320]=20
  issp$isei_f[issp$V57==9321]=20
  issp$isei_f[issp$V57==9322]=24
  issp$isei_f[issp$V57==9330]=29
  issp$isei_f[issp$V57==9331]=22
  issp$isei_f[issp$V57==9332]=22
  issp$isei_f[issp$V57==9333]=30
  issp$isei_f[issp$V57==110 ]=70}
issp$isei_f <- as.numeric(issp$isei_f)
summary(issp$isei_f)
table(issp$isei_f)


# Movilidad social objetiva -----------------------------------------------

issp$mov_obj<- issp$isei-issp$isei_f
table(issp$mov_obj)
hist(issp$mov_obj)
summary(issp$mov_obj)




# Movilidad social subjetiva ----------------------------------------------

issp$ess<-issp$V44
issp$essfam<-issp$V45
issp$mov_subj<- issp$ess-issp$essfam
table(issp$mov_subj)
hist(issp$mov_subj)
summary(issp$mov_subj)

issp$mov_subj_c<-car::recode(issp$mov_subj, "-9:-1='Mov. Des.';
                             0='Estable';1:9='Mov. Asc'", as.factor=T)
table(issp$mov_subj_c)







# Sindicato ---------------------------------------------------------------

frq(issp$UNION)
issp$sindicato<-car::recode(issp$UNION,"1:2=1;3=0")


# estatus laboral ---------------------------------------------------------

# 0          VE: NAP, never in paid work (NA)
# 1         Employed-full time, main job (empleado) (1)
# 2         Employed-part time, main job (empleado) (1)
# 3        Employed, less than part-time (empleado) (1)
# 4                Helping family member (trabajo domestico/cuidados)
# 5                           Unemployed (desempleado) (2)
# 6 Student, school, vocational training (estudia)
# 7                              Retired (retirado)
# 8         Housewife, -man, home duties (trabajo domestico/cuidados)
# 9                 Permanently disabled  (NA)
# 10            Other,not in labour force (NA)
# 97                              Refused (NA)
# 98                            Dont know (NA)
# 99                                   NA

table(issp$WRKST)
prop.table(table(issp$WRKST))*100

issp$WRKST[issp$WRKST %in% c(0,9,10,10,97,98,99)] <- NA

issp$labsta <- issp$WRKST
issp$labsta <- car::recode(issp$labsta,recodes = "c(1,2,3)=1;5=2;7=3;6=4;c(4,8)=5",as.factor = TRUE)

issp$labsta <- factor(issp$labsta,levels = c(1:5), labels = c("empleado","desempleado","retirado","estudia","trabajo domestico_cuidados"))

table(issp$labsta)
margin.table(table(issp$labsta))

# Sexo --------------------------------------------------------------------


# year 2009
table(issp$SEX)
issp$SEX[issp$SEX==9] <- NA
# 1 Male
# 2 Female
# 9 No answer, refused
issp$SEX[issp$SEX==2] <- 0 # Mujer a categoría de referencia

issp <- rename(issp,hombre=SEX)
issp$hombre <- as.integer(issp$hombre)
table(issp$hombre)
issp$hombre<-issp$hombre-1

# Edad --------------------------------------------------------------------

issp$edad <- issp$AGE

issp$edad[issp$edad==99] <- NA
table(issp$edad)


#armar variable dependiente

tesis<-as.data.frame(tesis)
table(tesis$V23) # cuanto gana GERENTE
table(tesis$V25) # cuanto gana OBRERO
table(tesis$V28) # cuando deberia ganar GERENTE
table(tesis$V30) # cuanto deberia ganar OBRERO
tesis$V23[tesis$V23 %in%c(-99,-98,-97,999999999996)] <- NA
tesis$V25[tesis$V25 %in%c(-99,-98,-97)]              <- NA
tesis$V28[tesis$V28 %in%c(-99,-98,-97)]              <- NA
tesis$V30[tesis$V30 %in%c(-99,-98,-97)]              <- NA





# Variables de salario ----------------------------------------------------
issp <- rename(issp,salperger=V23,salperobr=V25,saljusger=V28,saljusobr=V30)



# {r remover outliers 2009, include=FALSE}
table(issp$salperobr) #   880     150     158     160     180     190     200     300   12000   16000   50000
table(issp$salperobr) #   880     150     158     160     180     190     200     300   12000   16000   50000
table(issp$salperger) #   5000     10000     20000
table(issp$saljusobr) #   190      230      250      300      350      500
table(issp$saljusger) # 50000
issp$gap_perc  <-  as.numeric(issp$salperger/issp$salperobr) # brecha total salario percibido
issp$gap_just  <-  as.numeric(issp$saljusger/issp$saljusobr) # brecha total salario justo

summary(issp$gap_perc)
summary(issp$gap_just)

#remover casos extremos
quantile(issp$gap_perc, probs = c(0.025,0.975),na.rm = T)
quantile(issp$gap_just, probs = c(0.025,0.975),na.rm = T)

issp$gap_perc[issp$gap_perc<1.6 |issp$gap_perc>333.333]<-NA
issp$gap_just[issp$gap_just<1 |issp$gap_just>75]<-NA

hist(issp$gap_perc)
hist(issp$gap_just)

#logaritmizar
issp$lngap_perc <-  as.numeric(log(issp$gap_perc))  # diferencia log
issp$lngap_just <-  as.numeric(log(issp$gap_just))  # diferencia log
hist(issp$lngap_perc)

#pais
issp$pais<-as.factor(issp$V5)

# centrados ---------------------------------------------------------------

#centrado logaritmo de brecha de ingreso percibida
issp <- issp %>% group_by(pais) %>% mutate(lngap_perc_cgm = mean(lngap_perc,na.rm=T))
issp$lngap_perc_cwc<-issp$lngap_perc-issp$lngap_perc_cgm
summary(issp$lngap_perc_cwc)

#centrado movilidad objetiva
issp <- issp %>% group_by(pais) %>% mutate(mov_obj_cgm = mean(mov_obj,na.rm=T))
issp$mov_obj_cwc<-issp$mov_obj-issp$mov_obj_cgm

#centrado movilidad subjetiva
issp <- issp %>% group_by(pais) %>% mutate(mov_subj_cgm = mean(mov_subj,na.rm=T))
issp$mov_subj_cwc<-issp$mov_subj-issp$mov_subj_cgm

#centrado movilidad subjetiva categorica
issp$sub_asc<-as.integer(car::recode(issp$mov_subj_c, "'Mov. Asc'=1;c('Mov. Des.','Estable')=0"))
issp$sub_des<-as.integer(car::recode(issp$mov_subj_c, "'Mov. Des.'=1;c('Mov. Asc','Estable')=0"))
issp$sub_asc<-issp$sub_asc-1
issp$sub_des<-issp$sub_des-1

issp <- issp %>% group_by(pais) %>% mutate(sub_asc_cgm = mean(sub_asc,na.rm=T))
issp$sub_asc_cwc<-issp$sub_asc-issp$sub_asc_cgm
summary(issp$sub_asc_cwc)

issp <- issp %>% group_by(pais) %>% mutate(sub_des_cgm = mean(sub_des,na.rm=T))
issp$sub_des_cwc<-issp$sub_des-issp$sub_des_cgm
summary(issp$sub_des_cwc)

#centrado edad
issp <- issp %>% group_by(pais) %>% mutate(edad_cgm = mean(edad,na.rm=T))
issp$edad_cwc<-issp$edad-issp$edad_cgm
summary(issp$edad_cwc)

#centrado hombre
issp <- issp %>% group_by(pais) %>% mutate(hombre_cgm = mean((hombre),na.rm=T))
issp$hombre_cwc<-issp$hombre-issp$hombre_cgm
summary(issp$hombre_cwc)

#centrado educación
issp <- issp %>% group_by(pais) %>% mutate(educ_cgm = mean((educ),na.rm=T))
issp$educ_cwc<-issp$educ-issp$educ_cgm
summary(issp$educ_cwc)

#centrado sindicato
issp <- issp %>% group_by(pais) %>% mutate(sindicato_cgm = mean((sindicato),na.rm=T))
issp$sindicato_cwc<-issp$sindicato-issp$sindicato_cgm
summary(issp$sindicato_cwc)

#centrado ingreso
table(issp$Q05)
issp$q2<-car::recode(issp$Q05,"2=1;else=0")
issp$q3<-car::recode(issp$Q05,"3=1;else=0")
issp$q4<-car::recode(issp$Q05,"4=1;else=0")
issp$q5<-car::recode(issp$Q05,"5=1;else=0")

issp <- issp %>% group_by(pais) %>% mutate(q2_cgm = mean(q2,na.rm=T))
issp$q2_cwc<-issp$q2-issp$q2_cgm
summary(issp$q2_cwc)

issp <- issp %>% group_by(pais) %>% mutate(q3_cgm = mean(q3,na.rm=T))
issp$q3_cwc<-issp$q3-issp$q3_cgm
summary(issp$q3_cwc)

issp <- issp %>% group_by(pais) %>% mutate(q4_cgm = mean(q4,na.rm=T))
issp$q4_cwc<-issp$q4-issp$q4_cgm
summary(issp$q4_cwc)

issp <- issp %>% group_by(pais) %>% mutate(q5_cgm = mean(q5,na.rm=T))
issp$q5_cwc<-issp$q5-issp$q5_cgm
summary(issp$q5_cwc)



# listwise ----------------------------------------------------------------

df_study1<- issp %>% dplyr::select(pais,mov_obj_cwc,mov_obj_cgm,mov_subj_cwc,
                            mov_subj_cgm,sub_asc_cwc,sub_asc_cgm,sub_des_cwc,
                            sub_des_cgm,edad_cwc,edad_cgm,hombre_cwc,hombre_cgm,
                            educ_cwc,educ_cgm,sindicato_cwc,sindicato_cgm,
                            q2_cwc,q2_cgm,q3_cwc,q3_cgm,q4_cwc,q4_cgm,q5_cwc,q5_cgm,
                            lngap_perc_cwc,lngap_perc_cgm,
                            lngap_just) %>% na.omit()


save(df_study1,file = here::here("input/data/proc/df_study1.RData"))

# Estimaciones ------------------------------------------------------------

#modelo nulo
r01 <- lmer(lngap_just ~ 1 + (1 | pais), data = t1, REML = FALSE)

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
              q2_cwc +q3_cwc +q4_cwc +q5_cwc + (1 | pais), data = t1, REML = FALSE)

#modelo between
r03 <- lmer(lngap_just ~ 1 + lngap_perc_cgm+ mov_obj_cgm + mov_subj_cgm +
              edad_cgm + hombre_cgm + educ_cgm + sindicato_cgm+
              q2_cgm +q3_cgm +q4_cgm +q5_cgm + (1 | pais), data = t1, REML = FALSE)

#modelo full
r04 <- lmer(lngap_just ~ 1 + lngap_perc_cwc+ mov_obj_cwc + mov_subj_cwc +
              edad_cwc + hombre_cwc + educ_cwc + sindicato_cwc+
              q2_cwc +q3_cwc +q4_cwc +q5_cwc +
              lngap_perc_cgm+ mov_obj_cgm + mov_subj_cgm +
              edad_cgm + hombre_cgm + educ_cgm + sindicato_cgm+
              q2_cgm +q3_cgm +q4_cgm +q5_cgm + (1 | pais), data = t1, REML = FALSE)
screenreg(l=list(r01,r02,r03,r04))


#Modelos con variable movilidad subjetiva categorica

#modelo within
r05 <- lmer(lngap_just ~ 1 + lngap_perc_cwc+ mov_obj_cwc + sub_asc_cwc+sub_des_cwc +
              edad_cwc + hombre_cwc + educ_cwc + sindicato_cwc+
              q2_cwc +q3_cwc +q4_cwc +q5_cwc + (1 | pais), data = t1, REML = FALSE)

#modelo between
r06 <- lmer(lngap_just ~ 1 + lngap_perc_cgm+ mov_obj_cgm + sub_asc_cgm+sub_des_cgm +
              edad_cgm + hombre_cgm + educ_cgm + sindicato_cgm+
              q2_cgm +q3_cgm +q4_cgm +q5_cgm + (1 | pais), data = t1, REML = FALSE)

#modelo full
r07 <- lmer(lngap_just ~ 1 + lngap_perc_cwc+ mov_obj_cwc + sub_asc_cwc+sub_des_cwc +
              edad_cwc + hombre_cwc + educ_cwc + sindicato_cwc+
              q2_cwc +q3_cwc +q4_cwc +q5_cwc +
              lngap_perc_cgm+ mov_obj_cgm + sub_asc_cgm+sub_des_cgm +
              edad_cgm + hombre_cgm + educ_cgm + sindicato_cgm+
              q2_cgm +q3_cgm +q4_cgm +q5_cgm + (1 | pais), data = t1, REML = FALSE)

screenreg(l=list(r01,r05,r06,r07))

summary(r07)





