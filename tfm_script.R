####PREPARACION BASE DE DATOS
tfm<- base_de_datos
summary(tfm$Tratamiento)

factor(tfm$Tratamiento, levels= c("manual", "ejercicio", "control"), labels= c(0, 1, 2))
tfm$Tratamiento <- factor(tfm$Tratamiento, levels = c("manual", "ejercicio", "control"), labels = c("0", "1", "2"))
tfm$Ultimosmeses<-tfm$`Últimos 3 meses`
tfm$ALT0derecha<-tfm$`ALT 0derecha`
tfm$SLS0derecha<-tfm$`SLS 0derecha`
tfm$SLS0izquierda<-tfm$`SLS 0izquierda`
tfm$SLS1derecha<-tfm$`SLS 1derecha`
tfm$SLS1izquierda<-tfm$`SLS 1izquierda`
tfm$ALT3derecha<-tfm$`ALT 3derecha`
tfm$ALT3izquierda<-tfm$`ALT3 izquierda`
tfm$SLS3derecha<-tfm$`SLS 3 derecha`
tfm$SLS3izquierda<-tfm$`SLS 3izquierda`
###### manual 0, ejercicio 1, control 2, SI 0, NO 1

datos_numericos <- tfm[sapply(tfm, is.numeric)]
for (col in colnames(datos_numericos)) {
  print(col)
  qqnorm(datos_numericos[[col]])
  qqline(datos_numericos[[col]], col = 2)
}

tfm$ultimosmeses<-factor(tfm$Ultimosmeses, levels= c("SI", "NO"), labels= c("0", "1"))
summary(tfm$Tratamiento)


tfm<-tfm[, -8]

tfm<-tfm[, -c(9, 13, 14, 15, 16, 17, 18, 19, 20)]

## análisis descriptivo de la muestra y de las principales variables segmentando dicho análisis en función del grupo al que fueron asignados/as. 
tfm$Tratamiento<-factor(tfm$Tratamiento, levels = c("0", "1", "2"), labels = c("manual", "ejercicio", "control"))
summary(tfm)
table(tfm$Tratamiento)
install.packages("knitr")
library(knitr)
kable(tfm$Tratamiento)
print(kable(tfm$Tratamiento))
tfm_numericos <- tfm[, sapply(tfm, is.numeric)]
summary_por_tratamiento <- aggregate(. ~ tfm$Tratamiento, data = tfm_numericos, FUN = function(x) c(Mean = mean(x, na.rm = TRUE), SD = sd(x, na.rm = TRUE), Median = median(x, na.rm = TRUE), Min = min(x, na.rm = TRUE), Max = max(x, na.rm = TRUE)))
print(summary_por_tratamiento)

tfm$ALT0combo<-ifelse(is.na(tfm$ALT0derecha), tfm$ALT0izquierda, ifelse(is.na(tfm$ALT0izquierda), tfm$ALT0derecha, (tfm$ALT0derecha + tfm$ALT0izquierda) / 2))
tfm$ALT0combo[complete.cases(tfm$ALT0combo)]
tfm$ALT0combo[is.na(tfm$ALT0combo)] <- mean(tfm$ALT0combo, na.rm = TRUE)
tfm_num<-tfm[, sapply(tfm, is.numeric)]
summary_tratamiento_ALT0 <- aggregate(. ~ tfm$Tratamiento, data = tfm_num, FUN = function(x) c(Mean = mean(x, na.rm = TRUE), SD = sd(x, na.rm = TRUE), Median = median(x, na.rm = TRUE), Min = min(x, na.rm = TRUE), Max = max(x, na.rm = TRUE)))
print(summary_tratamiento_ALT0)

tfm$SLS0combo<-ifelse(is.na(tfm$SLS0derecha), tfm$SLS0izquierda, ifelse(is.na(tfm$SLS0izquierda), tfm$SLS0derecha, (tfm$SLS0derecha + tfm$SLS0izquierda) / 2))
tfm$SLS0combo[complete.cases(tfm$SLS0combo)]
tfm$SLS0combo[is.na(tfm$SLS0combo)] <- mean(tfm$SLS0combo, na.rm = TRUE)
tfm_num<-tfm[, sapply(tfm, is.numeric)]
summary_tratamiento_SLS0 <- aggregate(. ~ tfm$Tratamiento, data = tfm_num, FUN = function(x) c(Mean = mean(x, na.rm = TRUE), SD = sd(x, na.rm = TRUE), Median = median(x, na.rm = TRUE), Min = min(x, na.rm = TRUE), Max = max(x, na.rm = TRUE)))
print(summary_tratamiento_SLS0)

tfm$ALT1combo<-ifelse(is.na(tfm$ALT1derecha), tfm$ALT1izquierda, ifelse(is.na(tfm$ALT1izquierda), tfm$ALT1derecha, (tfm$ALT1derecha + tfm$ALT1izquierda) / 2))
tfm$ALT1combo[complete.cases(tfm$ALT1combo)]
tfm$ALT1combo[is.na(tfm$ALT1combo)] <- mean(tfm$ALT1combo, na.rm = TRUE)
tfm_num<-tfm[, sapply(tfm, is.numeric)]
summary_tratamiento_ALT1 <- aggregate(. ~ tfm$Tratamiento, data = tfm_num, FUN = function(x) c(Mean = mean(x, na.rm = TRUE), SD = sd(x, na.rm = TRUE), Median = median(x, na.rm = TRUE), Min = min(x, na.rm = TRUE), Max = max(x, na.rm = TRUE)))
print(summary_tratamiento_ALT1)

tfm$ALT3combo<-ifelse(is.na(tfm$ALT3derecha), tfm$ALT3izquierda, ifelse(is.na(tfm$ALT3izquierda), tfm$ALT3derecha, (tfm$ALT3derecha + tfm$ALT3izquierda) / 2))
tfm$ALT3combo[complete.cases(tfm$ALT3combo)]
tfm$ALT3combo[is.na(tfm$ALT3combo)] <- mean(tfm$ALT3combo, na.rm = TRUE)
tfm_num<-tfm[, sapply(tfm, is.numeric)]
summary_tratamiento_ALT3 <- aggregate(. ~ tfm$Tratamiento, data = tfm_num, FUN = function(x) c(Mean = mean(x, na.rm = TRUE), SD = sd(x, na.rm = TRUE), Median = median(x, na.rm = TRUE), Min = min(x, na.rm = TRUE), Max = max(x, na.rm = TRUE)))
print(summary_tratamiento_ALT3)

tfm$SLS1combo<-ifelse(is.na(tfm$SLS1derecha), tfm$SLS1izquierda, ifelse(is.na(tfm$SLS1izquierda), tfm$SLS1derecha, (tfm$SLS1derecha + tfm$SLS1izquierda) / 2))
tfm$SLS1combo[complete.cases(tfm$SLS1combo)]
tfm$SLS1combo[is.na(tfm$SLS1combo)] <- mean(tfm$SLS1combo, na.rm = TRUE)
tfm_num<-tfm[, sapply(tfm, is.numeric)]
summary_tratamiento_SLS1 <- aggregate(. ~ tfm$Tratamiento, data = tfm_num, FUN = function(x) c(Mean = mean(x, na.rm = TRUE), SD = sd(x, na.rm = TRUE), Median = median(x, na.rm = TRUE), Min = min(x, na.rm = TRUE), Max = max(x, na.rm = TRUE)))
print(summary_tratamiento_SLS1)

tfm$SLS3combo<-ifelse(is.na(tfm$SLS3derecha), tfm$SLS3izquierda, ifelse(is.na(tfm$SLS3izquierda), tfm$SLS3derecha, (tfm$SLS3derecha + tfm$SLS3izquierda) / 2))
tfm$SLS3combo[complete.cases(tfm$SLS3combo)]
tfm$SLS3combo[is.na(tfm$SLS3combo)] <- mean(tfm$SLS3combo, na.rm = TRUE)
tfm_num<-tfm[, sapply(tfm, is.numeric)]
summary_tratamiento_SLS3 <- aggregate(. ~ tfm$Tratamiento, data = tfm_num, FUN = function(x) c(Mean = mean(x, na.rm = TRUE), SD = sd(x, na.rm = TRUE), Median = median(x, na.rm = TRUE), Min = min(x, na.rm = TRUE), Max = max(x, na.rm = TRUE)))
print(summary_tratamiento_SLS3)

####diferencias pre y post-tratamiento intragrupo en grupos de muestra pequeña
                             ######## T STUDENT##################
### fase 1 manual izquierda
tfm_manual <- subset(tfm, Tratamiento == "0")
tfm_manual$FD_Combo <- ifelse(is.na(tfm_manual$ALT0derecha), tfm_manual$ALT0izquierda, ifelse(is.na(tfm_manual$ALT0izquierda), tfm_manual$ALT0derecha, (tfm_manual$ALT0derecha + tfm_manual$ALT0izquierda) / 2))
tfm_manual$FD_Combo <- tfm_manual$FD_Combo[complete.cases(tfm_manual$FD_Combo)]
print(tfm_manual$FD_Combo)
tfm_manual$FD_Combo1 <- ifelse(is.na(tfm_manual$ALT1derecha), tfm_manual$ALT1izquierda, ifelse(is.na(tfm_manual$ALT1izquierda), tfm_manual$ALT1derecha, (tfm_manual$ALT1derecha + tfm_manual$ALT1izquierda) / 2))
tfm_manual$FD_Combo1 <- tfm_manual$FD_Combo1[complete.cases(tfm_manual$FD_Combo1)]

tfm_manual$FD_Combo[is.na(tfm_manual$FD_Combo)] <- mean(tfm_manual$FD_Combo, na.rm = TRUE)
tfm_manual$FD_Combo1[is.na(tfm_manual$FD_Combo1)] <- mean(tfm_manual$FD_Combo1, na.rm = TRUE)

pre_manual<-tfm_manual$FD_Combo
print(pre_manual)
pre_manual_sin_na <- ifelse(is.na(pre_manual), NA, pre_manual)

post_manual <-tfm_manual$FD_Combo1

post_manual_sin_na <- ifelse(is.na(post_manual), NA, post_manual)

resultado_t_test <- t.test(pre_manual_sin_na, post_manual_sin_na, paired = TRUE, na.rm = TRUE)
print(resultado_t_test) ###no diferencia significativa


tfm_manual <- subset(tfm, Tratamiento == "0")


tfm_manual$SLS_Combo <- ifelse(is.na(tfm_manual$SLS0derecha), tfm_manual$SLS0izquierda, ifelse(is.na(tfm_manual$SLS0izquierda), tfm_manual$SLS0derecha, (tfm_manual$SLS0derecha + tfm_manual$SLS0izquierda) / 2))
tfm_manual$SLS_Combo <- tfm_manual$SLS_Combo[complete.cases(tfm_manual$SLS_Combo)]

pre_manual<-tfm_manual$SLS_Combo

pre_manual_sin_na <- ifelse(is.na(pre_manual), NA, pre_manual)
tfm_manual$SLS_Combo1 <- ifelse(is.na(tfm_manual$SLS1derecha), tfm_manual$SLS1izquierda, ifelse(is.na(tfm_manual$SLS1izquierda), tfm_manual$SLS1derecha, (tfm_manual$SLS1derecha + tfm_manual$SLS1izquierda) / 2))
tfm_manual$SLS_Combo1 <- tfm_manual$SLS_Combo1[complete.cases(tfm_manual$SLS_Combo1)]

post_manual <-tfm_manual$SLS_Combo1
post_manual_sin_na <- ifelse(is.na(post_manual), NA, post_manual)

resultado_t_test <- t.test(pre_manual_sin_na, post_manual_sin_na, paired = TRUE, na.rm = TRUE)
print(resultado_t_test) 
####no significativo


####fase 1 manual derecha  
pre_manual<-tfm_manual$ALT0derecha
pre_manual_sin_na <- ifelse(is.na(pre_manual), NA, pre_manual)
post_manual <-tfm_manual$ALT1derecha
post_manual_sin_na <- ifelse(is.na(post_manual), NA, post_manual)
resultado_t_test <- t.test(pre_manual_sin_na, post_manual_sin_na, paired = TRUE, na.rm = TRUE)
print(resultado_t_test)
#No diferencia significativa

pre_manual<-tfm_manual$SLS0derecha
pre_manual_sin_na <- ifelse(is.na(pre_manual), NA, pre_manual)
post_manual <-tfm_manual$SLS1derecha
post_manual_sin_na <- ifelse(is.na(post_manual), NA, post_manual)
resultado_t_test <- t.test(pre_manual_sin_na, post_manual_sin_na, paired = TRUE, na.rm = TRUE)
print(resultado_t_test) #### no significativo


#####fase 1 ejercicio izquierda
tfm_ejercicio <- subset(tfm, Tratamiento == "1")
tfm_ejercicio$FD_Combo <- ifelse(is.na(tfm_ejercicio$ALT0derecha), tfm_ejercicio$ALT0izquierda, ifelse(is.na(tfm_ejercicio$ALT0izquierda), tfm_ejercicio$ALT0derecha, (tfm_ejercicio$ALT0derecha + tfm_ejercicio$ALT0izquierda) / 2))
tfm_ejercicio$FD_Combo <- tfm_ejercicio$FD_Combo[complete.cases(tfm_ejercicio$FD_Combo)]
tfm_ejercicio$FD_Combo1 <- ifelse(is.na(tfm_ejercicio$ALT1derecha), tfm_ejercicio$ALT1izquierda, ifelse(is.na(tfm_ejercicio$ALT1izquierda), tfm_ejercicio$ALT1derecha, (tfm_ejercicio$ALT1derecha + tfm_ejercicio$ALT1izquierda) / 2))
tfm_ejercicio$FD_Combo1 <- tfm_ejercicio$FD_Combo1[complete.cases(tfm_ejercicio$FD_Combo1)]
pre_ejercicio<-tfm_ejercicio$FD_Combo
pre_ejercicio_sin_na <- ifelse(is.na(pre_ejercicio), NA, pre_ejercicio)
post_ejercicio<-tfm_ejercicio$FD_Combo1
post_ejercicio_sin_na <- ifelse(is.na(post_ejercicio), NA, post_ejercicio)
resultado_t_test <- t.test(pre_ejercicio_sin_na, post_ejercicio_sin_na, paired = TRUE, na.rm = TRUE)
print(resultado_t_test) ##no diferencia significativa

tfm_ejercicio <- subset(tfm, Tratamiento == "1")
tfm_ejercicio$SLS_Combo <- ifelse(is.na(tfm_ejercicio$SLS0derecha), tfm_ejercicio$SLS0izquierda, ifelse(is.na(tfm_ejercicio$SLS0izquierda), tfm_ejercicio$SLS0derecha, (tfm_ejercicio$SLS0derecha + tfm_ejercicio$SLS0izquierda) / 2))
tfm_ejercicio$SLS_Combo <- tfm_ejercicio$SLS_Combo[complete.cases(tfm_ejercicio$SLS_Combo)]
tfm_ejercicio$SLS_Combo1 <- ifelse(is.na(tfm_ejercicio$SLS1derecha), tfm_ejercicio$SLS1izquierda, ifelse(is.na(tfm_ejercicio$SLS1izquierda), tfm_ejercicio$SLS1derecha, (tfm_ejercicio$SLS1derecha + tfm_ejercicio$SLS1izquierda) / 2))
tfm_ejercicio$SLS_Combo1 <- tfm_ejercicio$SLS_Combo1[complete.cases(tfm_ejercicio$SLS_Combo1)]

pre_ejercicio<-tfm_ejercicio$SLS_Combo
pre_ejercicio_sin_na <- ifelse(is.na(pre_ejercicio), NA, pre_ejercicio)
post_ejercicio<-tfm_ejercicio$SLS_Combo1
post_ejercicio_sin_na <- ifelse(is.na(post_ejercicio), NA, post_ejercicio)
resultado_t_test <- t.test(pre_ejercicio_sin_na, post_ejercicio_sin_na, paired = TRUE, na.rm = TRUE)
print(resultado_t_test) ###no diferencia significativa


##Fase 1 ejercicio derecha
pre_ejercicio<-tfm_ejercicio$ALT0derecha
post_ejercicio<-tfm_ejercicio$ALT1derecha
pre_ejercicio_sin_na <- ifelse(is.na(pre_ejercicio), NA, pre_ejercicio)
post_ejercicio_sin_na <- ifelse(is.na(post_ejercicio), NA, post_ejercicio)
resultado_t_test <- t.test(pre_ejercicio_sin_na, post_ejercicio_sin_na, paired = TRUE, na.rm = TRUE)
print(resultado_t_test)## significativo

tfm_ejercicio <- subset(tfm, Tratamiento == "1")
pre_ejercicio<-tfm_ejercicio$ALT0derecha
pre_ejercicio_sin_na <- ifelse(is.na(pre_ejercicio), NA, pre_ejercicio)
post_ejercicio<-tfm_ejercicio$ALT1derecha
post_ejercicio_sin_na <- ifelse(is.na(post_ejercicio), NA, post_ejercicio)
resultado_t_test <- t.test(pre_ejercicio_sin_na, post_ejercicio_sin_na, paired = TRUE, na.rm = TRUE)
print(resultado_t_test)  ##### diferencia significativa

###Fase 1 control izquierda
tfm_control <- subset(tfm, Tratamiento == "2")
tfm_control$FD_Combo <- ifelse(is.na(tfm_control$ALT0derecha), tfm_control$ALT0izquierda, ifelse(is.na(tfm_control$ALT0izquierda), tfm_control$ALT0derecha, (tfm_control$ALT0derecha + tfm_control$ALT0izquierda) / 2))
tfm_control$FD_Combo <- tfm_control$FD_Combo[complete.cases(tfm_control$FD_Combo)]
tfm_control$FD_Combo1 <- ifelse(is.na(tfm_control$ALT1derecha), tfm_control$ALT1izquierda, ifelse(is.na(tfm_control$ALT1izquierda), tfm_control$ALT1derecha, (tfm_control$ALT1derecha + tfm_control$ALT1izquierda) / 2))
tfm_control$FD_Combo1 <- tfm_control$FD_Combo1[complete.cases(tfm_control$FD_Combo1)]
pre_control<-tfm_control$FD_Combo
post_control<-tfm_control$FD_Combo1
pre_control_sin_na <- ifelse(is.na(pre_control), NA, pre_control)
post_control_sin_na <- ifelse(is.na(post_control), NA, post_control)
resultado_t_test <- t.test(pre_control_sin_na, post_control_sin_na, paired = TRUE, na.rm = TRUE)
print(resultado_t_test)## no significativo

tfm_control <- subset(tfm, Tratamiento == "2")
tfm_control$SLS_Combo <- ifelse(is.na(tfm_control$SLS0derecha), tfm_control$SLS0izquierda, ifelse(is.na(tfm_control$SLS0izquierda), tfm_control$SLS0derecha, (tfm_control$SLS0derecha + tfm_control$SLS0izquierda) / 2))
tfm_control$SLS_Combo <- tfm_control$SLS_Combo[complete.cases(tfm_control$SLS_Combo)]
tfm_control$SLS_Combo1 <- ifelse(is.na(tfm_control$SLS1derecha), tfm_control$SLS1izquierda, ifelse(is.na(tfm_control$SLS1izquierda), tfm_control$SLS1derecha, (tfm_control$SLS1derecha + tfm_control$SLS1izquierda) / 2))
tfm_control$SLS_Combo1 <- tfm_control$SLS_Combo1[complete.cases(tfm_control$SLS_Combo1)]
pre_control<-tfm_control$SLS_Combo
post_control<-tfm_control$SLS_Combo1
pre_control_sin_na <- ifelse(is.na(pre_control), NA, pre_control)
post_control_sin_na <- ifelse(is.na(post_control), NA, post_control)
resultado_t_test <- t.test(pre_control_sin_na, post_control_sin_na, paired = TRUE, na.rm = TRUE)
print(resultado_t_test) #### no significativo


###Fase 1 control derecha
pre_control<-tfm_control$ALT0derecha
post_control<-tfm_control$ALT1derecha
pre_control_sin_na <- ifelse(is.na(pre_control), NA, pre_control)
post_control_sin_na <- ifelse(is.na(post_control), NA, post_control)
resultado_t_test <- t.test(pre_control_sin_na, post_control_sin_na, paired = TRUE,  na.rm = TRUE)
print(resultado_t_test) ###### no significativo

tfm_control <- subset(tfm, Tratamiento == "2")
pre_control<-tfm_control$SLS0derecha
post_control<-tfm_control$SLS1derecha
pre_control_sin_na <- ifelse(is.na(pre_control), NA, pre_control)
post_control_sin_na <- ifelse(is.na(post_control), NA, post_control)
resultado_t_test <- t.test(pre_control_sin_na, post_control_sin_na, paired = TRUE, na.rm = TRUE)
print(resultado_t_test) ######## no significativo



####FASE 2 manual izquierda
tfm_manual <- subset(tfm, Tratamiento == "0")
tfm_manual$FD_Combo <- ifelse(is.na(tfm_manual$ALT0derecha), tfm_manual$ALT0izquierda, ifelse(is.na(tfm_manual$ALT0izquierda), tfm_manual$ALT0derecha, (tfm_manual$ALT0derecha + tfm_manual$ALT0izquierda) / 2))
tfm_manual$FD_Combo <- tfm_manual$FD_Combo[complete.cases(tfm_manual$FD_Combo)]
tfm_manual$FD_Combo3 <- ifelse(is.na(tfm_manual$ALT3derecha), tfm_manual$ALT3izquierda, ifelse(is.na(tfm_manual$ALT3izquierda), tfm_manual$ALT3derecha, (tfm_manual$ALT3derecha + tfm_manual$ALT3izquierda) / 2))
tfm_manual$FD_Combo3 <- tfm_manual$FD_Combo3[complete.cases(tfm_manual$FD_Combo3)]
pre_manual<-tfm_manual$FD_Combo
post_manual<-tfm_manual$FD_Combo3
pre_manual_sin_na <- ifelse(is.na(pre_manual), NA, pre_manual)
post_manual_sin_na <- ifelse(is.na(post_manual), NA, post_manual)
resultado_t_test <- t.test(pre_manual_sin_na, post_manual_sin_na, paired = TRUE, na.rm = TRUE)
print(resultado_t_test) ## no significativo

tfm_manual <- subset(tfm, Tratamiento == "0")
tfm_manual$SLS_Combo <- ifelse(is.na(tfm_manual$SLS0derecha), tfm_manual$SLS0izquierda, ifelse(is.na(tfm_manual$SLS0izquierda), tfm_manual$SLS0derecha, (tfm_manual$SLS0derecha + tfm_manual$SLS0izquierda) / 2))
tfm_manual$SLS_Combo <- tfm_manual$SLS_Combo[complete.cases(tfm_manual$SLS_Combo)]
tfm_manual$SLS_Combo3 <- ifelse(is.na(tfm_manual$SLS3derecha), tfm_manual$SLS3izquierda, ifelse(is.na(tfm_manual$SLS3izquierda), tfm_manual$SLS3derecha, (tfm_manual$SLS3derecha + tfm_manual$SLS3izquierda) / 2))
tfm_manual$SLS_Combo3 <- tfm_manual$SLS_Combo3[complete.cases(tfm_manual$SLS_Combo3)]
pre_manual<-tfm_manual$SLS_Combo
post_manual<-tfm_manual$SLS_Combo3
pre_manual_sin_na <- ifelse(is.na(pre_manual), NA, pre_manual)
post_manual_sin_na <- ifelse(is.na(post_manual), NA, post_manual)
resultado_t_test <- t.test(pre_manual_sin_na, post_manual_sin_na, paired = TRUE, na.rm = TRUE)
print(resultado_t_test) #### no significativo

####FASE 2 manual derecha
tfm_manual <- subset(tfm, Tratamiento == "0")
pre_manual<-tfm_manual$ALT0derecha
post_manual<-tfm_manual$ALT3derecha
pre_manual_sin_na <- ifelse(is.na(pre_manual), NA, pre_manual)
post_manual_sin_na <- ifelse(is.na(post_manual), NA, post_manual)
resultado_t_test <- t.test(pre_manual_sin_na, post_manual_sin_na, paired = TRUE, na.rm = TRUE)
print(resultado_t_test) #### no significativo



tfm_manual <- subset(tfm, Tratamiento == "0")
pre_manual<-tfm_manual$SLS0derecha
post_manual<-tfm_manual$SLS3derecha
pre_manual_sin_na <- ifelse(is.na(pre_manual), NA, pre_manual)
post_manual_sin_na <- ifelse(is.na(post_manual), NA, post_manual)
resultado_t_test <- t.test(pre_manual_sin_na, post_manual_sin_na, paired = TRUE, na.rm = TRUE)
print(resultado_t_test) #### no significativo


####FASE 2 ejercicio izquierda
tfm_ejercicio <- subset(tfm, Tratamiento == "1")
tfm_ejercicio$FD_Combo <- ifelse(is.na(tfm_ejercicio$ALT0derecha), tfm_ejercicio$ALT0izquierda, ifelse(is.na(tfm_ejercicio$ALT0izquierda), tfm_ejercicio$ALT0derecha, (tfm_ejercicio$ALT0derecha + tfm_ejercicio$ALT0izquierda) / 2))
tfm_ejercicio$FD_Combo <- tfm_ejercicio$FD_Combo[complete.cases(tfm_ejercicio$FD_Combo)]
tfm_ejercicio$FD_Combo3 <- ifelse(is.na(tfm_ejercicio$ALT3derecha), tfm_ejercicio$ALT3izquierda, ifelse(is.na(tfm_ejercicio$ALT3izquierda), tfm_ejercicio$ALT3derecha, (tfm_ejercicio$ALT3derecha + tfm_ejercicio$ALT3izquierda) / 2))
tfm_ejercicio$FD_Combo3 <- tfm_ejercicio$FD_Combo3[complete.cases(tfm_ejercicio$FD_Combo3)]
pre_ejercicio<-tfm_ejercicio$FD_Combo
post_ejercicio<-tfm_ejercicio$FD_Combo3
pre_ejercicio_sin_na <- ifelse(is.na(pre_ejercicio), NA, pre_ejercicio)
post_ejercicio_sin_na <- ifelse(is.na(post_ejercicio), NA, post_ejercicio)
resultado_t_test <- t.test(pre_ejercicio_sin_na, post_ejercicio_sin_na, paired = TRUE, na.rm = TRUE)
print(resultado_t_test) #estadisticamente significativo 

tfm_ejercicio <- subset(tfm, Tratamiento == "1")
tfm_ejercicio$SLS_Combo <- ifelse(is.na(tfm_ejercicio$SLS0derecha), tfm_ejercicio$SLS0izquierda, ifelse(is.na(tfm_ejercicio$SLS0izquierda), tfm_ejercicio$SLS0derecha, (tfm_ejercicio$SLS0derecha + tfm_ejercicio$SLS0izquierda) / 2))
tfm_ejercicio$SLS_Combo <- tfm_ejercicio$SLS_Combo[complete.cases(tfm_ejercicio$SLS_Combo)]
tfm_ejercicio$SLS_Combo3 <- ifelse(is.na(tfm_ejercicio$SLS3derecha), tfm_ejercicio$SLS3izquierda, ifelse(is.na(tfm_ejercicio$SLS3izquierda), tfm_ejercicio$SLS3derecha, (tfm_ejercicio$SLS3derecha + tfm_ejercicio$SLS3izquierda) / 2))
tfm_ejercicio$SLS_Combo3 <- tfm_ejercicio$SLS_Combo3[complete.cases(tfm_ejercicio$SLS_Combo3)]
pre_ejercicio<-tfm_ejercicio$SLS_Combo
post_ejercicio<-tfm_ejercicio$SLS_Combo3
pre_ejercicio_sin_na <- ifelse(is.na(pre_ejercicio), NA, pre_ejercicio)
post_ejercicio_sin_na <- ifelse(is.na(post_ejercicio), NA, post_ejercicio)
resultado_t_test <- t.test(pre_ejercicio_sin_na, post_ejercicio_sin_na, paired = TRUE, na.rm = TRUE)
print(resultado_t_test) #### no significativo

####FASE 2 ejercicio derecha
tfm_ejercicio <- subset(tfm, Tratamiento == "1")
pre_ejercicio<-tfm_ejercicio$ALT0derecha
post_ejercicio<-tfm_ejercicio$ALT3derecha
pre_ejercicio_sin_na <- ifelse(is.na(pre_ejercicio), NA, pre_ejercicio)
post_ejercicio_sin_na <- ifelse(is.na(post_ejercicio), NA, post_ejercicio)
resultado_t_test <- t.test(pre_ejercicio_sin_na, post_ejercicio_sin_na, paired = TRUE, na.rm = TRUE)
print(resultado_t_test) ### estadisticamente significativo

tfm_ejercicio <- subset(tfm, Tratamiento == "1")
pre_ejercicio<-tfm_ejercicio$SLS0derecha
post_ejercicio<-tfm_ejercicio$SLS3derecha
pre_ejercicio_sin_na <- ifelse(is.na(pre_ejercicio), NA, pre_ejercicio)
post_ejercicio_sin_na <- ifelse(is.na(post_ejercicio), NA, post_ejercicio)
resultado_t_test <- t.test(pre_ejercicio_sin_na, post_ejercicio_sin_na, paired = TRUE, na.rm = TRUE)
print(resultado_t_test) ##### no significativo


####FASE 2 control izquierda
tfm_control <- subset(tfm, Tratamiento == "2")
tfm_control$FD_Combo <- ifelse(is.na(tfm_control$ALT0derecha), tfm_control$ALT0izquierda, ifelse(is.na(tfm_control$ALT0izquierda), tfm_control$ALT0derecha, (tfm_control$ALT0derecha + tfm_control$ALT0izquierda) / 2))
tfm_control$FD_Combo <- tfm_control$FD_Combo[complete.cases(tfm_control$FD_Combo)]
tfm_control$FD_Combo3 <- ifelse(is.na(tfm_control$ALT3derecha), tfm_control$ALT3izquierda, ifelse(is.na(tfm_control$ALT3izquierda), tfm_control$ALT3derecha, (tfm_control$ALT3derecha + tfm_control$ALT3izquierda) / 2))
tfm_control$FD_Combo3 <- tfm_control$FD_Combo3[complete.cases(tfm_control$FD_Combo3)]
pre_control<-tfm_control$FD_Combo
post_control<-tfm_control$FD_Combo3
pre_control_sin_na <- ifelse(is.na(pre_control), NA, pre_control)
post_control_sin_na <- ifelse(is.na(post_control), NA, post_control)
resultado_t_test <- t.test(pre_control_sin_na, post_control_sin_na, paired = TRUE,  na.rm = TRUE)
print(resultado_t_test)####no significativo

tfm_control <- subset(tfm, Tratamiento == "2")
tfm_control$SLS_Combo <- ifelse(is.na(tfm_control$SLS0derecha), tfm_control$SLS0izquierda, ifelse(is.na(tfm_control$SLS0izquierda), tfm_control$SLS0derecha, (tfm_control$SLS0derecha + tfm_control$SLS0izquierda) / 2))
tfm_control$SLS_Combo <- tfm_control$SLS_Combo[complete.cases(tfm_control$SLS_Combo)]
tfm_control$SLS_Combo3 <- ifelse(is.na(tfm_control$SLS3derecha), tfm_control$SLS3izquierda, ifelse(is.na(tfm_control$SLS3izquierda), tfm_control$SLS3derecha, (tfm_control$SLS3derecha + tfm_control$SLS3izquierda) / 2))
tfm_control$SLS_Combo3 <- tfm_control$SLS_Combo3[complete.cases(tfm_control$SLS_Combo3)]
pre_control<-tfm_control$SLS_Combo
post_control<-tfm_control$SLS_Combo3
pre_control_sin_na <- ifelse(is.na(pre_control), NA, pre_control)
post_control_sin_na <- ifelse(is.na(post_control), NA, post_control)
resultado_t_test <- t.test(pre_control_sin_na, post_control_sin_na, paired = TRUE,  na.rm = TRUE)
print(resultado_t_test)##### no significativa


####FASE 2 control derecha
tfm_control <- subset(tfm, Tratamiento == "2")
pre_control<-tfm_control$ALT0derecha
post_control<-tfm_control$ALT1derecha
pre_control_sin_na <- ifelse(is.na(pre_control), NA, pre_control)
post_control_sin_na <- ifelse(is.na(post_control), NA, post_control)
resultado_t_test <- t.test(pre_control_sin_na, post_control_sin_na, paired = TRUE,  na.rm = TRUE)
print(resultado_t_test) #### no significativo

tfm_control <- subset(tfm, Tratamiento == "2")
pre_control<-tfm_control$SLS0derecha
post_control<-tfm_control$SLS3derecha
pre_control_sin_na <- ifelse(is.na(pre_control), NA, pre_control)
post_control_sin_na <- ifelse(is.na(post_control), NA, post_control)
resultado_t_test <- t.test(pre_control_sin_na, post_control_sin_na, paired = TRUE,  na.rm = TRUE)
print(resultado_t_test) #### no significativo





################################## BOXPLOT ################################
###FD TOBILLO FASE 1
tfm$FD_Combo <- ifelse(is.na(tfm$ALT0derecha), tfm$ALT0izquierda, ifelse(is.na(tfm$ALT0izquierda), tfm$ALT0derecha, (tfm$ALT0derecha + tfm$ALT0izquierda) / 2))
tfm$FD_Combo <- tfm$FD_Combo[complete.cases(tfm$FD_Combo)]
boxplot(tfm$FD_Combo ~ tfm$Tratamiento, data = tfm, xlab = "Tratamiento", ylab = "Flexión Dorsal de Tobillo (cm)", main = "Boxplot de Flexión Dorsal de Tobillo por Grupo de Tratamiento Fase 1 pre")

tfm$FD_Combo <- ifelse(is.na(tfm$ALT1derecha), tfm$ALT1izquierda, ifelse(is.na(tfm$ALT1izquierda), tfm$ALT1derecha, (tfm$ALT1derecha + tfm$ALT1izquierda) / 2))
tfm$FD_Combo <- tfm$FD_Combo[complete.cases(tfm$FD_Combo)]
boxplot(tfm$FD_Combo ~ tfm$Tratamiento, data = tfm, xlab = "Tratamiento", ylab = "Flexión Dorsal de Tobillo (cm)", main = "Boxplot de Flexión Dorsal de Tobillo por Grupo de Tratamiento Fase 1 post")


##### VALGO RODILLA FASE 1
par(mfrow=c(2, 1))
tfm$SLS_Combo <- ifelse(is.na(tfm$SLS0derecha), tfm$SLS0izquierda, ifelse(is.na(tfm$SLS0izquierda), tfm$SLS0derecha, (tfm$SLS0derecha + tfm$SLS0izquierda) / 2))
tfm$SLS_Combo <- tfm$SLS_Combo[complete.cases(tfm$SLS_Combo)]
boxplot(tfm$SLS_Combo ~ tfm$Tratamiento, data = tfm, xlab = "Tratamiento", ylab = "Valgo de rodilla (º)", main = "Boxplot de Valgo de rodilla por Grupo de Tratamiento Fase 1 pre")

tfm$SLS_Combo <- ifelse(is.na(tfm$SLS1derecha), tfm$SLS1izquierda, ifelse(is.na(tfm$SLS1izquierda), tfm$SLS1derecha, (tfm$SLS1derecha + tfm$SLS1izquierda) / 2))
tfm$SLS_Combo <- tfm$SLS_Combo[complete.cases(tfm$SLS_Combo)]
boxplot(tfm$SLS_Combo ~ tfm$Tratamiento, data = tfm, xlab = "Tratamiento", ylab = "Valgo de rodilla (º)", main = "Boxplot de Valgo de rodilla por Grupo de Tratamiento Fase 1 post")


################################################ FASE 2 ###############################
############## FD TOBILLO ######################
tfm$FD_Combo <- ifelse(is.na(tfm$ALT0derecha), tfm$ALT0izquierda, ifelse(is.na(tfm$ALT0izquierda), tfm$ALT0derecha, (tfm$ALT0derecha + tfm$ALT0izquierda) / 2))
tfm$FD_Combo <- tfm$FD_Combo[complete.cases(tfm$FD_Combo)]
boxplot(tfm$FD_Combo ~ tfm$Tratamiento, data = tfm, xlab = "Tratamiento", ylab = "Flexión Dorsal de Tobillo (cm)", main = "Boxplot de Flexión Dorsal de Tobillo por Grupo de Tratamiento Fase 2 pre")

tfm$FD_Combo <- ifelse(is.na(tfm$ALT3derecha), tfm$ALT3izquierda, ifelse(is.na(tfm$ALT3izquierda), tfm$ALT3derecha, (tfm$ALT3derecha + tfm$ALT3izquierda) / 2))
tfm$FD_Combo <- tfm$FD_Combo[complete.cases(tfm$FD_Combo)]
boxplot(tfm$FD_Combo ~ tfm$Tratamiento, data = tfm, xlab = "Tratamiento", ylab = "Flexión Dorsal de Tobillo (cm)", main = "Boxplot de Flexión Dorsal de Tobillo por Grupo de Tratamiento Fase 2 post")

############################# VALGO ###################

tfm$SLS_Combo <- ifelse(is.na(tfm$SLS0derecha), tfm$SLS0izquierda, ifelse(is.na(tfm$SLS0izquierda), tfm$SLS0derecha, (tfm$SLS0derecha + tfm$SLS0izquierda) / 2))
tfm$SLS_Combo <- tfm$SLS_Combo[complete.cases(tfm$SLS_Combo)]
boxplot(tfm$SLS_Combo ~ tfm$Tratamiento, data = tfm, xlab = "Tratamiento", ylab = "Valgo de rodilla (º)", main = "Boxplot de Valgo de rodilla por Grupo de Tratamiento Fase 2 pre")

tfm$SLS_Combo <- ifelse(is.na(tfm$SLS3derecha), tfm$SLS3izquierda, ifelse(is.na(tfm$SLS3izquierda), tfm$SLS3derecha, (tfm$SLS3derecha + tfm$SLS3izquierda) / 2))
tfm$SLS_Combo <- tfm$SLS_Combo[complete.cases(tfm$SLS_Combo)]
boxplot(tfm$SLS_Combo ~ tfm$Tratamiento, data = tfm, xlab = "Tratamiento", ylab = "Valgo de rodilla (º)", main = "Boxplot de Valgo de rodilla por Grupo de Tratamiento Fase 2 post")






############### analisis pre y post intergrupo ########################

#ANOVA de un factor FASE 1#
tfm$FD_ComboPre <- ifelse(is.na(tfm$ALT0derecha), tfm$ALT0izquierda, ifelse(is.na(tfm$ALT0izquierda), tfm$ALT0derecha, (tfm$ALT0derecha + tfm$ALT0izquierda) / 2))
tfm$FD_ComboPre <- tfm$FD_ComboPre[complete.cases(tfm$FD_ComboPre)]

tfm$FD_ComboPost <- ifelse(is.na(tfm$ALT1derecha), tfm$ALT1izquierda, ifelse(is.na(tfm$ALT1izquierda), tfm$ALT1derecha, (tfm$ALT1derecha + tfm$ALT1izquierda) / 2))
tfm$FD_ComboPost <- tfm$FD_ComboPost[complete.cases(tfm$FD_ComboPost)]
modelo_anova <- aov(tfm$FD_ComboPost - tfm$FD_ComboPre ~ tfm$Tratamiento, data = tfm)
summary(modelo_anova)

tukey_resultados <- TukeyHSD(modelo_anova)
print(tukey_resultados)


#valgo#
tfm$SLS_ComboPre <- ifelse(is.na(tfm$SLS0derecha), tfm$SLS0izquierda, ifelse(is.na(tfm$SLS0izquierda), tfm$SLS0derecha, (tfm$SLS0derecha + tfm$SLS0izquierda) / 2))
tfm$SLS_ComboPre <- tfm$SLS_ComboPre[complete.cases(tfm$SLS_ComboPre)]

tfm$SLS_ComboPost <- ifelse(is.na(tfm$SLS1derecha), tfm$SLS1izquierda, ifelse(is.na(tfm$SLS1izquierda), tfm$SLS1derecha, (tfm$SLS1derecha + tfm$SLS1izquierda) / 2))
tfm$SLS_ComboPost <- tfm$SLS_ComboPost[complete.cases(tfm$SLS_ComboPost)]

modelo_anova <- aov(tfm$SLS_ComboPost - tfm$SLS_ComboPre ~ tfm$Tratamiento, data = tfm)
summary(modelo_anova)
tukey_resultados <- TukeyHSD(modelo_anova)
print(tukey_resultados) #no efectos del tratamiento
#### ANOVA de un factor FASE 2 ###########
tfm$FD_ComboPre <- ifelse(is.na(tfm$ALT0derecha), tfm$ALT0izquierda, ifelse(is.na(tfm$ALT0izquierda), tfm$ALT0derecha, (tfm$ALT0derecha + tfm$ALT0izquierda) / 2))
tfm$FD_ComboPre <- tfm$FD_ComboPre[complete.cases(tfm$FD_ComboPre)]

tfm$FD_ComboPost2 <- ifelse(is.na(tfm$ALT3derecha), tfm$ALT3izquierda, ifelse(is.na(tfm$ALT3izquierda), tfm$ALT3derecha, (tfm$ALT3derecha + tfm$ALT3izquierda) / 2))
tfm$FD_ComboPost2 <- tfm$FD_ComboPost2[complete.cases(tfm$FD_ComboPost2)]
modelo_anova <- aov(tfm$FD_ComboPost2 - tfm$FD_ComboPre ~ tfm$Tratamiento, data = tfm)
summary(modelo_anova)
tukey_resultados <- TukeyHSD(modelo_anova)
print(tukey_resultados)

modelo_anova<- aov(tfm$ALT3derecha-tfm$ALT0derecha ~ tfm$Tratamiento, data = tfm)
summary(modelo_anova)

######VALGO #######
tfm$SLS_ComboPre <- ifelse(is.na(tfm$SLS0derecha), tfm$SLS0izquierda, ifelse(is.na(tfm$SLS0izquierda), tfm$SLS0derecha, (tfm$SLS0derecha + tfm$SLS0izquierda) / 2))
tfm$SLS_ComboPre <- tfm$SLS_ComboPre[complete.cases(tfm$SLS_ComboPre)]

tfm$SLS_ComboPost2 <- ifelse(is.na(tfm$SLS3derecha), tfm$SLS3izquierda, ifelse(is.na(tfm$SLS3izquierda), tfm$SLS3derecha, (tfm$SLS3derecha + tfm$SLS3izquierda) / 2))
tfm$SLS_ComboPost2 <- tfm$SLS_ComboPost2[complete.cases(tfm$SLS_ComboPost2)]

modelo_anova <- aov(tfm$SLS_ComboPost2 - tfm$SLS_ComboPre ~ tfm$Tratamiento, data = tfm)
summary(modelo_anova)
tukey_resultados <- TukeyHSD(modelo_anova)
print(tukey_resultados)  ### No diferencia significativa
##########################

### Diferencias segun edad #######
limites_edad <- c(18, 24, 30, Inf)
etiquetas_edad <- c("Entre 18 y 24", "Entre 24 y 30", "Mayores de 30")
tfm$GrupoEdad <- cut(tfm$Edad, breaks = limites_edad, labels = etiquetas_edad, right = FALSE)
summary(tfm$GrupoEdad)
#FASE 1#
modelo_anova_edad<-aov(tfm$FD_ComboPost - tfm$FD_ComboPre ~ tfm$Tratamiento + tfm$GrupoEdad, data = tfm)
summary(modelo_anova_edad) # no diferencia significativa #

modelo_anova_edad<-aov(tfm$SLS_ComboPost - tfm$SLS_ComboPre ~ tfm$Tratamiento + tfm$GrupoEdad, data = tfm)
summary(modelo_anova_edad) # no diferencia significativa #

## FASE 2 ##
modelo_anova_edad<-aov(tfm$FD_ComboPost2 - tfm$FD_ComboPre ~ tfm$Tratamiento + tfm$GrupoEdad, data = tfm)
summary(modelo_anova_edad) # no diferencia significativa #

modelo_anova_edad<-aov(tfm$SLS_ComboPost2 - tfm$SLS_ComboPre ~ tfm$Tratamiento + tfm$GrupoEdad, data = tfm)
summary(modelo_anova_edad) # no diferencia significativa #

###### diferencias segun esguinces #####
limites_esguince <- c(0, 1, 3, Inf)
etiquetas_esguince <- c("Ningun esguince", "Entre 1 y 2", "3 o más")

tfm$EsguinceCombo<- ifelse(is.na(tfm$EsguinceDer), tfm$EsguinceIzq, ifelse(is.na(tfm$EsguinceIzq), tfm$EsguinceDer, (tfm$EsguinceDer + tfm$EsguinceIzq) / 2))
tfm$EsguinceCombo <- tfm$EsguinceCombo[complete.cases(tfm$EsguinceCombo)]

tfm$GrupoEsguince <- cut(tfm$EsguinceCombo, breaks = limites_esguince, labels = etiquetas_esguince, right = FALSE)
summary(tfm$GrupoEsguince)

# FASE 1 #
modelo_anova_esguinces<-aov(tfm$FD_ComboPost - tfm$FD_ComboPre ~ tfm$Tratamiento + tfm$GrupoEsguince, data = tfm)
summary(modelo_anova_esguinces) #No diferencia significativa #
modelo_anova_esguinces<-aov(tfm$SLS_ComboPost - tfm$SLS_ComboPre ~ tfm$Tratamiento + tfm$GrupoEsguince, data = tfm)
summary(modelo_anova_esguinces) # no diferencia significativa #


# FASE 2 #
modelo_anova_esguinces<-aov(tfm$FD_ComboPost2 - tfm$FD_ComboPre ~ tfm$Tratamiento + tfm$GrupoEsguince, data = tfm)
summary(modelo_anova_esguinces) # no diferencia significativa #

modelo_anova_esguinces<-aov(tfm$SLS_ComboPost2 - tfm$SLS_ComboPre ~ tfm$Tratamiento + tfm$GrupoEsguince, data = tfm)
summary(modelo_anova_esguinces) # no diferencia significativa #


##### diferencias segun IMC #####
tfm$peso<-tfm$`Peso (Kg)`
tfm$altura<-tfm$`Altura (cm)`
tfm$IMC <- tfm$peso / ((tfm$altura / 100) ^ 2)

limites_IMC <- c(18.5, 24.9, 29.9)
etiquetas_IMC <- c("Normopeso", "Sobrepeso")
tfm$GrupoIMC <- cut(tfm$IMC, breaks = limites_IMC, labels = etiquetas_IMC, right = FALSE)
summary(tfm$GrupoIMC)

# FASE 1 #
modelo_anova_IMC<-aov(tfm$FD_ComboPost - tfm$FD_ComboPre ~ tfm$Tratamiento + tfm$GrupoIMC, data = tfm)
summary(modelo_anova_IMC) # no diferencia significativa #
modelo_anova_IMC<-aov(tfm$SLS_ComboPost - tfm$SLS_ComboPre ~ tfm$Tratamiento + tfm$GrupoIMC, data = tfm)
summary(modelo_anova_IMC) # No diferencia significativa #

# FASE 2 #
modelo_anova_IMC<-aov(tfm$FD_ComboPost2 - tfm$FD_ComboPre ~ tfm$Tratamiento + tfm$GrupoIMC, data = tfm)
summary(modelo_anova_IMC) # No diferencia significativa #

modelo_anova_IMC<-aov(tfm$SLS_ComboPost2 - tfm$SLS_ComboPre ~ tfm$Tratamiento + tfm$GrupoIMC, data = tfm)
summary(modelo_anova_IMC) # No diferencia significativa #


