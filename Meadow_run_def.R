library(foreign)
library(doParallel)
library(foreach)

######
rm(list = ls())

setwd("/lunarc/nobackup/projects/snic2020-6-29/Ruiying/CENTURY/CENTURY_FUTURE/CENTURY_file/century_only_terra_1006/caodian_0713_SOCD2")
load("caodian_prev_terra.RData")

DA_parameters <- read.csv("caodian_parameter_1006.csv")
colnames(DA_parameters)<-c("No","Name","DA","Default","Min","Max","Full.Name","Unit","Reference")
paraTemp_origin <- DA_parameters
#pars_new <- read.csv("caodian_SOCbased_SOC_pars_1006.csv")

#pars_new <- pars_new[,-c(14:16)]
#point_4km<-read.dbf('/lunarc/nobackup/projects/snic2020-6-29/Ruiying/CENTURY/CENTURY_FUTURE/Point_Future/point_4km_0628_BD.dbf')
#caodian_bd <- point_4km$BD_1[which(point_4km$VEG==8)]
#soil_veg$BD <-caodian_bd
#na_data1<-which(is.na(prec$GFDL_historical_prScaled_1961_01.tif),arr.ind = TRUE)
#na_grave <- which(is.na(soil_veg$GRAVE),arr.ind = TRUE)
#na_bd<-which(soil_veg$BD==0,arr.ind = T)
#na_ph<-which(soil_veg$pH==0,arr.ind = T)
#na_texture<-which((soil_veg$silt+soil_veg$sand+soil_veg$clay)==0,arr.ind = T)
#na_all<-unique(c(na_bd,na_data1,na_ph,na_texture,na_grave))
#na_all<-sort(na_all)


paraTemp <- paraTemp_origin$Default
fixCopy$X=paraTemp[1:214]
write.table(fixCopy,file = "fix.100", quote = F, row.names = F)

cropCopy$CPR=paraTemp[-c(1:214)]
colnames(cropCopy)<-c("CPR","  CPER")
write.table(cropCopy,file="crop.100",quote=F,row.names=F)

#--------------------------run model-----------------------
cores<-detectCores(logical = FALSE)-2
c1 <- makeCluster(cores)
registerDoParallel(c1)
caodian_result <-foreach(i=1:nrow,.combine = "rbind",.packages = c("dplyr","magrittr")) %dopar% {fun_century(i,na_all,soil_veg,prec_mean,tmin_mean,tmax_mean,site_caodian,prec,tmin,tmax,climate,caption,lines_sch,lines_run,outvars)}
stopCluster(c1) 

#save(caodian_result,file="caodian_GFDL_ssp126_ss0719_1.RData")
save(caodian_result,file="caodian_def_result.RData")


# soilCt_caodian <- caodian_result[seq(1,nrow(caodian_result),by=4),] 
# cprodc_caodian <- caodian_result[seq(2,nrow(caodian_result),by=4),]
# # AR_caodian <- caodian_result[seq(3,nrow(caodian_result),by=4),]
# # HR_caodian <- caodian_result[seq(4,nrow(caodian_result),by=4),]
# 
# soilCt_caodian_mean <- c()
# for (som in 1:nrow(soilCt_caodian)){
#     soilCt_caodian_temp <- t(aggregate(soilCt_caodian[som,], list(rep(1:140, each = 12)), mean)[-1])
#     soilCt_caodian_mean <- rbind(soilCt_caodian_mean,soilCt_caodian_temp)  
# }
# colnames(soilCt_caodian_mean)<-c(paste0("Y",c(1958:2100)))
# 
# cprodc_caodian_sum <- c()
# for (cpr in 1:nrow(cprodc_caodian)){
#     cprodc_caodian_temp <- t(aggregate(cprodc_caodian[cpr,], list(rep(1:140, each = 12)), sum)[-1])
#     cprodc_caodian_sum <- rbind(cprodc_caodian_sum,soilCt_caodian_temp)
# }
# colnames(cprodc_caodian_sum)<-c(paste0("Y",c(1958:2100)))
# 
# write.table(soilCt_caodian_mean,"somtc_caodian_GFDL_ssp126.csv", row.names = FALSE,sep=",")
# write.table(cprodc_caodian_sum,"NPP_caodian_GFDL_ssp126.csv",row.names=FALSE,sep=",")
# 
# #RE_caodian = AR_caodian + HR_caodian
# #GPP_caodian = cprodc_caodian + AR_caodian
# #NEP_caodian = cprodc_caodian - HR_caodian
# #names <- c(paste0("Y",c(rep(1958:2015,each=12)),"_",c(rep(1:12,by=58))))
# 
# #write.table(RE_caodian,"RE_caodian.csv",row.names=FALSE,sep=",")
# #write.table(GPP_caodian,"GPP_caodian.csv",row.names=FALSE,sep=",")
# #write.table(NEP_caodian,"NEP_caodian.csv",row.names=FALSE,sep=",")
