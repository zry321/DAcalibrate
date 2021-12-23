library(magrittr)
library(foreign)
library(dplyr)
library(foreach)
library(doParallel)

######
rm(list = ls())

formatOutput = function(data){
  formatPattern = "%s  %s%7.2f%7.2f%7.2f%7.2f%7.2f%7.2f%7.2f%7.2f%7.2f%7.2f%7.2f%7.2f"
  
  chars = c()
  for(i in c(1:nrow(data))){
    chars[i] = data[i, ] %$%
      sprintf(
        formatPattern,
        V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14
      )
    
  }
  return(chars)
}

setwd("/lunarc/nobackup/projects/snic2020-6-29/Ruiying/CENTURY/CENTURY_FUTURE/FUTURE_DATA")
prec<-read.csv("prec_terra_GFDL_ssp126_1005_8.csv",header = T, sep = ",") ##when apply other type, prec_SURVEY_3
prec<-prec[,1:660]
tmin<-read.csv("tmin_terra_GFDL_ssp126_1005_8.csv",header = T, sep = ",")
tmin<-tmin[,1:660]
tmax<-read.csv("tmax_terra_GFDL_ssp126_1005_8.csv",header = T, sep = ",")
tmax<-tmax[,1:660]
soil_veg<-read.csv("soil_veg_terra_GFDL_ssp126_1005_8.csv",header = T, sep = ",")
prec_mean<-read.csv("prec_mean_terra_GFDL_ssp126_1005_8.csv",header = T,sep = ",")
# prec_std<-read.csv("prec_std_SURVEY_0422_8.csv",header = T,sep = ",")
# prec_skw<-read.csv("prec_skw_SURVEY_0422_8.csv",header = T,sep = ",")
tmin_mean<-read.csv("tmin_mean_terra_GFDL_ssp126_1005_8.csv",header = T, sep = ",")
tmax_mean<-read.csv("tmax_mean_terra_GFDL_ssp126_1005_8.csv",header = T, sep = ",")

#SIF_GPP <- read.csv("SIFGPP_terra_GFDL_ssp126_1005_8.csv",header = T, sep = ",")
na_data1<-which(is.na(prec$GFDL_historical_prScaled_1961_01.tif),arr.ind = TRUE)
#na_data1<-which(is.na(prec$GFDL_historical_prScaled_1961_01.tif),arr.ind = TRUE)
na_grave <- which(is.na(soil_veg$GRAVE),arr.ind = TRUE)
na_bd<-which(soil_veg$BD==0,arr.ind = T)
na_ph<-which(soil_veg$pH==0,arr.ind = T)
na_texture<-which((soil_veg$silt+soil_veg$sand+soil_veg$clay)==0,arr.ind = T)

na_all<-unique(c(na_bd,na_data1,na_ph,na_texture,na_grave))
na_all<-sort(na_all)


prec_mean<-round(prec_mean,5)
# prec_std<-round(prec_std,5)
# prec_skw<-round(prec_skw,5)
tmin_mean<-round(tmin_mean,5)
tmax_mean<-round(tmax_mean,5)


setwd("/lunarc/nobackup/projects/snic2020-6-29/Ruiying/CENTURY/CENTURY_FUTURE/CENTURY_file/century_only_terra_1006/caodian_0713_SOCD2")
#system("rm *.bin")

fixCopy=read.table("fixOrigin.100",header = T)

cropCopy=read.table("cropOrigin.100",header=T)

site_caodian=read.table("c3grs.100",header = T)
site_caodian$CPER %<>% sapply(.,function(item){
  paste0("'", item, "'")
}, USE.NAMES = F)

#shift_year=3000, for spin up
year<-rep(4961:5015,each=3)
variable<-rep(c("prec","tmin","tmax"),55)
climate<-cbind(variable,year)
climate<-as.data.frame(climate)
rm(year)
rm(variable)

nrow<-dim(soil_veg)[1]

soilCt_caodian <- matrix(NA,nrow,55)

f_sch = file("c3grs_v0.sch", "r+")
lines_sch = readLines(con = f_sch)
close(f_sch)


caption <-t(data.frame(c("*** Climate parameters", ""), c("*** Site and control parameters", ""), c("*** External nutrients input parameters", ""),
                       c("*** Organic matter initial values", ""), c("*** Forest organic matter initial parameters", ""),  c("*** Mineral initial parameters", ""), 
                       c("*** Water initial parameters", "")))

outvars <- read.table("outvars.txt")

f_run = file("run.sh","r+")
lines_run = readLines(con = f_run)
close(f_run)

fun_century = function(i,na_all,soil_veg,prec_mean,tmin_mean,tmax_mean,site_caodian,prec,tmin,tmax,climate,caption,lines_sch,lines_run,outvars){
  if (i %in% na_all){
    vegC <- matrix(NA,1,55)
#    deadC <- matrix(NA,1,55)
#    soilC1 <- matrix(NA,1,55)
#    soilC2 <- matrix(NA,1,55)
    litC <- matrix(NA,1,55)
#    litC2 <- matrix(NA,1,55)
    cprodc <- matrix(NA,1,55)
    HR_caodian_temp <- matrix(NA,1,55)
     soilC <- matrix(NA,1,55) 
 }else{
    climate_temp<-matrix(NA,12,1)
    soil_veg_temp<-soil_veg[i,]
    
    site_caodian[1:12,1]<-t(prec_mean[i,])
    site_caodian[37:48,1]<-t(tmin_mean[i,])
    site_caodian[49:60,1]<-t(tmax_mean[i,])      
    site_caodian[63,1]<-soil_veg_temp$LAT
    site_caodian[64,1]<-soil_veg_temp$LON
    site_caodian[65,1]<-soil_veg_temp$sand
    site_caodian[66,1]<-soil_veg_temp$silt
    site_caodian[67,1]<-soil_veg_temp$clay
#    site_caodian[68,1]<-soil_veg_temp$GRAVE
    site_caodian[69,1]<-soil_veg_temp$BD
    site_caodian[98,1]<-soil_veg_temp$pH
    
    site_caodian_temp <- rbind(caption[1,],site_caodian[1:60,],caption[2,],site_caodian[61:100,],caption[3,],site_caodian[101:107,],caption[4,],
                               site_caodian[108:157,],caption[5,],site_caodian[158:188,],caption[6,],site_caodian[189:225,],caption[7,],site_caodian[226:237,])
    
    write.table(site_caodian_temp,file = paste0("c3grsss",i,".100"), quote = F, row.names = F)
    
    
    for (j in 1:55)
    {
      climate_temp<-data.frame(climate_temp, 
                               t(prec[i,(1+(j-1)*12):(j*12)]), 
                               t(tmin[i,(1+(j-1)*12):(j*12)]), 
                               t(tmax[i,(1+(j-1)*12):(j*12)]))
    }
    climate_temp<-t(climate_temp[,-1])
    climate_temp<-cbind(climate,climate_temp)
    
    colnames(climate_temp)<-c("V1","V2","V3","V4","V5","V6","V7","V8",
                              "V9","V10","V11","V12","V13","V14")
    climate_temp %>%
      mutate_at(vars(-V1, -V2), function(vec) {
        round(vec, 2)
      }) %>% 
      formatOutput(.) %>%
      write.table(.,paste0("climate",i,".wth"),col.names = F,row.names = F,quote = F)
    
    #----------------sch file start------------------------------#
    
    lines_sch_temp = c(
      lines_sch[1:2],
      paste0("c3grsss",i,".100", "   Site file name"),
      lines_sch[4:37],
      paste0("climate",i,".wth"),
      lines_sch[39:43]
    )
    write.table(lines_sch_temp,paste0("c3grs_v",i,".sch"),quote = F,row.names = F,col.names = F)
    
    write.table(outvars,paste0("outvars_",i,".txt"),col.names = FALSE,row.names = FALSE,quote = FALSE)
    
    #####century
    lines_run_temp <- c(
      lines_run[1],
      paste0('./century_47  -s  c3grs_v',i,'  -n  caodian_',i,' >>/dev/null'),
      paste0('./list100_47  caodian_',i,'  caodian_',i,'  outvars_',i,'.txt >>/dev/null')
    )
    write.table(lines_run_temp,paste0("run_",i,".sh"), col.names = FALSE,row.names = FALSE,quote = FALSE)
    system(paste0("chmod 777 run_",i,".sh"))
    system(paste0('./run_',i,'.sh'))
    
    som_temp <- read.table(paste0("caodian_",i,".lis"), header = T)
    som_temp <- som_temp[c(2:56),]
    #colnames(som_temp) <- c("time","som1c1","som1c2","som2c1","som2c2","som3c","cprodc","resp")  
  colnames(som_temp)<-c("time","aglivc","bglivc","stdedc","strucc1","strucc2","metabc1","metabc2","som1c1","som1c2","som2c1","som2c2","som3c","cprodc","resp")
   vegC <- som_temp$aglivc + som_temp$bglivc + som_temp$stdedc
#    deadC <- som_temp$stdedc
#    soilC1 <- som_temp$som1c1 + som_temp$som2c1
#    soilC2 <- som_temp$som1c2 + som_temp$som2c2 + som_temp$som3c
    litC <- som_temp$strucc1 + som_temp$metabc1 + som_temp$strucc2 + som_temp$metabc2
#    litC2 <- som_temp$strucc2 + som_temp$metabc2
    cprodc <- som_temp$cprodc
    HR_caodian_temp <- som_temp$resp
     soilC <- som_temp$som1c1 + som_temp$som2c1+som_temp$som1c2 + som_temp$som2c2 + som_temp$som3c    
    system(paste0("rm caodian_",i,".bin"," caodian_",i,".lis"," climate",i,".wth"," c3grs_v",i,".sch"," outvars_",i,".txt"," run_",i,".sh"," c3grsss",i,".100"))
   
  }
  caodian_result <- rbind(vegC,litC,soilC,cprodc,HR_caodian_temp)
 # caodian_result <- soilC

  return(caodian_result)
}

save.image("caodian_prev_terra.RData")
