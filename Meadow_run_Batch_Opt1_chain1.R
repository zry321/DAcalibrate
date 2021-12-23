library(foreach)
library(doParallel)
generate<-function(a_old,amin,amax){
  while(TRUE){
    a_new=a_old+(runif(5,min=0,max=1)-0.5)*(amax-amin)/10
    if (a_new[1]>amin[1]&a_new[1]<amax[1]&+
        a_new[2]>amin[2]&a_new[2]<amax[2]&+
        a_new[3]>amin[3]&a_new[3]<amax[3]&+
        a_new[4]>amin[4]&a_new[4]<amax[4]&+
        a_new[5]>amin[5]&a_new[5]<amax[5]){
      break
    }
  }
  return(a_new)
}

generate2<-function(a_old,amin,amax){
  while(TRUE){
    a_new=a_old+(runif(2,min=0,max=1)-0.5)*(amax-amin)/10
    if (a_new[1]>amin[1]&a_new[1]<amax[1]&+
        a_new[2]>amin[2]&a_new[2]<amax[2]){
      break
    }
  }
  return(a_new)
}

#read observations and parameters
setwd("/home/zhaor/snic2020-6-29/Ruiying/CENTURY/CENTURY_SURVEY/CENTURY_file/Survey_Batch_Final/caodian_0515_SOCD1")
load("caodian_prev_Batch_SOCD_0927.RData")

DA_obs <- Num$SOCD0_30
DA_parameters <- read.csv("caodian_parameter_1006.csv")

nsim=10000        #number of simulation times
Nt=length(DA_obs) #number of data points
J_last=10000       #initial error

#parameters' range
colnames(DA_parameters)<-c("No","Name","DA","Default","Min","Max","Full.Name","Unit","Reference")
parameters<-DA_parameters[which(DA_parameters$DA==1,arr.ind = TRUE),]
parmin=parameters$Min
parmax=parameters$Max
pars_old=parameters$Default
pars_new=array(NA, dim=c(24,1))

#To save best simulation
pars_record=array(NA, dim=c(nsim,24))
J_record=array(NA, dim=c(nsim,1))
R_record=array(NA, dim=c(nsim,Nt))
somtc_record=array(NA,dim=c(nsim,Nt,55*12))
#VegC_record=array(NA,dim=c(nsim,Nt,58*12))
#NPP_record=array(NA,dim=c(nsim,Nt,58*12))
#HR_record=array(NA,dim=c(nsim,Nt,58*12))
#LitC_record=array(NA,dim=c(nsim,Nt,58*12))
#simulation satarts
upgraded=0
for (simu in 1:nsim){
  counter=simu
  upgraded
  
  #generate new parameters
  pars_new[1:5]=generate(pars_old[1:5],parmin[1:5],parmax[1:5])
  pars_new[6:10]=generate(pars_old[6:10],parmin[6:10],parmax[6:10])
  pars_new[11:15]=generate(pars_old[11:15],parmin[11:15],parmax[11:15])
  pars_new[16:20]=generate(pars_old[16:20],parmin[16:20],parmax[16:20])
  pars_new[21:22]=generate2(pars_old[21:22],parmin[21:22],parmax[21:22])
  pars_new[23:24]=generate2(pars_old[23:24],parmin[23:24],parmax[23:24])
  #set parameters
  #initial state parameters
  paraTemp_origin <- DA_parameters
  k<-1
  
  for (para in 1:nrow(DA_parameters)){
    if(paraTemp_origin$DA[para]==1){
      paraTemp_origin[para,"Default"] <- pars_new[k]
      k<-k+1
      }
  }
  paraTemp <- paraTemp_origin$Default
  fixCopy$X=paraTemp[1:214]
  write.table(fixCopy,file = "fix.100", quote = F, row.names = F)
  
  cropCopy$CPR=paraTemp[-c(1:214)]
  colnames(cropCopy)<-c("CPR","  CPER")
  write.table(cropCopy,file="crop.100",quote=F,row.names=F)
  
  #build empty arrays for data
  #soilCt_caodian <- matrix(NA,nrow,58)
  caodian <- c()
  #model simulation
  cores<-detectCores(logical = FALSE)-2
  c1 <- makeCluster(cores)
  registerDoParallel(c1)
  caodian_result <-foreach(m=1:nrow,.combine = "rbind",.packages = c("dplyr","magrittr")) %dopar% {fun_century(m,Num,na_all,soil_veg,prec_mean,tmin_mean,tmax_mean,site_caodian,prec,tmin,tmax,climate,caption,lines_sch,lines_run,outvars)}
  stopCluster(c1) 
  
#  liveC_caodian <- caodian_result[seq(1,nrow(caodian_result),by=8),] 
#  deadC_caodian <- caodian_result[seq(2,nrow(caodian_result),by=8),]
#  soilC1_caodian <- caodian_result[seq(3,nrow(caodian_result),by=8),]
  soilCt_caodian <- caodian_result
#  litC1_caodian <- caodian_result[seq(5,nrow(caodian_result),by=8),] 
#  litC2_caodian <- caodian_result[seq(6,nrow(caodian_result),by=8),]
#  NPP_caodian <- caodian_result[seq(7,nrow(caodian_result),by=8),]
#  HR_caodian <- caodian_result[seq(8,nrow(caodian_result),by=8),]
  
#  VegC_caodian <- liveC_caodian+deadC_caodian
#  LitC_caodian <- litC1_caodian+litC2_caodian
#  soilCt_caodian <- soilC1_caodian+soilC2_caodian

  soilCt_caodian_mean <- c()
  for (som in 1:nrow(soilCt_caodian)){
    soilCt_caodian_temp <- t(aggregate(soilCt_caodian[som,], list(rep(1:55, each = 12)), mean)[-1])
    soilCt_caodian_mean <- rbind(soilCt_caodian_mean,soilCt_caodian_temp)
  }

  colnames(soilCt_caodian_mean)<-c(paste0("Y",c(1961:2015)))

  names <- c(paste0("Y",c(rep(1961:2015,each=12)),"_",c(rep(1:12,by=55))))
#  colnames(NPP_caodian) <- colnames(HR_caodian) <- colnames(LitC_caodian) <- colnames(VegC_caodian) <- names
  
  soilCt_caodian_mean<-as.data.frame(soilCt_caodian_mean)
  caodian<-soilCt_caodian_mean[,"Y1985"]
  
  
  #calculated cost fuction
  # J1 = exp(sum((DA_obs-caodian)^2)/(0.05*sd(DA_obs$V1)))
  J1 = sum((DA_obs-caodian)^2/(2*var(DA_obs)))
  J_new = J1
  delta_J = J_new-J_last
  
  #Judge if it is accepted
  if (min(1,exp(-delta_J)) > runif(1,min=0,max=1)){
    pars_old=pars_new
    J_last=J_new
    upgraded=upgraded+1
    
    pars_record[upgraded,1:24]=pars_old
    J_record[upgraded,1]=J_last
    R_record[upgraded,1:Nt]=caodian
    somtc_record[upgraded,1:Nt,1:660]=soilCt_caodian
#    NPP_record[upgraded,1:Nt,1:696]=NPP_caodian
#    HR_record[upgraded,1:Nt,1:696]=HR_caodian
#    VegC_record[upgraded,1:Nt,1:696]=VegC_caodian
#    LitC_record[upgraded,1:Nt,1:696]=LitC_caodian
  }
  print(simu)
}  

save.image("caodian_run_Batch_SOCD5000_chain1_0927.RData")
