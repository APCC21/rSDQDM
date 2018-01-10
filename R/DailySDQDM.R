#' DailySDQDM
#'
#' Does SDQDM over station and observation data
#'
#' @param dbdir directory where downloaded data located
#' @param NtlCode 2 digit country(national) code
#'
#' @export
#'
# cmip5dir = EnvList$cmip5dir
# stnobsdir = EnvList$stnobsdir
# stndir = EnvList$stndir
# stnfile = EnvList$stnfile
# qmapdir = EnvList$qmapdir
# prjdir = EnvList$prjdir
# ModelNames = EnvList$ModelNames
# RcpNames = EnvList$RcpNames
# VarNames = EnvList$VarNames
# syear_obs = EnvList$syear_obs
# eyear_obs = EnvList$eyear_obs
# syear_his = EnvList$syear_his
# eyear_his = EnvList$eyear_his
# eyear_scn = EnvList$eyear_scn
DailySDQDM <- function(cmip5dir, stndir, stnfile, qmapdir, prjdir, ModelNames, RcpNames, VarNames, syear_obs, eyear_obs,
                       syear_his, eyear_his, eyear_scn,...){

#==================================================================================================
Path_OBS = stndir
Path_StnInfo = stndir
Path_NC = cmip5dir
File_Stn = stnfile
#==================================================================================================

Station_Info <- read.csv(file.path(Path_StnInfo, File_Stn), header=TRUE)
names( Station_Info ) <- c("STN_Lon", "STN_Lat","Height", "STN_Num", "STN_Name", "STN_SYear")
#attach(Station_Info)
STN_Lon <- Station_Info$STN_Lon
STN_Lat <- Station_Info$STN_Lat
Height <- Station_Info$Height
STN_Num <- Station_Info$STN_Num
STN_Name <- Station_Info$STN_Name
STN_SYear <- Station_Info$STN_SYear

Number_STN=as.numeric(length(Station_Info[,1]))

GCM_names = ModelNames
Number_GCMs= length(GCM_names)

Number_Var = length(VarNames)

First_Yr_obs=syear_obs
End_Yr_obs=eyear_obs
First_Yr_Hist=syear_his
End_Yr_Hist=eyear_his
#=====================================================
# These two variables need to move to input file
First_Yr_Future=as.numeric(End_Yr_Hist)+1
End_Yr_Future=as.numeric(eyear_scn[length(eyear_scn)])
#=====================================================
bd.obs<-as.Date(paste(First_Yr_obs,"-01-01",sep=""))
ed.obs<-as.Date(paste(End_Yr_obs,"-12-31",sep=""))
bd.Ref<-as.Date(paste(First_Yr_Hist,"-01-01",sep=""))
ed.Ref<-as.Date(paste(End_Yr_Hist,"-12-31",sep=""))
Date.obs<-as.Date((seq(bd.obs,ed.obs,by="1 day")))
Date.Ref<-as.Date((seq(bd.Ref,ed.Ref,by="1 day")))

Num_days_obs<-as.numeric(length(Date.obs))
Num_days_Ref<-as.numeric(length(Date.Ref))

bd.All<-as.Date(paste(First_Yr_Hist,"-01-01",sep=""))      #1976
ed.All<-as.Date(paste(End_Yr_Future,"-12-31",sep=""))      #2099
Date.All<-as.Date((seq(bd.All,ed.All,by="1 day")))

#=======================================================================================================================
# Read observation station data
#=======================================================================================================================
if("pr" %in% VarNames) STN_Data_PRCP=array(0,dim=c(Num_days_obs,Number_STN))
if("tasmax" %in% VarNames) STN_Data_TMAX=array(0,dim=c(Num_days_obs,Number_STN))
if("tasmin" %in% VarNames) STN_Data_TMIN=array(0,dim=c(Num_days_obs,Number_STN))

cat(paste("Station data reading"));cat("\n")
pb_stn1 <- txtProgressBar(min=1,max=Number_STN,style=3)
for (i_STN in 1:Number_STN) {
  #print(c("Station data reading...STN:",i_STN,"out of",Number_STN))
  setTxtProgressBar(pb_stn1,i_STN)
  STN_Num_text = STN_Num[i_STN]
  STN_Data<-ReadStationData(Path_OBS,STN_Num_text,First_Yr_obs,End_Yr_obs)  #STN_Data_Read function

  if("pr" %in% VarNames) STN_Data_PRCP[,i_STN]<-STN_Data[,1]
  if("tasmax" %in% VarNames) STN_Data_TMAX[,i_STN]<-STN_Data[,2]
  if("tasmin" %in% VarNames) STN_Data_TMIN[,i_STN]<-STN_Data[,3]
}
close(pb_stn1)
if("pr" %in% VarNames) {
  STN_Data_PRCP<-data.frame(STN_Data_PRCP)
  names(STN_Data_PRCP)<-STN_Num
  STN_Data_PRCP<-cbind(Date.obs,STN_Data_PRCP)
}
if("tasmax" %in% VarNames) {
  STN_Data_TMAX<-data.frame(STN_Data_TMAX)
  names(STN_Data_TMAX)<-STN_Num
  STN_Data_TMAX<-cbind(Date.obs,STN_Data_TMAX)
}
if("tasmin" %in% VarNames) {
  STN_Data_TMIN<-data.frame(STN_Data_TMIN)
  names(STN_Data_TMIN)<-STN_Num
  STN_Data_TMIN<-cbind(Date.obs,STN_Data_TMIN)
}


#=======================================================================================================================
# Informatin reagrding GCM data (netCDF files)
#=======================================================================================================================
folder<-Path_NC  # Folder path to read NC files


for(iGCM in 1:Number_GCMs){

for(iRCP in 1:length(RcpNames)){

GCM_Selected<-GCM_names[iGCM]  # GCM's name selected
rcp_sce<-RcpNames[iRCP]
#=======================================================================================================================
cat(paste("Program is running now... GCM : ",iGCM,"/",Number_GCMs," ",
          ModelNames[iGCM],"   RCP : ",iRCP,"/",length(RcpNames)," ",RcpNames[iRCP],sep=""))
cat("\n")
if("pr" %in% VarNames) {
  GCM_Hist_All<-NCcombine(folder,GCM_Selected,"pr","historical",First_Yr_Hist,End_Yr_Hist)
  GCM.PRCP.Hist<-GCM_Hist_All$NCData.Tar.Period
  lon.GCM<-GCM_Hist_All$lon.NCData  #longitude of GCM
  lat.GCM<-GCM_Hist_All$lat.NCData  #Latitude of GCM
}
if("tasmax" %in% VarNames) {
  GCM_Hist_All<-NCcombine(folder,GCM_Selected,"tasmax","historical",First_Yr_Hist,End_Yr_Hist)
  GCM.TMAX.HiST<-GCM_Hist_All$NCData.Tar.Period
  lon.GCM<-GCM_Hist_All$lon.NCData  #longitude of GCM
  lat.GCM<-GCM_Hist_All$lat.NCData  #Latitude of GCM
}
if("tasmin" %in% VarNames) {
  GCM_Hist_All<-NCcombine(folder,GCM_Selected,"tasmin","historical",First_Yr_Hist,End_Yr_Hist)
  GCM.TMIN.HiST<-GCM_Hist_All$NCData.Tar.Period
  lon.GCM<-GCM_Hist_All$lon.NCData  #longitude of GCM
  lat.GCM<-GCM_Hist_All$lat.NCData  #Latitude of GCM
}

if("pr" %in% VarNames) GCM.PRCP.RCP45<-NCcombine(folder,GCM_Selected,"pr",rcp_sce,First_Yr_Future,End_Yr_Future)$NCData.Tar.Period
if("tasmax" %in% VarNames)GCM.TMAX.RCP45<-NCcombine(folder,GCM_Selected,"tasmax",rcp_sce,First_Yr_Future,End_Yr_Future)$NCData.Tar.Period
if("tasmin" %in% VarNames)GCM.TMIN.RCP45<-NCcombine(folder,GCM_Selected,"tasmin",rcp_sce,First_Yr_Future,End_Yr_Future)$NCData.Tar.Period

#------------------------------------------------------------------------------------------------------------------
#  Applying IDW to GCM data to distribute GCM values to stations points
#------------------------------------------------------------------------------------------------------------------
if("pr" %in% VarNames) GCM.PRCP.Hist.intepol<-
  data.frame(IdwInterpolation(STN_Lon,STN_Lat,lon.GCM,lat.GCM,GCM.PRCP.Hist,First_Yr_Hist,End_Yr_Hist,
                              "Historical Precipitation"))
if("tasmax" %in% VarNames)GCM.TMAX.Hist.intepol<-
  data.frame(IdwInterpolation(STN_Lon,STN_Lat,lon.GCM,lat.GCM,GCM.TMAX.HiST,First_Yr_Hist,End_Yr_Hist,
                              "Historical Maximum Temperature"))
if("tasmin" %in% VarNames)GCM.TMIN.Hist.intepol<-
  data.frame(IdwInterpolation(STN_Lon,STN_Lat,lon.GCM,lat.GCM,GCM.TMIN.HiST,First_Yr_Hist,End_Yr_Hist,
                              "Historical Minimum Temperature"))

if("pr" %in% VarNames) GCM.PRCP.RCP45.intepol<-data.frame(IdwInterpolation(
  STN_Lon,STN_Lat,lon.GCM,lat.GCM,GCM.PRCP.RCP45,First_Yr_Future,End_Yr_Future,"Future Precipitation"))
if("tasmax" %in% VarNames) GCM.TMAX.RCP45.intepol<-data.frame(IdwInterpolation(
  STN_Lon,STN_Lat,lon.GCM,lat.GCM,GCM.TMAX.RCP45,First_Yr_Future,End_Yr_Future,"Future Maximum Temperature"))
if("tasmin" %in% VarNames) GCM.TMIN.RCP45.intepol<-data.frame(IdwInterpolation(
  STN_Lon,STN_Lat,lon.GCM,lat.GCM,GCM.TMIN.RCP45,First_Yr_Future,End_Yr_Future,"Future Minimum Temperature"))

if("pr" %in% VarNames) GCM.PRCP.ALL.interpol<-rbind(GCM.PRCP.Hist.intepol,GCM.PRCP.RCP45.intepol)
if("tasmax" %in% VarNames) GCM.TMAX.ALL.interpol<-rbind(GCM.TMAX.Hist.intepol,GCM.TMAX.RCP45.intepol)
if("tasmin" %in% VarNames) GCM.TMIN.ALL.interpol<-rbind(GCM.TMIN.Hist.intepol,GCM.TMIN.RCP45.intepol)

if("pr" %in% VarNames) GCM.PRCP.Ref<-cbind(Date.Ref,GCM.PRCP.Hist.intepol)  # Date, Reference[e.g. 1976-01-01 0.0 0.7 ....]
if("tasmax" %in% VarNames) GCM.TMAX.Ref<-cbind(Date.Ref,GCM.TMAX.Hist.intepol)
if("tasmin" %in% VarNames) GCM.TMIN.Ref<-cbind(Date.Ref,GCM.TMIN.Hist.intepol)

if("pr" %in% VarNames) GCM.PRCP.All.Date<-cbind(Date.All,GCM.PRCP.ALL.interpol)
if("tasmax" %in% VarNames) GCM.TMAX.All.Date<-cbind(Date.All,GCM.TMAX.ALL.interpol)
if("tasmin" %in% VarNames) GCM.TMIN.All.Date<-cbind(Date.All,GCM.TMIN.ALL.interpol)

#------------------------------------------------------------------------------------------------------------------
Date_end_Ref<-as.Date(paste(End_Yr_Hist,"-12-31",sep=""))
Date_start_Tar<-as.Date(paste(First_Yr_Hist,"-01-01",sep=""))
Date_end_Tar<-as.Date(paste(End_Yr_Future,"-12-31",sep=""))
Date_Tar_Period<-as.Date(seq(Date_start_Tar,Date_end_Tar,by="1 day")) #from 1976-01-01 to 2099-12-31
Date_Ref_Period<-as.Date(seq(Date_start_Tar,Date_end_Ref,by="1 day")) #from 1976-01-01 to 2005-12-31

# QDM
if("pr" %in% VarNames) Downscaled.Data.PRCP.QDM<-array(NA,dim=c(length(Date_Tar_Period),Number_STN))
if("tasmax" %in% VarNames) Downscaled.Data.TMAX.QDM<-array(NA,dim=c(length(Date_Tar_Period),Number_STN))
if("tasmin" %in% VarNames) Downscaled.Data.TMIN.QDM<-array(NA,dim=c(length(Date_Tar_Period),Number_STN))

# MBC
#Downscaled.Data.PRCP.MBC<-array(NA,dim=c(length(Date_Tar_Period),Number_STN))
#Downscaled.Data.TMAX.MBC<-array(NA,dim=c(length(Date_Tar_Period),Number_STN))
#Downscaled.Data.TMIN.MBC<-array(NA,dim=c(length(Date_Tar_Period),Number_STN))

#===============================================================================
#   Select start and end years between First_Yr_Hist(1976) and End_Yr_Future (2099)
Sim_year_start <- as.numeric(syear_his)
Sim_year_End <- as.numeric(eyear_scn[length(eyear_scn)])
#===============================================================================
Date_start_Sim<-as.Date(paste(Sim_year_start,"-01-01",sep=""))
Date_end_Sim<-as.Date(paste(Sim_year_End,"-12-31",sep=""))
Date_Sim_Period<-as.Date(seq(Date_start_Sim,Date_end_Sim,by="1 day"))

Days.momth1<-c(31,28,31,30,31,30,31,31,30,31,30,31)
Days.momth2<-c(31,29,31,30,31,30,31,31,30,31,30,31)  #leap year

#################
#  pb <- winProgressBar(title = "progress", min = Sim_year_start,max = Sim_year_End, width = 300)
iLength_start<-0
iLength_end<-0

cat("Year 1976 - 2099 Under Claculation");cat("\n")
pb_simyr <- txtProgressBar(min=Sim_year_start,max=Sim_year_End,style=3)
for (iSim_Yr in Sim_year_start:Sim_year_End) {
  setTxtProgressBar(pb_simyr,iSim_Yr)
  #    print(paste("GCM: ",GCM_Selected,"iSim_Yr=",iSim_Yr,"from",Sim_year_start,"to",Sim_year_End,sep =" "))
  #print(c(iLength_start,iLength_end))
  if (iSim_Yr<=End_Yr_Hist) { #current(reference) period
    for (iMon in 1:12) {
      if(iSim_Yr%%4 == 0) {
        iLength_start=iLength_end+1
        iLength_end=iLength_end+Days.momth2[iMon]
      } else {
        iLength_start=iLength_end+1
        iLength_end=iLength_end+Days.momth1[iMon]
      }
      Date_Sim_Period[iLength_start:iLength_end]
      if (iMon<10) {
        iMon_text <- paste("0",iMon,sep="")
      } else {
        iMon_text <- as.character(iMon)
      }
      #------------------------------------------------------------------------------------------------------------------
      #  Collecting data within the moving time window (+- 1 month) for applying BCSD
      #------------------------------------------------------------------------------------------------------------------
      if("pr" %in% VarNames) Data.Window.obs.PRCP<-CollectDataWindow(iMon,STN_Data_PRCP)  #Station data
      if("tasmax" %in% VarNames) Data.Window.obs.TMAX<-CollectDataWindow(iMon,STN_Data_TMAX)  #Station data
      if("tasmin" %in% VarNames) Data.Window.obs.TMIN<-CollectDataWindow(iMon,STN_Data_TMIN)  #Station data

      if("pr" %in% VarNames) Data.Window.GCM.cur.PRCP<-CollectDataWindow(iMon,GCM.PRCP.Ref)  #GCM data
      if("tasmax" %in% VarNames) Data.Window.GCM.cur.TMAX<-CollectDataWindow(iMon,GCM.TMAX.Ref)  #GCM data
      if("tasmin" %in% VarNames) Data.Window.GCM.cur.TMIN<-CollectDataWindow(iMon,GCM.TMIN.Ref)  #GCM data

      #------------------------------------------------------------------------------------------------------------------
      for (iSTN in 1:Number_STN) {
        #	    cat("year", iSim_Yr, "Station ", iSTN, ", Month", iMon,"\n")
        #	    cat("iLength start", iLength_start, "and end", iLength_end, "the length bet is", iLength_end-iLength_start+1,"\n")
        #	    cat("length of Downscaled.PRCP.QDM.All$mhat.p[pic.data] : ",
        #		   length(Downscaled.PRCP.QDM.All$mhat.p[pic.data]), "\n")
        #QDM
        if("pr" %in% VarNames) Downscaled.PRCP.QDM.All<-QDM(Data.Window.obs.PRCP[,iSTN+1],Data.Window.GCM.cur.PRCP[,iSTN+1],Data.Window.GCM.cur.PRCP[,iSTN+1],ratio=TRUE)
        if("tasmax" %in% VarNames) Downscaled.TMAX.QDM.All<-QDM(Data.Window.obs.TMAX[,iSTN+1],Data.Window.GCM.cur.TMAX[,iSTN+1],Data.Window.GCM.cur.TMAX[,iSTN+1],ratio=FALSE)
        if("tasmin" %in% VarNames) Downscaled.TMIN.QDM.All<-QDM(Data.Window.obs.TMIN[,iSTN+1],Data.Window.GCM.cur.TMIN[,iSTN+1],Data.Window.GCM.cur.TMIN[,iSTN+1],ratio=FALSE)
        pic.data<-format(Data.Window.GCM.cur.PRCP[,1],"%Y")==as.character(iSim_Yr) & format(Data.Window.GCM.cur.PRCP[,1],"%m")==iMon_text
        if("pr" %in% VarNames) Downscaled.Data.PRCP.QDM[iLength_start:iLength_end,iSTN]<-Downscaled.PRCP.QDM.All$mhat.p[pic.data]
        if("tasmax" %in% VarNames) Downscaled.Data.TMAX.QDM[iLength_start:iLength_end,iSTN]<-Downscaled.TMAX.QDM.All$mhat.p[pic.data]
        if("tasmin" %in% VarNames) Downscaled.Data.TMIN.QDM[iLength_start:iLength_end,iSTN]<-Downscaled.TMIN.QDM.All$mhat.p[pic.data]

        #MBC
        #ratio.seq<-c("TRUE","FALSE","FALSE")
        #Data.Window.obs.All<-cbind(Data.Window.obs.PRCP[,iSTN+1],Data.Window.obs.TMAX[,iSTN+1],Data.Window.obs.TMIN[,iSTN+1])
        #Data.Window.GCM.cur.All<-cbind(Data.Window.GCM.cur.PRCP[,iSTN+1],Data.Window.GCM.cur.TMAX[,iSTN+1],Data.Window.GCM.cur.TMIN[,iSTN+1])
        #Data.Window.GCM.fut.All<-Data.Window.GCM.cur.All
        #Downscaled.MBC.All<-MBCp(Data.Window.obs.All,Data.Window.GCM.cur.All,Data.Window.GCM.fut.All,ratio.seq)$mhat.p

        #Downscaled.Data.PRCP.MBC[iLength_start:iLength_end,iSTN]<-Downscaled.MBC.All[pic.data,1]
        #Downscaled.Data.TMAX.MBC[iLength_start:iLength_end,iSTN]<-Downscaled.MBC.All[pic.data,2]
        #Downscaled.Data.TMIN.MBC[iLength_start:iLength_end,iSTN]<-Downscaled.MBC.All[pic.data,3]

      }
    }

  } else {  # future period
    #------------------------------------------------------------------------------------------------------------------
    #  Define 30-year future time window with the center of current year
    #------------------------------------------------------------------------------------------------------------------
    if("pr" %in% VarNames) GCM.PRCP.30Yr.Fut<-SelectData30Yr(End_Yr_Future,iSim_Yr,Date_Tar_Period,GCM.PRCP.All.Date)
    if("tasmax" %in% VarNames) GCM.TMAX.30Yr.Fut<-SelectData30Yr(End_Yr_Future,iSim_Yr,Date_Tar_Period,GCM.TMAX.All.Date)
    if("tasmin" %in% VarNames) GCM.TMIN.30Yr.Fut<-SelectData30Yr(End_Yr_Future,iSim_Yr,Date_Tar_Period,GCM.TMIN.All.Date)

    for (iMon in 1:12) {
      if(leap.year(iSim_Yr) == TRUE) {
        iLength_start=iLength_end+1
        iLength_end=iLength_end+Days.momth2[iMon]
      } else {
        iLength_start=iLength_end+1
        iLength_end=iLength_end+Days.momth1[iMon]
      }
      Date_Sim_Period[iLength_start:iLength_end]
      if (iMon<10) {
        iMon_text <- paste("0",iMon,sep="")
      } else {
        iMon_text <- as.character(iMon)
      }
      #------------------------------------------------------------------------------------------------------------------
      #  Collecting data within the moving time window (+- 1 month) for applying BCSD
      #------------------------------------------------------------------------------------------------------------------
      if("pr" %in% VarNames) Data.Window.obs.PRCP<-CollectDataWindow(iMon,STN_Data_PRCP)  #Station data
      if("tasmax" %in% VarNames) Data.Window.obs.TMAX<-CollectDataWindow(iMon,STN_Data_TMAX)  #Station data
      if("tasmin" %in% VarNames) Data.Window.obs.TMIN<-CollectDataWindow(iMon,STN_Data_TMIN)  #Station data

      if("pr" %in% VarNames) Data.Window.GCM.cur.PRCP<-CollectDataWindow(iMon,GCM.PRCP.Ref)  #GCM current data
      if("tasmax" %in% VarNames) Data.Window.GCM.cur.TMAX<-CollectDataWindow(iMon,GCM.TMAX.Ref)  #GCM current data
      if("tasmin" %in% VarNames) Data.Window.GCM.cur.TMIN<-CollectDataWindow(iMon,GCM.TMIN.Ref)  #GCM current data

      if("pr" %in% VarNames) Data.Window.GCM.fut.PRCP<-CollectDataWindow(iMon,GCM.PRCP.30Yr.Fut)  #GCM future data
      if("tasmax" %in% VarNames) Data.Window.GCM.fut.TMAX<-CollectDataWindow(iMon,GCM.TMAX.30Yr.Fut)  #GCM future data
      if("tasmin" %in% VarNames) Data.Window.GCM.fut.TMIN<-CollectDataWindow(iMon,GCM.TMIN.30Yr.Fut)  #GCM future data
      #------------------------------------------------------------------------------------------------------------------
      #  Bias correction - QM, DQM, QDM
      #------------------------------------------------------------------------------------------------------------------
      for (iSTN in 1:Number_STN) {

        #QDM
        if("pr" %in% VarNames) Downscaled.PRCP.QDM.All<-QDM(Data.Window.obs.PRCP[,iSTN+1],Data.Window.GCM.cur.PRCP[,iSTN+1],Data.Window.GCM.fut.PRCP[,iSTN+1],ratio=TRUE)
        if("tasmax" %in% VarNames) Downscaled.TMAX.QDM.All<-QDM(Data.Window.obs.TMAX[,iSTN+1],Data.Window.GCM.cur.TMAX[,iSTN+1],Data.Window.GCM.fut.TMAX[,iSTN+1],ratio=FALSE)
        if("tasmin" %in% VarNames) Downscaled.TMIN.QDM.All<-QDM(Data.Window.obs.TMIN[,iSTN+1],Data.Window.GCM.cur.TMIN[,iSTN+1],Data.Window.GCM.fut.TMIN[,iSTN+1],ratio=FALSE)
        pic.data<-format(Data.Window.GCM.fut.PRCP[,1],"%Y")==as.character(iSim_Yr) & format(Data.Window.GCM.fut.PRCP[,1],"%m")==iMon_text

        if("pr" %in% VarNames) Downscaled.Data.PRCP.QDM[iLength_start:iLength_end, iSTN]<-Downscaled.PRCP.QDM.All$mhat.p[pic.data]
        if("tasmax" %in% VarNames) Downscaled.Data.TMAX.QDM[iLength_start:iLength_end, iSTN]<-Downscaled.TMAX.QDM.All$mhat.p[pic.data]
        if("tasmin" %in% VarNames) Downscaled.Data.TMIN.QDM[iLength_start:iLength_end, iSTN]<-Downscaled.TMIN.QDM.All$mhat.p[pic.data]

      } #iSTN
    }   #iMon
  }   #future
  #info <- sprintf("Year %d Under Calculation...\n", iSim_Yr); cat(info)

  #    setWinProgressBar(pb, iSim_Yr, label = info)

}     #iSim_Yr
close(pb_simyr)
##############################


#----------------------------------------------------
# Output file
#----------------------------------------------------
model.dir <- file.path(qmapdir,GCM_Selected,sep="")

if(!file.exists(model.dir)) {dir.create(model.dir, showWarnings=F,recursive=T)}

leng.sim.hist<-length(which(Date_Sim_Period<=ed.Ref))
leng.sim.fut<-length(Date_Sim_Period)

if("pr" %in% VarNames) {
  DS.PRCP.QDM.hist<-Downscaled.Data.PRCP.QDM[1:leng.sim.hist,]
  DS.PRCP.QDM.fut<-Downscaled.Data.PRCP.QDM[(leng.sim.hist+1):leng.sim.fut,]
  GCM.PRCP.hist.out<-GCM.PRCP.Hist.intepol
  GCM.PRCP.fut.out<-GCM.PRCP.RCP45.intepol
} else {
  DS.PRCP.QDM.hist=array(-99,dim=c(leng.sim.hist, Number_STN))
  DS.PRCP.QDM.fut=array(-99,dim=c((leng.sim.fut-leng.sim.hist), Number_STN))
  GCM.PRCP.hist.out=array(-99,dim=c(leng.sim.hist, Number_STN))
  GCM.PRCP.fut.out=array(-99,dim=c((leng.sim.fut-leng.sim.hist), Number_STN))
}
if("tasmax" %in% VarNames) {
  DS.TMAX.QDM.hist<-Downscaled.Data.TMAX.QDM[1:leng.sim.hist,]
  DS.TMAX.QDM.fut<-Downscaled.Data.TMAX.QDM[(leng.sim.hist+1):leng.sim.fut,]
  GCM.TMAX.hist.out<-GCM.TMAX.Hist.intepol
  GCM.TMAX.fut.out<-GCM.TMAX.RCP45.intepol
} else {
  DS.TMAX.QDM.hist=array(-99,dim=c(leng.sim.hist, Number_STN))
  DS.TMAX.QDM.fut=array(-99,dim=c((leng.sim.fut-leng.sim.hist), Number_STN))
  GCM.TMAX.hist.out=array(-99,dim=c(leng.sim.hist, Number_STN))
  GCM.TMAX.fut.out=array(-99,dim=c((leng.sim.fut-leng.sim.hist), Number_STN))
}

if("tasmin" %in% VarNames) {
  DS.TMIN.QDM.hist<-Downscaled.Data.TMIN.QDM[1:leng.sim.hist,]
  DS.TMIN.QDM.fut<-Downscaled.Data.TMIN.QDM[(leng.sim.hist+1):leng.sim.fut,]
  GCM.TMIN.hist.out <- GCM.TMIN.Hist.intepol
  GCM.TMIN.fut.out <- GCM.TMIN.RCP45.intepol
} else {
  DS.TMIN.QDM.hist=array(-99,dim=c(leng.sim.hist, Number_STN))
  DS.TMIN.QDM.fut=array(-99,dim=c((leng.sim.fut-leng.sim.hist), Number_STN))
  GCM.TMIN.hist.out=array(-99,dim=c(leng.sim.hist, Number_STN))
  GCM.TMIN.fut.out=array(-99,dim=c((leng.sim.fut-leng.sim.hist), Number_STN))
}
if(length(VarNames)<6){
Missing.Var<-6-length(VarNames) #No data for "wspd","rhum","rsds"
Out.hist.Dummy=array(-99,dim=c(leng.sim.hist,Missing.Var))
Out.fut.Dummy=array(-99,dim=c((leng.sim.fut-leng.sim.hist),Missing.Var))
}

cat("Writing output files...");cat("\n")
pb_stn2 <- txtProgressBar(min=1,max=Number_STN,style=3)

for(iSTN in 1:Number_STN){

  setTxtProgressBar(pb_stn2,iSTN)
  #print(paste("Writing output files...STN:",iSTN,"out of",Number_STN,sep="  "))
  filename.qdm.hist <- file.path(model.dir, paste(Station_Info[iSTN, "STN_Num"],"_SDQDM_",GCM_Selected,"_historical.csv",sep=""))
  filename.gcm.hist <- file.path(model.dir, paste(Station_Info[iSTN, "STN_Num"],"_SDQDM_",GCM_Selected,"_historical_original.csv",sep=""))
  filename.qdm.fut <- file.path(model.dir, paste(Station_Info[iSTN, "STN_Num"],"_SDQDM_",GCM_Selected,"_",rcp_sce,".csv",sep=""))
  filename.gcm.fut <- file.path(model.dir, paste(Station_Info[iSTN, "STN_Num"],"_SDQDM_",GCM_Selected,"_",rcp_sce,"_original.csv",sep=""))
  print.GCM.hist<-cbind(as.numeric(substr(Date_Sim_Period[1:leng.sim.hist],1,4)),as.numeric(substr(Date_Sim_Period[1:leng.sim.hist],6,7)),
                        as.numeric(substr(Date_Sim_Period[1:leng.sim.hist],9,10)),GCM.PRCP.hist.out[,iSTN],GCM.TMAX.hist.out[,iSTN],
                        GCM.TMIN.hist.out[,iSTN],Out.hist.Dummy)

  print.GCM.fut<-cbind(as.numeric(substr(Date_Sim_Period[(leng.sim.hist+1):leng.sim.fut],1,4)),as.numeric(substr(Date_Sim_Period[(leng.sim.hist+1):leng.sim.fut],6,7)),
                       as.numeric(substr(Date_Sim_Period[(leng.sim.hist+1):leng.sim.fut],9,10)), GCM.PRCP.fut.out[,iSTN],GCM.TMAX.fut.out[,iSTN],
                       GCM.TMIN.fut.out[,iSTN],Out.fut.Dummy)

  print.QDM.hist<-cbind(as.numeric(substr(Date_Sim_Period[1:leng.sim.hist],1,4)),as.numeric(substr(Date_Sim_Period[1:leng.sim.hist],6,7)),
                        as.numeric(substr(Date_Sim_Period[1:leng.sim.hist],9,10)),DS.PRCP.QDM.hist[,iSTN],DS.TMAX.QDM.hist[,iSTN],
                        DS.TMIN.QDM.hist[,iSTN],Out.hist.Dummy)
  print.QDM.fut<-cbind(as.numeric(substr(Date_Sim_Period[(leng.sim.hist+1):leng.sim.fut],1,4)),as.numeric(substr(Date_Sim_Period[(leng.sim.hist+1):leng.sim.fut],6,7)),
                       as.numeric(substr(Date_Sim_Period[(leng.sim.hist+1):leng.sim.fut],9,10)),DS.PRCP.QDM.fut[,iSTN],DS.TMAX.QDM.fut[,iSTN],
                       DS.TMIN.QDM.fut[,iSTN],Out.fut.Dummy)
  colnames(print.GCM.hist)<-c("year", "mon", "day","prcp","tmax","tmin","wspd",
                              "rhum","rsds")
  colnames(print.GCM.fut)<-c("year", "mon", "day","prcp","tmax","tmin","wspd",
                             "rhum","rsds")
  colnames(print.QDM.hist)<-c("year", "mon", "day","prcp","tmax","tmin","wspd",
                              "rhum","rsds")
  colnames(print.QDM.fut)<-c("year", "mon", "day","prcp","tmax","tmin","wspd",
                             "rhum","rsds")
  write.csv(print.GCM.hist,file=filename.gcm.hist,row.names = FALSE)
  write.csv(print.QDM.hist,file=filename.qdm.hist,row.names = FALSE)
  write.csv(print.QDM.fut,file=filename.qdm.fut,row.names = FALSE)
  write.csv(print.GCM.fut,file=filename.gcm.fut,row.names= FALSE)
}
close(pb_stn2)

} # loop over RCP scenarios
} # loop over GCM Models
#detach(Station_Info)
} # end of DailySDQDM (the outest loop)
