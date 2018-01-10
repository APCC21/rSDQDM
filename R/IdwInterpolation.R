IdwInterpolation<- function (Lon.Target,Lat.Target,Lon.GCM,Lat.GCM,GCM.data,First_Yr,End_Yr,name,...) {

  # Lon.Target <- STN_Lon
  # Lat.Target <- STN_Lat
  # Lon.GCM <- lon.GCM
  # Lat.GCM <- lat.GCM
  # GCM.data <- GCM.PRCP.Hist #GCM.TMAX.HiST,GCM.TMIN.HiST,GCM.PRCP.RCP45,GCM.TMAX.RCP45,GCM.TMIN.RCP45
  # First_Yr <- First_Yr_Hist
  # End_Yr <- End_Yr_Hist
  # name <- "PRCP.Hist"
  # Spatial interpolation function
  Target.grd<-as.data.frame(cbind(Lon.Target,Lat.Target))
  coordinates(Target.grd) = ~Lon.Target + Lat.Target

  nSim<-length(GCM.data[1,1,])
  nlon<-as.numeric(length(Lon.GCM))
  nlat<-as.numeric(length(Lat.GCM))
  GCM.data.lon<-array(0,dim=c(nlon*nlat))
  GCM.data.lat<-array(0,dim=c(nlon*nlat))
  GCM.data_col<-array(0,dim=c(nlon*nlat))
  GCM.data.interpol=NULL
  cat(paste("Inverse distance weighted interpolation :",name,sep=" "));cat("\n")
  pb_sim <- txtProgressBar(min=1,max=nSim,style=3)
  for (isim in 1:nSim) {
    #print(c(isim,"out of",nSim))
    setTxtProgressBar(pb_sim,isim)
    ilength=0
    for (ilon in 1:nlon) {
      for (ilat in 1:nlat) {
        ilength=ilength+1
        GCM.data.lon[ilength]<-Lon.GCM[ilon]
        GCM.data.lat[ilength]<-Lat.GCM[ilat]
        GCM.data_col[ilength]<-GCM.data[ilon,ilat,isim]
      }
    }
    GCM.grd<-as.data.frame(cbind(GCM.data.lon,GCM.data.lat,GCM.data_col))
    coordinates(GCM.grd) = ~GCM.data.lon + GCM.data.lat
    invisible(capture.output(data.idw <- idw(formula = GCM.data_col ~ 1,
                                             locations = GCM.grd, newdata = Target.grd)$var1.pred))
    #krige for kriging interpolation

    GCM.data.interpol<-rbind(GCM.data.interpol,data.idw)
  }
  close(pb_sim)
  cat("\n")
  #GCM.data.interpol<-data.frame(GCM.data.interpol)
  #Date_start_Tar<-as.Date(paste(First_Yr,"-01-01",sep=""))
  #Date_end_Tar<-as.Date(paste(End_Yr,"-12-31",sep=""))
  #Date_Tar_Period<-as.Date(seq(Date_start_Tar,Date_end_Tar,by="1 day"))
  #GCM.data.interpol<-cbind(Date_Tar_Period,GCM.data.interpol)
  rownames(GCM.data.interpol) <- NULL
  return(GCM.data.interpol)
}
