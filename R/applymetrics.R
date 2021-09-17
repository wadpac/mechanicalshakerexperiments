applymetrics = function(data,n=4,sf, lb=0.2, hb=15, epochsize=5){
  # This is an adaptation of the g.applymetrics function in GGIR
  allmetrics = c()
  averageperepochsize = function(x,sf,epochsize) {
    x2 =cumsum(c(0,x))
    select = seq(1,length(x2),by=sf*epochsize)
    x3 = diff(x2[round(select)]) / abs(diff(round(select)))
  }
  sumperepochsize = function(x,sf,epochsize) {
    x2 =cumsum(c(0,x))
    select = seq(1,length(x2),by=sf*epochsize)
    x3 = diff(x2[round(select)])
  }
  
  if (sf <= (hb *2)) { #avoid having a higher filter boundary higher than sf/2
    hb = round(sf/2) - 1
  }
  gravity = 1
  #================================================
  # Functions to aid metric extraction
  process_axes = function(data, filtertype, cut_point, n=4, sf=c()) {
    if (length(sf) == 0) warning("sf not found")
    if (filtertype == "pass" | filtertype == "high" | filtertype == "low") {
      hf_lf_filter = function(bound, n, sf, filtertype) {
        return(signal::butter(n,c(bound/(sf/2)),type=filtertype))
      }
      bf_filter = function(lb, hb, n, sf) {
        Wc = matrix(0,2,1)
        Wc[1,1] = lb / (sf/2)
        Wc[2,1] = hb / (sf/2)
        if (sf/2 < hb | sf/2 < hb) {
          warning("\nSample frequency ",sf," too low for calculating this metric.")
        }
        return(signal::butter(n,Wc,type=c("pass")))
      }
      if (filtertype == "pass") {
        coef = bf_filter(cut_point[1], cut_point[2], n, sf)
      } else {
        if (filtertype == "low") {
          bound = hb
        } else {
          bound = lb
        }
        coef = hf_lf_filter(bound, n, sf, filtertype)
      }
      data_processed = data
      for (i in 1:3) {
        data_processed[,i] = signal::filter(coef, data[,i])
      }
    } else if (filtertype == "rollmedian") {
      rollmed = function(x, sf) {
        winsi = round(sf*5)
        if (round(winsi/2) == (winsi/2)) winsi = winsi+1
        xm = zoo::rollmedian(x,k=winsi,na.pad=TRUE)
        xm[which(is.na(xm[1:1000]) ==T)] = xm[which(is.na(xm[1:1000]) ==F)[1]]
        return(xm)
      }
      data_processed = data
      for (i in 1:3) {
        data_processed[,i] = rollmed(data[,i], sf)
      }
      if (length(which(is.na(data_processed[,1]) == T |
                       is.na(data_processed[,2]) == T |
                       is.na(data_processed[,3]) == T)) > 0) {
        for (j in 1:3) {
          p1 = which(is.na(data_processed[,j]) ==F)
          data_processed[which(is.na( data_processed[,j]) ==T),j] =  data_processed[p1[length(p1)],j]
        }
      }
    }
    return(data_processed)
  }
  
  EuclideanNorm = function(xyz) {
    return(sqrt((xyz[,1]^2) + (xyz[,2]^2) + (xyz[,3]^2)))
  }
  #==========================
  # Band-pass filtering related metrics
  
  data_processed = abs(process_axes(data, filtertype="pass", cut_point=c(lb,hb), n, sf))
  # allmetrics$BFX = averageperepochsize(x=data_processed[,1], sf, epochsize)
  # allmetrics$BFY = averageperepochsize(x=data_processed[,2], sf, epochsize)
  # allmetrics$BFZ = averageperepochsize(x=data_processed[,3], sf, epochsize)
  allmetrics$BFEN = averageperepochsize(x=EuclideanNorm(data_processed),sf,epochsize)

  # 1) apply band-pass filter to mimic old-sensor
  # probably necessary to experiment with exact configuration
  # 0.25 - 3 Hertz to be in line with Ancoli Isreal's paper from 2003,
  # and online "Motionlogger User’s Guide Version 2K1.1" from Ambulatory Monitoring,
  # Inc. Ardsley, New York 10502
  # Be aware that specific boundaries differs between Actigraph brands that copied the
  # Sadeh algorithm 
  # We use a second order filter because if it was an analog filter it was 
  # most likely not very steep filter.
  data_processed = process_axes(data, filtertype="pass", cut_point=c(0.25, 3), 2, sf)
  zil = c()
  
  # 2) Sadeh reported to have used the y-axis but did not specify the orientation of
  # the y-axis in their accelerometer. Therefore, we keep selection of axis flexible for the user
  # if (do.zcx == TRUE) zil = 1
  # if (do.zcy == TRUE) zil = c(zil, 2)
  # if (do.zcz == TRUE) zil = c(zil, 3)
  zil=1:3
  Ndat = nrow(data_processed)
  for (zi in zil) {
    # 3) apply stop-band to minic old sensitivity
    # 0.01g threshold based on book by Tyron "Activity Measurementy in Psychology And Medicine"
    smallvalues = which(abs(data_processed[,zi]) < 0.01)
    if (length(smallvalues) > 0) {
      data_processed[smallvalues, zi] = 0
    }
    rm(smallvalues)
    # output binary time series zeros and ones with 1 for zero-crossing
    data_processed[,zi] = ifelse(test = data_processed[,zi] >= 0,yes = 1, no = -1)
    # 4) detect zero-crossings
    # The exact algorithm for the original monitor not found, maybe it happened analog
    # we will use: http://rug.mnhn.fr/seewave/HTML/MAN/zcr.html
    zerocross = function(x, Ndat) {
      tmp = abs(sign(x[2:Ndat]) - sign(x[1:(Ndat-1)])) * 0.5
      tmp = c(tmp[1], tmp) # add value to ensure length aligns
      return(tmp)
    }
      allmetrics$ZCX = sumperepochsize(zerocross(data_processed[,zi], Ndat), sf, epochsize)
      allmetrics$ZCY = sumperepochsize(zerocross(data_processed[,zi], Ndat), sf, epochsize)

      allmetrics$ZCZ = sumperepochsize(zerocross(data_processed[,zi], Ndat), sf, epochsize)

  }
  # Note that this is per epoch, in GGIR part 3 we aggregate (sum) this per minute
  # to follow Sadeh. In Sadeh 1987 this resulted in values up to 280
  # 280 = 60 x 2 x frequency of movement which would mean near 2.33 Hertz average
  # movement frequencies, which may have reflected walking
  
  
  #================================================
  # High-pass filtering related metrics
  data_processed = abs(process_axes(data, filtertype="high", cut_point=c(lb), n, sf))
  allmetrics$HFEN = averageperepochsize(x=EuclideanNorm(data_processed),sf,epochsize)
  
  #================================================
  # Combined low-pass and high-pass filtering metric:
  
  # Note that we are using intentionally the lower boundary for the low pass filter
  data_processed = process_axes(data, filtertype="low", cut_point=lb, n, sf)
  GCP = EuclideanNorm(data_processed) - gravity
  # Note that we are using intentionally the lower boundary for the high pass filter
  data_processed = process_axes(data, filtertype="high", cut_point=lb, n, sf)
  HFENplus = EuclideanNorm(data_processed) + GCP
  HFENplus[which(HFENplus < 0)] = 0
  allmetrics$HFENplus = averageperepochsize(x=HFENplus,sf,epochsize)
  return(allmetrics)
}