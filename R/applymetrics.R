#' applymetrics
#'
#' @description 'applymetrics' adaptation of the g.applymetrics function in GGIR
#'
#' @param data Three column acceleration time series
#' @param n Order of frequency filter to be applied
#' @param sr Sampling rate (Hertz)
#' @param lb Lower boundary (Hertz) of frequency filter
#' @param hb Higher boundary (Hertz) of frequency filter
#' @param epochsize epochsize to be used
#' @return Data.frame with acceleration metrics values at epoch level
#' @export
#' 
applymetrics = function(data,n=4,sr, lb=0.2, hb=15, epochsize=5){
  allmetrics = c()
  averageperepochsize = function(x,sr,epochsize) {
    x2 =cumsum(c(0,x))
    select = seq(1,length(x2),by=sr*epochsize)
    x3 = diff(x2[round(select)]) / abs(diff(round(select)))
  }
   if (sr <= (hb *2)) { #avoid having a higher filter boundary higher than sr/2
    hb = round(sr/2) - 1
  }
  gravity = 1
  #================================================
  # Functions to aid metric extraction
  process_axes = function(data, filtertype, cut_point, n=4, sr=c()) {
    if (length(sr) == 0) warning("sr not found")
    if (filtertype == "pass" | filtertype == "high" | filtertype == "low") {
      hf_lf_filter = function(bound, n, sr, filtertype) {
        return(signal::butter(n,c(bound/(sr/2)),type=filtertype))
      }
      bf_filter = function(lb, hb, n, sr) {
        Wc = matrix(0,2,1)
        Wc[1,1] = lb / (sr/2)
        Wc[2,1] = hb / (sr/2)
        if (sr/2 < hb | sr/2 < hb) {
          warning("\nSampling rate ",sr," too low for calculating this metric.")
        }
        return(signal::butter(n,Wc,type=c("pass")))
      }
      if (filtertype == "pass") {
        coef = bf_filter(cut_point[1], cut_point[2], n, sr)
      } else {
        if (filtertype == "low") {
          bound = hb
        } else {
          bound = lb
        }
        coef = hf_lf_filter(bound, n, sr, filtertype)
      }
      data_processed = data
      for (i in 1:3) {
        data_processed[,i] = signal::filter(coef, data[,i])
      }
    } else if (filtertype == "rollmedian") {
      rollmed = function(x, sr) {
        winsi = round(sr*5)
        if (round(winsi/2) == (winsi/2)) winsi = winsi+1
        xm = zoo::rollmedian(x,k=winsi,na.pad=TRUE)
        xm[which(is.na(xm[1:1000]) ==T)] = xm[which(is.na(xm[1:1000]) ==F)[1]]
        return(xm)
      }
      data_processed = data
      for (i in 1:3) {
        data_processed[,i] = rollmed(data[,i], sr)
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
  data_processed = process_axes(data, filtertype="pass", cut_point=c(lb,hb), n, sr)
  allmetrics$BFEN = averageperepochsize(x=EuclideanNorm(data_processed),sr,epochsize)

  #================================================
  # High-pass filtering related metrics
  data_processed = abs(process_axes(data, filtertype="high", cut_point=c(lb), n, sr))
  allmetrics$HFEN = averageperepochsize(x=EuclideanNorm(data_processed),sr,epochsize)
  
  #================================================
  # Combined low-pass and high-pass filtering metric:
  
  # Note that we are using intentionally the lower boundary for the low pass filter
  data_processed = process_axes(data, filtertype="low", cut_point=lb, n, sr)
  GCP = EuclideanNorm(data_processed) - gravity
  # Note that we are using intentionally the lower boundary for the high pass filter
  data_processed = process_axes(data, filtertype="high", cut_point=lb, n, sr)
  HFENplus = EuclideanNorm(data_processed) + GCP
  HFENplus[which(HFENplus < 0)] = 0
  allmetrics$HFENplus = averageperepochsize(x=HFENplus,sr,epochsize)
  return(allmetrics)
}