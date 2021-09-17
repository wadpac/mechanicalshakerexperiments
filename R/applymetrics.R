applymetrics = function(data,n=4,sf, lb=0.2, hb=15, epochsize=5){
  # This is an adaptation of the g.applymetrics function in GGIR
  allmetrics = c()
  averageperepochsize = function(x,sf,epochsize) {
    x2 =cumsum(c(0,x))
    select = seq(1,length(x2),by=sf*epochsize)
    x3 = diff(x2[round(select)]) / abs(diff(round(select)))
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
  data_processed = process_axes(data, filtertype="pass", cut_point=c(lb,hb), n, sf)
  allmetrics$BFEN = averageperepochsize(x=EuclideanNorm(data_processed),sf,epochsize)

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