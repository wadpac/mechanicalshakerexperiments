rm(list = ls())
graphics.off()
load("/media/vincent/DATA/VUMC/shaker_experiments/structured_raw_data/Activpal_ms_lfcr.RData")


for (i in 1:length(extracteddata$data)) {
  D = extracteddata$data[[i]]
  S = extracteddata$specifications[[i]]
  if (!is.null(D)) {
    x11()
    plot(D$time, D$x, type ="l", col = "black", main = S, ylim = c(-8, 8))
    lines(D$time, D$y, type ="l", col = "blue")
    lines(D$time, D$z, col = "red")
  }
  # kkkk
}

fn = "/media/vincent/DATA/VUMC/shaker_experiments/unstructured_raw_data/Activpal_ms_cr/8-AP473254 408a 24Nov20 8-45am for 6h-VANE-PB08101256-AccelDataUncompressed.csv"
M = read.csv(file = fn, sep = ";", skip = 1)
x11()
plot(M$X, type = "l", ylim = c(50, 200))
lines(M$Y, type ="l", col = "blue")
lines(M$Z, col = "red")