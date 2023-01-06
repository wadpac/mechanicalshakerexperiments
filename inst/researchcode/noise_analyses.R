rm(list=ls())
graphics.off()

# User input
shaker_experiments_folder = "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/Mechanical Shaker Machine" # Update to be your local directory
# shaker_experiments_folder = "~/data/VUMC/shaker_experiments"

# Specify file paths
datafile = paste0(shaker_experiments_folder, "/analyses/no_movement.RData")
noise_output = extracted_data_path = paste0(shaker_experiments_folder, "/analyses/noise")

if (!dir.exists(noise_output)) {
  dir.create(noise_output)
}

#======================================================================================
# Load no movement data
load(datafile)

# Exclude two devices (aP_490 and aP_254) from the low sampling frequency experiment (ms_lfcr)
deviates <- c("aP_490", "aP_254")
index_deviates <- which(names(data$data) %in% deviates & data$specifications$experiment == "ms_lfcr")

specifications <- data$specifications[-index_deviates,]
data <- data$data
data[index_deviates] <- NULL
data <- list(data = data, specifications = specifications)
rm(specifications, deviates, index_deviates)

# Calculate the standard deviation for all three axes per device
sd_x <- c()
sd_y <- c()
sd_z <- c()

for (device in 1:length(data$data)) {
  df <- data$data[[device]]
  sd_x <- c(sd_x, sd(df$x))
  sd_y <- c(sd_y, sd(df$y))
  sd_z <- c(sd_z, sd(df$z))
}

noise_data <- cbind(data$specifications, sd_x, sd_y, sd_z)
noise_data$brand <- as.factor(noise_data$brand)
noise_data$experiment <- as.factor(noise_data$experiment)

rm(device, df, sd_x, sd_y, sd_z)
save(noise_data, file = paste0(noise_output, "/noise.RData"))

# Plot noise for the brands with separate panels for the axes
positions <- c("ms_hfcr", "ms_lfcr", "ms_hfmr", "ms_lfmr")

plot_noise_x <- ggplot2::ggplot(noise_data, ggplot2::aes(x = experiment, y = sd_x)) +
  ggplot2::geom_boxplot(ggplot2::aes(color = brand)) +
  ggplot2::theme_light() + ggplot2::scale_x_discrete(limits = positions)
plot_noise_y <- ggplot2::ggplot(noise_data, ggplot2::aes(x = experiment, y = sd_y)) +
  ggplot2::geom_boxplot(ggplot2::aes(color = brand)) +
  ggplot2::theme_light() + ggplot2::scale_x_discrete(limits = positions)
plot_noise_z <- ggplot2::ggplot(noise_data, ggplot2::aes(x = experiment, y = sd_z)) +
  ggplot2::geom_boxplot(ggplot2::aes(color = brand)) +
  ggplot2::theme_light() + ggplot2::scale_x_discrete(limits = positions)

gridExtra::grid.arrange(plot_noise_x, plot_noise_y, plot_noise_z, nrow=3) #arranges plots within grid

noise_plots <- gridExtra::arrangeGrob(plot_noise_x + ggplot2::theme(legend.position="top"), 
                                      plot_noise_y + ggplot2::theme(legend.position="none"), 
                                      plot_noise_z + ggplot2::theme(legend.position="none"), 
                                      nrow=3) # generates plot
plot(noise_plots) #print the plot

ggplot2::ggsave(file=paste0(noise_output, "/boxplots_noise.pdf"), noise_plots) #saves plot