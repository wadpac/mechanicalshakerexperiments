#---------------------------------------------------------------------
# This script assesses the magnitude of noise of the raw acceleration signals in rest across brands and various configurations: dynamic range and sampling frequency.
# - No-movement segments collected as part of the mechanical shaker experiments (except ms_bag experiment) were used as input
# - Measure of noise: standard deviation of the raw acceleration signal (x-, y-, and z-axis)
# - Aggregated per unique combination of brand, sampling frequency, and dynamic range as the median, mean and 95th percentile
rm(list=ls())
graphics.off()

# User input
shaker_experiments_folder = "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/Mechanical Shaker Machine" # Update to be your local directory
# shaker_experiments_folder = "~/data/VUMC/shaker_experiments"

# Specify file paths
datafile = paste0(shaker_experiments_folder, "/analyses/no_movement.RData") # Can be retrieved by running subset_data.R, setting: analysis <- "noise"

noise_output = extracted_data_path = paste0(shaker_experiments_folder, "/analyses/noise")
if (!dir.exists(noise_output)) {
  dir.create(noise_output)
}

#======================================================================================
# Load no-movement segment data
load(datafile)

# Visual inspection showed no apparent movement for: activPAL devices aP_490 and aP_254 (ms_lfcr), 
# These devices were therefore removed from analyses
deviates_lfcr <- c("aP_490", "aP_254") 

index_lfcr <- which(data$specifications$label %in% deviates_lfcr & data$specifications$experiment == "ms_lfcr")
data$specifications <- data$specifications[-index_lfcr,]
data$data[index_lfcr] <- NULL

# Aggregate per unique combination of brand, dynamic range and sampling frequency
combinations <- unique(data$specifications[c("brand", "dynamic_range", "sampling_frequency")])

# Calculate the standard deviation for all three axes for each device
sd_max_y <- c()
sd_middle_x <- c()
sd_min_z <- c()

for(combination in 1:nrow(combinations)){
  index <- which(data$specifications$brand == combinations$brand[combination] & 
                   data$specifications$dynamic_range == combinations$dynamic_range[combination] &
                   data$specifications$sampling_frequency == combinations$sampling_frequency[combination])
  tmp <- data$specifications[index,]
  sub <- data$data[index]
  
  for (device in 1:length(sub)) {
    df <- sub[[device]]
    
    sds <- c(sd(df$x), sd(df$y), sd(df$z)) # calculate the standard deviation of the axes
    if(which.max(sds) == which.min(sds)){
      sd_middle_x <- c(sd_middle_x, sd(df$x))
      sd_max_y <- c(sd_max_y, sd(df$y))
      sd_min_z <- c(sd_min_z, sd(df$z))
    } else {
      sd_max_y <- c(sd_max_y, sd(df[,which.max(sds) + 1]))
      sd_min_z <- c(sd_min_z, sd(df[,which.min(sds) + 1]))
      index <- c("1", "2", "3")
      axes <- c(which.min(sds), which.max(sds))
      sd_middle_x <- c(sd_middle_x, sd(df[,as.integer(setdiff(index, axes)) + 1]))
    }
  }
}

noise_data <- cbind(data$specifications, sd_middle_x, sd_max_y, sd_min_z)
noise_data$brand <- as.factor(noise_data$brand)
noise_data$sampling_frequency <- as.integer(noise_data$sampling_frequency)
noise_data$dynamic_range <- as.integer(noise_data$dynamic_range)
save(noise_data, file = paste0(noise_output, "/noise_results.RData"))

# Create descriptive table by aggregating
require(dplyr)
results_agg <- noise_data %>%
  group_by(brand, dynamic_range, sampling_frequency) %>%
  summarize(N = n(), mean_x = mean(sd_middle_x), mean_y = mean(sd_max_y), mean_z = mean(sd_min_z), 
            median_x = median(sd_middle_x), median_y = median(sd_max_y), median_z = median(sd_min_z), 
            percentile_x = quantile(sd_middle_x, .95), percentile_y = quantile(sd_max_y, .95), percentile_z = quantile(sd_min_z, .95))
save(results_agg, file = paste0(noise_output, "/noise_aggregated_results.RData"))

# Statistical test differences MANOVA
# -Assumption: n in each cell > the number of outcome variables
noise_data %>%
group_by(brand, dynamic_range, sampling_frequency) %>%
  summarise(N = n())

# -Assumption: univariate or multivariate outliers
noise_data %>%
  group_by(brand, dynamic_range, sampling_frequency) %>%
  rstatix::identify_outliers(sd_middle_x)
noise_data %>%
  group_by(brand, dynamic_range, sampling_frequency) %>%
  rstatix::identify_outliers(sd_max_y)
noise_data %>%
  group_by(brand, dynamic_range, sampling_frequency) %>%
  rstatix::identify_outliers(sd_min_z)

# -Assumption: multivariate normality
ggpubr::ggqqplot(noise_data, "sd_middle_x", facet.by = "brand",
         ylab = "sd_x")
ggpubr::ggqqplot(noise_data, "sd_middle_x", facet.by = "dynamic_range",
                 ylab = "sd_x")
ggpubr::ggqqplot(noise_data, "sd_middle_x", facet.by = "sampling_frequency",
                 ylab = "sd_x")

ggpubr::ggqqplot(noise_data, "sd_max_y", facet.by = "brand",
                 ylab = "sd_y")
ggpubr::ggqqplot(noise_data, "sd_max_y", facet.by = "dynamic_range",
                 ylab = "sd_y")
ggpubr::ggqqplot(noise_data, "sd_max_y", facet.by = "sampling_frequency",
                 ylab = "sd_y")

ggpubr::ggqqplot(noise_data, "sd_min_z", facet.by = "brand",
                 ylab = "sd_z")
ggpubr::ggqqplot(noise_data, "sd_min_z", facet.by = "dynamic_range",
                 ylab = "sd_z")
ggpubr::ggqqplot(noise_data, "sd_min_z", facet.by = "sampling_frequency",
                 ylab = "sd_z")

noise_data %>%
  select(sd_middle_x, sd_max_y, sd_min_z) %>%
  rstatix::mshapiro_test()

# -Assumption: multicollinearity; dependent (outcome) variables cannot be too correlated to each other < 0.90 (Tabachnick & Fidell, 2012)
noise_data %>% rstatix::cor_test(sd_middle_x, sd_max_y, sd_min_z)

# -Assumption: linearity between all outcome variables for each group
# Create a scatterplot matrix by group
library(GGally)
results <- noise_data %>%
  select(sd_middle_x, sd_max_y, sd_min_z, brand) %>%
  group_by(brand) %>%
  rstatix::doo(~ggpairs(.) + theme_bw(), result = "plots")
results$plots

results <- noise_data %>%
  select(sd_middle_x, sd_max_y, sd_min_z, dynamic_range) %>%
  group_by(dynamic_range) %>%
  rstatix::doo(~ggpairs(.) + theme_bw(), result = "plots")
results$plots

results <- noise_data %>%
  select(sd_middle_x, sd_max_y, sd_min_z, sampling_frequency) %>%
  group_by(sampling_frequency) %>%
  rstatix::doo(~ggpairs(.) + theme_bw(), result = "plots")
results$plots

# -Assumption: homogeneity of variances. The Levene’s test can be used to test the equality of variances between groups. Non-significant values of Levene’s test indicate equal variance between groups.
rstatix::box_m(noise_data[, c("sd_middle_x", "sd_max_y", "sd_min_z")], noise_data$brand)
rstatix::box_m(noise_data[, c("sd_middle_x", "sd_max_y", "sd_min_z")], noise_data$dynamic_range)
rstatix::box_m(noise_data[, c("sd_middle_x", "sd_max_y", "sd_min_z")], noise_data$sampling_frequency)
# violated: using Pillai's statistics instead of Willks'

# -Assumption: homogeneity of variance-covariance matrices. The Box’s M Test can be used to check the equality of covariance between the groups. This is the equivalent of a multivariate homogeneity of variance. This test is considered as highly sensitive. Therefore, significance for this test is determined at alpha = 0.001.
noise_data %>% 
  tidyr::gather(key = "variable", value = "value", sd_middle_x, sd_max_y, sd_min_z) %>%
  group_by(variable) %>%
  rstatix::levene_test(value ~ brand)

noise_data %>% 
  tidyr::gather(key = "variable", value = "value", sd_middle_x, sd_max_y, sd_min_z) %>%
  group_by(variable) %>%
  rstatix::levene_test(value ~ as.factor(dynamic_range))

noise_data %>% 
  tidyr::gather(key = "variable", value = "value", sd_middle_x, sd_max_y, sd_min_z) %>%
  group_by(variable) %>%
  rstatix::levene_test(value ~ as.factor(sampling_frequency))

# For brand (sd_x, sd_y, sd_z), dynamic range (sd_x, sd_z) and sampling frequency the Levene’s test is non-significant (p > 0.05), so there was homogeneity of variances.
# For dynamic range (sd_y) the Levene’s test is significant (p < 0.05), so there was no homogeneity of variances.
# Use different post hoc to test for differences between dynamic ranges (sd_y)

#Build MANOVA model
model <- lm(cbind(sd_middle_x, sd_max_y, sd_min_z) ~ brand + dynamic_range + sampling_frequency, data = noise_data)
car::Manova(model, test.statistic = "Pillai")

# Follow-up univariate models
summary.aov(model)
effectsize::eta_squared(model)

#Post-hoc test
posthoc_brand <- noise_data %>%
  tidyr::gather(key = "variables", value = "value", sd_middle_x, sd_max_y, sd_min_z) %>%
  group_by(variables) %>%
  rstatix::tukey_hsd(value ~ brand) %>%
  select(-estimate, -conf.low, -conf.high) # Remove details

require(dplyr)
results_agg_brand <- noise_data %>%
  group_by(brand) %>%
  summarize(N = n(), mean_x = mean(sd_middle_x*1000), mean_y = mean(sd_max_y*1000), mean_z = mean(sd_min_z*1000))

# Visualize results
positions <- c("Actigraph", "Activpal", "Axivity", "GENEActiv", "MOX")
noise_data$sampling_frequency <- as.factor(noise_data$sampling_frequency)

plot_noise_x <- ggplot2::ggplot(noise_data, ggplot2::aes(x = brand, y = sd_middle_x*1000)) +
  ggplot2::geom_boxplot(ggplot2::aes(color = sampling_frequency)) +
  ggplot2::theme_light() + ggplot2::scale_x_discrete(limits = positions) + ylab("x-axis (mg)") +
  scale_fill_discrete(name="sampling frequency (Hz)")

plot_noise_y <- ggplot2::ggplot(noise_data, ggplot2::aes(x = brand, y = sd_max_y*1000)) +
  ggplot2::geom_boxplot(ggplot2::aes(color = sampling_frequency)) +
  ggplot2::theme_light() + ggplot2::scale_x_discrete(limits = positions) + ylab("y-axis (mg)")

plot_noise_z <- ggplot2::ggplot(noise_data, ggplot2::aes(x = brand, y = sd_min_z*1000)) +
  ggplot2::geom_boxplot(ggplot2::aes(color = sampling_frequency)) +
  ggplot2::theme_light() + ggplot2::scale_x_discrete(limits = positions) + ylab("z-axis (mg)")

gridExtra::grid.arrange(plot_noise_x, plot_noise_y, plot_noise_z, nrow=3) #arranges plots within grid

noise_plots <- gridExtra::arrangeGrob(plot_noise_x + ggplot2::theme(legend.position="top"), 
                                      plot_noise_y + ggplot2::theme(legend.position="none"), 
                                      plot_noise_z + ggplot2::theme(legend.position="none"), 
                                      nrow=3) # generates plot
plot(noise_plots) #print the plot
ggplot2::ggsave(file=paste0(noise_output, "/plots/boxplot_noise.png"), noise_plots, width = 10, height = 8, dpi = 900) #saves g

