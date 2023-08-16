[![R-CMD-check-basic](https://github.com/wadpac/mechanicalshakerexperiments/actions/workflows/r_basic_check.yml/badge.svg)](https://github.com/wadpac/mechanicalshakerexperiments/actions/workflows/r_basic_check.yml)
[![codecov](https://codecov.io/gh/wadpac/mechanicalshakerexperiments/branch/main/graph/badge.svg?token=C2X6Z6AJLL)](https://codecov.io/gh/wadpac/mechanicalshakerexperiments)


The code in this repository corresponds to our research project to facilitate processing and analyzing data from a large pool of acceleration sensors that were attached to a mechanical shaker machine. The data itself is available here (doi: 10.5281/zenodo.8160791).

### File reading

Experimental raw acceleration data (unstructured_raw_data.zip) is read and structured per brand (ActiGraph, activPAL, Axivity, GENEActiv, and MOX) and experiment (ms_lrcr, ms_hrcr, ms_lrmr, ms_hrmr, ms_mrcr, box, and timer_check) in a consistent data format to facilitate comparisons by running inst/researchcode/structure_data.R.

Here we used available open-source licensed R packages such as read.gt3x and GGIRread for reading in the data. As we noticed that data from ActiGraph and Axivity devices had irregularly sampled timestamps, acceleration values from these devices were resampled using nearest neighbor timestamp interpolation, where we ran sensitivity analysis to investigate the possible role of interpolation.

### Structuring the data

This resulted in structured data objects (.RData) for the different brands and experiments (structured_raw_data.zip). Each .Rdata file contains the raw data from all devices for that brand per experiment, including the following elements:

●        $data is a list of data.frames, one for each accelerometer file, including columns: the time stamps (time), the triaxial accelerometer data (x, y, z), labels to indicate the shaker frequency condition (shaking_frequency), and the experimental set-up (condition)

●        $specifications is a matrix, including the columns: the device label (label), serial number (serial_number), sampling rate (sampling_rate), and dynamic range (dynamic_range) associated with each accelerometer file.

We applied the previously described auto-calibration method (1) to the no-movement periods during the box experiment to derive calibration correction coefficients (derive_calibration_coefficients.R).

The structured data per experiment can be loaded using the script subset_data.R (subsetted_raw_data.zip). Here we excluded data recorded outside the experiment and double observations during the experiment. Note that we removed data of devices for which IDLE sleep mode was enabled during ms_hrcr. This resulted in structured data for the different experiments. Each .Rdata file contains the selected data from all devices during that experiment:

●        $data is a list of data.frames, one for each accelerometer device, including columns: labels to indicate the shaker frequency condition (shaking_frequency), the time stamps (time), the data from the axis with the highest standard deviation corresponding with the direction of shaking  (SD)

●        $specifications is a matrix, including the columns: the device label (label), serial number (serial_number), the brand (brand), the experiment name (experiment), the sampling rate (sampling_rate), and dynamic range (dynamic_range) associated with each accelerometer device.

### Analyses

Analyses will be described in a scientific paper and range from frequency spectrum analysis to temporal correlation analysis. The scripts used for the data analyses performed include ‘analyses’ in their name.


### References

1. van Hees VT, Fang Z, Langford J, Assah F, Mohammad A, da Silva ICM, et al. Autocalibration of accelerometer data for free-living physical activity assessment using local gravity and temperature: an evaluation on four continents. Journal of Applied Physiology. 2014;117(7):738-44. doi: 10.1152/japplphysiol.00421.2014.