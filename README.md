### Order to run scripts:

1. main.R => load raw data, adds labels, and cuts out rundandent data at the beginning and the end, and stores the data in lists.
2. derive_calibration_coefficients.R => runs auto-calibration routine

Next, we aim to provide separate scripts to:
- Extract and compare acceleration metrics
- Compare raw data characteristic
- Evaluate time keeping