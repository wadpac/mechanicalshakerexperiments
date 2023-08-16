# alternatingly old and new name, adjusted to the wording of the article

foldernames = c("Actigraph_timer_check","Actigraph_timer_check",
          "Actigraph_ms_hfcr", "Actigraph_ms_hrcr",
          "Actigraph_ms_lfcr", "Actigraph_ms_lrcr",
          "Actigraph_ms_mfcr", "Actigraph_ms_mrcr",
          "Actigraph_ms_bag", "Actigraph_ms_bag",
          "Activpal_timer_check", "Activpal_timer_check",
          "Activpal_ms_cr", "Activpal_ms_cr",
          "Activpal_ms_bag",  "Activpal_ms_bag",
          "Acttrust_Condor", "Acttrust_Condor",
          "Axivity_timer_check", "Axivity_timer_check",
          "Axivity_ms_hfcr", "Axivity_ms_hrcr",
          "Axivity_ms_lfcr", "Axivity_ms_lrcr",
          "Axivity_ms_mfcr", "Axivity_ms_mrcr",
          "Axivity_ms_hfmr", "Axivity_ms_hrmr",
          "Axivity_ms_lfmr", "Axivity_ms_lrmr",
          "Axivity_ms_bag", "Axivity_ms_bag",
          "Fitbit", "Fitbit",
          "GENEActiv_timer_check", "GENEActiv_timer_check",
          "GENEActiv_ms_hfcr", "GENEActiv_ms_hrcr",
          "GENEActiv_ms_lfcr", "GENEActiv_ms_lrcr",
          "GENEActiv_ms_mfcr", "GENEActiv_ms_mrcr",
          "GENEActiv_ms_bag", "GENEActiv_ms_bag",
          "MOX_exportedCSV/exportedCSV/MOX_timer_check", "MOX_exportedCSV/exportedCSV/MOX_timer_check",
          "MOX_exportedCSV/exportedCSV/MOX_ms_hfcr", "MOX_exportedCSV/exportedCSV/MOX_ms_hrcr",
          "MOX_exportedCSV/exportedCSV/MOX_ms_lfcr", "MOX_exportedCSV/exportedCSV/MOX_ms_lrcr",
          "MOX_exportedCSV/exportedCSV/MOX_mfcr", "MOX_exportedCSV/exportedCSV/MOX_mrcr",
          "MOX_exportedCSV/exportedCSV/MOX_ms_bag", "MOX_exportedCSV/exportedCSV/MOX_ms_bag",
          "Shimmer_timer_check", "Shimmer_timer_check",
          "Shimmer_ms_hfcr", "Shimmer_ms_hrcr",
          "Shimmer_ms_lfcr", "Shimmer_ms_lrcr",
          "Shimmer_ms_mfcrA", "Shimmer_ms_mrcrA",
          "Shimmer_ms_mfcrB", "Shimmer_ms_mrcrB",
          "Shimmer_ms_bag", "Shimmer_ms_bag")

path = "/Users/annelindelettink/Documents/Work MacBook Pro Annelinde/Mechanical Shaker Machine/unstructured_raw_data/"
#path = "/home/vincent/data/VUMC/shaker_experiments/unstructured_raw_data/"
foldernames = paste0(path, foldernames)
N = length(foldernames)
file.rename(from = foldernames[seq(1, N, by = 2)], to = foldernames[seq(2, N, by = 2)])
