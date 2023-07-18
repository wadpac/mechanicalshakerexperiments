# alternatingly old and new name

foldernames = c("Actigraph_pro1", "ActiGraph_timer_check",
          "Actigraph_pro2_ses1", "ActiGraph_ms_hfcr",
          "Actigraph_pro2_ses2", "ActiGraph_ms_lfcr",
          "Actigraph_pro2_ses3", "ActiGraph_ms_mfcr",
          "Actigraph_pro3", "ActiGraph_ms_bag",
          "Activpal_pro1", "activPAL_timer_check",
          "Activpal_pro2", "activPAL_ms_cr",
          "Activpal_pro3", "activPAL_ms_bag",
          "Acttrust_Condor", "Acttrust_Condor",
          "Axivity_pro1", "Axivity_timer_check",
          "Axivity_pro2_ses1", "Axivity_ms_hfcr",
          "Axivity_pro2_ses2", "Axivity_ms_lfcr",
          "Axivity_pro2_ses3", "Axivity_ms_mfcr",
          "Axivity_pro3_ses1", "Axivity_ms_hfmr",
          "Axivity_pro3_ses2", "Axivity_ms_lfmr",
          "Axivity_pro3_ses3", "Axivity_ms_bag",
          "Fitbit", "Fitbit",
          "GENEActiv_pro1", "GENEActiv_timer_check",
          "GENEActiv_pro2_ses1", "GENEActiv_ms_hfcr",
          "GENEActiv_pro2_ses2", "GENEActiv_ms_lfcr",
          "GENEActiv_pro2_ses3", "GENEActiv_ms_mfcr",
          "GENEActiv_pro3", "GENEActiv_ms_bag",
          "MOX_exportedCSV/exportedCSV/MOX_pro1", "MOX_exportedCSV/exportedCSV/MOX_timer_check",
          "MOX_exportedCSV/exportedCSV/MOX_pro2_ses1", "MOX_exportedCSV/exportedCSV/MOX_ms_hfcr",
          "MOX_exportedCSV/exportedCSV/MOX_pro2_ses2", "MOX_exportedCSV/exportedCSV/MOX_ms_lfcr",
          "MOX_exportedCSV/exportedCSV/MOX_pro2_ses3", "MOX_exportedCSV/exportedCSV/MOX_mfcr",
          "MOX_exportedCSV/exportedCSV/MOX_pro3", "MOX_exportedCSV/exportedCSV/MOX_ms_bag",
          "Shimmer_pro1", "Shimmer_timer_check",
          "Shimmer_pro2_ses1", "Shimmer_ms_hfcr",
          "Shimmer_pro2_ses2", "Shimmer_ms_lfcr",
          "Shimmer_pro2_ses3A", "Shimmer_ms_mfcrA",
          "Shimmer_pro2_ses3B", "Shimmer_ms_mfcrB",
          "Shimmer_pro3", "Shimmer_ms_bag")

path = "/home/vincent/data/VUMC/shaker_experiments/unstructured_raw_data/"
foldernames = paste0(path, foldernames)
N = length(foldernames)
file.rename(from = foldernames[seq(1, N, by = 2)], to = foldernames[seq(2, N, by = 2)])
