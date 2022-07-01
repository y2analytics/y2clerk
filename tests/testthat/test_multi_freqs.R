# #### Packages -----------------------------------------------------------------
#
# library(dplyr)
# library(y2clerk)
# library(testthat)
#
# #### Loading some Example Data ------------------------------------------------
#
# FILE_PATH <- "~/Dropbox (Y2 Analytics)/Y2 Analytics Team Folder/Active Projects_KG/Y2 Archive - KLG 2021/Draper Parks Plan 2021/"
# DATA_PATH <- str_c(FILE_PATH, "/Data/")
# CHART_PATH <- str_c(FILE_PATH, "/Charts/")
#
# responses <- read_rds(str_c(DATA_PATH,
#                             "/Final Weighted Geocoded Data.RDS"))
#
# # Testing Basic ---------------------------------------------------------------
#
# freqs1 <- responses %>%
#   select(starts_with("m_nottrails"),
#          trimmed_weights) %>%
#   multi_freqs(wt = trimmed_weights,
#               remove_nas = FALSE)
#
# # Comparison
#
# freqs2 <- responses %>%
#   filter(!(is.na(m_nottrails_1) &
#              is.na(m_nottrails_2) &
#              is.na(m_nottrails_3) &
#              is.na(m_nottrails_4) &
#              is.na(m_nottrails_5) &
#              is.na(m_nottrails_6) &
#              is.na(m_nottrails_7) &
#              is.na(m_nottrails_8) &
#              is.na(m_nottrails_9) &
#              is.na(m_nottrails_10) &
#              is.na(m_nottrails_11) &
#              is.na(m_nottrails_12) &
#              is.na(m_nottrails_13) &
#              is.na(m_nottrails_15))) %>%
#   select(starts_with("m_nottrails"),
#          -ends_with("_TEXT"),
#          trimmed_weights) %>%
#   freqs(wt = trimmed_weights)
#
# identical(freqs1, freqs2) # Success
#
# # Testing Multiple ------------------------------------------------------------
#
# freqs1 <- responses %>%
#   multi_freqs(m_nottrails_1,
#               m_nottrails_2,
#               m_whynot_1,
#               m_solutions_1,
#               m_priority_1,
#               wt = trimmed_weights)
#
# # Comparison
#
# freqs2 <- rbind(
#   responses %>%
#     select(starts_with("m_nottrails"),
#            -ends_with("_TEXT"),
#            trimmed_weights) %>%
#     rowwise() %>%
#     filter(!all(is.na(c_across(starts_with("m_nottrails"))))) %>%
#     ungroup() %>%
#     freqs(wt = trimmed_weights),
#   responses %>%
#     select(starts_with("m_whynot"),
#            -ends_with("_TEXT"),
#            trimmed_weights) %>%
#     rowwise() %>%
#     filter(!all(is.na(c_across(starts_with("m_whynot"))))) %>%
#     ungroup() %>%
#     freqs(wt = trimmed_weights),
#   responses %>%
#     select(starts_with("m_solutions"),
#            -ends_with("_TEXT"),
#            trimmed_weights) %>%
#     rowwise() %>%
#     filter(!all(is.na(c_across(starts_with("m_solutions"))))) %>%
#     ungroup() %>%
#     freqs(wt = trimmed_weights),
#   responses %>%
#     select(starts_with("m_priority"),
#            -ends_with("_TEXT"),
#            trimmed_weights) %>%
#     rowwise() %>%
#     filter(!all(is.na(c_across(starts_with("m_priority"))))) %>%
#     ungroup() %>%
#     freqs(wt = trimmed_weights)
# ) %>%
#   drop_na()
#
# identical(freqs1, freqs2)
#
# # Testing All -----------------------------------------------------------------
#
# freqs_topline <- responses %>%
#   select(starts_with("m_"),
#          trimmed_weights) %>%
#   multi_freqs(wt = trimmed_weights)
#
# # Testing MaxDiff -------------------------------------------------------------
#
# ### THIS IS THE BIG ONE
#
# # multi_freqs() result
#
# responses %>%
#   select(starts_with("md_priority"),
#          trimmed_weights) %>%
#   multi_freqs(wt = trimmed_weights)
#
# # Correct result
# # It worked I checked with the final deliverable
#
# # Testing Matrix Single Select ------------------------------------------------
#
# responses %>%
#   select(starts_with("s_trailmaint"),
#          trimmed_weights) %>%
#   multi_freqs(wt = trimmed_weights,
#               prompt = TRUE) %>%
#   preamble_rm()
#
# # The warning worked as expected
# # Trying on "Q" prefix questions
#
# test_df <- responses %>%
#   rename(QTRAILTYPE_1 = s_trailtype_1,
#          QTRAILTYPE_2 = s_trailtype_2,
#          QTRAILTYPE_3 = s_trailtype_3,
#          QTRAILTYPE_4 = s_trailtype_4) %>%
#   select(starts_with("QTRAILTYPE"),
#          trimmed_weights)
#
# test_df %>%
#   multi_freqs(wt = trimmed_weights,
#               prompt = TRUE) %>%
#   preamble_rm() # success
#
# # Testing Forced Single Select ------------------------------------------------
#
# responses %>%
#   select(starts_with("s_qualify"),
#          trimmed_weights) %>%
#   multi_freqs(wt = trimmed_weights)
#
# # Testing group_by ------------------------------------------------------------
#
# # Test
#
# responses %>%
#   group_by(AgeRange) %>%
#   select(starts_with("m_priority"),
#          trimmed_weights) %>%
#   multi_freqs(wt = trimmed_weights)
#
# ### End -----------------------------------------------------------------------
#
#
