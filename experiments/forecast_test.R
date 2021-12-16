
M_all_part <- qs::qread(paste0("nonstandard_data/M_all_1.qs"))

ts_train_test(M_all_part) -> train_test
