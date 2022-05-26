
pkgload::load_all()
series <- qs::qread(paste0("nonstandard_data/M_all_1.qs"))
series2 <- qs::qread(paste0("nonstandard_data/M_all_2.qs"))

series <- series %>%
  dplyr::bind_rows(series2) %>%
  dplyr::select( -period ) %>%
  tsibble::as_tsibble( index = "index", key = "key" )

res <- series %>%
  dplyr::group_by( key ) %>%
  dplyr::mutate( train = index < max( index) - 14 + 1) %>%
  dplyr::ungroup()

train <- res %>%
  dplyr::filter( train ) %>%
  dplyr::select(-train) %>%
  dplyr::group_by(key) %>%
  dplyr::mutate( n_obs = length(value) ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(n_obs > 12) %>%
  dplyr::select( -n_obs )
test <- res %>%
  dplyr::filter( !train ) %>%
  dplyr::select(-train)


fcsts <- qs::qread("local_data/fcsts1.qs") #%>%
  # dplyr::group_by(key, .model) %>%
  # dplyr::mutate( h = dplyr::row_number()) %>%
  # dplyr::ungroup()


tictoc::tic()
tst2 <- fast_accuracy(fcsts, test)#, train)
tictoc::toc()
