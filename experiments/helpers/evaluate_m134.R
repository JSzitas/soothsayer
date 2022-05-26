remove(list = ls())
pkgload::load_all()

fcsts1 <- qs::qread( "local_data/fcsts1.qs" )
fcsts2 <- qs::qread( "local_data/fcsts2.qs" )

fcsts <- dplyr::bind_rows( tsibble::as_tibble(fcsts1),
                           tsibble::as_tibble(fcsts2)) %>%
  # dplyr::mutate( h = dplyr::row_number() ) %>%
  fabletools::fable( response = "value",
                     distribution = "value",
                     index = "index",
                     key = c("key",".model"))

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


fcsts <- fcsts %>%
  dplyr::group_by(.model, key) %>%
  mutate( h = dplyr::row_number() ) %>%
  dplyr::ungroup()

# acc <- fabletools::accuracy( fcsts, test, by = c( ".model") )
# tictoc::tic()
# acc <- fcsts %>%
#   dplyr::group_by(.model, key) %>%
#   mutate( h = dplyr::row_number() ) %>%
#   dplyr::ungroup() %>%
#   fabletools::accuracy( test, by = c("h",".model") )
# tictoc::toc()
