
# load("~/Desktop/soothsayer/data/M3.rda")
# load("~/Desktop/soothsayer/data/M4.rda")

load("~/Desktop/soothsayer/data/M1.rda")

# get_freq <- function( type ) {
#   list( "YEARLY" = 1,
#         "QUARTERLY" = 4,
#         "MONTHLY" = 12,
#         "WEEKLY" = 365.25 / 7,
#         "DAILY" = 365,
#         "BIANNUALLY" = 2)[[type]]
# }

M1_selection <- purrr::map( M1, ~ .x[c("period","x","xx")] ) %>%
  purrr::map(  ~ data.frame( period = .x["period"], value = unlist(.x[c("x","xx")])) )

M1 <- purrr::map( names(M1_selection), ~ cbind( M1_selection[[.x]], key = .x)  ) %>%
  purrr::map( ~ cbind( index = 1:nrow(.x), .x ) ) %>%
  dplyr::bind_rows() %>%
  tsibble::as_tsibble( key = "key", index = "index")

load("~/Desktop/soothsayer/data/M3.rda")
M3_selection <- purrr::map( M3, ~ .x[c("period","x","xx")] ) %>%
  purrr::map(  ~ data.frame( period = .x["period"], value = unlist(.x[c("x","xx")])) )

M3 <- purrr::map( names(M3_selection), ~ cbind( M3_selection[[.x]], key = .x)  ) %>%
  purrr::map( ~ cbind( index = 1:nrow(.x), .x ) ) %>%
  dplyr::bind_rows() %>%
  tsibble::as_tsibble( key = "key", index = "index")

load("~/Desktop/soothsayer/data/M4.rda")
M4_selection <- purrr::map( M4, ~ .x[c("period","past","future")] ) %>%
  purrr::map(  ~ data.frame( period = .x["period"], value = unlist(.x[c("past","future")])) )
names(M4_selection) <- paste0("M4_", seq_len(length(M4_selection)))

M4 <- purrr::map( names(M4_selection), ~ cbind( M4_selection[[.x]], key = .x)  ) %>%
  purrr::map( ~ cbind( index = 1:nrow(.x), .x ) ) %>%
  dplyr::bind_rows() %>%
  tsibble::as_tsibble( key = "key", index = "index")

M_all <- dplyr::bind_rows(M1, M3, M4)
# saveRDS(M_all, "M_all.rda")
#

