library(magrittr)

accuracies <- qs::qread("local_data/accuracies.qs") %>%
  as.data.frame()
features <- qs::qread("local_data/features.qs")

tbl <- accuracies %>%
  dplyr::select(-h) %>%
  dplyr::group_by(key, .model) %>%
  dplyr::summarise(dplyr::across(-tidyselect::any_of("h"), .fns = mean)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(h = -1) %>%
  dplyr::filter( !is.nan(RMSSE) ) %>%
  dplyr::group_by(key)

full_tbl <- tbl %>%
  dplyr::mutate( weight = 1/RMSSE ) %>%
  dplyr::mutate(weight = weight/sum(weight)) %>%
  dplyr::ungroup() %>%
  dplyr::select(key, weight, .model) %>%
  tidyr::pivot_wider(names_from = ".model", values_from = "weight") %>%
  dplyr::mutate(dplyr::across( .fns = function(x) { replace(x, is.na(x), 0)  })) %>%
  dplyr::rename_with( .fn = ~paste0("target_",.x) ) %>%
  dplyr::rename(key = target_key)



top_5 <- tbl %>%
  dplyr::mutate( weight = 1/RMSSE ) %>%
  dplyr::mutate( rank = dplyr::min_rank(weight) ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate( weight = ifelse( rank <= 5, weight, 0 ) ) %>%
  dplyr::select(key, weight, .model) %>%
  dplyr::group_by(key) %>%
  dplyr::mutate( weight = weight/sum(weight) ) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(names_from = ".model", values_from = "weight") %>%
  dplyr::mutate(dplyr::across( .fns = function(x) { replace(x, is.na(x), 0)  })) %>%
  dplyr::rename_with( .fn = ~paste0("target_",.x) ) %>%
  dplyr::rename(key = target_key)

best <- tbl %>%
  dplyr::mutate( weight = 1/RMSSE ) %>%
  dplyr::mutate( rank = dplyr::min_rank(weight) ) %>%
  dplyr::ungroup() %>%
  dplyr::filter( rank == 1 ) %>%
  dplyr::select( key, .model)

qs::qsave( dplyr::full_join( full_tbl, features, by = c("key") ),
           "oracle_weighed.qs")

qs::qsave( dplyr::full_join( top_5,
                             features, by = c("key") ),
           "oracle_weighed_top_5.qs")

qs::qsave(  dplyr::full_join( best, features, by = c("key") ),
            "oracle_best.qs" )

# ignore individual forecast hs for now


