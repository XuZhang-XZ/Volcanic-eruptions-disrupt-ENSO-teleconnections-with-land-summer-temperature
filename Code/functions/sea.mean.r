
sea.mean <- function(x, event, nbefore = 6, nafter = 4, event_range = TRUE,n_iter = 1000) {
  
  if (is_fhx(event)) {
    if (length(unique(event$series)) > 1) {
      stop("event must have a single series")
    } else {
      event <- get_event_years(event)[[1]]
    }
  }
  
  # set up
  rnames <- as.numeric(rownames(x))
  if (all(as.character(seq(length(rnames))) == rnames)) {
    warning(
      "`x` arg for `sea()` could be missing rownames ",
      "- be sure that time series years are rownames"
    )
  }
  event.cut <- rnames[rnames %in% event]
  if (length(event.cut) <= 0) {
    stop("`x` and `event` have no shared years")
  }
  period <- range(event.cut)
  rnames.cut <- seq(period[1], period[2])
  n <- length(event.cut)
  if (length(event.cut) != length(event)) {
    warning(
      "One or more event years is outside the range of the climate series. ",
      "Using ", n, " event years: ", period[1], " to ", period[2], ".",
      call. = FALSE
    )
  }
  m <- nbefore + nafter + 1
  yrs.base <- -nbefore:nafter
  out_table <- data.frame(matrix(NA_real_,
                                 nrow = m, ncol = 16,
                                 dimnames = list(1:m, c(
                                   "lag", "mean",
                                   "n", "St_dev", "lower_95",
                                   "upper_95", "lower_99", "upper_99",
                                   "lower_99.9", "upper_99.9",
                                   "lower_95_perc",
                                   "upper_95_perc", "lower_99_perc", "upper_99_perc",
                                   "min", "max"
                                 ))
  ))
  out_table[, 1] <- yrs.base
  
  # event-event matrix
  # bb = 1902
  yrs.before <- -nbefore:(-1)
  event.table <-
    matrix(unlist(lapply(event.cut, function(bb) {
      x[rnames %in% (bb + yrs.base), ] - mean(x[rnames %in% (bb + yrs.before), ])
    })),
    nrow = n,
    ncol = m,
    byrow = TRUE)
  ## Remove mean values
  # event.table = t(apply(event.table,1,function(x) {x - mean(x[1:nbefore])}))
  
  ## Properties
  actual_event_table <- out_table[, -c(11:14)]
  actual_event_table[, 2] <- colMeans(event.table, na.rm = TRUE)
  actual_event_table[, 3] <- apply(event.table, 2, function(x) sum(!is.na(x)))
  actual_event_table[, 4] <- apply(event.table, 2, stats::sd, na.rm = TRUE)
  actual_event_table[, 5] <- apply(
    event.table, 2,
    function(x) mean(x) - 1.960 * stats::sd(x, na.rm = TRUE)
  )
  actual_event_table[, 6] <- apply(event.table, 2,
                                   function(x) mean(x) + 1.960 * stats::sd(x, na.rm = TRUE)
  )
  actual_event_table[, 7] <- apply(event.table, 2,
                                   function(x) mean(x) - 2.575 * stats::sd(x, na.rm = TRUE)
  )
  actual_event_table[, 8] <- apply(event.table, 2,
                                   function(x) mean(x) + 2.575 * stats::sd(x, na.rm = TRUE)
  )
  actual_event_table[, 9] <- apply(event.table, 2,
                                   function(x) mean(x) - 3.294 * stats::sd(x, na.rm = TRUE)
  )
  actual_event_table[, 10] <- apply(event.table, 2,
                                    function(x) mean(x) + 3.294 * stats::sd(x, na.rm = TRUE)
  )
  actual_event_table[, 11] <- apply(event.table, 2, min, na.rm = TRUE)
  actual_event_table[, 12] <- apply(event.table, 2, max, na.rm = TRUE)
  actual_event_table <- round(actual_event_table, 3)
  
  # random event matrix
  if (event_range == TRUE) {
    rand_yrs <- rnames.cut
  } else {
    rand_yrs <- rnames
  }
  
  rand_pick <- matrix(sample(rand_yrs, n * n_iter, replace = TRUE),
                      ncol = n_iter, nrow = n, byrow = FALSE
  )
  
  rand_list <- lapply(seq_len(ncol(rand_pick)), function(aa) {
    matrix(
      unlist(lapply(
        rand_pick[, aa], function(bb) x[rnames %in% (bb + yrs.base), ]-mean(x[rnames %in% (bb + yrs.before), ])
      )),
      nrow = n, ncol = m, byrow = TRUE
    )
  })
  
  ## Remove Mean values
  # for(j in 1:length(rand_list)){
  #   rand_list[j] = t(apply(as.matrix(rand_list[j]),1,function(x) {x - mean(x[1:nbefore])}))
  # }
  
  ## Mean Values
  re.table <- t(sapply(rand_list, function(x) colMeans(x)))
  rand_event_table <- out_table
  rand_event_table[, 2] <- colMeans(re.table, na.rm = TRUE)
  rand_event_table[, 3] <- apply(re.table, 2, function(x) sum(!is.na(x)))
  rand_event_table[, 4] <- apply(re.table, 2,
                                 function(x) stats::sd(x, na.rm = TRUE)
  )
  rand_event_table[, 5] <- apply(re.table, 2,
                                 function(x) mean(x) - 1.960 * stats::sd(x, na.rm = TRUE)
  )
  rand_event_table[, 6] <- apply(re.table, 2,
                                 function(x) mean(x) + 1.960 * stats::sd(x, na.rm = TRUE)
  )
  rand_event_table[, 7] <- apply(re.table, 2,
                                 function(x) mean(x) - 2.575 * stats::sd(x, na.rm = TRUE)
  )
  rand_event_table[, 8] <- apply(re.table, 2,
                                 function(x) mean(x) + 2.575 * stats::sd(x, na.rm = TRUE)
  )
  rand_event_table[, 9] <- apply(re.table, 2,
                                 function(x) mean(x) - 3.294 * stats::sd(x, na.rm = TRUE)
  )
  rand_event_table[, 10] <- apply(re.table, 2,
                                  function(x) mean(x) + 3.294 * stats::sd(x, na.rm = TRUE)
  )
  rand_event_table[, 11] <- apply(re.table, 2,
                                  function(x) stats::quantile(x, .025, na.rm = TRUE)
  )
  rand_event_table[, 12] <- apply(re.table, 2,
                                  function(x) stats::quantile(x, .975, na.rm = TRUE)
  )
  rand_event_table[, 13] <- apply(re.table, 2,
                                  function(x) stats::quantile(x, .005, na.rm = TRUE)
  )
  rand_event_table[, 14] <- apply(re.table, 2,
                                  function(x) stats::quantile(x, .995, na.rm = TRUE)
  )
  rand_event_table[, 15] <- apply(re.table, 2, min, na.rm = TRUE)
  rand_event_table[, 16] <- apply(re.table, 2, max, na.rm = TRUE)
  rand_event_table <- round(rand_event_table, 3)
  
  # Departure table
  departure_table <- out_table[, c(1,2,3,11:14)]
  departure_table[, 2:3] <- actual_event_table[, 2:3]
  departure_table[, 4:7] <- rand_event_table[, 11:14]
  
  # departure_table[, 2] <- actual_event_table[, 2] - rand_event_table[, 2]
  # departure_table[, 3] <- apply(re.table, 2,
  #                               function(x) -1 * 1.960 * stats::sd(x, na.rm = TRUE)
  # )
  # departure_table[, 4] <- apply(re.table, 2,
  #                               function(x) 1.960 * stats::sd(x, na.rm = TRUE)
  # )
  # departure_table[, 5] <- apply(re.table, 2,
  #                               function(x) -1 * 2.575 * stats::sd(x, na.rm = TRUE)
  # )
  # departure_table[, 6] <- apply(re.table, 2,
  #                               function(x) 2.575 * stats::sd(x, na.rm = TRUE)
  # )
  # departure_table[, 7] <- apply(re.table, 2,
  #                               function(x) -1 * 3.294 * stats::sd(x, na.rm = TRUE)
  # )
  # departure_table[, 8] <- apply(re.table, 2,
  #                               function(x) 3.294 * stats::sd(x, na.rm = TRUE)
  # )
  # temp <- apply(re.table, 2, function(x) stats::median(x)) # Simulated medians
  # departure_table[, 9] <- rand_event_table[, 11] - temp
  # departure_table[, 10] <- rand_event_table[, 12] - temp
  # departure_table[, 11] <- rand_event_table[, 13] - temp
  # departure_table[, 12] <- rand_event_table[, 14] - temp
  # rm(temp)
  departure_table <- round(departure_table, 3)
  
  out <- list(
    "event_years" = event,
    "actual" = actual_event_table,
    "random" = rand_event_table,
    "departure" = departure_table,
    "simulated" = re.table,
    "observed" = event.table
  )
  class(out) <- c("sea")
  
  out
}
