t_table <- function(data, dvs, iv,
                    var_equal = TRUE,
                    p_adj = "none",
                    alpha = 0.05,
                    paired = FALSE,
                    wilcoxon = FALSE) {
  if (!inherits(data, "data.frame")) {
    stop("data must be a data.frame")
  }
  if (!all(c(dvs, iv) %in% names(data))) {
    stop("at least one column given in dvs and iv are not in the data")
  }
  if (!all(sapply(data[, dvs], is.numeric))) {
    stop("all dvs must be numeric")
  }
  if (length(unique(na.omit(data[[iv]]))) != 2) {
    stop("independent variable must only have two unique values")
  }
  out <- lapply(dvs, function(x) {
    if (paired == FALSE & wilcoxon == FALSE) {
      tres <- t.test(data[[x]] ~ data[[iv]], var.equal = var_equal)
    }
    else if (paired == FALSE & wilcoxon == TRUE) {
      tres <- wilcox.test(data[[x]] ~ data[[iv]])
    }
    else if (paired == TRUE & wilcoxon == FALSE) {
      tres <- t.test(data[[x]] ~ data[[iv]],
                     var.equal = var_equal,
                     paired = TRUE
      )
    }
    else {
      tres <- wilcox.test(data[[x]] ~ data[[iv]],
                          paired = TRUE
      )
    }
    c(
      p_value = tres$p.value
    )
  })
  out <- as.data.frame(do.call(rbind, out))
  out <- cbind(variable = dvs, out)
  names(out) <- gsub("[^0-9A-Za-z_]", "", names(out))
  out$p_value <- ifelse(out$p_value < 0.001,
                        "<0.001",
                        round(p.adjust(out$p_value, p_adj), 3)
  )
  out$conclusion <- ifelse(out$p_value < alpha,
                           paste0("Reject H0 at ", alpha * 100, "%"),
                           paste0("Do not reject H0 at ", alpha * 100, "%")
  )
  return(out)
}