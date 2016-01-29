# Table 1

  table.1.data <- select(data,
                         female, age,
                         bmi, height,
                         smoke, alcohol,
                         cirsg_total, no._of_meds,
                         status,
                         sppb1_total,
                         score,
                         fes_total,
                         eq5d5l1_scale, X6cit_1,
                         avg.steps, log.avg.steps,
                         los, log.los)

  colnames(table.1.data) <- c("Female",
                              "Age (years)",
                              "Body mass index (kg/m^2)",
                              "Height (cm)",
                              "Smoke",
                              "Alcohol",
                              "CIRSG",
                              "Number of medications",
                              "Marital status",
                              "SPPB at baseline",
                              "SHARE FI score",
                              "FES score",
                              "Self rated health (EQ5D)",
                              "6CIT Score",
                              "Average daily step count",
                              "log(average daily step count",
                              "Length of stay (days)",
                              "log(length of stay)")

  name.1 <- function(x, ...) {

    var.names <- list()

    for (i in seq_along(x)) {

      if(is.numeric(x[[i]])){
        var.names[[i]] <- names(x[i])
      }

      if(is.factor(x[[i]])){

        if(length(levels(x[[i]])) == 2) {
          var.names[[i]] <- c(names(x[i]))
        }

        if(length(levels(x[[i]])) > 2) {
          var.names[[i]] <- c(names(x[i]), levels(x[[i]]))
        }
      }
    }
    unlist(var.names)
  }


  summary.1 <- function(x, ...) {

    summary.list <- list()

    for (i in seq_along(x)) {

      if(is.numeric(x[[i]])){
        summary.list[[i]] <- paste0(round(mean(x[[i]], na.rm = TRUE), 1),
                                    " \u00B1 ",
                                    round(sd(x[[i]],   na.rm = TRUE), 1))
      }

      if(is.factor(x[[i]])){

        if(length(levels(x[[i]])) == 2) {
          summary.list[[i]] <- paste0(table(x[[i]])[2],
                                      " (",
                                      round(table(x[[i]])[2] /
                                            sum(table(x[[i]])), 2) * 100,
                                      "%)")
        }

        if(length(levels(x[[i]])) > 2) {
          summary.list[[i]] <- c("", paste0(table(x[[i]]),
                                            " (",
                                            round(table(x[[i]]) /
                                                  sum(table(x[[i]])), 1) * 100,
                                            "%)"))
        }
      }
    }
    unlist(summary.list)
  }


  n.miss <- function(x, ...) {

    miss.list <- list()

    for (i in seq_along(x)) {

      if(is.numeric(x[[i]])){
        miss.list[[i]] <- length(x[[i]][!is.na(x[[i]])])
      }

      if(is.factor(x[[i]])){

        if(length(levels(x[[i]])) == 2) {
          miss.list[[i]] <- length(x[[i]][!is.na(x[[i]])])
        }

        if(length(levels(x[[i]])) > 2) {
          miss.list[[i]] <- c(length(x[[i]][!is.na(x[[i]])]),
                              rep("", length(levels(x[[i]]))))
        }
      }
    }
    unlist(miss.list)
  }



  min.max <- function(x, ...) {

    min.max.list <- list()

    for (i in seq_along(x)) {

      if(is.numeric(x[[i]])){
        min.max.list[[i]] <- paste0("(",
                                    round(min(x[[i]], na.rm = TRUE), 1),
                                    ", ",
                                    round(max(x[[i]], na.rm = TRUE), 1),
                                    ")")
      }

      if(is.factor(x[[i]])){

        if(length(levels(x[[i]])) == 2) {
          min.max.list[[i]] <- ""
        }

        if(length(levels(x[[i]])) > 2) {
          min.max.list[[i]] <- c("", rep("", length(levels(x[[i]]))))
        }
      }
    }
    unlist(min.max.list)
  }


  tiles <- function(x, ...) {

    quantiles.list <- list()

    for (i in seq_along(x)) {

      if(is.numeric(x[[i]])){
        quantiles.list[[i]] <- paste0(round(quantile(x[[i]], probs = c(0.25),
                                                     na.rm = TRUE), 1),
                                      ", ",
                                      round(quantile(x[[i]], probs = c(0.50),
                                                     na.rm = TRUE), 1),
                                      ", ",
                                      round(quantile(x[[i]], probs = c(0.75),
                                                     na.rm = TRUE), 1))
      }

      if(is.factor(x[[i]])){

        if(length(levels(x[[i]])) == 2) {
          quantiles.list[[i]] <- ""
        }

        if(length(levels(x[[i]])) > 2) {
          quantiles.list[[i]] <- c("", rep("", length(levels(x[[i]]))))
        }
      }
    }
    unlist(quantiles.list)
  }




  data_frame(Variable = name.1(x),
             Obs   = n.miss(x),
             col2 = summary.1(x),
             "(Min, Max)" = min.max(x),
             "25th, 50th, 75th quantiles" = tiles(x)) %>%


  stargazer(type = "html",
            summary = FALSE,
            out = "table1.htm",
            digits = 1, rownames = FALSE)

