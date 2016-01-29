

  source("data.R")

  analysis <-  select(data,
                      id,
                      female, age,
                      bmi, height,
                      smoke, alcohol,
                      cirsg_total, no._of_meds,
                      status,
                      sppb1_total,
                      sppbf_total, sppb_diff,
                      score,
                      fes_total,
                      eq5d5l1_scale, X6cit_1,
                      avg.steps, log.avg.steps,
                      los, log.los)

  ggplot(analysis, aes(x = avg.steps, y = los)) +
    geom_jitter() +
    geom_smooth(method = "lm", color = "blue", se = FALSE) +
    geom_smooth(color = "red", linetype = "dashed", se = FALSE)

  ggplot(analysis, aes(x = avg.steps, y = log.los)) +
    geom_jitter() +
    geom_smooth(method = "lm", color = "blue", se = FALSE) +
    geom_smooth(color = "red", linetype = "dashed", se = FALSE)

  ggplot(analysis, aes(x = log.avg.steps, y = log.los)) +
    geom_jitter() +
    geom_smooth(method = "lm", color = "blue", se = FALSE) +
    geom_smooth(color = "red", linetype = "dashed", se = FALSE)



  ggplot(analysis, aes(x = log.avg.steps, y = log.los)) +
    geom_jitter(size = 4, color = "black", alpha = 0.5) +
    geom_smooth(method = "lm", color = "red", size = 0.7, linetype = "dashed",
                se = FALSE) +
    geom_smooth(color = "blue", size = 0.7, linetype = "dashed",
                se = FALSE) +
    ylab("log(length of stay in days)") +
    xlab("log(average daily step count)") +
    theme_bw() +
    theme(axis.text = element_text(color = "black"),
          text      = element_text(color = "black", size = 16),
          panel.border = element_blank())

  ggsave("raw.reg.png", height = 6, width = 1.61 * 6)

  ggplot(analysis, aes(x = avg.steps, y = los)) +
    geom_jitter(size = 4, color = "black", alpha = 0.5) +
    geom_line(data = data.frame(x = exp(log.log.lr$model$log.avg.steps),
                                y = exp(predict(log.log.lr))),
              aes(x = x, y = y),
              color = "red", size = 0.7, linetype = "dashed") +
    geom_smooth(color = "blue", size = 0.7, linetype = "dashed",
                se = FALSE) +
    xlab("Average daily step count") +
    ylab("Length of stay in days") +
    theme_bw() +
    theme(axis.text = element_text(color = "black"),
          text      = element_text(color = "black", size = 16),
          panel.border = element_blank())

  ggsave("raw.reg.exp.png", height = 6, width = 1.61 * 6)


  log.log.lr <- lm(log.los ~ log.avg.steps, analysis)

  log.log.mlr <- lm(log.los ~ log.avg.steps +
                              female +
                              age +
                              bmi +
                              height+
                              smoke +
                              alcohol +
                              cirsg_total +
                              no._of_meds +
                              status +
                              sppb1_total +
                              score +
                              fes_total +
                              eq5d5l1_scale +
                              X6cit_1,
                              analysis)


  stargazer(log.log.lr, log.log.mlr, ci = T, digits = 2,
            out = "reg.full.htm",
            single.row = T, type = "latex",
            column.labels = c("Unadjusted", "Adjusted"),
            dep.var.labels = c("log(Length of stay in days)"),
            covariate.labels = c("log(Average daily step count)",
                                 "Female (vs. male)",
                                 "Age (years)",
                                 "Body mass index (kg/m)",
                                 "Height (cm)",
                                 "Former smoker (vs. never)",
                                 "Current smoker (vs. never)",
                                 "Doesn't drink alcohol anymore (vs. never)",
                                 "Still drinks alcohol (vs. never)",
                                 "Heavy drinker (vs. never)",
                                 "CIRSG",
                                 "Number of medications",
                                 "Married (vs. single)",
                                 "Widowed (vs. single)",
                                 "SPPB at baseline",
                                 "SHARE FI score",
                                 "FES score",
                                 "Self rated health (EQ5D)",
                                 "6CIT Score"))

# Raw model results in log scale

  ggplot(analysis, aes(x = log.avg.steps, y = log.los)) +
    geom_jitter(size = 4, color = "black", alpha = 0.5) +
    geom_smooth(method = "lm", color = "red", size = 0.7, linetype = "dashed",
                se = FALSE) +
    ylab("log(length of stay in days)") +
    xlab("log(average daily step count)") +
    theme_bw() +
    theme(axis.text = element_text(color = "black"),
          text      = element_text(color = "black", size = 16),
          panel.border = element_blank())

  ggsave("raw.reg.png", height = 6, width = 1.61 * 6)


# Raw model results in original scale

  ggplot(analysis, aes(x = avg.steps, y = los)) +
    geom_jitter(size = 4, color = "black", alpha = 0.5) +
    geom_line(data = data.frame(x = exp(log.log.lr$model$log.avg.steps),
                                y = exp(predict(log.log.lr))),
              aes(x = x, y = y),
              color = "red", size = 0.7, linetype = "dashed") +
    xlab("Average daily step count") +
    ylab("Length of stay in days") +
    theme_bw() +
    theme(axis.text = element_text(color = "black"),
          text      = element_text(color = "black", size = 16),
          panel.border = element_blank())

  ggsave("raw.reg.exp.png", height = 6, width = 1.61 * 6)



# SPPB

  ggplot(analysis, aes(x = avg.steps, y = sppb1_total)) +
    geom_jitter() +
    geom_smooth(method = "lm", color = "blue", se = FALSE) +
    geom_smooth(color = "red", linetype = "dashed", se = FALSE)

  ggplot(analysis, aes(x = log.avg.steps, y = sppb1_total)) +
    geom_jitter() +
    geom_smooth(method = "lm", color = "blue", se = FALSE) +
    geom_smooth(color = "red", linetype = "dashed", se = FALSE)

  ggplot(analysis, aes(x = log.avg.steps, y = sppb_diff)) +
    geom_jitter() +
    geom_smooth(method = "lm", color = "blue", se = FALSE) +
    geom_smooth(color = "red", linetype = "dashed", se = FALSE)

  sppb.raw <- lm(sppbf_total ~ log.avg.steps, analysis)

  sppb.adj <- lm(sppbf_total ~ log.avg.steps + sppb1_total, analysis)

  ggplot(analysis, aes(x = log.avg.steps, y = sppbf_total)) +
    geom_jitter(size = 4, color = "black", alpha = 0.5) +
    geom_smooth(method = "lm", color = "red", size = 0.7, linetype = "dashed",
                se = FALSE) +
    ylab("SPPB final") +
    xlab("log(average daily step count)") +
    theme_bw() +
    theme(axis.text = element_text(color = "black"),
          text      = element_text(color = "black", size = 16),
          panel.border = element_blank())

  ggsave("raw.sppb.reg.png", height = 6, width = 1.61 * 6)

  ggplot(analysis, aes(x = log.avg.steps, y = sppb_diff)) +
    geom_jitter(size = 4, color = "black", alpha = 0.5) +
    geom_smooth(method = "lm", color = "red", size = 0.7, linetype = "dashed",
                se = FALSE) +
    ylab("SPPB final - SPPB baseline") +
    xlab("log(average daily step count)") +
    theme_bw() +
    theme(axis.text = element_text(color = "black"),
          text      = element_text(color = "black", size = 16),
          panel.border = element_blank())

  ggsave("raw.sppb.diff.reg.png", height = 6, width = 1.61 * 6)

  stargazer(sppb.raw, sppb.adj, ci = T, digits = 2,
            out = "sppb.full.htm",
            single.row = T, type = "latex",
            column.labels = c("Unadjusted", "Adjusted"),
            dep.var.labels = c("SPPB final"),
            covariate.labels = c("log(Average daily step count)",
                                 "SPPB baseline"))

