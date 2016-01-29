

  library(magrittr)

  varNames <- c("Intercept",
                "Healthy GWG",
                "Inadequate GWG",
                "Overweight (BMI 25 to 30 kg/m2)",
                "Normal weight (BMI $<$ 25 kg/m2)",
                "Takes folate ($geq$ 400 $mu$ g)",
                "Some moderate-intensity exercise",
                "Frequent moderate-intensity exercise",
                "2 to 4 hours of television",
                "$<$ 2 hours of television",
                "Quit drinking during pregnancy",
                "Quit drinking prepregnancy",
                "Never drank",
                "Quit smoking during pregnancy",
                "Never smoked",
                "Eats 5 fruit/veg a day",
                "Eats $\\geq$ 1 serving of oily fish weekly")

# FFM ####

  a <- ffma[[05]][c(1:17), ]
  a$var <- varNames
  a$ll <-  a$b - (1.96 * a$se)
  a$ul <-  a$b + (1.96 * a$se)

  b <- ffma[[50]][c(1:17), ]
  b$var <- varNames
  b$ll <-  b$b - (1.96 * b$se)
  b$ul <-  b$b + (1.96 * b$se)

  c <- ffma[[95]][c(1:17), ]
  c$var <- varNames
  c$ll <-  c$b - (1.96 * c$se)
  c$ul <-  c$b + (1.96 * c$se)

  paste(a$var, " & ",
        round(a$b, 1), " & ", "(",
        round(a$ll, 1), " to ", round(a$ul, 1), ")", " & ",
        round(b$b, 1), " & ", "(",
        round(b$ll, 1), " to ", round(b$ul, 1), ")", " & ",
        round(c$b, 1), " & ", "(",
        round(c$ll, 1), " to ", round(c$ul, 1), ")", "\\\\",
        sep = "") %>%

  write.table("ffmaTable.txt",
    row.names = F, col.names = F, quote = F)

# FM ####

  a <- fma[[05]][c(1:17), ]
  a$var <- varNames
  a$ll <-  a$b - (1.96 * a$se)
  a$ul <-  a$b + (1.96 * a$se)

  b <- fma[[50]][c(1:17), ]
  b$var <- varNames
  b$ll <-  b$b - (1.96 * b$se)
  b$ul <-  b$b + (1.96 * b$se)

  c <- fma[[95]][c(1:17), ]
  c$var <- varNames
  c$ll <-  c$b - (1.96 * c$se)
  c$ul <-  c$b + (1.96 * c$se)

  paste(a$var, " & ",
        round(a$b, 1), " & ", "(",
        round(a$ll, 1), " to ", round(a$ul, 1), ")", " & ",
        round(b$b, 1), " & ", "(",
        round(b$ll, 1), " to ", round(b$ul, 1), ")", " & ",
        round(c$b, 1), " & ", "(",
        round(c$ll, 1), " to ", round(c$ul, 1), ")", "\\\\",
        sep = "") %>%

  write.table("fmaTable.txt",
              row.names = F, col.names = F, quote = F)

# Len ####

  a <- len[[05]][c(1:17), ]
  a$var <- varNames
  a$ll <-  a$b - (1.96 * a$se)
  a$ul <-  a$b + (1.96 * a$se)

  b <- len[[50]][c(1:17), ]
  b$var <- varNames
  b$ll <-  b$b - (1.96 * b$se)
  b$ul <-  b$b + (1.96 * b$se)

  c <- len[[95]][c(1:17), ]
  c$var <- varNames
  c$ll <-  c$b - (1.96 * c$se)
  c$ul <-  c$b + (1.96 * c$se)

  paste(a$var, " & ",
        round(a$b, 1), " & ", "(",
        round(a$ll, 1), " to ", round(a$ul, 1), ")", " & ",
        round(b$b, 1), " & ", "(",
        round(b$ll, 1), " to ", round(b$ul, 1), ")", " & ",
        round(c$b, 1), " & ", "(",
        round(c$ll, 1), " to ", round(c$ul, 1), ")", "\\\\",
        sep = "") %>%

  write.table("lenTable.txt",
              row.names = F, col.names = F, quote = F)

# GA ####

  a <- gw[[05]][c(1:17), ]
  a$var <- varNames
  a$ll <-  a$b - (1.96 * a$se)
  a$ul <-  a$b + (1.96 * a$se)

  b <- gw[[50]][c(1:17), ]
  b$var <- varNames
  b$ll <-  b$b - (1.96 * b$se)
  b$ul <-  b$b + (1.96 * b$se)

  c <- gw[[95]][c(1:17), ]
  c$var <- varNames
  c$ll <-  c$b - (1.96 * c$se)
  c$ul <-  c$b + (1.96 * c$se)

  paste(a$var, " & ",
        round(a$b, 1), " & ", "(",
        round(a$ll, 1), " to ", round(a$ul, 1), ")", " & ",
        round(b$b, 1), " & ", "(",
        round(b$ll, 1), " to ", round(b$ul, 1), ")", " & ",
        round(c$b, 1), " & ", "(",
        round(c$ll, 1), " to ", round(c$ul, 1), ")", "\\\\",
        sep = "") %>%

  write.table("gwTable.txt",
    row.names = F, col.names = F, quote = F)


