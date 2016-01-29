
  library(magrittr)

  sumTableDf <- corkDf[, c("regid",
                           "f26_sex",
                           "f26_birthwgt",
                           "f26_length",
                           "f26_fat_mass_kg",
                           "f26_fat_free_mass_kg",
                           "fat_mass_pct",
                           "f39c_final_del_gest",
                           "bmiPreCat2",
                           "healthyGWG",
                           "modEx",
                           "tv",
                           "alc",
                           "smk",
                           "folateRec",
                           "fiveDay",
                           "fishRec",
                           "f1_age",
                           "f11_hgt",
                           "f6_partcpt_bwgt",
                           "gravidity",
                           "white",
                           "f5c_marital_partnr",
                           "f5c_any_tertiary_edu",
                           "f5_socioeconomic_index",
                           "f5_type_maternity_care",
                           "f13c_depress_cat",
                           "f12c_pss")]

  sumTableDf <- sumTableDf[sumTableDf$f39c_final_del_gest >= 32, ]

  sumTableDf$f26_fat_mass_kg      <- sumTableDf$f26_fat_mass_kg * 1000
  sumTableDf$f26_fat_free_mass_kg <- sumTableDf$f26_fat_free_mass_kg * 1000
  sumTableDf$fat_mass_pct         <- sumTableDf$fat_mass_pct * 100

  colnames(sumTableDf) <- (c("ID",
                             "Sex",
                             "Birth weight (g)",
                             "Length (cm)",
                             "Fat mass (g)",
                             "Fat free mass (g)",
                             "Percent fat mass",
                             "Gestational age (wks)",
                             "BMI category",
                             "GWG",
                             "Moderate exercise",
                             "Amount of TV viewing",
                             "Alcohol use",
                             "Any smoking",
                             "Takes folate",
                             "Eats $\\geq$ 5 servings fruit and veg per day",
                             "Eats $\\geq$ 1 serving oily fish per week",
                             "Age",
                             "Height",
                             "Birth weight",
                             "Gravidity",
                             "White",
                             "Has partner",
                             "3rd level education",
                             "SEI",
                             "Maternity care",
                             "Depressed",
                             "Stress score"))

  cbind(summaryFull(sumTableDf),
        c("\\\\")) %>%

    write.table("summaryTable1.txt", row.names = F, col.names = F, quote = F)