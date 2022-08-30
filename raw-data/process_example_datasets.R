


XY_eg <- readr::read_csv("raw-data/exam_anxiety_short.csv") |>
  dplyr::select(Code, Revise, Anxiety, Gender) |>
  tidyr::pivot_wider(names_prefix = "Anxiety_", values_from=Anxiety, names_from = Gender)
saveRDS(XY_eg, file = "data/XY_eg.rds")
