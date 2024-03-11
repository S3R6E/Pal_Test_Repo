## A function for converting classifications to points
cpce_classif_to_points <- function(dat) {
  dat |>
    group_by(`Frame image name`) |>
    mutate(total = n()) |>
    group_by(`Major Category`, .add = TRUE) |>
    summarise(total = max(total),
      count_groupcode = n()) |> 
    dplyr::select(`Frame image name`, `Major Category`, total, count_groupcode) 
}
<<<<<<< HEAD
=======

## A function for converting classifications to points
cpce_raw_classif_to_points <- function(dat) {
  dat |>
    group_by(`Frame image name`) |>
    mutate(total = n()) |>
    group_by(`Raw Data`, `Major Category`, .add = TRUE) |>
    summarise(total = max(total),
              count_groupcode = n()) |> 
    dplyr::select(`Frame image name`, `Major Category`, `Raw Data`, total, count_groupcode) 
}
>>>>>>> dbe02ccf1620e13db72a556b1b3c828e4cef4070
