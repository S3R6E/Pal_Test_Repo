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
