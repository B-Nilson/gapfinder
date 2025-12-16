# real case works

    Code
      expect_silent(prioritize_installations(prioritize_installations(install_at,
        to_cover = dplyr::filter(to_cover, type == "population"), cover_distance = units::set_units(
          10, "km"), weight_columns = c("total_population", "ease_of_install"),
        suffix = "_population"), to_cover = dplyr::filter(to_cover, type !=
        "population"), cover_distance = units::set_units(10, "km"), weight_columns = c(
        "total_population", "ease_of_install"), suffix = "_communities"))
    Condition
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"matches"` instead of `.data$matches`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"matches"` instead of `.data$matches`

