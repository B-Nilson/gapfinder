# test case works

    Code
      expect_silent(mark_nearest_location(test_case$to_cover, to = test_case$
        install_at, within = test_case$cover_distance))

# grouped data works

    Code
      expect_silent(mark_nearest_location(dplyr::group_by(test_case$to_cover, type),
      to = test_case$install_at, within = test_case$cover_distance))

---

    Code
      expect_silent(mark_nearest_location(test_case$to_cover, to = dplyr::group_by(
        test_case$install_at, type), within = test_case$cover_distance))
    Condition
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(new_columns)
      
        # Now:
        data %>% select(all_of(new_columns))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(new_columns)
      
        # Now:
        data %>% select(all_of(new_columns))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(new_columns)
      
        # Now:
        data %>% select(all_of(new_columns))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(new_columns)
      
        # Now:
        data %>% select(all_of(new_columns))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(new_columns)
      
        # Now:
        data %>% select(all_of(new_columns))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(new_columns)
      
        # Now:
        data %>% select(all_of(new_columns))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(new_columns)
      
        # Now:
        data %>% select(all_of(new_columns))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(new_columns)
      
        # Now:
        data %>% select(all_of(new_columns))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(new_columns)
      
        # Now:
        data %>% select(all_of(new_columns))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(new_columns)
      
        # Now:
        data %>% select(all_of(new_columns))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.

---

    Code
      expect_silent(mark_nearest_location(dplyr::group_by(test_case$to_cover, type),
      to = dplyr::group_by(test_case$install_at, type), within = test_case$
        cover_distance))
    Condition
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(new_columns)
      
        # Now:
        data %>% select(all_of(new_columns))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(new_columns)
      
        # Now:
        data %>% select(all_of(new_columns))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(new_columns)
      
        # Now:
        data %>% select(all_of(new_columns))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(new_columns)
      
        # Now:
        data %>% select(all_of(new_columns))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(new_columns)
      
        # Now:
        data %>% select(all_of(new_columns))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(new_columns)
      
        # Now:
        data %>% select(all_of(new_columns))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(new_columns)
      
        # Now:
        data %>% select(all_of(new_columns))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(new_columns)
      
        # Now:
        data %>% select(all_of(new_columns))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(new_columns)
      
        # Now:
        data %>% select(all_of(new_columns))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      Warning:
      Using an external vector in selections was deprecated in tidyselect 1.1.0.
      i Please use `all_of()` or `any_of()` instead.
        # Was:
        data %>% select(new_columns)
      
        # Now:
        data %>% select(all_of(new_columns))
      
      See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.

