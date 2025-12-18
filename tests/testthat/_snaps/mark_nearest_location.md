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

---

    Code
      expect_silent(mark_nearest_location(dplyr::group_by(test_case$to_cover, type),
      to = dplyr::group_by(test_case$install_at, type), within = test_case$
        cover_distance))

