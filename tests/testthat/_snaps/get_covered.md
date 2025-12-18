# test case works

    Code
      print(expect_silent(get_covered(install_at = test_case$install_at, to_cover = test_case$
        to_cover, cover_distance = test_case$cover_distance)))
    Output
      # A tibble: 269 x 2
         to_cover_id install_at_id
               <int>         <int>
       1           2            62
       2           3             8
       3           3            60
       4           3            74
       5           8            14
       6           8            78
       7           9            14
       8           9            78
       9          12            62
      10          13             8
      # i 259 more rows

