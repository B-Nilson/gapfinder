# test case works

    Code
      print(expect_silent(prioritize_installations(prioritize_installations(
        optimized_locations, to_cover = dplyr::group_by(dplyr::filter(test_case$
          to_cover, type %in% population_types), type = factor(type, levels = population_types)),
        cover_distance = test_case$cover_distance, weight_columns = test_case$
          weight_columns, suffix = "_population"), to_cover = dplyr::group_by(dplyr::filter(
        test_case$to_cover, !type %in% population_types), type = factor(type, levels = community_types)),
      cover_distance = test_case$cover_distance, weight_columns = test_case$
        weight_columns, suffix = "_communities")), n = 100)
    Output
      Simple feature collection with 36 features and 16 fields
      Geometry type: POINT
      Dimension:     XY
      Bounding box:  xmin: -139.3843 ymin: 60.00194 xmax: -128.7068 ymax: 68.98877
      Geodetic CRS:  WGS 84
      # A tibble: 36 x 17
         name      type  prov_terr fcst_zone             geometry ease_of_install
       * <chr>     <fct> <fct>     <fct>              <POINT [°]>           <dbl>
       1 Keno City haml~ YT        Mayo      (-135.3022 63.90958)             1.5
       2 Watson L~ town  YT        Watson L~ (-128.7068 60.06243)             2.5
       3 Beloud P~ haml~ YT        Haines J~   (-137.05 60.36667)             1.5
       4 Lansing   haml~ YT        <NA>        (-133.65 63.73333)             1.5
       5 Ross Riv~ town  YT        Faro - R~  (-132.4495 61.9798)             2.5
       6 Mount Lo~ vill~ YT        Whitehor~ (-134.8568 60.44817)             2  
       7 Aishihik  haml~ YT        Kluane L~    (-137.5 61.56667)             1.5
       8 Stewart ~ vill~ YT        Mayo       (-136.679 63.37544)             2  
       9 Tuchitua~ vill~ YT        Watson L~ (-129.2178 60.92665)             2  
      10 Bear Cre~ haml~ YT        Dawson    (-139.2241 64.02769)             1.5
      11 Takhini   haml~ YT        Whitehor~ (-135.5244 60.85375)             1.5
      12 McClinto~ haml~ YT        Whitehor~  (-134.5415 60.5672)             1.5
      13 Upper Li~ haml~ YT        Watson L~  (-128.9121 60.0523)             1.5
      14 Destruct~ haml~ YT        Kluane L~ (-138.7998 61.25143)             1.5
      15 Eureka C~ haml~ YT        Dawson    (-138.8768 63.58632)             1.5
      16 Mendenha~ haml~ YT        Whitehor~ (-136.0167 60.76667)             1.5
      17 Kloo Lake haml~ YT        Haines J~   (-137.8701 60.918)             1.5
      18 Crag Lake haml~ YT        Whitehor~ (-134.5097 60.24345)             1.5
      19 Clear Cr~ haml~ YT        Mayo      (-137.2788 63.78159)             1.5
      20 Jakes Co~ haml~ YT        Whitehor~ (-133.9859 60.33931)             1.5
      21 Barlow    haml~ YT        Dawson    (-137.6828 63.75429)             1.5
      22 Coffee C~ haml~ YT        Beaver C~ (-139.0861 62.91327)             1.5
      23 Eagle Pl~ haml~ YT        Dempster  (-136.7183 66.37272)             1.5
      24 Gordon L~ haml~ YT        Mayo        (-135.45 63.63333)             1.5
      25 Hutshi    haml~ YT        Haines J~    (-136.5333 61.15)             1.5
      26 Kirkman ~ haml~ YT        Beaver C~ (-139.3843 62.99005)             1.5
      27 Kynocks   haml~ YT        Haines J~    (-135.9833 61.25)             1.5
      28 Little S~ haml~ YT        Pelly - ~ (-135.6844 62.05295)             1.5
      29 Livingst~ haml~ YT        Whitehor~   (-134.35 61.33333)             1.5
      30 Mason La~ haml~ YT        Whitehor~ (-134.6333 61.43333)             1.5
      31 Minto     haml~ YT        Pelly - ~ (-136.8724 62.59242)             1.5
      32 Rancheria haml~ YT        Cassiar ~ (-130.6051 60.08585)             1.5
      33 Ruby Camp haml~ YT        Kluane L~ (-137.9167 61.08333)             1.5
      34 Shingle ~ haml~ YT        <NA>      (-137.3623 68.98877)             1.5
      35 Swift Ri~ haml~ YT        Cassiar ~ (-131.1893 60.00194)             1.5
      36 Winter C~ haml~ YT        Whitehor~ (-134.6165 61.24171)             1.5
      # i 11 more variables: newly_covered_population <dbl>,
      #   newly_covered_rural_population <dbl>, newly_covered_urban_population <dbl>,
      #   priority_population <int>, newly_covered_communities <dbl>,
      #   newly_covered_city <dbl>, newly_covered_town <dbl>,
      #   newly_covered_village <dbl>, newly_covered_hamlet <dbl>,
      #   newly_covered_indigenous <dbl>, priority_communities <int>

