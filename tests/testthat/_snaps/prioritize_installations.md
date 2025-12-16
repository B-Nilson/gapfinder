# test case works

    Code
      print(expect_silent(prioritize_installations(prioritize_installations(test_case$
        install_at, to_cover = dplyr::group_by(dplyr::filter(test_case$to_cover,
      type %in% population_types), type), cover_distance = test_case$cover_distance,
      weight_columns = test_case$weight_columns, suffix = "_population"), to_cover = dplyr::group_by(
        dplyr::filter(test_case$to_cover, !type %in% population_types), type),
      cover_distance = test_case$cover_distance, weight_columns = test_case$
        weight_columns, suffix = "_communities")), n = 100)
    Output
      Simple feature collection with 81 features and 17 fields
      Geometry type: POINT
      Dimension:     XY
      Bounding box:  xmin: -140.8773 ymin: 60.00194 xmax: -128.7068 ymax: 68.98877
      Geodetic CRS:  WGS 84
      # A tibble: 81 x 18
         name      type  prov_terr fcst_zone             geometry ease_of_install
       * <chr>     <fct> <fct>     <fct>              <POINT [°]>           <dbl>
       1 Dawson C~ town  YT        Dawson    (-139.4317 64.06066)             2.5
       2 Keno City haml~ YT        Mayo      (-135.3022 63.90958)             1.5
       3 Whitehor~ city  YT        Whitehor~ (-135.0549 60.72157)             3  
       4 Watson L~ town  YT        Watson L~ (-128.7068 60.06243)             2.5
       5 Haines J~ vill~ YT        Haines J~   (-137.51 60.75272)             2  
       6 Beloud P~ haml~ YT        Haines J~   (-137.05 60.36667)             1.5
       7 Lansing   haml~ YT        <NA>        (-133.65 63.73333)             1.5
       8 Carmacks  town  YT        Pelly - ~ (-136.2899 62.08877)             2.5
       9 Mayo      town  YT        Mayo      (-135.8961 63.59269)             2.5
      10 Mount Lo~ vill~ YT        Whitehor~ (-134.8568 60.44817)             2  
      11 Old Crow  vill~ YT        Old Crow    (-139.8257 67.571)             2  
      12 Teslin    vill~ YT        Teslin    (-132.7249 60.16595)             2  
      13 Beaver C~ haml~ YT        Beaver C~ (-140.8773 62.38396)             1.5
      14 Carcross  town  YT        Whitehor~ (-134.7087 60.16679)             2.5
      15 Ross Riv~ town  YT        Faro - R~  (-132.4495 61.9798)             2.5
      16 Pelly Cr~ vill~ YT        Pelly - ~  (-136.5742 62.8228)             2  
      17 Burwash ~ haml~ YT        Kluane L~ (-138.9982 61.35546)             1.5
      18 Champagne haml~ YT        Haines J~     (-136.4833 60.8)             1.5
      19 Aishihik  haml~ YT        Kluane L~    (-137.5 61.56667)             1.5
      20 Lansdowne haml~ YT        Whitehor~ (-134.2951 60.26083)             1.5
      21 Bear Cre~ haml~ YT        Dawson    (-139.2241 64.02769)             1.5
      22 Eureka C~ haml~ YT        Dawson    (-138.8768 63.58632)             1.5
      23 Faro      town  YT        Faro - R~ (-133.3553 62.22954)             2.5
      24 Stewart ~ vill~ YT        Mayo       (-136.679 63.37544)             2  
      25 McClinto~ haml~ YT        Whitehor~  (-134.5415 60.5672)             1.5
      26 Takhini   haml~ YT        Whitehor~ (-135.5244 60.85375)             1.5
      27 Upper La~ haml~ YT        Whitehor~ (-135.0833 60.93333)             1.5
      28 Upper Li~ haml~ YT        Watson L~  (-128.9121 60.0523)             1.5
      29 Clear Cr~ haml~ YT        Mayo      (-137.2788 63.78159)             1.5
      30 Destruct~ haml~ YT        Kluane L~ (-138.7998 61.25143)             1.5
      31 Jakes Co~ haml~ YT        Whitehor~ (-133.9859 60.33931)             1.5
      32 Mendenha~ haml~ YT        Whitehor~ (-136.0167 60.76667)             1.5
      33 Tuchitua~ vill~ YT        Watson L~ (-129.2178 60.92665)             2  
      34 Barlow    haml~ YT        Dawson    (-137.6828 63.75429)             1.5
      35 Brooks B~ haml~ YT        Teslin    (-133.1979 60.41589)             1.5
      36 Coffee C~ haml~ YT        Beaver C~ (-139.0861 62.91327)             1.5
      37 Crag Lake haml~ YT        Whitehor~ (-134.5097 60.24345)             1.5
      38 Eagle Pl~ haml~ YT        Dempster  (-136.7183 66.37272)             1.5
      39 Gordon L~ haml~ YT        Mayo        (-135.45 63.63333)             1.5
      40 Henderso~ haml~ YT        Dawson    (-139.0234 64.06006)             1.5
      41 Hutshi    haml~ YT        Haines J~    (-136.5333 61.15)             1.5
      42 Kirkman ~ haml~ YT        Beaver C~ (-139.3843 62.99005)             1.5
      43 Kloo Lake haml~ YT        Haines J~   (-137.8701 60.918)             1.5
      44 Kynocks   haml~ YT        Haines J~    (-135.9833 61.25)             1.5
      45 Little S~ haml~ YT        Pelly - ~ (-135.6844 62.05295)             1.5
      46 Livingst~ haml~ YT        Whitehor~   (-134.35 61.33333)             1.5
      47 Mason La~ haml~ YT        Whitehor~ (-134.6333 61.43333)             1.5
      48 Minto     haml~ YT        Pelly - ~ (-136.8724 62.59242)             1.5
      49 Rancheria haml~ YT        Cassiar ~ (-130.6051 60.08585)             1.5
      50 Ruby Camp haml~ YT        Kluane L~ (-137.9167 61.08333)             1.5
      51 Shingle ~ haml~ YT        <NA>      (-137.3623 68.98877)             1.5
      52 Swift Ri~ haml~ YT        Cassiar ~ (-131.1893 60.00194)             1.5
      53 Winter C~ haml~ YT        Whitehor~ (-134.6165 61.24171)             1.5
      54 Bear Cre~ haml~ YT        Haines J~ (-137.6682 60.79649)             1.5
      55 Cowley    haml~ YT        Whitehor~ (-134.9001 60.52323)             1.5
      56 MacRae    haml~ YT        Whitehor~ (-135.0009 60.63781)             1.5
      57 Kwanlin ~ indi~ YT        Whitehor~ (-135.1082 60.71906)             1  
      58 Elsa      haml~ YT        Mayo      (-135.4902 63.91172)             1.5
      59 Keno Hill haml~ YT        Mayo             (-135.3 63.9)             1.5
      60 Klukshu   haml~ YT        Haines J~ (-137.0106 60.29468)             1.5
      61 Lansing ~ haml~ YT        <NA>      (-133.4662 63.74078)             1.5
      62 Moosehide haml~ YT        Dawson    (-139.4374 64.09509)             1.5
      63 Sunnydale haml~ YT        Dawson    (-139.5016 64.03937)             1.5
      64 Two Mile~ haml~ YT        Watson L~ (-128.7439 60.08715)             1.5
      65 Wernecke  haml~ YT        Mayo       (-135.279 63.95541)             1.5
      66 West Daw~ haml~ YT        Dawson    (-139.4775 64.07544)             1.5
      67 West Daw~ haml~ YT        Dawson    (-139.4519 64.07661)             1.5
      68 Aishihik  indi~ YT        Kluane L~ (-137.5067 61.59917)             1  
      69 Carcross~ indi~ YT        Whitehor~ (-134.7038 60.17045)             1  
      70 Champagne indi~ YT        Haines J~ (-136.4814 60.78613)             1  
      71 Champagn~ indi~ YT        Haines J~ (-137.5065 60.75263)             1  
      72 First Na~ indi~ YT        Mayo      (-135.8973 63.59481)             1  
      73 Kluane F~ indi~ YT        Kluane L~ (-138.9902 61.35646)             1  
      74 Liard Fi~ indi~ YT        Watson L~ (-128.7166 60.06602)             1  
      75 Little S~ indi~ YT        Pelly - ~ (-136.2889 62.09617)             1  
      76 Ross Riv~ indi~ YT        Faro - R~   (-132.451 61.9795)             1  
      77 Selkirk ~ indi~ YT        Pelly - ~  (-136.573 62.82387)             1  
      78 Teslin T~ indi~ YT        Teslin    (-132.7299 60.16782)             1  
      79 Tr'ondëk~ indi~ YT        Dawson    (-139.4312 64.06493)             1  
      80 Vuntut G~ indi~ YT        Old Crow  (-139.8279 67.57039)             1  
      81 White Ri~ indi~ YT        Beaver C~ (-140.8753 62.38376)             1  
      # i 12 more variables: total <dbl>, newly_covered_population <dbl>,
      #   newly_covered_rural_population <dbl>, newly_covered_urban_population <dbl>,
      #   priority_population <int>, newly_covered_communities <dbl>,
      #   newly_covered_city <dbl>, newly_covered_hamlet <dbl>,
      #   newly_covered_indigenous <dbl>, newly_covered_town <dbl>,
      #   newly_covered_village <dbl>, priority_communities <int>

