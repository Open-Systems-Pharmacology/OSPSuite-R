# expressionProfileBBToDataFrame returns a list with expressionParameters and initialConditions data frames

    Code
      result$expressionParameters
    Output
                                                                Container Path
      1                                     Organism|VenousBlood|Plasma|CYP3A4
      2                                 Organism|VenousBlood|BloodCells|CYP3A4
      3                                   Organism|ArterialBlood|Plasma|CYP3A4
      4                               Organism|ArterialBlood|BloodCells|CYP3A4
      5                                            Organism|Bone|Plasma|CYP3A4
      6                                        Organism|Bone|BloodCells|CYP3A4
      7                                      Organism|Bone|Interstitial|CYP3A4
      8                                      Organism|Bone|Interstitial|CYP3A4
      9                                     Organism|Bone|Intracellular|CYP3A4
      10                                    Organism|Bone|Intracellular|CYP3A4
      11                                    Organism|Bone|Intracellular|CYP3A4
      12                                         Organism|Bone|Endosome|CYP3A4
      13                                          Organism|Brain|Plasma|CYP3A4
      14                                      Organism|Brain|BloodCells|CYP3A4
      15                                    Organism|Brain|Interstitial|CYP3A4
      16                                    Organism|Brain|Interstitial|CYP3A4
      17                                   Organism|Brain|Intracellular|CYP3A4
      18                                   Organism|Brain|Intracellular|CYP3A4
      19                                   Organism|Brain|Intracellular|CYP3A4
      20                                        Organism|Brain|Endosome|CYP3A4
      21                                            Organism|Fat|Plasma|CYP3A4
      22                                        Organism|Fat|BloodCells|CYP3A4
      23                                      Organism|Fat|Interstitial|CYP3A4
      24                                      Organism|Fat|Interstitial|CYP3A4
      25                                     Organism|Fat|Intracellular|CYP3A4
      26                                     Organism|Fat|Intracellular|CYP3A4
      27                                     Organism|Fat|Intracellular|CYP3A4
      28                                          Organism|Fat|Endosome|CYP3A4
      29                                         Organism|Gonads|Plasma|CYP3A4
      30                                     Organism|Gonads|BloodCells|CYP3A4
      31                                   Organism|Gonads|Interstitial|CYP3A4
      32                                   Organism|Gonads|Interstitial|CYP3A4
      33                                  Organism|Gonads|Intracellular|CYP3A4
      34                                  Organism|Gonads|Intracellular|CYP3A4
      35                                  Organism|Gonads|Intracellular|CYP3A4
      36                                       Organism|Gonads|Endosome|CYP3A4
      37                                          Organism|Heart|Plasma|CYP3A4
      38                                      Organism|Heart|BloodCells|CYP3A4
      39                                    Organism|Heart|Interstitial|CYP3A4
      40                                    Organism|Heart|Interstitial|CYP3A4
      41                                   Organism|Heart|Intracellular|CYP3A4
      42                                   Organism|Heart|Intracellular|CYP3A4
      43                                   Organism|Heart|Intracellular|CYP3A4
      44                                        Organism|Heart|Endosome|CYP3A4
      45                                         Organism|Kidney|Plasma|CYP3A4
      46                                     Organism|Kidney|BloodCells|CYP3A4
      47                                   Organism|Kidney|Interstitial|CYP3A4
      48                                   Organism|Kidney|Interstitial|CYP3A4
      49                                  Organism|Kidney|Intracellular|CYP3A4
      50                                  Organism|Kidney|Intracellular|CYP3A4
      51                                  Organism|Kidney|Intracellular|CYP3A4
      52                                       Organism|Kidney|Endosome|CYP3A4
      53                                         Organism|Lumen|Stomach|CYP3A4
      54                                         Organism|Lumen|Stomach|CYP3A4
      55                                        Organism|Lumen|Duodenum|CYP3A4
      56                                        Organism|Lumen|Duodenum|CYP3A4
      57                                    Organism|Lumen|UpperJejunum|CYP3A4
      58                                    Organism|Lumen|UpperJejunum|CYP3A4
      59                                    Organism|Lumen|LowerJejunum|CYP3A4
      60                                    Organism|Lumen|LowerJejunum|CYP3A4
      61                                      Organism|Lumen|UpperIleum|CYP3A4
      62                                      Organism|Lumen|UpperIleum|CYP3A4
      63                                      Organism|Lumen|LowerIleum|CYP3A4
      64                                      Organism|Lumen|LowerIleum|CYP3A4
      65                                          Organism|Lumen|Caecum|CYP3A4
      66                                          Organism|Lumen|Caecum|CYP3A4
      67                                  Organism|Lumen|ColonAscendens|CYP3A4
      68                                  Organism|Lumen|ColonAscendens|CYP3A4
      69                                Organism|Lumen|ColonTransversum|CYP3A4
      70                                Organism|Lumen|ColonTransversum|CYP3A4
      71                                 Organism|Lumen|ColonDescendens|CYP3A4
      72                                 Organism|Lumen|ColonDescendens|CYP3A4
      73                                    Organism|Lumen|ColonSigmoid|CYP3A4
      74                                    Organism|Lumen|ColonSigmoid|CYP3A4
      75                                          Organism|Lumen|Rectum|CYP3A4
      76                                          Organism|Lumen|Rectum|CYP3A4
      77                                        Organism|Stomach|Plasma|CYP3A4
      78                                    Organism|Stomach|BloodCells|CYP3A4
      79                                  Organism|Stomach|Interstitial|CYP3A4
      80                                  Organism|Stomach|Interstitial|CYP3A4
      81                                 Organism|Stomach|Intracellular|CYP3A4
      82                                 Organism|Stomach|Intracellular|CYP3A4
      83                                 Organism|Stomach|Intracellular|CYP3A4
      84                                      Organism|Stomach|Endosome|CYP3A4
      85                                 Organism|SmallIntestine|Plasma|CYP3A4
      86                             Organism|SmallIntestine|BloodCells|CYP3A4
      87                           Organism|SmallIntestine|Interstitial|CYP3A4
      88                           Organism|SmallIntestine|Interstitial|CYP3A4
      89                          Organism|SmallIntestine|Intracellular|CYP3A4
      90                          Organism|SmallIntestine|Intracellular|CYP3A4
      91                          Organism|SmallIntestine|Intracellular|CYP3A4
      92                               Organism|SmallIntestine|Endosome|CYP3A4
      93                 Organism|SmallIntestine|Mucosa|Duodenum|Plasma|CYP3A4
      94             Organism|SmallIntestine|Mucosa|Duodenum|BloodCells|CYP3A4
      95           Organism|SmallIntestine|Mucosa|Duodenum|Interstitial|CYP3A4
      96           Organism|SmallIntestine|Mucosa|Duodenum|Interstitial|CYP3A4
      97          Organism|SmallIntestine|Mucosa|Duodenum|Intracellular|CYP3A4
      98          Organism|SmallIntestine|Mucosa|Duodenum|Intracellular|CYP3A4
      99          Organism|SmallIntestine|Mucosa|Duodenum|Intracellular|CYP3A4
      100              Organism|SmallIntestine|Mucosa|Duodenum|Endosome|CYP3A4
      101            Organism|SmallIntestine|Mucosa|UpperJejunum|Plasma|CYP3A4
      102        Organism|SmallIntestine|Mucosa|UpperJejunum|BloodCells|CYP3A4
      103      Organism|SmallIntestine|Mucosa|UpperJejunum|Interstitial|CYP3A4
      104      Organism|SmallIntestine|Mucosa|UpperJejunum|Interstitial|CYP3A4
      105     Organism|SmallIntestine|Mucosa|UpperJejunum|Intracellular|CYP3A4
      106     Organism|SmallIntestine|Mucosa|UpperJejunum|Intracellular|CYP3A4
      107     Organism|SmallIntestine|Mucosa|UpperJejunum|Intracellular|CYP3A4
      108          Organism|SmallIntestine|Mucosa|UpperJejunum|Endosome|CYP3A4
      109            Organism|SmallIntestine|Mucosa|LowerJejunum|Plasma|CYP3A4
      110        Organism|SmallIntestine|Mucosa|LowerJejunum|BloodCells|CYP3A4
      111      Organism|SmallIntestine|Mucosa|LowerJejunum|Interstitial|CYP3A4
      112      Organism|SmallIntestine|Mucosa|LowerJejunum|Interstitial|CYP3A4
      113     Organism|SmallIntestine|Mucosa|LowerJejunum|Intracellular|CYP3A4
      114     Organism|SmallIntestine|Mucosa|LowerJejunum|Intracellular|CYP3A4
      115     Organism|SmallIntestine|Mucosa|LowerJejunum|Intracellular|CYP3A4
      116          Organism|SmallIntestine|Mucosa|LowerJejunum|Endosome|CYP3A4
      117              Organism|SmallIntestine|Mucosa|UpperIleum|Plasma|CYP3A4
      118          Organism|SmallIntestine|Mucosa|UpperIleum|BloodCells|CYP3A4
      119        Organism|SmallIntestine|Mucosa|UpperIleum|Interstitial|CYP3A4
      120        Organism|SmallIntestine|Mucosa|UpperIleum|Interstitial|CYP3A4
      121       Organism|SmallIntestine|Mucosa|UpperIleum|Intracellular|CYP3A4
      122       Organism|SmallIntestine|Mucosa|UpperIleum|Intracellular|CYP3A4
      123       Organism|SmallIntestine|Mucosa|UpperIleum|Intracellular|CYP3A4
      124            Organism|SmallIntestine|Mucosa|UpperIleum|Endosome|CYP3A4
      125              Organism|SmallIntestine|Mucosa|LowerIleum|Plasma|CYP3A4
      126          Organism|SmallIntestine|Mucosa|LowerIleum|BloodCells|CYP3A4
      127        Organism|SmallIntestine|Mucosa|LowerIleum|Interstitial|CYP3A4
      128        Organism|SmallIntestine|Mucosa|LowerIleum|Interstitial|CYP3A4
      129       Organism|SmallIntestine|Mucosa|LowerIleum|Intracellular|CYP3A4
      130       Organism|SmallIntestine|Mucosa|LowerIleum|Intracellular|CYP3A4
      131       Organism|SmallIntestine|Mucosa|LowerIleum|Intracellular|CYP3A4
      132            Organism|SmallIntestine|Mucosa|LowerIleum|Endosome|CYP3A4
      133                                Organism|LargeIntestine|Plasma|CYP3A4
      134                            Organism|LargeIntestine|BloodCells|CYP3A4
      135                          Organism|LargeIntestine|Interstitial|CYP3A4
      136                          Organism|LargeIntestine|Interstitial|CYP3A4
      137                         Organism|LargeIntestine|Intracellular|CYP3A4
      138                         Organism|LargeIntestine|Intracellular|CYP3A4
      139                         Organism|LargeIntestine|Intracellular|CYP3A4
      140                              Organism|LargeIntestine|Endosome|CYP3A4
      141                  Organism|LargeIntestine|Mucosa|Caecum|Plasma|CYP3A4
      142              Organism|LargeIntestine|Mucosa|Caecum|BloodCells|CYP3A4
      143            Organism|LargeIntestine|Mucosa|Caecum|Interstitial|CYP3A4
      144            Organism|LargeIntestine|Mucosa|Caecum|Interstitial|CYP3A4
      145           Organism|LargeIntestine|Mucosa|Caecum|Intracellular|CYP3A4
      146           Organism|LargeIntestine|Mucosa|Caecum|Intracellular|CYP3A4
      147           Organism|LargeIntestine|Mucosa|Caecum|Intracellular|CYP3A4
      148                Organism|LargeIntestine|Mucosa|Caecum|Endosome|CYP3A4
      149          Organism|LargeIntestine|Mucosa|ColonAscendens|Plasma|CYP3A4
      150      Organism|LargeIntestine|Mucosa|ColonAscendens|BloodCells|CYP3A4
      151    Organism|LargeIntestine|Mucosa|ColonAscendens|Interstitial|CYP3A4
      152    Organism|LargeIntestine|Mucosa|ColonAscendens|Interstitial|CYP3A4
      153   Organism|LargeIntestine|Mucosa|ColonAscendens|Intracellular|CYP3A4
      154   Organism|LargeIntestine|Mucosa|ColonAscendens|Intracellular|CYP3A4
      155   Organism|LargeIntestine|Mucosa|ColonAscendens|Intracellular|CYP3A4
      156        Organism|LargeIntestine|Mucosa|ColonAscendens|Endosome|CYP3A4
      157        Organism|LargeIntestine|Mucosa|ColonTransversum|Plasma|CYP3A4
      158    Organism|LargeIntestine|Mucosa|ColonTransversum|BloodCells|CYP3A4
      159  Organism|LargeIntestine|Mucosa|ColonTransversum|Interstitial|CYP3A4
      160  Organism|LargeIntestine|Mucosa|ColonTransversum|Interstitial|CYP3A4
      161 Organism|LargeIntestine|Mucosa|ColonTransversum|Intracellular|CYP3A4
      162 Organism|LargeIntestine|Mucosa|ColonTransversum|Intracellular|CYP3A4
      163 Organism|LargeIntestine|Mucosa|ColonTransversum|Intracellular|CYP3A4
      164      Organism|LargeIntestine|Mucosa|ColonTransversum|Endosome|CYP3A4
      165         Organism|LargeIntestine|Mucosa|ColonDescendens|Plasma|CYP3A4
      166     Organism|LargeIntestine|Mucosa|ColonDescendens|BloodCells|CYP3A4
      167   Organism|LargeIntestine|Mucosa|ColonDescendens|Interstitial|CYP3A4
      168   Organism|LargeIntestine|Mucosa|ColonDescendens|Interstitial|CYP3A4
      169  Organism|LargeIntestine|Mucosa|ColonDescendens|Intracellular|CYP3A4
      170  Organism|LargeIntestine|Mucosa|ColonDescendens|Intracellular|CYP3A4
      171  Organism|LargeIntestine|Mucosa|ColonDescendens|Intracellular|CYP3A4
      172       Organism|LargeIntestine|Mucosa|ColonDescendens|Endosome|CYP3A4
      173            Organism|LargeIntestine|Mucosa|ColonSigmoid|Plasma|CYP3A4
      174        Organism|LargeIntestine|Mucosa|ColonSigmoid|BloodCells|CYP3A4
      175      Organism|LargeIntestine|Mucosa|ColonSigmoid|Interstitial|CYP3A4
      176      Organism|LargeIntestine|Mucosa|ColonSigmoid|Interstitial|CYP3A4
      177     Organism|LargeIntestine|Mucosa|ColonSigmoid|Intracellular|CYP3A4
      178     Organism|LargeIntestine|Mucosa|ColonSigmoid|Intracellular|CYP3A4
      179     Organism|LargeIntestine|Mucosa|ColonSigmoid|Intracellular|CYP3A4
      180          Organism|LargeIntestine|Mucosa|ColonSigmoid|Endosome|CYP3A4
      181                  Organism|LargeIntestine|Mucosa|Rectum|Plasma|CYP3A4
      182              Organism|LargeIntestine|Mucosa|Rectum|BloodCells|CYP3A4
      183            Organism|LargeIntestine|Mucosa|Rectum|Interstitial|CYP3A4
      184            Organism|LargeIntestine|Mucosa|Rectum|Interstitial|CYP3A4
      185           Organism|LargeIntestine|Mucosa|Rectum|Intracellular|CYP3A4
      186           Organism|LargeIntestine|Mucosa|Rectum|Intracellular|CYP3A4
      187           Organism|LargeIntestine|Mucosa|Rectum|Intracellular|CYP3A4
      188                Organism|LargeIntestine|Mucosa|Rectum|Endosome|CYP3A4
      189                              Organism|Liver|Periportal|Plasma|CYP3A4
      190                          Organism|Liver|Periportal|BloodCells|CYP3A4
      191                        Organism|Liver|Periportal|Interstitial|CYP3A4
      192                        Organism|Liver|Periportal|Interstitial|CYP3A4
      193                       Organism|Liver|Periportal|Intracellular|CYP3A4
      194                       Organism|Liver|Periportal|Intracellular|CYP3A4
      195                       Organism|Liver|Periportal|Intracellular|CYP3A4
      196                            Organism|Liver|Periportal|Endosome|CYP3A4
      197                             Organism|Liver|Pericentral|Plasma|CYP3A4
      198                         Organism|Liver|Pericentral|BloodCells|CYP3A4
      199                       Organism|Liver|Pericentral|Interstitial|CYP3A4
      200                       Organism|Liver|Pericentral|Interstitial|CYP3A4
      201                      Organism|Liver|Pericentral|Intracellular|CYP3A4
      202                      Organism|Liver|Pericentral|Intracellular|CYP3A4
      203                      Organism|Liver|Pericentral|Intracellular|CYP3A4
      204                           Organism|Liver|Pericentral|Endosome|CYP3A4
      205                                          Organism|Lung|Plasma|CYP3A4
      206                                      Organism|Lung|BloodCells|CYP3A4
      207                                    Organism|Lung|Interstitial|CYP3A4
      208                                    Organism|Lung|Interstitial|CYP3A4
      209                                   Organism|Lung|Intracellular|CYP3A4
      210                                   Organism|Lung|Intracellular|CYP3A4
      211                                   Organism|Lung|Intracellular|CYP3A4
      212                                        Organism|Lung|Endosome|CYP3A4
      213                                        Organism|Muscle|Plasma|CYP3A4
      214                                    Organism|Muscle|BloodCells|CYP3A4
      215                                  Organism|Muscle|Interstitial|CYP3A4
      216                                  Organism|Muscle|Interstitial|CYP3A4
      217                                 Organism|Muscle|Intracellular|CYP3A4
      218                                 Organism|Muscle|Intracellular|CYP3A4
      219                                 Organism|Muscle|Intracellular|CYP3A4
      220                                      Organism|Muscle|Endosome|CYP3A4
      221                                      Organism|Pancreas|Plasma|CYP3A4
      222                                  Organism|Pancreas|BloodCells|CYP3A4
      223                                Organism|Pancreas|Interstitial|CYP3A4
      224                                Organism|Pancreas|Interstitial|CYP3A4
      225                               Organism|Pancreas|Intracellular|CYP3A4
      226                               Organism|Pancreas|Intracellular|CYP3A4
      227                               Organism|Pancreas|Intracellular|CYP3A4
      228                                    Organism|Pancreas|Endosome|CYP3A4
      229                                    Organism|PortalVein|Plasma|CYP3A4
      230                                Organism|PortalVein|BloodCells|CYP3A4
      231                                          Organism|Skin|Plasma|CYP3A4
      232                                      Organism|Skin|BloodCells|CYP3A4
      233                                    Organism|Skin|Interstitial|CYP3A4
      234                                    Organism|Skin|Interstitial|CYP3A4
      235                                   Organism|Skin|Intracellular|CYP3A4
      236                                   Organism|Skin|Intracellular|CYP3A4
      237                                   Organism|Skin|Intracellular|CYP3A4
      238                                        Organism|Skin|Endosome|CYP3A4
      239                                        Organism|Spleen|Plasma|CYP3A4
      240                                    Organism|Spleen|BloodCells|CYP3A4
      241                                  Organism|Spleen|Interstitial|CYP3A4
      242                                  Organism|Spleen|Interstitial|CYP3A4
      243                                 Organism|Spleen|Intracellular|CYP3A4
      244                                 Organism|Spleen|Intracellular|CYP3A4
      245                                 Organism|Spleen|Intracellular|CYP3A4
      246                                      Organism|Spleen|Endosome|CYP3A4
      247                                                               CYP3A4
      248                                                               CYP3A4
      249                                                               CYP3A4
      250                                                               CYP3A4
      251                                                               CYP3A4
      252                                                               CYP3A4
      253                                                               CYP3A4
      254                                                               CYP3A4
      255                                                               CYP3A4
      256                                                               CYP3A4
      257                                                               CYP3A4
      258                                                               CYP3A4
      259                                                               CYP3A4
      260                                                               CYP3A4
      261                                                               CYP3A4
      262                                                               CYP3A4
                                                              Parameter Name   Value
      1                                                Initial concentration     NaN
      2                                                Initial concentration     NaN
      3                                                Initial concentration     NaN
      4                                                Initial concentration     NaN
      5                                                Initial concentration     NaN
      6                                                Initial concentration     NaN
      7                                      Fraction expressed interstitial     NaN
      8                                                Initial concentration     NaN
      9                                                  Relative expression    0.00
      10                                    Fraction expressed intracellular    1.00
      11                                               Initial concentration     NaN
      12                                               Initial concentration     NaN
      13                                               Initial concentration     NaN
      14                                               Initial concentration     NaN
      15                                     Fraction expressed interstitial     NaN
      16                                               Initial concentration     NaN
      17                                                 Relative expression    0.00
      18                                    Fraction expressed intracellular    1.00
      19                                               Initial concentration     NaN
      20                                               Initial concentration     NaN
      21                                               Initial concentration     NaN
      22                                               Initial concentration     NaN
      23                                     Fraction expressed interstitial     NaN
      24                                               Initial concentration     NaN
      25                                                 Relative expression    0.00
      26                                    Fraction expressed intracellular    1.00
      27                                               Initial concentration     NaN
      28                                               Initial concentration     NaN
      29                                               Initial concentration     NaN
      30                                               Initial concentration     NaN
      31                                     Fraction expressed interstitial     NaN
      32                                               Initial concentration     NaN
      33                                                 Relative expression    0.00
      34                                    Fraction expressed intracellular    1.00
      35                                               Initial concentration     NaN
      36                                               Initial concentration     NaN
      37                                               Initial concentration     NaN
      38                                               Initial concentration     NaN
      39                                     Fraction expressed interstitial     NaN
      40                                               Initial concentration     NaN
      41                                                 Relative expression    0.00
      42                                    Fraction expressed intracellular    1.00
      43                                               Initial concentration     NaN
      44                                               Initial concentration     NaN
      45                                               Initial concentration     NaN
      46                                               Initial concentration     NaN
      47                                     Fraction expressed interstitial     NaN
      48                                               Initial concentration     NaN
      49                                                 Relative expression    0.00
      50                                    Fraction expressed intracellular    1.00
      51                                               Initial concentration     NaN
      52                                               Initial concentration     NaN
      53                                                 Relative expression    0.00
      54                                               Initial concentration     NaN
      55                                                 Relative expression    0.00
      56                                               Initial concentration     NaN
      57                                                 Relative expression    0.00
      58                                               Initial concentration     NaN
      59                                                 Relative expression    0.00
      60                                               Initial concentration     NaN
      61                                                 Relative expression    0.00
      62                                               Initial concentration     NaN
      63                                                 Relative expression    0.00
      64                                               Initial concentration     NaN
      65                                                 Relative expression    0.00
      66                                               Initial concentration     NaN
      67                                                 Relative expression    0.00
      68                                               Initial concentration     NaN
      69                                                 Relative expression    0.00
      70                                               Initial concentration     NaN
      71                                                 Relative expression    0.00
      72                                               Initial concentration     NaN
      73                                                 Relative expression    0.00
      74                                               Initial concentration     NaN
      75                                                 Relative expression    0.00
      76                                               Initial concentration     NaN
      77                                               Initial concentration     NaN
      78                                               Initial concentration     NaN
      79                                     Fraction expressed interstitial     NaN
      80                                               Initial concentration     NaN
      81                                                 Relative expression    0.00
      82                                    Fraction expressed intracellular    1.00
      83                                               Initial concentration     NaN
      84                                               Initial concentration     NaN
      85                                               Initial concentration     NaN
      86                                               Initial concentration     NaN
      87                                     Fraction expressed interstitial     NaN
      88                                               Initial concentration     NaN
      89                                                 Relative expression    0.00
      90                                    Fraction expressed intracellular    1.00
      91                                               Initial concentration     NaN
      92                                               Initial concentration     NaN
      93                                               Initial concentration     NaN
      94                                               Initial concentration     NaN
      95                                     Fraction expressed interstitial     NaN
      96                                               Initial concentration     NaN
      97                                                 Relative expression    0.00
      98                                    Fraction expressed intracellular    1.00
      99                                               Initial concentration     NaN
      100                                              Initial concentration     NaN
      101                                              Initial concentration     NaN
      102                                              Initial concentration     NaN
      103                                    Fraction expressed interstitial     NaN
      104                                              Initial concentration     NaN
      105                                                Relative expression    0.00
      106                                   Fraction expressed intracellular    1.00
      107                                              Initial concentration     NaN
      108                                              Initial concentration     NaN
      109                                              Initial concentration     NaN
      110                                              Initial concentration     NaN
      111                                    Fraction expressed interstitial     NaN
      112                                              Initial concentration     NaN
      113                                                Relative expression    0.00
      114                                   Fraction expressed intracellular    1.00
      115                                              Initial concentration     NaN
      116                                              Initial concentration     NaN
      117                                              Initial concentration     NaN
      118                                              Initial concentration     NaN
      119                                    Fraction expressed interstitial     NaN
      120                                              Initial concentration     NaN
      121                                                Relative expression    0.00
      122                                   Fraction expressed intracellular    1.00
      123                                              Initial concentration     NaN
      124                                              Initial concentration     NaN
      125                                              Initial concentration     NaN
      126                                              Initial concentration     NaN
      127                                    Fraction expressed interstitial     NaN
      128                                              Initial concentration     NaN
      129                                                Relative expression    0.00
      130                                   Fraction expressed intracellular    1.00
      131                                              Initial concentration     NaN
      132                                              Initial concentration     NaN
      133                                              Initial concentration     NaN
      134                                              Initial concentration     NaN
      135                                    Fraction expressed interstitial     NaN
      136                                              Initial concentration     NaN
      137                                                Relative expression    0.00
      138                                   Fraction expressed intracellular    1.00
      139                                              Initial concentration     NaN
      140                                              Initial concentration     NaN
      141                                              Initial concentration     NaN
      142                                              Initial concentration     NaN
      143                                    Fraction expressed interstitial     NaN
      144                                              Initial concentration     NaN
      145                                                Relative expression    0.00
      146                                   Fraction expressed intracellular    1.00
      147                                              Initial concentration     NaN
      148                                              Initial concentration     NaN
      149                                              Initial concentration     NaN
      150                                              Initial concentration     NaN
      151                                    Fraction expressed interstitial     NaN
      152                                              Initial concentration     NaN
      153                                                Relative expression    0.00
      154                                   Fraction expressed intracellular    1.00
      155                                              Initial concentration     NaN
      156                                              Initial concentration     NaN
      157                                              Initial concentration     NaN
      158                                              Initial concentration     NaN
      159                                    Fraction expressed interstitial     NaN
      160                                              Initial concentration     NaN
      161                                                Relative expression    0.00
      162                                   Fraction expressed intracellular    1.00
      163                                              Initial concentration     NaN
      164                                              Initial concentration     NaN
      165                                              Initial concentration     NaN
      166                                              Initial concentration     NaN
      167                                    Fraction expressed interstitial     NaN
      168                                              Initial concentration     NaN
      169                                                Relative expression    0.00
      170                                   Fraction expressed intracellular    1.00
      171                                              Initial concentration     NaN
      172                                              Initial concentration     NaN
      173                                              Initial concentration     NaN
      174                                              Initial concentration     NaN
      175                                    Fraction expressed interstitial     NaN
      176                                              Initial concentration     NaN
      177                                                Relative expression    0.00
      178                                   Fraction expressed intracellular    1.00
      179                                              Initial concentration     NaN
      180                                              Initial concentration     NaN
      181                                              Initial concentration     NaN
      182                                              Initial concentration     NaN
      183                                    Fraction expressed interstitial     NaN
      184                                              Initial concentration     NaN
      185                                                Relative expression    0.00
      186                                   Fraction expressed intracellular    1.00
      187                                              Initial concentration     NaN
      188                                              Initial concentration     NaN
      189                                              Initial concentration     NaN
      190                                              Initial concentration     NaN
      191                                    Fraction expressed interstitial     NaN
      192                                              Initial concentration     NaN
      193                                                Relative expression    0.00
      194                                   Fraction expressed intracellular    1.00
      195                                              Initial concentration     NaN
      196                                              Initial concentration     NaN
      197                                              Initial concentration     NaN
      198                                              Initial concentration     NaN
      199                                    Fraction expressed interstitial     NaN
      200                                              Initial concentration     NaN
      201                                                Relative expression    0.00
      202                                   Fraction expressed intracellular    1.00
      203                                              Initial concentration     NaN
      204                                              Initial concentration     NaN
      205                                              Initial concentration     NaN
      206                                              Initial concentration     NaN
      207                                    Fraction expressed interstitial     NaN
      208                                              Initial concentration     NaN
      209                                                Relative expression    0.00
      210                                   Fraction expressed intracellular    1.00
      211                                              Initial concentration     NaN
      212                                              Initial concentration     NaN
      213                                              Initial concentration     NaN
      214                                              Initial concentration     NaN
      215                                    Fraction expressed interstitial     NaN
      216                                              Initial concentration     NaN
      217                                                Relative expression    0.00
      218                                   Fraction expressed intracellular    1.00
      219                                              Initial concentration     NaN
      220                                              Initial concentration     NaN
      221                                              Initial concentration     NaN
      222                                              Initial concentration     NaN
      223                                    Fraction expressed interstitial     NaN
      224                                              Initial concentration     NaN
      225                                                Relative expression    0.00
      226                                   Fraction expressed intracellular    1.00
      227                                              Initial concentration     NaN
      228                                              Initial concentration     NaN
      229                                              Initial concentration     NaN
      230                                              Initial concentration     NaN
      231                                              Initial concentration     NaN
      232                                              Initial concentration     NaN
      233                                    Fraction expressed interstitial     NaN
      234                                              Initial concentration     NaN
      235                                                Relative expression    0.00
      236                                   Fraction expressed intracellular    1.00
      237                                              Initial concentration     NaN
      238                                              Initial concentration     NaN
      239                                              Initial concentration     NaN
      240                                              Initial concentration     NaN
      241                                    Fraction expressed interstitial     NaN
      242                                              Initial concentration     NaN
      243                                                Relative expression    0.00
      244                                   Fraction expressed intracellular    1.00
      245                                              Initial concentration     NaN
      246                                              Initial concentration     NaN
      247                                            Reference concentration    4.32
      248                                                       t1/2 (liver) 2220.00
      249                                                   t1/2 (intestine) 1380.00
      250                                                     Disease factor    1.00
      251                                           Ontogeny factor GI table     NaN
      252                                              Ontogeny factor table     NaN
      253                                                    Ontogeny factor     NaN
      254                                                 Ontogeny factor GI     NaN
      255                                 Relative expression in blood cells    0.00
      256                                  Fraction expressed in blood cells    1.00
      257                         Fraction expressed in blood cells membrane     NaN
      258                                      Relative expression in plasma    0.00
      259                        Relative expression in vascular endothelium    0.00
      260                                    Fraction expressed in endosomes    1.00
      261 Fraction expressed on plasma-side membrane of vascular endothelium    0.00
      262 Fraction expressed on tissue-side membrane of vascular endothelium     NaN
            Unit Value Origin
      1   µmol/l             
      2   µmol/l             
      3   µmol/l             
      4   µmol/l             
      5   µmol/l             
      6   µmol/l             
      7                      
      8   µmol/l             
      9                      
      10                     
      11  µmol/l             
      12  µmol/l             
      13  µmol/l             
      14  µmol/l             
      15                     
      16  µmol/l             
      17                     
      18                     
      19  µmol/l             
      20  µmol/l             
      21  µmol/l             
      22  µmol/l             
      23                     
      24  µmol/l             
      25                     
      26                     
      27  µmol/l             
      28  µmol/l             
      29  µmol/l             
      30  µmol/l             
      31                     
      32  µmol/l             
      33                     
      34                     
      35  µmol/l             
      36  µmol/l             
      37  µmol/l             
      38  µmol/l             
      39                     
      40  µmol/l             
      41                     
      42                     
      43  µmol/l             
      44  µmol/l             
      45  µmol/l             
      46  µmol/l             
      47                     
      48  µmol/l             
      49                     
      50                     
      51  µmol/l             
      52  µmol/l             
      53                     
      54  µmol/l             
      55                     
      56  µmol/l             
      57                     
      58  µmol/l             
      59                     
      60  µmol/l             
      61                     
      62  µmol/l             
      63                     
      64  µmol/l             
      65                     
      66  µmol/l             
      67                     
      68  µmol/l             
      69                     
      70  µmol/l             
      71                     
      72  µmol/l             
      73                     
      74  µmol/l             
      75                     
      76  µmol/l             
      77  µmol/l             
      78  µmol/l             
      79                     
      80  µmol/l             
      81                     
      82                     
      83  µmol/l             
      84  µmol/l             
      85  µmol/l             
      86  µmol/l             
      87                     
      88  µmol/l             
      89                     
      90                     
      91  µmol/l             
      92  µmol/l             
      93  µmol/l             
      94  µmol/l             
      95                     
      96  µmol/l             
      97                     
      98                     
      99  µmol/l             
      100 µmol/l             
      101 µmol/l             
      102 µmol/l             
      103                    
      104 µmol/l             
      105                    
      106                    
      107 µmol/l             
      108 µmol/l             
      109 µmol/l             
      110 µmol/l             
      111                    
      112 µmol/l             
      113                    
      114                    
      115 µmol/l             
      116 µmol/l             
      117 µmol/l             
      118 µmol/l             
      119                    
      120 µmol/l             
      121                    
      122                    
      123 µmol/l             
      124 µmol/l             
      125 µmol/l             
      126 µmol/l             
      127                    
      128 µmol/l             
      129                    
      130                    
      131 µmol/l             
      132 µmol/l             
      133 µmol/l             
      134 µmol/l             
      135                    
      136 µmol/l             
      137                    
      138                    
      139 µmol/l             
      140 µmol/l             
      141 µmol/l             
      142 µmol/l             
      143                    
      144 µmol/l             
      145                    
      146                    
      147 µmol/l             
      148 µmol/l             
      149 µmol/l             
      150 µmol/l             
      151                    
      152 µmol/l             
      153                    
      154                    
      155 µmol/l             
      156 µmol/l             
      157 µmol/l             
      158 µmol/l             
      159                    
      160 µmol/l             
      161                    
      162                    
      163 µmol/l             
      164 µmol/l             
      165 µmol/l             
      166 µmol/l             
      167                    
      168 µmol/l             
      169                    
      170                    
      171 µmol/l             
      172 µmol/l             
      173 µmol/l             
      174 µmol/l             
      175                    
      176 µmol/l             
      177                    
      178                    
      179 µmol/l             
      180 µmol/l             
      181 µmol/l             
      182 µmol/l             
      183                    
      184 µmol/l             
      185                    
      186                    
      187 µmol/l             
      188 µmol/l             
      189 µmol/l             
      190 µmol/l             
      191                    
      192 µmol/l             
      193                    
      194                    
      195 µmol/l             
      196 µmol/l             
      197 µmol/l             
      198 µmol/l             
      199                    
      200 µmol/l             
      201                    
      202                    
      203 µmol/l             
      204 µmol/l             
      205 µmol/l             
      206 µmol/l             
      207                    
      208 µmol/l             
      209                    
      210                    
      211 µmol/l             
      212 µmol/l             
      213 µmol/l             
      214 µmol/l             
      215                    
      216 µmol/l             
      217                    
      218                    
      219 µmol/l             
      220 µmol/l             
      221 µmol/l             
      222 µmol/l             
      223                    
      224 µmol/l             
      225                    
      226                    
      227 µmol/l             
      228 µmol/l             
      229 µmol/l             
      230 µmol/l             
      231 µmol/l             
      232 µmol/l             
      233                    
      234 µmol/l             
      235                    
      236                    
      237 µmol/l             
      238 µmol/l             
      239 µmol/l             
      240 µmol/l             
      241                    
      242 µmol/l             
      243                    
      244                    
      245 µmol/l             
      246 µmol/l             
      247 µmol/l             
      248    min             
      249    min             
      250                    
      251                    
      252                    
      253                    
      254                    
      255                    
      256                    
      257                    
      258                    
      259                    
      260                    
      261                    
      262                    

---

    Code
      result$initialConditions
    Output
                                                         Container Path Molecule Name
      1                                     Organism|VenousBlood|Plasma        CYP3A4
      2                                 Organism|VenousBlood|BloodCells        CYP3A4
      3                                   Organism|ArterialBlood|Plasma        CYP3A4
      4                               Organism|ArterialBlood|BloodCells        CYP3A4
      5                                            Organism|Bone|Plasma        CYP3A4
      6                                        Organism|Bone|BloodCells        CYP3A4
      7                                      Organism|Bone|Interstitial        CYP3A4
      8                                     Organism|Bone|Intracellular        CYP3A4
      9                                          Organism|Bone|Endosome        CYP3A4
      10                                          Organism|Brain|Plasma        CYP3A4
      11                                      Organism|Brain|BloodCells        CYP3A4
      12                                    Organism|Brain|Interstitial        CYP3A4
      13                                   Organism|Brain|Intracellular        CYP3A4
      14                                        Organism|Brain|Endosome        CYP3A4
      15                                            Organism|Fat|Plasma        CYP3A4
      16                                        Organism|Fat|BloodCells        CYP3A4
      17                                      Organism|Fat|Interstitial        CYP3A4
      18                                     Organism|Fat|Intracellular        CYP3A4
      19                                          Organism|Fat|Endosome        CYP3A4
      20                                         Organism|Gonads|Plasma        CYP3A4
      21                                     Organism|Gonads|BloodCells        CYP3A4
      22                                   Organism|Gonads|Interstitial        CYP3A4
      23                                  Organism|Gonads|Intracellular        CYP3A4
      24                                       Organism|Gonads|Endosome        CYP3A4
      25                                          Organism|Heart|Plasma        CYP3A4
      26                                      Organism|Heart|BloodCells        CYP3A4
      27                                    Organism|Heart|Interstitial        CYP3A4
      28                                   Organism|Heart|Intracellular        CYP3A4
      29                                        Organism|Heart|Endosome        CYP3A4
      30                                         Organism|Kidney|Plasma        CYP3A4
      31                                     Organism|Kidney|BloodCells        CYP3A4
      32                                   Organism|Kidney|Interstitial        CYP3A4
      33                                  Organism|Kidney|Intracellular        CYP3A4
      34                                       Organism|Kidney|Endosome        CYP3A4
      35                                         Organism|Lumen|Stomach        CYP3A4
      36                                        Organism|Lumen|Duodenum        CYP3A4
      37                                    Organism|Lumen|UpperJejunum        CYP3A4
      38                                    Organism|Lumen|LowerJejunum        CYP3A4
      39                                      Organism|Lumen|UpperIleum        CYP3A4
      40                                      Organism|Lumen|LowerIleum        CYP3A4
      41                                          Organism|Lumen|Caecum        CYP3A4
      42                                  Organism|Lumen|ColonAscendens        CYP3A4
      43                                Organism|Lumen|ColonTransversum        CYP3A4
      44                                 Organism|Lumen|ColonDescendens        CYP3A4
      45                                    Organism|Lumen|ColonSigmoid        CYP3A4
      46                                          Organism|Lumen|Rectum        CYP3A4
      47                                        Organism|Stomach|Plasma        CYP3A4
      48                                    Organism|Stomach|BloodCells        CYP3A4
      49                                  Organism|Stomach|Interstitial        CYP3A4
      50                                 Organism|Stomach|Intracellular        CYP3A4
      51                                      Organism|Stomach|Endosome        CYP3A4
      52                                 Organism|SmallIntestine|Plasma        CYP3A4
      53                             Organism|SmallIntestine|BloodCells        CYP3A4
      54                           Organism|SmallIntestine|Interstitial        CYP3A4
      55                          Organism|SmallIntestine|Intracellular        CYP3A4
      56                               Organism|SmallIntestine|Endosome        CYP3A4
      57                 Organism|SmallIntestine|Mucosa|Duodenum|Plasma        CYP3A4
      58             Organism|SmallIntestine|Mucosa|Duodenum|BloodCells        CYP3A4
      59           Organism|SmallIntestine|Mucosa|Duodenum|Interstitial        CYP3A4
      60          Organism|SmallIntestine|Mucosa|Duodenum|Intracellular        CYP3A4
      61               Organism|SmallIntestine|Mucosa|Duodenum|Endosome        CYP3A4
      62             Organism|SmallIntestine|Mucosa|UpperJejunum|Plasma        CYP3A4
      63         Organism|SmallIntestine|Mucosa|UpperJejunum|BloodCells        CYP3A4
      64       Organism|SmallIntestine|Mucosa|UpperJejunum|Interstitial        CYP3A4
      65      Organism|SmallIntestine|Mucosa|UpperJejunum|Intracellular        CYP3A4
      66           Organism|SmallIntestine|Mucosa|UpperJejunum|Endosome        CYP3A4
      67             Organism|SmallIntestine|Mucosa|LowerJejunum|Plasma        CYP3A4
      68         Organism|SmallIntestine|Mucosa|LowerJejunum|BloodCells        CYP3A4
      69       Organism|SmallIntestine|Mucosa|LowerJejunum|Interstitial        CYP3A4
      70      Organism|SmallIntestine|Mucosa|LowerJejunum|Intracellular        CYP3A4
      71           Organism|SmallIntestine|Mucosa|LowerJejunum|Endosome        CYP3A4
      72               Organism|SmallIntestine|Mucosa|UpperIleum|Plasma        CYP3A4
      73           Organism|SmallIntestine|Mucosa|UpperIleum|BloodCells        CYP3A4
      74         Organism|SmallIntestine|Mucosa|UpperIleum|Interstitial        CYP3A4
      75        Organism|SmallIntestine|Mucosa|UpperIleum|Intracellular        CYP3A4
      76             Organism|SmallIntestine|Mucosa|UpperIleum|Endosome        CYP3A4
      77               Organism|SmallIntestine|Mucosa|LowerIleum|Plasma        CYP3A4
      78           Organism|SmallIntestine|Mucosa|LowerIleum|BloodCells        CYP3A4
      79         Organism|SmallIntestine|Mucosa|LowerIleum|Interstitial        CYP3A4
      80        Organism|SmallIntestine|Mucosa|LowerIleum|Intracellular        CYP3A4
      81             Organism|SmallIntestine|Mucosa|LowerIleum|Endosome        CYP3A4
      82                                 Organism|LargeIntestine|Plasma        CYP3A4
      83                             Organism|LargeIntestine|BloodCells        CYP3A4
      84                           Organism|LargeIntestine|Interstitial        CYP3A4
      85                          Organism|LargeIntestine|Intracellular        CYP3A4
      86                               Organism|LargeIntestine|Endosome        CYP3A4
      87                   Organism|LargeIntestine|Mucosa|Caecum|Plasma        CYP3A4
      88               Organism|LargeIntestine|Mucosa|Caecum|BloodCells        CYP3A4
      89             Organism|LargeIntestine|Mucosa|Caecum|Interstitial        CYP3A4
      90            Organism|LargeIntestine|Mucosa|Caecum|Intracellular        CYP3A4
      91                 Organism|LargeIntestine|Mucosa|Caecum|Endosome        CYP3A4
      92           Organism|LargeIntestine|Mucosa|ColonAscendens|Plasma        CYP3A4
      93       Organism|LargeIntestine|Mucosa|ColonAscendens|BloodCells        CYP3A4
      94     Organism|LargeIntestine|Mucosa|ColonAscendens|Interstitial        CYP3A4
      95    Organism|LargeIntestine|Mucosa|ColonAscendens|Intracellular        CYP3A4
      96         Organism|LargeIntestine|Mucosa|ColonAscendens|Endosome        CYP3A4
      97         Organism|LargeIntestine|Mucosa|ColonTransversum|Plasma        CYP3A4
      98     Organism|LargeIntestine|Mucosa|ColonTransversum|BloodCells        CYP3A4
      99   Organism|LargeIntestine|Mucosa|ColonTransversum|Interstitial        CYP3A4
      100 Organism|LargeIntestine|Mucosa|ColonTransversum|Intracellular        CYP3A4
      101      Organism|LargeIntestine|Mucosa|ColonTransversum|Endosome        CYP3A4
      102         Organism|LargeIntestine|Mucosa|ColonDescendens|Plasma        CYP3A4
      103     Organism|LargeIntestine|Mucosa|ColonDescendens|BloodCells        CYP3A4
      104   Organism|LargeIntestine|Mucosa|ColonDescendens|Interstitial        CYP3A4
      105  Organism|LargeIntestine|Mucosa|ColonDescendens|Intracellular        CYP3A4
      106       Organism|LargeIntestine|Mucosa|ColonDescendens|Endosome        CYP3A4
      107            Organism|LargeIntestine|Mucosa|ColonSigmoid|Plasma        CYP3A4
      108        Organism|LargeIntestine|Mucosa|ColonSigmoid|BloodCells        CYP3A4
      109      Organism|LargeIntestine|Mucosa|ColonSigmoid|Interstitial        CYP3A4
      110     Organism|LargeIntestine|Mucosa|ColonSigmoid|Intracellular        CYP3A4
      111          Organism|LargeIntestine|Mucosa|ColonSigmoid|Endosome        CYP3A4
      112                  Organism|LargeIntestine|Mucosa|Rectum|Plasma        CYP3A4
      113              Organism|LargeIntestine|Mucosa|Rectum|BloodCells        CYP3A4
      114            Organism|LargeIntestine|Mucosa|Rectum|Interstitial        CYP3A4
      115           Organism|LargeIntestine|Mucosa|Rectum|Intracellular        CYP3A4
      116                Organism|LargeIntestine|Mucosa|Rectum|Endosome        CYP3A4
      117                              Organism|Liver|Periportal|Plasma        CYP3A4
      118                          Organism|Liver|Periportal|BloodCells        CYP3A4
      119                        Organism|Liver|Periportal|Interstitial        CYP3A4
      120                       Organism|Liver|Periportal|Intracellular        CYP3A4
      121                            Organism|Liver|Periportal|Endosome        CYP3A4
      122                             Organism|Liver|Pericentral|Plasma        CYP3A4
      123                         Organism|Liver|Pericentral|BloodCells        CYP3A4
      124                       Organism|Liver|Pericentral|Interstitial        CYP3A4
      125                      Organism|Liver|Pericentral|Intracellular        CYP3A4
      126                           Organism|Liver|Pericentral|Endosome        CYP3A4
      127                                          Organism|Lung|Plasma        CYP3A4
      128                                      Organism|Lung|BloodCells        CYP3A4
      129                                    Organism|Lung|Interstitial        CYP3A4
      130                                   Organism|Lung|Intracellular        CYP3A4
      131                                        Organism|Lung|Endosome        CYP3A4
      132                                        Organism|Muscle|Plasma        CYP3A4
      133                                    Organism|Muscle|BloodCells        CYP3A4
      134                                  Organism|Muscle|Interstitial        CYP3A4
      135                                 Organism|Muscle|Intracellular        CYP3A4
      136                                      Organism|Muscle|Endosome        CYP3A4
      137                                      Organism|Pancreas|Plasma        CYP3A4
      138                                  Organism|Pancreas|BloodCells        CYP3A4
      139                                Organism|Pancreas|Interstitial        CYP3A4
      140                               Organism|Pancreas|Intracellular        CYP3A4
      141                                    Organism|Pancreas|Endosome        CYP3A4
      142                                    Organism|PortalVein|Plasma        CYP3A4
      143                                Organism|PortalVein|BloodCells        CYP3A4
      144                                          Organism|Skin|Plasma        CYP3A4
      145                                      Organism|Skin|BloodCells        CYP3A4
      146                                    Organism|Skin|Interstitial        CYP3A4
      147                                   Organism|Skin|Intracellular        CYP3A4
      148                                        Organism|Skin|Endosome        CYP3A4
      149                                        Organism|Spleen|Plasma        CYP3A4
      150                                    Organism|Spleen|BloodCells        CYP3A4
      151                                  Organism|Spleen|Interstitial        CYP3A4
      152                                 Organism|Spleen|Intracellular        CYP3A4
      153                                      Organism|Spleen|Endosome        CYP3A4
          Is Present Value Unit Scale Divisor Neg. Values Allowed
      1         TRUE   NaN µmol             1               FALSE
      2         TRUE   NaN µmol             1               FALSE
      3         TRUE   NaN µmol             1               FALSE
      4         TRUE   NaN µmol             1               FALSE
      5         TRUE   NaN µmol             1               FALSE
      6         TRUE   NaN µmol             1               FALSE
      7         TRUE   NaN µmol             1               FALSE
      8         TRUE   NaN µmol             1               FALSE
      9         TRUE   NaN µmol             1               FALSE
      10        TRUE   NaN µmol             1               FALSE
      11        TRUE   NaN µmol             1               FALSE
      12        TRUE   NaN µmol             1               FALSE
      13        TRUE   NaN µmol             1               FALSE
      14        TRUE   NaN µmol             1               FALSE
      15        TRUE   NaN µmol             1               FALSE
      16        TRUE   NaN µmol             1               FALSE
      17        TRUE   NaN µmol             1               FALSE
      18        TRUE   NaN µmol             1               FALSE
      19        TRUE   NaN µmol             1               FALSE
      20        TRUE   NaN µmol             1               FALSE
      21        TRUE   NaN µmol             1               FALSE
      22        TRUE   NaN µmol             1               FALSE
      23        TRUE   NaN µmol             1               FALSE
      24        TRUE   NaN µmol             1               FALSE
      25        TRUE   NaN µmol             1               FALSE
      26        TRUE   NaN µmol             1               FALSE
      27        TRUE   NaN µmol             1               FALSE
      28        TRUE   NaN µmol             1               FALSE
      29        TRUE   NaN µmol             1               FALSE
      30        TRUE   NaN µmol             1               FALSE
      31        TRUE   NaN µmol             1               FALSE
      32        TRUE   NaN µmol             1               FALSE
      33        TRUE   NaN µmol             1               FALSE
      34        TRUE   NaN µmol             1               FALSE
      35        TRUE   NaN µmol             1               FALSE
      36        TRUE   NaN µmol             1               FALSE
      37        TRUE   NaN µmol             1               FALSE
      38        TRUE   NaN µmol             1               FALSE
      39        TRUE   NaN µmol             1               FALSE
      40        TRUE   NaN µmol             1               FALSE
      41        TRUE   NaN µmol             1               FALSE
      42        TRUE   NaN µmol             1               FALSE
      43        TRUE   NaN µmol             1               FALSE
      44        TRUE   NaN µmol             1               FALSE
      45        TRUE   NaN µmol             1               FALSE
      46        TRUE   NaN µmol             1               FALSE
      47        TRUE   NaN µmol             1               FALSE
      48        TRUE   NaN µmol             1               FALSE
      49        TRUE   NaN µmol             1               FALSE
      50        TRUE   NaN µmol             1               FALSE
      51        TRUE   NaN µmol             1               FALSE
      52        TRUE   NaN µmol             1               FALSE
      53        TRUE   NaN µmol             1               FALSE
      54        TRUE   NaN µmol             1               FALSE
      55        TRUE   NaN µmol             1               FALSE
      56        TRUE   NaN µmol             1               FALSE
      57        TRUE   NaN µmol             1               FALSE
      58        TRUE   NaN µmol             1               FALSE
      59        TRUE   NaN µmol             1               FALSE
      60        TRUE   NaN µmol             1               FALSE
      61        TRUE   NaN µmol             1               FALSE
      62        TRUE   NaN µmol             1               FALSE
      63        TRUE   NaN µmol             1               FALSE
      64        TRUE   NaN µmol             1               FALSE
      65        TRUE   NaN µmol             1               FALSE
      66        TRUE   NaN µmol             1               FALSE
      67        TRUE   NaN µmol             1               FALSE
      68        TRUE   NaN µmol             1               FALSE
      69        TRUE   NaN µmol             1               FALSE
      70        TRUE   NaN µmol             1               FALSE
      71        TRUE   NaN µmol             1               FALSE
      72        TRUE   NaN µmol             1               FALSE
      73        TRUE   NaN µmol             1               FALSE
      74        TRUE   NaN µmol             1               FALSE
      75        TRUE   NaN µmol             1               FALSE
      76        TRUE   NaN µmol             1               FALSE
      77        TRUE   NaN µmol             1               FALSE
      78        TRUE   NaN µmol             1               FALSE
      79        TRUE   NaN µmol             1               FALSE
      80        TRUE   NaN µmol             1               FALSE
      81        TRUE   NaN µmol             1               FALSE
      82        TRUE   NaN µmol             1               FALSE
      83        TRUE   NaN µmol             1               FALSE
      84        TRUE   NaN µmol             1               FALSE
      85        TRUE   NaN µmol             1               FALSE
      86        TRUE   NaN µmol             1               FALSE
      87        TRUE   NaN µmol             1               FALSE
      88        TRUE   NaN µmol             1               FALSE
      89        TRUE   NaN µmol             1               FALSE
      90        TRUE   NaN µmol             1               FALSE
      91        TRUE   NaN µmol             1               FALSE
      92        TRUE   NaN µmol             1               FALSE
      93        TRUE   NaN µmol             1               FALSE
      94        TRUE   NaN µmol             1               FALSE
      95        TRUE   NaN µmol             1               FALSE
      96        TRUE   NaN µmol             1               FALSE
      97        TRUE   NaN µmol             1               FALSE
      98        TRUE   NaN µmol             1               FALSE
      99        TRUE   NaN µmol             1               FALSE
      100       TRUE   NaN µmol             1               FALSE
      101       TRUE   NaN µmol             1               FALSE
      102       TRUE   NaN µmol             1               FALSE
      103       TRUE   NaN µmol             1               FALSE
      104       TRUE   NaN µmol             1               FALSE
      105       TRUE   NaN µmol             1               FALSE
      106       TRUE   NaN µmol             1               FALSE
      107       TRUE   NaN µmol             1               FALSE
      108       TRUE   NaN µmol             1               FALSE
      109       TRUE   NaN µmol             1               FALSE
      110       TRUE   NaN µmol             1               FALSE
      111       TRUE   NaN µmol             1               FALSE
      112       TRUE   NaN µmol             1               FALSE
      113       TRUE   NaN µmol             1               FALSE
      114       TRUE   NaN µmol             1               FALSE
      115       TRUE   NaN µmol             1               FALSE
      116       TRUE   NaN µmol             1               FALSE
      117       TRUE   NaN µmol             1               FALSE
      118       TRUE   NaN µmol             1               FALSE
      119       TRUE   NaN µmol             1               FALSE
      120       TRUE   NaN µmol             1               FALSE
      121       TRUE   NaN µmol             1               FALSE
      122       TRUE   NaN µmol             1               FALSE
      123       TRUE   NaN µmol             1               FALSE
      124       TRUE   NaN µmol             1               FALSE
      125       TRUE   NaN µmol             1               FALSE
      126       TRUE   NaN µmol             1               FALSE
      127       TRUE   NaN µmol             1               FALSE
      128       TRUE   NaN µmol             1               FALSE
      129       TRUE   NaN µmol             1               FALSE
      130       TRUE   NaN µmol             1               FALSE
      131       TRUE   NaN µmol             1               FALSE
      132       TRUE   NaN µmol             1               FALSE
      133       TRUE   NaN µmol             1               FALSE
      134       TRUE   NaN µmol             1               FALSE
      135       TRUE   NaN µmol             1               FALSE
      136       TRUE   NaN µmol             1               FALSE
      137       TRUE   NaN µmol             1               FALSE
      138       TRUE   NaN µmol             1               FALSE
      139       TRUE   NaN µmol             1               FALSE
      140       TRUE   NaN µmol             1               FALSE
      141       TRUE   NaN µmol             1               FALSE
      142       TRUE   NaN µmol             1               FALSE
      143       TRUE   NaN µmol             1               FALSE
      144       TRUE   NaN µmol             1               FALSE
      145       TRUE   NaN µmol             1               FALSE
      146       TRUE   NaN µmol             1               FALSE
      147       TRUE   NaN µmol             1               FALSE
      148       TRUE   NaN µmol             1               FALSE
      149       TRUE   NaN µmol             1               FALSE
      150       TRUE   NaN µmol             1               FALSE
      151       TRUE   NaN µmol             1               FALSE
      152       TRUE   NaN µmol             1               FALSE
      153       TRUE   NaN µmol             1               FALSE

# createExpressionProfileBuildingBlock creates a building block

    Code
      expressionProfile
    Output
      <BuildingBlock>
        * Name: CYP3A4|Human|healthy
        * Type: Expression Profile
        * Protein type: healthy
        * Molecule: CYP3A4
        * Species: Human

