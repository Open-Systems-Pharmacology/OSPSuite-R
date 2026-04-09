# initialConditionsBBToDataFrame returns a data frame with the expected columns

    Code
      df
    Output
                                                         Container Path
      1                                            Organism|Gallbladder
      2                                     Organism|VenousBlood|Plasma
      3                                 Organism|VenousBlood|BloodCells
      4                                   Organism|ArterialBlood|Plasma
      5                               Organism|ArterialBlood|BloodCells
      6                                            Organism|Bone|Plasma
      7                                        Organism|Bone|BloodCells
      8                                      Organism|Bone|Interstitial
      9                                     Organism|Bone|Intracellular
      10                                          Organism|Brain|Plasma
      11                                      Organism|Brain|BloodCells
      12                                    Organism|Brain|Interstitial
      13                                   Organism|Brain|Intracellular
      14                                            Organism|Fat|Plasma
      15                                        Organism|Fat|BloodCells
      16                                      Organism|Fat|Interstitial
      17                                     Organism|Fat|Intracellular
      18                                         Organism|Gonads|Plasma
      19                                     Organism|Gonads|BloodCells
      20                                   Organism|Gonads|Interstitial
      21                                  Organism|Gonads|Intracellular
      22                                          Organism|Heart|Plasma
      23                                      Organism|Heart|BloodCells
      24                                    Organism|Heart|Interstitial
      25                                   Organism|Heart|Intracellular
      26                                         Organism|Kidney|Plasma
      27                                     Organism|Kidney|BloodCells
      28                                   Organism|Kidney|Interstitial
      29                                  Organism|Kidney|Intracellular
      30                                          Organism|Kidney|Urine
      31                                         Organism|Lumen|Stomach
      32                                        Organism|Lumen|Duodenum
      33                                    Organism|Lumen|UpperJejunum
      34                                    Organism|Lumen|LowerJejunum
      35                                      Organism|Lumen|UpperIleum
      36                                      Organism|Lumen|LowerIleum
      37                                          Organism|Lumen|Caecum
      38                                  Organism|Lumen|ColonAscendens
      39                                Organism|Lumen|ColonTransversum
      40                                 Organism|Lumen|ColonDescendens
      41                                    Organism|Lumen|ColonSigmoid
      42                                          Organism|Lumen|Rectum
      43                                           Organism|Lumen|Feces
      44                                        Organism|Stomach|Plasma
      45                                    Organism|Stomach|BloodCells
      46                                  Organism|Stomach|Interstitial
      47                                 Organism|Stomach|Intracellular
      48                                 Organism|SmallIntestine|Plasma
      49                             Organism|SmallIntestine|BloodCells
      50                           Organism|SmallIntestine|Interstitial
      51                          Organism|SmallIntestine|Intracellular
      52                 Organism|SmallIntestine|Mucosa|Duodenum|Plasma
      53             Organism|SmallIntestine|Mucosa|Duodenum|BloodCells
      54           Organism|SmallIntestine|Mucosa|Duodenum|Interstitial
      55          Organism|SmallIntestine|Mucosa|Duodenum|Intracellular
      56             Organism|SmallIntestine|Mucosa|UpperJejunum|Plasma
      57         Organism|SmallIntestine|Mucosa|UpperJejunum|BloodCells
      58       Organism|SmallIntestine|Mucosa|UpperJejunum|Interstitial
      59      Organism|SmallIntestine|Mucosa|UpperJejunum|Intracellular
      60             Organism|SmallIntestine|Mucosa|LowerJejunum|Plasma
      61         Organism|SmallIntestine|Mucosa|LowerJejunum|BloodCells
      62       Organism|SmallIntestine|Mucosa|LowerJejunum|Interstitial
      63      Organism|SmallIntestine|Mucosa|LowerJejunum|Intracellular
      64               Organism|SmallIntestine|Mucosa|UpperIleum|Plasma
      65           Organism|SmallIntestine|Mucosa|UpperIleum|BloodCells
      66         Organism|SmallIntestine|Mucosa|UpperIleum|Interstitial
      67        Organism|SmallIntestine|Mucosa|UpperIleum|Intracellular
      68               Organism|SmallIntestine|Mucosa|LowerIleum|Plasma
      69           Organism|SmallIntestine|Mucosa|LowerIleum|BloodCells
      70         Organism|SmallIntestine|Mucosa|LowerIleum|Interstitial
      71        Organism|SmallIntestine|Mucosa|LowerIleum|Intracellular
      72                                 Organism|LargeIntestine|Plasma
      73                             Organism|LargeIntestine|BloodCells
      74                           Organism|LargeIntestine|Interstitial
      75                          Organism|LargeIntestine|Intracellular
      76                   Organism|LargeIntestine|Mucosa|Caecum|Plasma
      77               Organism|LargeIntestine|Mucosa|Caecum|BloodCells
      78             Organism|LargeIntestine|Mucosa|Caecum|Interstitial
      79            Organism|LargeIntestine|Mucosa|Caecum|Intracellular
      80           Organism|LargeIntestine|Mucosa|ColonAscendens|Plasma
      81       Organism|LargeIntestine|Mucosa|ColonAscendens|BloodCells
      82     Organism|LargeIntestine|Mucosa|ColonAscendens|Interstitial
      83    Organism|LargeIntestine|Mucosa|ColonAscendens|Intracellular
      84         Organism|LargeIntestine|Mucosa|ColonTransversum|Plasma
      85     Organism|LargeIntestine|Mucosa|ColonTransversum|BloodCells
      86   Organism|LargeIntestine|Mucosa|ColonTransversum|Interstitial
      87  Organism|LargeIntestine|Mucosa|ColonTransversum|Intracellular
      88          Organism|LargeIntestine|Mucosa|ColonDescendens|Plasma
      89      Organism|LargeIntestine|Mucosa|ColonDescendens|BloodCells
      90    Organism|LargeIntestine|Mucosa|ColonDescendens|Interstitial
      91   Organism|LargeIntestine|Mucosa|ColonDescendens|Intracellular
      92             Organism|LargeIntestine|Mucosa|ColonSigmoid|Plasma
      93         Organism|LargeIntestine|Mucosa|ColonSigmoid|BloodCells
      94       Organism|LargeIntestine|Mucosa|ColonSigmoid|Interstitial
      95      Organism|LargeIntestine|Mucosa|ColonSigmoid|Intracellular
      96                   Organism|LargeIntestine|Mucosa|Rectum|Plasma
      97               Organism|LargeIntestine|Mucosa|Rectum|BloodCells
      98             Organism|LargeIntestine|Mucosa|Rectum|Interstitial
      99            Organism|LargeIntestine|Mucosa|Rectum|Intracellular
      100                              Organism|Liver|Periportal|Plasma
      101                          Organism|Liver|Periportal|BloodCells
      102                        Organism|Liver|Periportal|Interstitial
      103                       Organism|Liver|Periportal|Intracellular
      104                       Organism|Liver|Periportal|Intracellular
      105                       Organism|Liver|Periportal|Intracellular
      106                             Organism|Liver|Pericentral|Plasma
      107                         Organism|Liver|Pericentral|BloodCells
      108                       Organism|Liver|Pericentral|Interstitial
      109                      Organism|Liver|Pericentral|Intracellular
      110                      Organism|Liver|Pericentral|Intracellular
      111                      Organism|Liver|Pericentral|Intracellular
      112                                          Organism|Lung|Plasma
      113                                      Organism|Lung|BloodCells
      114                                    Organism|Lung|Interstitial
      115                                   Organism|Lung|Intracellular
      116                                        Organism|Muscle|Plasma
      117                                    Organism|Muscle|BloodCells
      118                                  Organism|Muscle|Interstitial
      119                                 Organism|Muscle|Intracellular
      120                                      Organism|Pancreas|Plasma
      121                                  Organism|Pancreas|BloodCells
      122                                Organism|Pancreas|Interstitial
      123                               Organism|Pancreas|Intracellular
      124                                    Organism|PortalVein|Plasma
      125                                Organism|PortalVein|BloodCells
      126                                          Organism|Skin|Plasma
      127                                      Organism|Skin|BloodCells
      128                                    Organism|Skin|Interstitial
      129                                   Organism|Skin|Intracellular
      130                                        Organism|Spleen|Plasma
      131                                    Organism|Spleen|BloodCells
      132                                  Organism|Spleen|Interstitial
      133                                 Organism|Spleen|Intracellular
      134                                        Organism|Saliva|Saliva
      135                                   Organism|Saliva|SalivaGland
                                 Molecule Name Is Present Value Unit Scale Divisor
      1                              Aciclovir       TRUE     0 µmol             1
      2                              Aciclovir       TRUE     0 µmol             1
      3                              Aciclovir       TRUE     0 µmol             1
      4                              Aciclovir       TRUE     0 µmol             1
      5                              Aciclovir       TRUE     0 µmol             1
      6                              Aciclovir       TRUE     0 µmol             1
      7                              Aciclovir       TRUE     0 µmol             1
      8                              Aciclovir       TRUE     0 µmol             1
      9                              Aciclovir       TRUE     0 µmol             1
      10                             Aciclovir       TRUE     0 µmol             1
      11                             Aciclovir       TRUE     0 µmol             1
      12                             Aciclovir       TRUE     0 µmol             1
      13                             Aciclovir       TRUE     0 µmol             1
      14                             Aciclovir       TRUE     0 µmol             1
      15                             Aciclovir       TRUE     0 µmol             1
      16                             Aciclovir       TRUE     0 µmol             1
      17                             Aciclovir       TRUE     0 µmol             1
      18                             Aciclovir       TRUE     0 µmol             1
      19                             Aciclovir       TRUE     0 µmol             1
      20                             Aciclovir       TRUE     0 µmol             1
      21                             Aciclovir       TRUE     0 µmol             1
      22                             Aciclovir       TRUE     0 µmol             1
      23                             Aciclovir       TRUE     0 µmol             1
      24                             Aciclovir       TRUE     0 µmol             1
      25                             Aciclovir       TRUE     0 µmol             1
      26                             Aciclovir       TRUE     0 µmol             1
      27                             Aciclovir       TRUE     0 µmol             1
      28                             Aciclovir       TRUE     0 µmol             1
      29                             Aciclovir       TRUE     0 µmol             1
      30                             Aciclovir       TRUE     0 µmol             1
      31                             Aciclovir       TRUE     0 µmol             1
      32                             Aciclovir       TRUE     0 µmol             1
      33                             Aciclovir       TRUE     0 µmol             1
      34                             Aciclovir       TRUE     0 µmol             1
      35                             Aciclovir       TRUE     0 µmol             1
      36                             Aciclovir       TRUE     0 µmol             1
      37                             Aciclovir       TRUE     0 µmol             1
      38                             Aciclovir       TRUE     0 µmol             1
      39                             Aciclovir       TRUE     0 µmol             1
      40                             Aciclovir       TRUE     0 µmol             1
      41                             Aciclovir       TRUE     0 µmol             1
      42                             Aciclovir       TRUE     0 µmol             1
      43                             Aciclovir       TRUE     0 µmol             1
      44                             Aciclovir       TRUE     0 µmol             1
      45                             Aciclovir       TRUE     0 µmol             1
      46                             Aciclovir       TRUE     0 µmol             1
      47                             Aciclovir       TRUE     0 µmol             1
      48                             Aciclovir       TRUE     0 µmol             1
      49                             Aciclovir       TRUE     0 µmol             1
      50                             Aciclovir       TRUE     0 µmol             1
      51                             Aciclovir       TRUE     0 µmol             1
      52                             Aciclovir       TRUE     0 µmol             1
      53                             Aciclovir       TRUE     0 µmol             1
      54                             Aciclovir       TRUE     0 µmol             1
      55                             Aciclovir       TRUE     0 µmol             1
      56                             Aciclovir       TRUE     0 µmol             1
      57                             Aciclovir       TRUE     0 µmol             1
      58                             Aciclovir       TRUE     0 µmol             1
      59                             Aciclovir       TRUE     0 µmol             1
      60                             Aciclovir       TRUE     0 µmol             1
      61                             Aciclovir       TRUE     0 µmol             1
      62                             Aciclovir       TRUE     0 µmol             1
      63                             Aciclovir       TRUE     0 µmol             1
      64                             Aciclovir       TRUE     0 µmol             1
      65                             Aciclovir       TRUE     0 µmol             1
      66                             Aciclovir       TRUE     0 µmol             1
      67                             Aciclovir       TRUE     0 µmol             1
      68                             Aciclovir       TRUE     0 µmol             1
      69                             Aciclovir       TRUE     0 µmol             1
      70                             Aciclovir       TRUE     0 µmol             1
      71                             Aciclovir       TRUE     0 µmol             1
      72                             Aciclovir       TRUE     0 µmol             1
      73                             Aciclovir       TRUE     0 µmol             1
      74                             Aciclovir       TRUE     0 µmol             1
      75                             Aciclovir       TRUE     0 µmol             1
      76                             Aciclovir       TRUE     0 µmol             1
      77                             Aciclovir       TRUE     0 µmol             1
      78                             Aciclovir       TRUE     0 µmol             1
      79                             Aciclovir       TRUE     0 µmol             1
      80                             Aciclovir       TRUE     0 µmol             1
      81                             Aciclovir       TRUE     0 µmol             1
      82                             Aciclovir       TRUE     0 µmol             1
      83                             Aciclovir       TRUE     0 µmol             1
      84                             Aciclovir       TRUE     0 µmol             1
      85                             Aciclovir       TRUE     0 µmol             1
      86                             Aciclovir       TRUE     0 µmol             1
      87                             Aciclovir       TRUE     0 µmol             1
      88                             Aciclovir       TRUE     0 µmol             1
      89                             Aciclovir       TRUE     0 µmol             1
      90                             Aciclovir       TRUE     0 µmol             1
      91                             Aciclovir       TRUE     0 µmol             1
      92                             Aciclovir       TRUE     0 µmol             1
      93                             Aciclovir       TRUE     0 µmol             1
      94                             Aciclovir       TRUE     0 µmol             1
      95                             Aciclovir       TRUE     0 µmol             1
      96                             Aciclovir       TRUE     0 µmol             1
      97                             Aciclovir       TRUE     0 µmol             1
      98                             Aciclovir       TRUE     0 µmol             1
      99                             Aciclovir       TRUE     0 µmol             1
      100                            Aciclovir       TRUE     0 µmol             1
      101                            Aciclovir       TRUE     0 µmol             1
      102                            Aciclovir       TRUE     0 µmol             1
      103                      Undefined Liver       TRUE   NaN µmol             1
      104                            Aciclovir       TRUE     0 µmol             1
      105 Aciclovir-Undefined Liver Metabolite       TRUE     0 µmol             1
      106                            Aciclovir       TRUE     0 µmol             1
      107                            Aciclovir       TRUE     0 µmol             1
      108                            Aciclovir       TRUE     0 µmol             1
      109                      Undefined Liver       TRUE   NaN µmol             1
      110                            Aciclovir       TRUE     0 µmol             1
      111 Aciclovir-Undefined Liver Metabolite       TRUE     0 µmol             1
      112                            Aciclovir       TRUE     0 µmol             1
      113                            Aciclovir       TRUE     0 µmol             1
      114                            Aciclovir       TRUE     0 µmol             1
      115                            Aciclovir       TRUE     0 µmol             1
      116                            Aciclovir       TRUE     0 µmol             1
      117                            Aciclovir       TRUE     0 µmol             1
      118                            Aciclovir       TRUE     0 µmol             1
      119                            Aciclovir       TRUE     0 µmol             1
      120                            Aciclovir       TRUE     0 µmol             1
      121                            Aciclovir       TRUE     0 µmol             1
      122                            Aciclovir       TRUE     0 µmol             1
      123                            Aciclovir       TRUE     0 µmol             1
      124                            Aciclovir       TRUE     0 µmol             1
      125                            Aciclovir       TRUE     0 µmol             1
      126                            Aciclovir       TRUE     0 µmol             1
      127                            Aciclovir       TRUE     0 µmol             1
      128                            Aciclovir       TRUE     0 µmol             1
      129                            Aciclovir       TRUE     0 µmol             1
      130                            Aciclovir       TRUE     0 µmol             1
      131                            Aciclovir       TRUE     0 µmol             1
      132                            Aciclovir       TRUE     0 µmol             1
      133                            Aciclovir       TRUE     0 µmol             1
      134                            Aciclovir       TRUE     0 µmol             1
      135                            Aciclovir       TRUE     0 µmol             1
          Neg. Values Allowed
      1                 FALSE
      2                 FALSE
      3                 FALSE
      4                 FALSE
      5                 FALSE
      6                 FALSE
      7                 FALSE
      8                 FALSE
      9                 FALSE
      10                FALSE
      11                FALSE
      12                FALSE
      13                FALSE
      14                FALSE
      15                FALSE
      16                FALSE
      17                FALSE
      18                FALSE
      19                FALSE
      20                FALSE
      21                FALSE
      22                FALSE
      23                FALSE
      24                FALSE
      25                FALSE
      26                FALSE
      27                FALSE
      28                FALSE
      29                FALSE
      30                FALSE
      31                FALSE
      32                FALSE
      33                FALSE
      34                FALSE
      35                FALSE
      36                FALSE
      37                FALSE
      38                FALSE
      39                FALSE
      40                FALSE
      41                FALSE
      42                FALSE
      43                FALSE
      44                FALSE
      45                FALSE
      46                FALSE
      47                FALSE
      48                FALSE
      49                FALSE
      50                FALSE
      51                FALSE
      52                FALSE
      53                FALSE
      54                FALSE
      55                FALSE
      56                FALSE
      57                FALSE
      58                FALSE
      59                FALSE
      60                FALSE
      61                FALSE
      62                FALSE
      63                FALSE
      64                FALSE
      65                FALSE
      66                FALSE
      67                FALSE
      68                FALSE
      69                FALSE
      70                FALSE
      71                FALSE
      72                FALSE
      73                FALSE
      74                FALSE
      75                FALSE
      76                FALSE
      77                FALSE
      78                FALSE
      79                FALSE
      80                FALSE
      81                FALSE
      82                FALSE
      83                FALSE
      84                FALSE
      85                FALSE
      86                FALSE
      87                FALSE
      88                FALSE
      89                FALSE
      90                FALSE
      91                FALSE
      92                FALSE
      93                FALSE
      94                FALSE
      95                FALSE
      96                FALSE
      97                FALSE
      98                FALSE
      99                FALSE
      100               FALSE
      101               FALSE
      102               FALSE
      103               FALSE
      104               FALSE
      105               FALSE
      106               FALSE
      107               FALSE
      108               FALSE
      109               FALSE
      110               FALSE
      111               FALSE
      112               FALSE
      113               FALSE
      114               FALSE
      115               FALSE
      116               FALSE
      117               FALSE
      118               FALSE
      119               FALSE
      120               FALSE
      121               FALSE
      122               FALSE
      123               FALSE
      124               FALSE
      125               FALSE
      126               FALSE
      127               FALSE
      128               FALSE
      129               FALSE
      130               FALSE
      131               FALSE
      132               FALSE
      133               FALSE
      134               FALSE
      135                TRUE

# parameterValuesBBToDataFrame returns a data frame with the expected columns

    Code
      df
    Output
                                                   Container Path
      1                                           Undefined Liver
      2                                           Undefined Liver
      3                                           Undefined Liver
      4                                           Undefined Liver
      5   Organism|Liver|Periportal|Intracellular|Undefined Liver
      6   Organism|Liver|Periportal|Intracellular|Undefined Liver
      7   Organism|Liver|Periportal|Intracellular|Undefined Liver
      8  Organism|Liver|Pericentral|Intracellular|Undefined Liver
      9  Organism|Liver|Pericentral|Intracellular|Undefined Liver
      10 Organism|Liver|Pericentral|Intracellular|Undefined Liver
                           Parameter Name      Value   Unit
      1           Reference concentration    1.00000 µmol/l
      2                      t1/2 (liver) 2160.00000    min
      3                  t1/2 (intestine) 1380.00000    min
      4                    Disease factor    1.00000       
      5               Relative expression    1.00000       
      6  Fraction expressed intracellular    1.00000       
      7             Initial concentration    1.49925 µmol/l
      8               Relative expression    1.00000       
      9  Fraction expressed intracellular    1.00000       
      10            Initial concentration    1.49925 µmol/l

# extendInitialConditionsBB extends with all molecules if moleculeNames is NULL

    Code
      newPaths_df
    Output
                           Container Path                  Molecule Name Is Present
      1505        Organism|Thyroid|Plasma                           DIO1       TRUE
      1506        Organism|Thyroid|Plasma                         UDPGT1       TRUE
      1507        Organism|Thyroid|Plasma                         UDPGT2       TRUE
      1508        Organism|Thyroid|Plasma                           DIO3       TRUE
      1509        Organism|Thyroid|Plasma                Undefined Liver       TRUE
      1510        Organism|Thyroid|Plasma                             T3       TRUE
      1511        Organism|Thyroid|Plasma                             T4       TRUE
      1512        Organism|Thyroid|Plasma                            TSH       TRUE
      1513        Organism|Thyroid|Plasma                T3-FcRn_Complex       TRUE
      1514        Organism|Thyroid|Plasma                     LigandEndo       TRUE
      1515        Organism|Thyroid|Plasma                           FcRn       TRUE
      1516        Organism|Thyroid|Plasma             LigandEndo_Complex       TRUE
      1517        Organism|Thyroid|Plasma                T4-FcRn_Complex       TRUE
      1518        Organism|Thyroid|Plasma               TSH-FcRn_Complex       TRUE
      1519        Organism|Thyroid|Plasma  T3-DIO1-Assumption Metabolite       TRUE
      1520        Organism|Thyroid|Plasma        T3-UGT2B4-NA Metabolite       TRUE
      1521        Organism|Thyroid|Plasma  T3-DIO3-Assumption Metabolite       TRUE
      1522        Organism|Thyroid|Plasma  T3-Undefined Liver Metabolite       TRUE
      1523        Organism|Thyroid|Plasma  T4-DIO1-Assumption Metabolite       TRUE
      1524        Organism|Thyroid|Plasma        T4-UDPGT1-NA Metabolite       TRUE
      1525        Organism|Thyroid|Plasma  T4-Undefined Liver Metabolite       TRUE
      1526        Organism|Thyroid|Plasma TSH-Undefined Liver Metabolite       TRUE
      1527    Organism|Thyroid|BloodCells                           DIO1       TRUE
      1528    Organism|Thyroid|BloodCells                         UDPGT1       TRUE
      1529    Organism|Thyroid|BloodCells                         UDPGT2       TRUE
      1530    Organism|Thyroid|BloodCells                           DIO3       TRUE
      1531    Organism|Thyroid|BloodCells                Undefined Liver       TRUE
      1532    Organism|Thyroid|BloodCells                             T3       TRUE
      1533    Organism|Thyroid|BloodCells                             T4       TRUE
      1534    Organism|Thyroid|BloodCells                            TSH       TRUE
      1535    Organism|Thyroid|BloodCells                T3-FcRn_Complex       TRUE
      1536    Organism|Thyroid|BloodCells                     LigandEndo       TRUE
      1537    Organism|Thyroid|BloodCells                           FcRn       TRUE
      1538    Organism|Thyroid|BloodCells             LigandEndo_Complex       TRUE
      1539    Organism|Thyroid|BloodCells                T4-FcRn_Complex       TRUE
      1540    Organism|Thyroid|BloodCells               TSH-FcRn_Complex       TRUE
      1541    Organism|Thyroid|BloodCells  T3-DIO1-Assumption Metabolite       TRUE
      1542    Organism|Thyroid|BloodCells        T3-UGT2B4-NA Metabolite       TRUE
      1543    Organism|Thyroid|BloodCells  T3-DIO3-Assumption Metabolite       TRUE
      1544    Organism|Thyroid|BloodCells  T3-Undefined Liver Metabolite       TRUE
      1545    Organism|Thyroid|BloodCells  T4-DIO1-Assumption Metabolite       TRUE
      1546    Organism|Thyroid|BloodCells        T4-UDPGT1-NA Metabolite       TRUE
      1547    Organism|Thyroid|BloodCells  T4-Undefined Liver Metabolite       TRUE
      1548    Organism|Thyroid|BloodCells TSH-Undefined Liver Metabolite       TRUE
      1549  Organism|Thyroid|Interstitial                           DIO1       TRUE
      1550  Organism|Thyroid|Interstitial                         UDPGT1       TRUE
      1551  Organism|Thyroid|Interstitial                         UDPGT2       TRUE
      1552  Organism|Thyroid|Interstitial                           DIO3       TRUE
      1553  Organism|Thyroid|Interstitial                Undefined Liver       TRUE
      1554  Organism|Thyroid|Interstitial                             T3       TRUE
      1555  Organism|Thyroid|Interstitial                             T4       TRUE
      1556  Organism|Thyroid|Interstitial                            TSH       TRUE
      1557  Organism|Thyroid|Interstitial                T3-FcRn_Complex       TRUE
      1558  Organism|Thyroid|Interstitial                     LigandEndo       TRUE
      1559  Organism|Thyroid|Interstitial                           FcRn       TRUE
      1560  Organism|Thyroid|Interstitial             LigandEndo_Complex       TRUE
      1561  Organism|Thyroid|Interstitial                T4-FcRn_Complex       TRUE
      1562  Organism|Thyroid|Interstitial               TSH-FcRn_Complex       TRUE
      1563  Organism|Thyroid|Interstitial  T3-DIO1-Assumption Metabolite       TRUE
      1564  Organism|Thyroid|Interstitial        T3-UGT2B4-NA Metabolite       TRUE
      1565  Organism|Thyroid|Interstitial  T3-DIO3-Assumption Metabolite       TRUE
      1566  Organism|Thyroid|Interstitial  T3-Undefined Liver Metabolite       TRUE
      1567  Organism|Thyroid|Interstitial  T4-DIO1-Assumption Metabolite       TRUE
      1568  Organism|Thyroid|Interstitial        T4-UDPGT1-NA Metabolite       TRUE
      1569  Organism|Thyroid|Interstitial  T4-Undefined Liver Metabolite       TRUE
      1570  Organism|Thyroid|Interstitial TSH-Undefined Liver Metabolite       TRUE
      1571 Organism|Thyroid|Intracellular                           DIO1       TRUE
      1572 Organism|Thyroid|Intracellular                         UDPGT1       TRUE
      1573 Organism|Thyroid|Intracellular                         UDPGT2       TRUE
      1574 Organism|Thyroid|Intracellular                           DIO3       TRUE
      1575 Organism|Thyroid|Intracellular                Undefined Liver       TRUE
      1576 Organism|Thyroid|Intracellular                             T3       TRUE
      1577 Organism|Thyroid|Intracellular                             T4       TRUE
      1578 Organism|Thyroid|Intracellular                            TSH       TRUE
      1579 Organism|Thyroid|Intracellular                T3-FcRn_Complex       TRUE
      1580 Organism|Thyroid|Intracellular                     LigandEndo       TRUE
      1581 Organism|Thyroid|Intracellular                           FcRn       TRUE
      1582 Organism|Thyroid|Intracellular             LigandEndo_Complex       TRUE
      1583 Organism|Thyroid|Intracellular                T4-FcRn_Complex       TRUE
      1584 Organism|Thyroid|Intracellular               TSH-FcRn_Complex       TRUE
      1585 Organism|Thyroid|Intracellular  T3-DIO1-Assumption Metabolite       TRUE
      1586 Organism|Thyroid|Intracellular        T3-UGT2B4-NA Metabolite       TRUE
      1587 Organism|Thyroid|Intracellular  T3-DIO3-Assumption Metabolite       TRUE
      1588 Organism|Thyroid|Intracellular  T3-Undefined Liver Metabolite       TRUE
      1589 Organism|Thyroid|Intracellular  T4-DIO1-Assumption Metabolite       TRUE
      1590 Organism|Thyroid|Intracellular        T4-UDPGT1-NA Metabolite       TRUE
      1591 Organism|Thyroid|Intracellular  T4-Undefined Liver Metabolite       TRUE
      1592 Organism|Thyroid|Intracellular TSH-Undefined Liver Metabolite       TRUE
      1593      Organism|Thyroid|Endosome                           DIO1       TRUE
      1594      Organism|Thyroid|Endosome                         UDPGT1       TRUE
      1595      Organism|Thyroid|Endosome                         UDPGT2       TRUE
      1596      Organism|Thyroid|Endosome                           DIO3       TRUE
      1597      Organism|Thyroid|Endosome                Undefined Liver       TRUE
      1598      Organism|Thyroid|Endosome                             T3       TRUE
      1599      Organism|Thyroid|Endosome                             T4       TRUE
      1600      Organism|Thyroid|Endosome                            TSH       TRUE
      1601      Organism|Thyroid|Endosome                T3-FcRn_Complex       TRUE
      1602      Organism|Thyroid|Endosome                     LigandEndo       TRUE
      1603      Organism|Thyroid|Endosome                           FcRn       TRUE
      1604      Organism|Thyroid|Endosome             LigandEndo_Complex       TRUE
      1605      Organism|Thyroid|Endosome                T4-FcRn_Complex       TRUE
      1606      Organism|Thyroid|Endosome               TSH-FcRn_Complex       TRUE
      1607      Organism|Thyroid|Endosome  T3-DIO1-Assumption Metabolite       TRUE
      1608      Organism|Thyroid|Endosome        T3-UGT2B4-NA Metabolite       TRUE
      1609      Organism|Thyroid|Endosome  T3-DIO3-Assumption Metabolite       TRUE
      1610      Organism|Thyroid|Endosome  T3-Undefined Liver Metabolite       TRUE
      1611      Organism|Thyroid|Endosome  T4-DIO1-Assumption Metabolite       TRUE
      1612      Organism|Thyroid|Endosome        T4-UDPGT1-NA Metabolite       TRUE
      1613      Organism|Thyroid|Endosome  T4-Undefined Liver Metabolite       TRUE
      1614      Organism|Thyroid|Endosome TSH-Undefined Liver Metabolite       TRUE
      1615         Organism|Thyroid|Lumen                           DIO1       TRUE
      1616         Organism|Thyroid|Lumen                         UDPGT1       TRUE
      1617         Organism|Thyroid|Lumen                         UDPGT2       TRUE
      1618         Organism|Thyroid|Lumen                           DIO3       TRUE
      1619         Organism|Thyroid|Lumen                Undefined Liver       TRUE
      1620         Organism|Thyroid|Lumen                             T3       TRUE
      1621         Organism|Thyroid|Lumen                             T4       TRUE
      1622         Organism|Thyroid|Lumen                            TSH       TRUE
      1623         Organism|Thyroid|Lumen                T3-FcRn_Complex       TRUE
      1624         Organism|Thyroid|Lumen                     LigandEndo       TRUE
      1625         Organism|Thyroid|Lumen                           FcRn       TRUE
      1626         Organism|Thyroid|Lumen             LigandEndo_Complex       TRUE
      1627         Organism|Thyroid|Lumen                T4-FcRn_Complex       TRUE
      1628         Organism|Thyroid|Lumen               TSH-FcRn_Complex       TRUE
      1629         Organism|Thyroid|Lumen  T3-DIO1-Assumption Metabolite       TRUE
      1630         Organism|Thyroid|Lumen        T3-UGT2B4-NA Metabolite       TRUE
      1631         Organism|Thyroid|Lumen  T3-DIO3-Assumption Metabolite       TRUE
      1632         Organism|Thyroid|Lumen  T3-Undefined Liver Metabolite       TRUE
      1633         Organism|Thyroid|Lumen  T4-DIO1-Assumption Metabolite       TRUE
      1634         Organism|Thyroid|Lumen        T4-UDPGT1-NA Metabolite       TRUE
      1635         Organism|Thyroid|Lumen  T4-Undefined Liver Metabolite       TRUE
      1636         Organism|Thyroid|Lumen TSH-Undefined Liver Metabolite       TRUE
           Value Unit Scale Divisor Neg. Values Allowed
      1505   NaN µmol             1               FALSE
      1506   NaN µmol             1               FALSE
      1507   NaN µmol             1               FALSE
      1508   NaN µmol             1               FALSE
      1509   NaN µmol             1               FALSE
      1510     0 µmol             1               FALSE
      1511     0 µmol             1               FALSE
      1512     0 µmol             1               FALSE
      1513     0 µmol             1               FALSE
      1514     0 µmol             1               FALSE
      1515     0 µmol             1               FALSE
      1516     0 µmol             1               FALSE
      1517     0 µmol             1               FALSE
      1518     0 µmol             1               FALSE
      1519     0 µmol             1               FALSE
      1520     0 µmol             1               FALSE
      1521     0 µmol             1               FALSE
      1522     0 µmol             1               FALSE
      1523     0 µmol             1               FALSE
      1524     0 µmol             1               FALSE
      1525     0 µmol             1               FALSE
      1526     0 µmol             1               FALSE
      1527   NaN µmol             1               FALSE
      1528   NaN µmol             1               FALSE
      1529   NaN µmol             1               FALSE
      1530   NaN µmol             1               FALSE
      1531   NaN µmol             1               FALSE
      1532     0 µmol             1               FALSE
      1533     0 µmol             1               FALSE
      1534     0 µmol             1               FALSE
      1535     0 µmol             1               FALSE
      1536     0 µmol             1               FALSE
      1537     0 µmol             1               FALSE
      1538     0 µmol             1               FALSE
      1539     0 µmol             1               FALSE
      1540     0 µmol             1               FALSE
      1541     0 µmol             1               FALSE
      1542     0 µmol             1               FALSE
      1543     0 µmol             1               FALSE
      1544     0 µmol             1               FALSE
      1545     0 µmol             1               FALSE
      1546     0 µmol             1               FALSE
      1547     0 µmol             1               FALSE
      1548     0 µmol             1               FALSE
      1549   NaN µmol             1               FALSE
      1550   NaN µmol             1               FALSE
      1551   NaN µmol             1               FALSE
      1552   NaN µmol             1               FALSE
      1553   NaN µmol             1               FALSE
      1554     0 µmol             1               FALSE
      1555     0 µmol             1               FALSE
      1556     0 µmol             1               FALSE
      1557     0 µmol             1               FALSE
      1558     0 µmol             1               FALSE
      1559     0 µmol             1               FALSE
      1560     0 µmol             1               FALSE
      1561     0 µmol             1               FALSE
      1562     0 µmol             1               FALSE
      1563     0 µmol             1               FALSE
      1564     0 µmol             1               FALSE
      1565     0 µmol             1               FALSE
      1566     0 µmol             1               FALSE
      1567     0 µmol             1               FALSE
      1568     0 µmol             1               FALSE
      1569     0 µmol             1               FALSE
      1570     0 µmol             1               FALSE
      1571   NaN µmol             1               FALSE
      1572   NaN µmol             1               FALSE
      1573   NaN µmol             1               FALSE
      1574   NaN µmol             1               FALSE
      1575   NaN µmol             1               FALSE
      1576     0 µmol             1               FALSE
      1577     0 µmol             1               FALSE
      1578     0 µmol             1               FALSE
      1579     0 µmol             1               FALSE
      1580     0 µmol             1               FALSE
      1581     0 µmol             1               FALSE
      1582     0 µmol             1               FALSE
      1583     0 µmol             1               FALSE
      1584     0 µmol             1               FALSE
      1585     0 µmol             1               FALSE
      1586     0 µmol             1               FALSE
      1587     0 µmol             1               FALSE
      1588     0 µmol             1               FALSE
      1589     0 µmol             1               FALSE
      1590     0 µmol             1               FALSE
      1591     0 µmol             1               FALSE
      1592     0 µmol             1               FALSE
      1593   NaN µmol             1               FALSE
      1594   NaN µmol             1               FALSE
      1595   NaN µmol             1               FALSE
      1596   NaN µmol             1               FALSE
      1597   NaN µmol             1               FALSE
      1598     0 µmol             1               FALSE
      1599     0 µmol             1               FALSE
      1600     0 µmol             1               FALSE
      1601     0 µmol             1               FALSE
      1602     0 µmol             1               FALSE
      1603     0 µmol             1               FALSE
      1604     0 µmol             1               FALSE
      1605     0 µmol             1               FALSE
      1606     0 µmol             1               FALSE
      1607     0 µmol             1               FALSE
      1608     0 µmol             1               FALSE
      1609     0 µmol             1               FALSE
      1610     0 µmol             1               FALSE
      1611     0 µmol             1               FALSE
      1612     0 µmol             1               FALSE
      1613     0 µmol             1               FALSE
      1614     0 µmol             1               FALSE
      1615   NaN µmol             1               FALSE
      1616   NaN µmol             1               FALSE
      1617   NaN µmol             1               FALSE
      1618   NaN µmol             1               FALSE
      1619   NaN µmol             1               FALSE
      1620     0 µmol             1               FALSE
      1621     0 µmol             1               FALSE
      1622     0 µmol             1               FALSE
      1623     0 µmol             1               FALSE
      1624     0 µmol             1               FALSE
      1625     0 µmol             1               FALSE
      1626     0 µmol             1               FALSE
      1627     0 µmol             1               FALSE
      1628     0 µmol             1               FALSE
      1629     0 µmol             1               FALSE
      1630     0 µmol             1               FALSE
      1631     0 µmol             1               FALSE
      1632     0 µmol             1               FALSE
      1633     0 µmol             1               FALSE
      1634     0 µmol             1               FALSE
      1635     0 µmol             1               FALSE
      1636     0 µmol             1               FALSE

# extendInitialConditionsBB does not add new entries for existing molecules and compartments

    Code
      newPaths_df
    Output
                                      Container Path Molecule Name Is Present Value
      1637             Organism|EndogenousIgG|Plasma            T3      FALSE     0
      1638             Organism|EndogenousIgG|Plasma            T4      FALSE     0
      1639       Organism|EndogenousIgG|Interstitial            T3      FALSE     0
      1640       Organism|EndogenousIgG|Interstitial            T4      FALSE     0
      1641           Organism|EndogenousIgG|Endosome            T3      FALSE     0
      1642           Organism|EndogenousIgG|Endosome            T4      FALSE     0
      1643         Organism|EndogenousIgG|IgG_Source            T3      FALSE     0
      1644         Organism|EndogenousIgG|IgG_Source            T4      FALSE     0
      1645 Organism|EndogenousIgG|EndosomalClearance            T3      FALSE     0
      1646 Organism|EndogenousIgG|EndosomalClearance            T4      FALSE     0
           Unit Scale Divisor Neg. Values Allowed
      1637 µmol             1               FALSE
      1638 µmol             1               FALSE
      1639 µmol             1               FALSE
      1640 µmol             1               FALSE
      1641 µmol             1               FALSE
      1642 µmol             1               FALSE
      1643 µmol             1               FALSE
      1644 µmol             1               FALSE
      1645 µmol             1               FALSE
      1646 µmol             1               FALSE

# extendInitialConditionsBB extends only with specified molecules

    Code
      newPaths_df
    Output
                           Container Path Molecule Name Is Present Value Unit
      1505        Organism|Thyroid|Plasma            T3       TRUE     0 µmol
      1506        Organism|Thyroid|Plasma            T4       TRUE     0 µmol
      1507    Organism|Thyroid|BloodCells            T3       TRUE     0 µmol
      1508    Organism|Thyroid|BloodCells            T4       TRUE     0 µmol
      1509  Organism|Thyroid|Interstitial            T3       TRUE     0 µmol
      1510  Organism|Thyroid|Interstitial            T4       TRUE     0 µmol
      1511 Organism|Thyroid|Intracellular            T3       TRUE     0 µmol
      1512 Organism|Thyroid|Intracellular            T4       TRUE     0 µmol
      1513      Organism|Thyroid|Endosome            T3       TRUE     0 µmol
      1514      Organism|Thyroid|Endosome            T4       TRUE     0 µmol
      1515         Organism|Thyroid|Lumen            T3       TRUE     0 µmol
      1516         Organism|Thyroid|Lumen            T4       TRUE     0 µmol
           Scale Divisor Neg. Values Allowed
      1505             1               FALSE
      1506             1               FALSE
      1507             1               FALSE
      1508             1               FALSE
      1509             1               FALSE
      1510             1               FALSE
      1511             1               FALSE
      1512             1               FALSE
      1513             1               FALSE
      1514             1               FALSE
      1515             1               FALSE
      1516             1               FALSE

# addLocalMoleculeParametersToParameterValuesBB adds parameters for all molecules when moleculeNames is NULL

    Code
      newPaths_df
    Output
                                                           Container Path
      1                                            Organism|Gallbladder|A
      2                                            Organism|Gallbladder|B
      3                               Organism|ArterialBlood|BloodCells|A
      4                               Organism|ArterialBlood|BloodCells|B
      5                                   Organism|ArterialBlood|Plasma|A
      6                                   Organism|ArterialBlood|Plasma|B
      7                                      Organism|Bone|Interstitial|A
      8                                      Organism|Bone|Interstitial|B
      9                                     Organism|Bone|Intracellular|A
      10                                    Organism|Bone|Intracellular|B
      11                                       Organism|Bone|BloodCells|A
      12                                       Organism|Bone|BloodCells|B
      13                                           Organism|Bone|Plasma|A
      14                                           Organism|Bone|Plasma|B
      15                                      Organism|Brain|BloodCells|A
      16                                      Organism|Brain|BloodCells|B
      17                                    Organism|Brain|Interstitial|A
      18                                    Organism|Brain|Interstitial|B
      19                                   Organism|Brain|Intracellular|A
      20                                   Organism|Brain|Intracellular|B
      21                                          Organism|Brain|Plasma|A
      22                                          Organism|Brain|Plasma|B
      23                                        Organism|Fat|BloodCells|A
      24                                        Organism|Fat|BloodCells|B
      25                                      Organism|Fat|Interstitial|A
      26                                      Organism|Fat|Interstitial|B
      27                                     Organism|Fat|Intracellular|A
      28                                     Organism|Fat|Intracellular|B
      29                                            Organism|Fat|Plasma|A
      30                                            Organism|Fat|Plasma|B
      31                                     Organism|Gonads|BloodCells|A
      32                                     Organism|Gonads|BloodCells|B
      33                                   Organism|Gonads|Interstitial|A
      34                                   Organism|Gonads|Interstitial|B
      35                                  Organism|Gonads|Intracellular|A
      36                                  Organism|Gonads|Intracellular|B
      37                                         Organism|Gonads|Plasma|A
      38                                         Organism|Gonads|Plasma|B
      39                                      Organism|Heart|BloodCells|A
      40                                      Organism|Heart|BloodCells|B
      41                                    Organism|Heart|Interstitial|A
      42                                    Organism|Heart|Interstitial|B
      43                                   Organism|Heart|Intracellular|A
      44                                   Organism|Heart|Intracellular|B
      45                                          Organism|Heart|Plasma|A
      46                                          Organism|Heart|Plasma|B
      47                                     Organism|Kidney|BloodCells|A
      48                                     Organism|Kidney|BloodCells|B
      49                                   Organism|Kidney|Interstitial|A
      50                                   Organism|Kidney|Interstitial|B
      51                                  Organism|Kidney|Intracellular|A
      52                                  Organism|Kidney|Intracellular|B
      53                                         Organism|Kidney|Plasma|A
      54                                         Organism|Kidney|Plasma|B
      55                             Organism|LargeIntestine|BloodCells|A
      56                             Organism|LargeIntestine|BloodCells|B
      57                           Organism|LargeIntestine|Interstitial|A
      58                           Organism|LargeIntestine|Interstitial|B
      59                          Organism|LargeIntestine|Intracellular|A
      60                          Organism|LargeIntestine|Intracellular|B
      61                                 Organism|LargeIntestine|Plasma|A
      62                                 Organism|LargeIntestine|Plasma|B
      63               Organism|LargeIntestine|Mucosa|Caecum|BloodCells|A
      64               Organism|LargeIntestine|Mucosa|Caecum|BloodCells|B
      65             Organism|LargeIntestine|Mucosa|Caecum|Interstitial|A
      66             Organism|LargeIntestine|Mucosa|Caecum|Interstitial|B
      67            Organism|LargeIntestine|Mucosa|Caecum|Intracellular|A
      68            Organism|LargeIntestine|Mucosa|Caecum|Intracellular|B
      69                   Organism|LargeIntestine|Mucosa|Caecum|Plasma|A
      70                   Organism|LargeIntestine|Mucosa|Caecum|Plasma|B
      71       Organism|LargeIntestine|Mucosa|ColonAscendens|BloodCells|A
      72       Organism|LargeIntestine|Mucosa|ColonAscendens|BloodCells|B
      73     Organism|LargeIntestine|Mucosa|ColonAscendens|Interstitial|A
      74     Organism|LargeIntestine|Mucosa|ColonAscendens|Interstitial|B
      75    Organism|LargeIntestine|Mucosa|ColonAscendens|Intracellular|A
      76    Organism|LargeIntestine|Mucosa|ColonAscendens|Intracellular|B
      77           Organism|LargeIntestine|Mucosa|ColonAscendens|Plasma|A
      78           Organism|LargeIntestine|Mucosa|ColonAscendens|Plasma|B
      79      Organism|LargeIntestine|Mucosa|ColonDescendens|BloodCells|A
      80      Organism|LargeIntestine|Mucosa|ColonDescendens|BloodCells|B
      81    Organism|LargeIntestine|Mucosa|ColonDescendens|Interstitial|A
      82    Organism|LargeIntestine|Mucosa|ColonDescendens|Interstitial|B
      83   Organism|LargeIntestine|Mucosa|ColonDescendens|Intracellular|A
      84   Organism|LargeIntestine|Mucosa|ColonDescendens|Intracellular|B
      85          Organism|LargeIntestine|Mucosa|ColonDescendens|Plasma|A
      86          Organism|LargeIntestine|Mucosa|ColonDescendens|Plasma|B
      87         Organism|LargeIntestine|Mucosa|ColonSigmoid|BloodCells|A
      88         Organism|LargeIntestine|Mucosa|ColonSigmoid|BloodCells|B
      89       Organism|LargeIntestine|Mucosa|ColonSigmoid|Interstitial|A
      90       Organism|LargeIntestine|Mucosa|ColonSigmoid|Interstitial|B
      91      Organism|LargeIntestine|Mucosa|ColonSigmoid|Intracellular|A
      92      Organism|LargeIntestine|Mucosa|ColonSigmoid|Intracellular|B
      93             Organism|LargeIntestine|Mucosa|ColonSigmoid|Plasma|A
      94             Organism|LargeIntestine|Mucosa|ColonSigmoid|Plasma|B
      95     Organism|LargeIntestine|Mucosa|ColonTransversum|BloodCells|A
      96     Organism|LargeIntestine|Mucosa|ColonTransversum|BloodCells|B
      97   Organism|LargeIntestine|Mucosa|ColonTransversum|Interstitial|A
      98   Organism|LargeIntestine|Mucosa|ColonTransversum|Interstitial|B
      99  Organism|LargeIntestine|Mucosa|ColonTransversum|Intracellular|A
      100 Organism|LargeIntestine|Mucosa|ColonTransversum|Intracellular|B
      101        Organism|LargeIntestine|Mucosa|ColonTransversum|Plasma|A
      102        Organism|LargeIntestine|Mucosa|ColonTransversum|Plasma|B
      103              Organism|LargeIntestine|Mucosa|Rectum|BloodCells|A
      104              Organism|LargeIntestine|Mucosa|Rectum|BloodCells|B
      105            Organism|LargeIntestine|Mucosa|Rectum|Interstitial|A
      106            Organism|LargeIntestine|Mucosa|Rectum|Interstitial|B
      107           Organism|LargeIntestine|Mucosa|Rectum|Intracellular|A
      108           Organism|LargeIntestine|Mucosa|Rectum|Intracellular|B
      109                  Organism|LargeIntestine|Mucosa|Rectum|Plasma|A
      110                  Organism|LargeIntestine|Mucosa|Rectum|Plasma|B
      111                         Organism|Liver|Pericentral|BloodCells|A
      112                         Organism|Liver|Pericentral|BloodCells|B
      113                       Organism|Liver|Pericentral|Interstitial|A
      114                       Organism|Liver|Pericentral|Interstitial|B
      115                      Organism|Liver|Pericentral|Intracellular|A
      116                      Organism|Liver|Pericentral|Intracellular|B
      117                             Organism|Liver|Pericentral|Plasma|A
      118                             Organism|Liver|Pericentral|Plasma|B
      119                          Organism|Liver|Periportal|BloodCells|A
      120                          Organism|Liver|Periportal|BloodCells|B
      121                        Organism|Liver|Periportal|Interstitial|A
      122                        Organism|Liver|Periportal|Interstitial|B
      123                       Organism|Liver|Periportal|Intracellular|A
      124                       Organism|Liver|Periportal|Intracellular|B
      125                              Organism|Liver|Periportal|Plasma|A
      126                              Organism|Liver|Periportal|Plasma|B
      127                                         Organism|Lumen|Caecum|A
      128                                         Organism|Lumen|Caecum|B
      129                                 Organism|Lumen|ColonAscendens|A
      130                                 Organism|Lumen|ColonAscendens|B
      131                                Organism|Lumen|ColonDescendens|A
      132                                Organism|Lumen|ColonDescendens|B
      133                                   Organism|Lumen|ColonSigmoid|A
      134                                   Organism|Lumen|ColonSigmoid|B
      135                               Organism|Lumen|ColonTransversum|A
      136                               Organism|Lumen|ColonTransversum|B
      137                                       Organism|Lumen|Duodenum|A
      138                                       Organism|Lumen|Duodenum|B
      139                                          Organism|Lumen|Feces|A
      140                                          Organism|Lumen|Feces|B
      141                                     Organism|Lumen|LowerIleum|A
      142                                     Organism|Lumen|LowerIleum|B
      143                                   Organism|Lumen|LowerJejunum|A
      144                                   Organism|Lumen|LowerJejunum|B
      145                                         Organism|Lumen|Rectum|A
      146                                         Organism|Lumen|Rectum|B
      147                                        Organism|Lumen|Stomach|A
      148                                        Organism|Lumen|Stomach|B
      149                                     Organism|Lumen|UpperIleum|A
      150                                     Organism|Lumen|UpperIleum|B
      151                                   Organism|Lumen|UpperJejunum|A
      152                                   Organism|Lumen|UpperJejunum|B
      153                                      Organism|Lung|BloodCells|A
      154                                      Organism|Lung|BloodCells|B
      155                                    Organism|Lung|Interstitial|A
      156                                    Organism|Lung|Interstitial|B
      157                                   Organism|Lung|Intracellular|A
      158                                   Organism|Lung|Intracellular|B
      159                                          Organism|Lung|Plasma|A
      160                                          Organism|Lung|Plasma|B
      161                                    Organism|Muscle|BloodCells|A
      162                                    Organism|Muscle|BloodCells|B
      163                                  Organism|Muscle|Interstitial|A
      164                                  Organism|Muscle|Interstitial|B
      165                                 Organism|Muscle|Intracellular|A
      166                                 Organism|Muscle|Intracellular|B
      167                                        Organism|Muscle|Plasma|A
      168                                        Organism|Muscle|Plasma|B
      169                                  Organism|Pancreas|BloodCells|A
      170                                  Organism|Pancreas|BloodCells|B
      171                                Organism|Pancreas|Interstitial|A
      172                                Organism|Pancreas|Interstitial|B
      173                               Organism|Pancreas|Intracellular|A
      174                               Organism|Pancreas|Intracellular|B
      175                                      Organism|Pancreas|Plasma|A
      176                                      Organism|Pancreas|Plasma|B
      177                                Organism|PortalVein|BloodCells|A
      178                                Organism|PortalVein|BloodCells|B
      179                                    Organism|PortalVein|Plasma|A
      180                                    Organism|PortalVein|Plasma|B
      181                                      Organism|Skin|BloodCells|A
      182                                      Organism|Skin|BloodCells|B
      183                                    Organism|Skin|Interstitial|A
      184                                    Organism|Skin|Interstitial|B
      185                                   Organism|Skin|Intracellular|A
      186                                   Organism|Skin|Intracellular|B
      187                                          Organism|Skin|Plasma|A
      188                                          Organism|Skin|Plasma|B
      189                            Organism|SmallIntestine|BloodCells|A
      190                            Organism|SmallIntestine|BloodCells|B
      191                          Organism|SmallIntestine|Interstitial|A
      192                          Organism|SmallIntestine|Interstitial|B
      193                         Organism|SmallIntestine|Intracellular|A
      194                         Organism|SmallIntestine|Intracellular|B
      195                                Organism|SmallIntestine|Plasma|A
      196                                Organism|SmallIntestine|Plasma|B
      197            Organism|SmallIntestine|Mucosa|Duodenum|BloodCells|A
      198            Organism|SmallIntestine|Mucosa|Duodenum|BloodCells|B
      199          Organism|SmallIntestine|Mucosa|Duodenum|Interstitial|A
      200          Organism|SmallIntestine|Mucosa|Duodenum|Interstitial|B
      201         Organism|SmallIntestine|Mucosa|Duodenum|Intracellular|A
      202         Organism|SmallIntestine|Mucosa|Duodenum|Intracellular|B
      203                Organism|SmallIntestine|Mucosa|Duodenum|Plasma|A
      204                Organism|SmallIntestine|Mucosa|Duodenum|Plasma|B
      205          Organism|SmallIntestine|Mucosa|LowerIleum|BloodCells|A
      206          Organism|SmallIntestine|Mucosa|LowerIleum|BloodCells|B
      207        Organism|SmallIntestine|Mucosa|LowerIleum|Interstitial|A
      208        Organism|SmallIntestine|Mucosa|LowerIleum|Interstitial|B
      209       Organism|SmallIntestine|Mucosa|LowerIleum|Intracellular|A
      210       Organism|SmallIntestine|Mucosa|LowerIleum|Intracellular|B
      211              Organism|SmallIntestine|Mucosa|LowerIleum|Plasma|A
      212              Organism|SmallIntestine|Mucosa|LowerIleum|Plasma|B
      213        Organism|SmallIntestine|Mucosa|LowerJejunum|BloodCells|A
      214        Organism|SmallIntestine|Mucosa|LowerJejunum|BloodCells|B
      215      Organism|SmallIntestine|Mucosa|LowerJejunum|Interstitial|A
      216      Organism|SmallIntestine|Mucosa|LowerJejunum|Interstitial|B
      217     Organism|SmallIntestine|Mucosa|LowerJejunum|Intracellular|A
      218     Organism|SmallIntestine|Mucosa|LowerJejunum|Intracellular|B
      219            Organism|SmallIntestine|Mucosa|LowerJejunum|Plasma|A
      220            Organism|SmallIntestine|Mucosa|LowerJejunum|Plasma|B
      221          Organism|SmallIntestine|Mucosa|UpperIleum|BloodCells|A
      222          Organism|SmallIntestine|Mucosa|UpperIleum|BloodCells|B
      223        Organism|SmallIntestine|Mucosa|UpperIleum|Interstitial|A
      224        Organism|SmallIntestine|Mucosa|UpperIleum|Interstitial|B
      225       Organism|SmallIntestine|Mucosa|UpperIleum|Intracellular|A
      226       Organism|SmallIntestine|Mucosa|UpperIleum|Intracellular|B
      227              Organism|SmallIntestine|Mucosa|UpperIleum|Plasma|A
      228              Organism|SmallIntestine|Mucosa|UpperIleum|Plasma|B
      229        Organism|SmallIntestine|Mucosa|UpperJejunum|BloodCells|A
      230        Organism|SmallIntestine|Mucosa|UpperJejunum|BloodCells|B
      231      Organism|SmallIntestine|Mucosa|UpperJejunum|Interstitial|A
      232      Organism|SmallIntestine|Mucosa|UpperJejunum|Interstitial|B
      233     Organism|SmallIntestine|Mucosa|UpperJejunum|Intracellular|A
      234     Organism|SmallIntestine|Mucosa|UpperJejunum|Intracellular|B
      235            Organism|SmallIntestine|Mucosa|UpperJejunum|Plasma|A
      236            Organism|SmallIntestine|Mucosa|UpperJejunum|Plasma|B
      237                                    Organism|Spleen|BloodCells|A
      238                                    Organism|Spleen|BloodCells|B
      239                                  Organism|Spleen|Interstitial|A
      240                                  Organism|Spleen|Interstitial|B
      241                                 Organism|Spleen|Intracellular|A
      242                                 Organism|Spleen|Intracellular|B
      243                                        Organism|Spleen|Plasma|A
      244                                        Organism|Spleen|Plasma|B
      245                                   Organism|Stomach|BloodCells|A
      246                                   Organism|Stomach|BloodCells|B
      247                                 Organism|Stomach|Interstitial|A
      248                                 Organism|Stomach|Interstitial|B
      249                                Organism|Stomach|Intracellular|A
      250                                Organism|Stomach|Intracellular|B
      251                                       Organism|Stomach|Plasma|A
      252                                       Organism|Stomach|Plasma|B
      253                               Organism|VenousBlood|BloodCells|A
      254                               Organism|VenousBlood|BloodCells|B
      255                                   Organism|VenousBlood|Plasma|A
      256                                   Organism|VenousBlood|Plasma|B
                  Parameter Name Value Unit
      1   LocalMoleculeParameter     0 µmol
      2   LocalMoleculeParameter     0 µmol
      3   LocalMoleculeParameter     0 µmol
      4   LocalMoleculeParameter     0 µmol
      5   LocalMoleculeParameter     0 µmol
      6   LocalMoleculeParameter     0 µmol
      7   LocalMoleculeParameter     0 µmol
      8   LocalMoleculeParameter     0 µmol
      9   LocalMoleculeParameter     0 µmol
      10  LocalMoleculeParameter     0 µmol
      11  LocalMoleculeParameter     0 µmol
      12  LocalMoleculeParameter     0 µmol
      13  LocalMoleculeParameter     0 µmol
      14  LocalMoleculeParameter     0 µmol
      15  LocalMoleculeParameter     0 µmol
      16  LocalMoleculeParameter     0 µmol
      17  LocalMoleculeParameter     0 µmol
      18  LocalMoleculeParameter     0 µmol
      19  LocalMoleculeParameter     0 µmol
      20  LocalMoleculeParameter     0 µmol
      21  LocalMoleculeParameter     0 µmol
      22  LocalMoleculeParameter     0 µmol
      23  LocalMoleculeParameter     0 µmol
      24  LocalMoleculeParameter     0 µmol
      25  LocalMoleculeParameter     0 µmol
      26  LocalMoleculeParameter     0 µmol
      27  LocalMoleculeParameter     0 µmol
      28  LocalMoleculeParameter     0 µmol
      29  LocalMoleculeParameter     0 µmol
      30  LocalMoleculeParameter     0 µmol
      31  LocalMoleculeParameter     0 µmol
      32  LocalMoleculeParameter     0 µmol
      33  LocalMoleculeParameter     0 µmol
      34  LocalMoleculeParameter     0 µmol
      35  LocalMoleculeParameter     0 µmol
      36  LocalMoleculeParameter     0 µmol
      37  LocalMoleculeParameter     0 µmol
      38  LocalMoleculeParameter     0 µmol
      39  LocalMoleculeParameter     0 µmol
      40  LocalMoleculeParameter     0 µmol
      41  LocalMoleculeParameter     0 µmol
      42  LocalMoleculeParameter     0 µmol
      43  LocalMoleculeParameter     0 µmol
      44  LocalMoleculeParameter     0 µmol
      45  LocalMoleculeParameter     0 µmol
      46  LocalMoleculeParameter     0 µmol
      47  LocalMoleculeParameter     0 µmol
      48  LocalMoleculeParameter     0 µmol
      49  LocalMoleculeParameter     0 µmol
      50  LocalMoleculeParameter     0 µmol
      51  LocalMoleculeParameter     0 µmol
      52  LocalMoleculeParameter     0 µmol
      53  LocalMoleculeParameter     0 µmol
      54  LocalMoleculeParameter     0 µmol
      55  LocalMoleculeParameter     0 µmol
      56  LocalMoleculeParameter     0 µmol
      57  LocalMoleculeParameter     0 µmol
      58  LocalMoleculeParameter     0 µmol
      59  LocalMoleculeParameter     0 µmol
      60  LocalMoleculeParameter     0 µmol
      61  LocalMoleculeParameter     0 µmol
      62  LocalMoleculeParameter     0 µmol
      63  LocalMoleculeParameter     0 µmol
      64  LocalMoleculeParameter     0 µmol
      65  LocalMoleculeParameter     0 µmol
      66  LocalMoleculeParameter     0 µmol
      67  LocalMoleculeParameter     0 µmol
      68  LocalMoleculeParameter     0 µmol
      69  LocalMoleculeParameter     0 µmol
      70  LocalMoleculeParameter     0 µmol
      71  LocalMoleculeParameter     0 µmol
      72  LocalMoleculeParameter     0 µmol
      73  LocalMoleculeParameter     0 µmol
      74  LocalMoleculeParameter     0 µmol
      75  LocalMoleculeParameter     0 µmol
      76  LocalMoleculeParameter     0 µmol
      77  LocalMoleculeParameter     0 µmol
      78  LocalMoleculeParameter     0 µmol
      79  LocalMoleculeParameter     0 µmol
      80  LocalMoleculeParameter     0 µmol
      81  LocalMoleculeParameter     0 µmol
      82  LocalMoleculeParameter     0 µmol
      83  LocalMoleculeParameter     0 µmol
      84  LocalMoleculeParameter     0 µmol
      85  LocalMoleculeParameter     0 µmol
      86  LocalMoleculeParameter     0 µmol
      87  LocalMoleculeParameter     0 µmol
      88  LocalMoleculeParameter     0 µmol
      89  LocalMoleculeParameter     0 µmol
      90  LocalMoleculeParameter     0 µmol
      91  LocalMoleculeParameter     0 µmol
      92  LocalMoleculeParameter     0 µmol
      93  LocalMoleculeParameter     0 µmol
      94  LocalMoleculeParameter     0 µmol
      95  LocalMoleculeParameter     0 µmol
      96  LocalMoleculeParameter     0 µmol
      97  LocalMoleculeParameter     0 µmol
      98  LocalMoleculeParameter     0 µmol
      99  LocalMoleculeParameter     0 µmol
      100 LocalMoleculeParameter     0 µmol
      101 LocalMoleculeParameter     0 µmol
      102 LocalMoleculeParameter     0 µmol
      103 LocalMoleculeParameter     0 µmol
      104 LocalMoleculeParameter     0 µmol
      105 LocalMoleculeParameter     0 µmol
      106 LocalMoleculeParameter     0 µmol
      107 LocalMoleculeParameter     0 µmol
      108 LocalMoleculeParameter     0 µmol
      109 LocalMoleculeParameter     0 µmol
      110 LocalMoleculeParameter     0 µmol
      111 LocalMoleculeParameter     0 µmol
      112 LocalMoleculeParameter     0 µmol
      113 LocalMoleculeParameter     0 µmol
      114 LocalMoleculeParameter     0 µmol
      115 LocalMoleculeParameter     0 µmol
      116 LocalMoleculeParameter     0 µmol
      117 LocalMoleculeParameter     0 µmol
      118 LocalMoleculeParameter     0 µmol
      119 LocalMoleculeParameter     0 µmol
      120 LocalMoleculeParameter     0 µmol
      121 LocalMoleculeParameter     0 µmol
      122 LocalMoleculeParameter     0 µmol
      123 LocalMoleculeParameter     0 µmol
      124 LocalMoleculeParameter     0 µmol
      125 LocalMoleculeParameter     0 µmol
      126 LocalMoleculeParameter     0 µmol
      127 LocalMoleculeParameter     0 µmol
      128 LocalMoleculeParameter     0 µmol
      129 LocalMoleculeParameter     0 µmol
      130 LocalMoleculeParameter     0 µmol
      131 LocalMoleculeParameter     0 µmol
      132 LocalMoleculeParameter     0 µmol
      133 LocalMoleculeParameter     0 µmol
      134 LocalMoleculeParameter     0 µmol
      135 LocalMoleculeParameter     0 µmol
      136 LocalMoleculeParameter     0 µmol
      137 LocalMoleculeParameter     0 µmol
      138 LocalMoleculeParameter     0 µmol
      139 LocalMoleculeParameter     0 µmol
      140 LocalMoleculeParameter     0 µmol
      141 LocalMoleculeParameter     0 µmol
      142 LocalMoleculeParameter     0 µmol
      143 LocalMoleculeParameter     0 µmol
      144 LocalMoleculeParameter     0 µmol
      145 LocalMoleculeParameter     0 µmol
      146 LocalMoleculeParameter     0 µmol
      147 LocalMoleculeParameter     0 µmol
      148 LocalMoleculeParameter     0 µmol
      149 LocalMoleculeParameter     0 µmol
      150 LocalMoleculeParameter     0 µmol
      151 LocalMoleculeParameter     0 µmol
      152 LocalMoleculeParameter     0 µmol
      153 LocalMoleculeParameter     0 µmol
      154 LocalMoleculeParameter     0 µmol
      155 LocalMoleculeParameter     0 µmol
      156 LocalMoleculeParameter     0 µmol
      157 LocalMoleculeParameter     0 µmol
      158 LocalMoleculeParameter     0 µmol
      159 LocalMoleculeParameter     0 µmol
      160 LocalMoleculeParameter     0 µmol
      161 LocalMoleculeParameter     0 µmol
      162 LocalMoleculeParameter     0 µmol
      163 LocalMoleculeParameter     0 µmol
      164 LocalMoleculeParameter     0 µmol
      165 LocalMoleculeParameter     0 µmol
      166 LocalMoleculeParameter     0 µmol
      167 LocalMoleculeParameter     0 µmol
      168 LocalMoleculeParameter     0 µmol
      169 LocalMoleculeParameter     0 µmol
      170 LocalMoleculeParameter     0 µmol
      171 LocalMoleculeParameter     0 µmol
      172 LocalMoleculeParameter     0 µmol
      173 LocalMoleculeParameter     0 µmol
      174 LocalMoleculeParameter     0 µmol
      175 LocalMoleculeParameter     0 µmol
      176 LocalMoleculeParameter     0 µmol
      177 LocalMoleculeParameter     0 µmol
      178 LocalMoleculeParameter     0 µmol
      179 LocalMoleculeParameter     0 µmol
      180 LocalMoleculeParameter     0 µmol
      181 LocalMoleculeParameter     0 µmol
      182 LocalMoleculeParameter     0 µmol
      183 LocalMoleculeParameter     0 µmol
      184 LocalMoleculeParameter     0 µmol
      185 LocalMoleculeParameter     0 µmol
      186 LocalMoleculeParameter     0 µmol
      187 LocalMoleculeParameter     0 µmol
      188 LocalMoleculeParameter     0 µmol
      189 LocalMoleculeParameter     0 µmol
      190 LocalMoleculeParameter     0 µmol
      191 LocalMoleculeParameter     0 µmol
      192 LocalMoleculeParameter     0 µmol
      193 LocalMoleculeParameter     0 µmol
      194 LocalMoleculeParameter     0 µmol
      195 LocalMoleculeParameter     0 µmol
      196 LocalMoleculeParameter     0 µmol
      197 LocalMoleculeParameter     0 µmol
      198 LocalMoleculeParameter     0 µmol
      199 LocalMoleculeParameter     0 µmol
      200 LocalMoleculeParameter     0 µmol
      201 LocalMoleculeParameter     0 µmol
      202 LocalMoleculeParameter     0 µmol
      203 LocalMoleculeParameter     0 µmol
      204 LocalMoleculeParameter     0 µmol
      205 LocalMoleculeParameter     0 µmol
      206 LocalMoleculeParameter     0 µmol
      207 LocalMoleculeParameter     0 µmol
      208 LocalMoleculeParameter     0 µmol
      209 LocalMoleculeParameter     0 µmol
      210 LocalMoleculeParameter     0 µmol
      211 LocalMoleculeParameter     0 µmol
      212 LocalMoleculeParameter     0 µmol
      213 LocalMoleculeParameter     0 µmol
      214 LocalMoleculeParameter     0 µmol
      215 LocalMoleculeParameter     0 µmol
      216 LocalMoleculeParameter     0 µmol
      217 LocalMoleculeParameter     0 µmol
      218 LocalMoleculeParameter     0 µmol
      219 LocalMoleculeParameter     0 µmol
      220 LocalMoleculeParameter     0 µmol
      221 LocalMoleculeParameter     0 µmol
      222 LocalMoleculeParameter     0 µmol
      223 LocalMoleculeParameter     0 µmol
      224 LocalMoleculeParameter     0 µmol
      225 LocalMoleculeParameter     0 µmol
      226 LocalMoleculeParameter     0 µmol
      227 LocalMoleculeParameter     0 µmol
      228 LocalMoleculeParameter     0 µmol
      229 LocalMoleculeParameter     0 µmol
      230 LocalMoleculeParameter     0 µmol
      231 LocalMoleculeParameter     0 µmol
      232 LocalMoleculeParameter     0 µmol
      233 LocalMoleculeParameter     0 µmol
      234 LocalMoleculeParameter     0 µmol
      235 LocalMoleculeParameter     0 µmol
      236 LocalMoleculeParameter     0 µmol
      237 LocalMoleculeParameter     0 µmol
      238 LocalMoleculeParameter     0 µmol
      239 LocalMoleculeParameter     0 µmol
      240 LocalMoleculeParameter     0 µmol
      241 LocalMoleculeParameter     0 µmol
      242 LocalMoleculeParameter     0 µmol
      243 LocalMoleculeParameter     0 µmol
      244 LocalMoleculeParameter     0 µmol
      245 LocalMoleculeParameter     0 µmol
      246 LocalMoleculeParameter     0 µmol
      247 LocalMoleculeParameter     0 µmol
      248 LocalMoleculeParameter     0 µmol
      249 LocalMoleculeParameter     0 µmol
      250 LocalMoleculeParameter     0 µmol
      251 LocalMoleculeParameter     0 µmol
      252 LocalMoleculeParameter     0 µmol
      253 LocalMoleculeParameter     0 µmol
      254 LocalMoleculeParameter     0 µmol
      255 LocalMoleculeParameter     0 µmol
      256 LocalMoleculeParameter     0 µmol

# addLocalMoleculeParametersToParameterValuesBB adds parameters only for specified molecules

    Code
      newPaths_df
    Output
                                                           Container Path
      1                                            Organism|Gallbladder|A
      2                               Organism|ArterialBlood|BloodCells|A
      3                                   Organism|ArterialBlood|Plasma|A
      4                                      Organism|Bone|Interstitial|A
      5                                     Organism|Bone|Intracellular|A
      6                                        Organism|Bone|BloodCells|A
      7                                            Organism|Bone|Plasma|A
      8                                       Organism|Brain|BloodCells|A
      9                                     Organism|Brain|Interstitial|A
      10                                   Organism|Brain|Intracellular|A
      11                                          Organism|Brain|Plasma|A
      12                                        Organism|Fat|BloodCells|A
      13                                      Organism|Fat|Interstitial|A
      14                                     Organism|Fat|Intracellular|A
      15                                            Organism|Fat|Plasma|A
      16                                     Organism|Gonads|BloodCells|A
      17                                   Organism|Gonads|Interstitial|A
      18                                  Organism|Gonads|Intracellular|A
      19                                         Organism|Gonads|Plasma|A
      20                                      Organism|Heart|BloodCells|A
      21                                    Organism|Heart|Interstitial|A
      22                                   Organism|Heart|Intracellular|A
      23                                          Organism|Heart|Plasma|A
      24                                     Organism|Kidney|BloodCells|A
      25                                   Organism|Kidney|Interstitial|A
      26                                  Organism|Kidney|Intracellular|A
      27                                         Organism|Kidney|Plasma|A
      28                             Organism|LargeIntestine|BloodCells|A
      29                           Organism|LargeIntestine|Interstitial|A
      30                          Organism|LargeIntestine|Intracellular|A
      31                                 Organism|LargeIntestine|Plasma|A
      32               Organism|LargeIntestine|Mucosa|Caecum|BloodCells|A
      33             Organism|LargeIntestine|Mucosa|Caecum|Interstitial|A
      34            Organism|LargeIntestine|Mucosa|Caecum|Intracellular|A
      35                   Organism|LargeIntestine|Mucosa|Caecum|Plasma|A
      36       Organism|LargeIntestine|Mucosa|ColonAscendens|BloodCells|A
      37     Organism|LargeIntestine|Mucosa|ColonAscendens|Interstitial|A
      38    Organism|LargeIntestine|Mucosa|ColonAscendens|Intracellular|A
      39           Organism|LargeIntestine|Mucosa|ColonAscendens|Plasma|A
      40      Organism|LargeIntestine|Mucosa|ColonDescendens|BloodCells|A
      41    Organism|LargeIntestine|Mucosa|ColonDescendens|Interstitial|A
      42   Organism|LargeIntestine|Mucosa|ColonDescendens|Intracellular|A
      43          Organism|LargeIntestine|Mucosa|ColonDescendens|Plasma|A
      44         Organism|LargeIntestine|Mucosa|ColonSigmoid|BloodCells|A
      45       Organism|LargeIntestine|Mucosa|ColonSigmoid|Interstitial|A
      46      Organism|LargeIntestine|Mucosa|ColonSigmoid|Intracellular|A
      47             Organism|LargeIntestine|Mucosa|ColonSigmoid|Plasma|A
      48     Organism|LargeIntestine|Mucosa|ColonTransversum|BloodCells|A
      49   Organism|LargeIntestine|Mucosa|ColonTransversum|Interstitial|A
      50  Organism|LargeIntestine|Mucosa|ColonTransversum|Intracellular|A
      51         Organism|LargeIntestine|Mucosa|ColonTransversum|Plasma|A
      52               Organism|LargeIntestine|Mucosa|Rectum|BloodCells|A
      53             Organism|LargeIntestine|Mucosa|Rectum|Interstitial|A
      54            Organism|LargeIntestine|Mucosa|Rectum|Intracellular|A
      55                   Organism|LargeIntestine|Mucosa|Rectum|Plasma|A
      56                          Organism|Liver|Pericentral|BloodCells|A
      57                        Organism|Liver|Pericentral|Interstitial|A
      58                       Organism|Liver|Pericentral|Intracellular|A
      59                              Organism|Liver|Pericentral|Plasma|A
      60                           Organism|Liver|Periportal|BloodCells|A
      61                         Organism|Liver|Periportal|Interstitial|A
      62                        Organism|Liver|Periportal|Intracellular|A
      63                               Organism|Liver|Periportal|Plasma|A
      64                                          Organism|Lumen|Caecum|A
      65                                  Organism|Lumen|ColonAscendens|A
      66                                 Organism|Lumen|ColonDescendens|A
      67                                    Organism|Lumen|ColonSigmoid|A
      68                                Organism|Lumen|ColonTransversum|A
      69                                        Organism|Lumen|Duodenum|A
      70                                           Organism|Lumen|Feces|A
      71                                      Organism|Lumen|LowerIleum|A
      72                                    Organism|Lumen|LowerJejunum|A
      73                                          Organism|Lumen|Rectum|A
      74                                         Organism|Lumen|Stomach|A
      75                                      Organism|Lumen|UpperIleum|A
      76                                    Organism|Lumen|UpperJejunum|A
      77                                       Organism|Lung|BloodCells|A
      78                                     Organism|Lung|Interstitial|A
      79                                    Organism|Lung|Intracellular|A
      80                                           Organism|Lung|Plasma|A
      81                                     Organism|Muscle|BloodCells|A
      82                                   Organism|Muscle|Interstitial|A
      83                                  Organism|Muscle|Intracellular|A
      84                                         Organism|Muscle|Plasma|A
      85                                   Organism|Pancreas|BloodCells|A
      86                                 Organism|Pancreas|Interstitial|A
      87                                Organism|Pancreas|Intracellular|A
      88                                       Organism|Pancreas|Plasma|A
      89                                 Organism|PortalVein|BloodCells|A
      90                                     Organism|PortalVein|Plasma|A
      91                                       Organism|Skin|BloodCells|A
      92                                     Organism|Skin|Interstitial|A
      93                                    Organism|Skin|Intracellular|A
      94                                           Organism|Skin|Plasma|A
      95                             Organism|SmallIntestine|BloodCells|A
      96                           Organism|SmallIntestine|Interstitial|A
      97                          Organism|SmallIntestine|Intracellular|A
      98                                 Organism|SmallIntestine|Plasma|A
      99             Organism|SmallIntestine|Mucosa|Duodenum|BloodCells|A
      100          Organism|SmallIntestine|Mucosa|Duodenum|Interstitial|A
      101         Organism|SmallIntestine|Mucosa|Duodenum|Intracellular|A
      102                Organism|SmallIntestine|Mucosa|Duodenum|Plasma|A
      103          Organism|SmallIntestine|Mucosa|LowerIleum|BloodCells|A
      104        Organism|SmallIntestine|Mucosa|LowerIleum|Interstitial|A
      105       Organism|SmallIntestine|Mucosa|LowerIleum|Intracellular|A
      106              Organism|SmallIntestine|Mucosa|LowerIleum|Plasma|A
      107        Organism|SmallIntestine|Mucosa|LowerJejunum|BloodCells|A
      108      Organism|SmallIntestine|Mucosa|LowerJejunum|Interstitial|A
      109     Organism|SmallIntestine|Mucosa|LowerJejunum|Intracellular|A
      110            Organism|SmallIntestine|Mucosa|LowerJejunum|Plasma|A
      111          Organism|SmallIntestine|Mucosa|UpperIleum|BloodCells|A
      112        Organism|SmallIntestine|Mucosa|UpperIleum|Interstitial|A
      113       Organism|SmallIntestine|Mucosa|UpperIleum|Intracellular|A
      114              Organism|SmallIntestine|Mucosa|UpperIleum|Plasma|A
      115        Organism|SmallIntestine|Mucosa|UpperJejunum|BloodCells|A
      116      Organism|SmallIntestine|Mucosa|UpperJejunum|Interstitial|A
      117     Organism|SmallIntestine|Mucosa|UpperJejunum|Intracellular|A
      118            Organism|SmallIntestine|Mucosa|UpperJejunum|Plasma|A
      119                                    Organism|Spleen|BloodCells|A
      120                                  Organism|Spleen|Interstitial|A
      121                                 Organism|Spleen|Intracellular|A
      122                                        Organism|Spleen|Plasma|A
      123                                   Organism|Stomach|BloodCells|A
      124                                 Organism|Stomach|Interstitial|A
      125                                Organism|Stomach|Intracellular|A
      126                                       Organism|Stomach|Plasma|A
      127                               Organism|VenousBlood|BloodCells|A
      128                                   Organism|VenousBlood|Plasma|A
                  Parameter Name Value Unit
      1   LocalMoleculeParameter     0 µmol
      2   LocalMoleculeParameter     0 µmol
      3   LocalMoleculeParameter     0 µmol
      4   LocalMoleculeParameter     0 µmol
      5   LocalMoleculeParameter     0 µmol
      6   LocalMoleculeParameter     0 µmol
      7   LocalMoleculeParameter     0 µmol
      8   LocalMoleculeParameter     0 µmol
      9   LocalMoleculeParameter     0 µmol
      10  LocalMoleculeParameter     0 µmol
      11  LocalMoleculeParameter     0 µmol
      12  LocalMoleculeParameter     0 µmol
      13  LocalMoleculeParameter     0 µmol
      14  LocalMoleculeParameter     0 µmol
      15  LocalMoleculeParameter     0 µmol
      16  LocalMoleculeParameter     0 µmol
      17  LocalMoleculeParameter     0 µmol
      18  LocalMoleculeParameter     0 µmol
      19  LocalMoleculeParameter     0 µmol
      20  LocalMoleculeParameter     0 µmol
      21  LocalMoleculeParameter     0 µmol
      22  LocalMoleculeParameter     0 µmol
      23  LocalMoleculeParameter     0 µmol
      24  LocalMoleculeParameter     0 µmol
      25  LocalMoleculeParameter     0 µmol
      26  LocalMoleculeParameter     0 µmol
      27  LocalMoleculeParameter     0 µmol
      28  LocalMoleculeParameter     0 µmol
      29  LocalMoleculeParameter     0 µmol
      30  LocalMoleculeParameter     0 µmol
      31  LocalMoleculeParameter     0 µmol
      32  LocalMoleculeParameter     0 µmol
      33  LocalMoleculeParameter     0 µmol
      34  LocalMoleculeParameter     0 µmol
      35  LocalMoleculeParameter     0 µmol
      36  LocalMoleculeParameter     0 µmol
      37  LocalMoleculeParameter     0 µmol
      38  LocalMoleculeParameter     0 µmol
      39  LocalMoleculeParameter     0 µmol
      40  LocalMoleculeParameter     0 µmol
      41  LocalMoleculeParameter     0 µmol
      42  LocalMoleculeParameter     0 µmol
      43  LocalMoleculeParameter     0 µmol
      44  LocalMoleculeParameter     0 µmol
      45  LocalMoleculeParameter     0 µmol
      46  LocalMoleculeParameter     0 µmol
      47  LocalMoleculeParameter     0 µmol
      48  LocalMoleculeParameter     0 µmol
      49  LocalMoleculeParameter     0 µmol
      50  LocalMoleculeParameter     0 µmol
      51  LocalMoleculeParameter     0 µmol
      52  LocalMoleculeParameter     0 µmol
      53  LocalMoleculeParameter     0 µmol
      54  LocalMoleculeParameter     0 µmol
      55  LocalMoleculeParameter     0 µmol
      56  LocalMoleculeParameter     0 µmol
      57  LocalMoleculeParameter     0 µmol
      58  LocalMoleculeParameter     0 µmol
      59  LocalMoleculeParameter     0 µmol
      60  LocalMoleculeParameter     0 µmol
      61  LocalMoleculeParameter     0 µmol
      62  LocalMoleculeParameter     0 µmol
      63  LocalMoleculeParameter     0 µmol
      64  LocalMoleculeParameter     0 µmol
      65  LocalMoleculeParameter     0 µmol
      66  LocalMoleculeParameter     0 µmol
      67  LocalMoleculeParameter     0 µmol
      68  LocalMoleculeParameter     0 µmol
      69  LocalMoleculeParameter     0 µmol
      70  LocalMoleculeParameter     0 µmol
      71  LocalMoleculeParameter     0 µmol
      72  LocalMoleculeParameter     0 µmol
      73  LocalMoleculeParameter     0 µmol
      74  LocalMoleculeParameter     0 µmol
      75  LocalMoleculeParameter     0 µmol
      76  LocalMoleculeParameter     0 µmol
      77  LocalMoleculeParameter     0 µmol
      78  LocalMoleculeParameter     0 µmol
      79  LocalMoleculeParameter     0 µmol
      80  LocalMoleculeParameter     0 µmol
      81  LocalMoleculeParameter     0 µmol
      82  LocalMoleculeParameter     0 µmol
      83  LocalMoleculeParameter     0 µmol
      84  LocalMoleculeParameter     0 µmol
      85  LocalMoleculeParameter     0 µmol
      86  LocalMoleculeParameter     0 µmol
      87  LocalMoleculeParameter     0 µmol
      88  LocalMoleculeParameter     0 µmol
      89  LocalMoleculeParameter     0 µmol
      90  LocalMoleculeParameter     0 µmol
      91  LocalMoleculeParameter     0 µmol
      92  LocalMoleculeParameter     0 µmol
      93  LocalMoleculeParameter     0 µmol
      94  LocalMoleculeParameter     0 µmol
      95  LocalMoleculeParameter     0 µmol
      96  LocalMoleculeParameter     0 µmol
      97  LocalMoleculeParameter     0 µmol
      98  LocalMoleculeParameter     0 µmol
      99  LocalMoleculeParameter     0 µmol
      100 LocalMoleculeParameter     0 µmol
      101 LocalMoleculeParameter     0 µmol
      102 LocalMoleculeParameter     0 µmol
      103 LocalMoleculeParameter     0 µmol
      104 LocalMoleculeParameter     0 µmol
      105 LocalMoleculeParameter     0 µmol
      106 LocalMoleculeParameter     0 µmol
      107 LocalMoleculeParameter     0 µmol
      108 LocalMoleculeParameter     0 µmol
      109 LocalMoleculeParameter     0 µmol
      110 LocalMoleculeParameter     0 µmol
      111 LocalMoleculeParameter     0 µmol
      112 LocalMoleculeParameter     0 µmol
      113 LocalMoleculeParameter     0 µmol
      114 LocalMoleculeParameter     0 µmol
      115 LocalMoleculeParameter     0 µmol
      116 LocalMoleculeParameter     0 µmol
      117 LocalMoleculeParameter     0 µmol
      118 LocalMoleculeParameter     0 µmol
      119 LocalMoleculeParameter     0 µmol
      120 LocalMoleculeParameter     0 µmol
      121 LocalMoleculeParameter     0 µmol
      122 LocalMoleculeParameter     0 µmol
      123 LocalMoleculeParameter     0 µmol
      124 LocalMoleculeParameter     0 µmol
      125 LocalMoleculeParameter     0 µmol
      126 LocalMoleculeParameter     0 µmol
      127 LocalMoleculeParameter     0 µmol
      128 LocalMoleculeParameter     0 µmol

