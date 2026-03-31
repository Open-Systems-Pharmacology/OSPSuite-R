# initialConditionsToDataFrame returns a data frame with the expected columns

    Code
      df
    Output
                                                         Container Path Molecule Name
      1                                            Organism|Gallbladder     Aciclovir
      2                                     Organism|VenousBlood|Plasma     Aciclovir
      3                                 Organism|VenousBlood|BloodCells     Aciclovir
      4                                   Organism|ArterialBlood|Plasma     Aciclovir
      5                               Organism|ArterialBlood|BloodCells     Aciclovir
      6                                            Organism|Bone|Plasma     Aciclovir
      7                                        Organism|Bone|BloodCells     Aciclovir
      8                                      Organism|Bone|Interstitial     Aciclovir
      9                                     Organism|Bone|Intracellular     Aciclovir
      10                                          Organism|Brain|Plasma     Aciclovir
      11                                      Organism|Brain|BloodCells     Aciclovir
      12                                    Organism|Brain|Interstitial     Aciclovir
      13                                   Organism|Brain|Intracellular     Aciclovir
      14                                            Organism|Fat|Plasma     Aciclovir
      15                                        Organism|Fat|BloodCells     Aciclovir
      16                                      Organism|Fat|Interstitial     Aciclovir
      17                                     Organism|Fat|Intracellular     Aciclovir
      18                                         Organism|Gonads|Plasma     Aciclovir
      19                                     Organism|Gonads|BloodCells     Aciclovir
      20                                   Organism|Gonads|Interstitial     Aciclovir
      21                                  Organism|Gonads|Intracellular     Aciclovir
      22                                          Organism|Heart|Plasma     Aciclovir
      23                                      Organism|Heart|BloodCells     Aciclovir
      24                                    Organism|Heart|Interstitial     Aciclovir
      25                                   Organism|Heart|Intracellular     Aciclovir
      26                                         Organism|Kidney|Plasma     Aciclovir
      27                                     Organism|Kidney|BloodCells     Aciclovir
      28                                   Organism|Kidney|Interstitial     Aciclovir
      29                                  Organism|Kidney|Intracellular     Aciclovir
      30                                          Organism|Kidney|Urine     Aciclovir
      31                                         Organism|Lumen|Stomach     Aciclovir
      32                                        Organism|Lumen|Duodenum     Aciclovir
      33                                    Organism|Lumen|UpperJejunum     Aciclovir
      34                                    Organism|Lumen|LowerJejunum     Aciclovir
      35                                      Organism|Lumen|UpperIleum     Aciclovir
      36                                      Organism|Lumen|LowerIleum     Aciclovir
      37                                          Organism|Lumen|Caecum     Aciclovir
      38                                  Organism|Lumen|ColonAscendens     Aciclovir
      39                                Organism|Lumen|ColonTransversum     Aciclovir
      40                                 Organism|Lumen|ColonDescendens     Aciclovir
      41                                    Organism|Lumen|ColonSigmoid     Aciclovir
      42                                          Organism|Lumen|Rectum     Aciclovir
      43                                           Organism|Lumen|Feces     Aciclovir
      44                                        Organism|Stomach|Plasma     Aciclovir
      45                                    Organism|Stomach|BloodCells     Aciclovir
      46                                  Organism|Stomach|Interstitial     Aciclovir
      47                                 Organism|Stomach|Intracellular     Aciclovir
      48                                 Organism|SmallIntestine|Plasma     Aciclovir
      49                             Organism|SmallIntestine|BloodCells     Aciclovir
      50                           Organism|SmallIntestine|Interstitial     Aciclovir
      51                          Organism|SmallIntestine|Intracellular     Aciclovir
      52                 Organism|SmallIntestine|Mucosa|Duodenum|Plasma     Aciclovir
      53             Organism|SmallIntestine|Mucosa|Duodenum|BloodCells     Aciclovir
      54           Organism|SmallIntestine|Mucosa|Duodenum|Interstitial     Aciclovir
      55          Organism|SmallIntestine|Mucosa|Duodenum|Intracellular     Aciclovir
      56             Organism|SmallIntestine|Mucosa|UpperJejunum|Plasma     Aciclovir
      57         Organism|SmallIntestine|Mucosa|UpperJejunum|BloodCells     Aciclovir
      58       Organism|SmallIntestine|Mucosa|UpperJejunum|Interstitial     Aciclovir
      59      Organism|SmallIntestine|Mucosa|UpperJejunum|Intracellular     Aciclovir
      60             Organism|SmallIntestine|Mucosa|LowerJejunum|Plasma     Aciclovir
      61         Organism|SmallIntestine|Mucosa|LowerJejunum|BloodCells     Aciclovir
      62       Organism|SmallIntestine|Mucosa|LowerJejunum|Interstitial     Aciclovir
      63      Organism|SmallIntestine|Mucosa|LowerJejunum|Intracellular     Aciclovir
      64               Organism|SmallIntestine|Mucosa|UpperIleum|Plasma     Aciclovir
      65           Organism|SmallIntestine|Mucosa|UpperIleum|BloodCells     Aciclovir
      66         Organism|SmallIntestine|Mucosa|UpperIleum|Interstitial     Aciclovir
      67        Organism|SmallIntestine|Mucosa|UpperIleum|Intracellular     Aciclovir
      68               Organism|SmallIntestine|Mucosa|LowerIleum|Plasma     Aciclovir
      69           Organism|SmallIntestine|Mucosa|LowerIleum|BloodCells     Aciclovir
      70         Organism|SmallIntestine|Mucosa|LowerIleum|Interstitial     Aciclovir
      71        Organism|SmallIntestine|Mucosa|LowerIleum|Intracellular     Aciclovir
      72                                 Organism|LargeIntestine|Plasma     Aciclovir
      73                             Organism|LargeIntestine|BloodCells     Aciclovir
      74                           Organism|LargeIntestine|Interstitial     Aciclovir
      75                          Organism|LargeIntestine|Intracellular     Aciclovir
      76                   Organism|LargeIntestine|Mucosa|Caecum|Plasma     Aciclovir
      77               Organism|LargeIntestine|Mucosa|Caecum|BloodCells     Aciclovir
      78             Organism|LargeIntestine|Mucosa|Caecum|Interstitial     Aciclovir
      79            Organism|LargeIntestine|Mucosa|Caecum|Intracellular     Aciclovir
      80           Organism|LargeIntestine|Mucosa|ColonAscendens|Plasma     Aciclovir
      81       Organism|LargeIntestine|Mucosa|ColonAscendens|BloodCells     Aciclovir
      82     Organism|LargeIntestine|Mucosa|ColonAscendens|Interstitial     Aciclovir
      83    Organism|LargeIntestine|Mucosa|ColonAscendens|Intracellular     Aciclovir
      84         Organism|LargeIntestine|Mucosa|ColonTransversum|Plasma     Aciclovir
      85     Organism|LargeIntestine|Mucosa|ColonTransversum|BloodCells     Aciclovir
      86   Organism|LargeIntestine|Mucosa|ColonTransversum|Interstitial     Aciclovir
      87  Organism|LargeIntestine|Mucosa|ColonTransversum|Intracellular     Aciclovir
      88          Organism|LargeIntestine|Mucosa|ColonDescendens|Plasma     Aciclovir
      89      Organism|LargeIntestine|Mucosa|ColonDescendens|BloodCells     Aciclovir
      90    Organism|LargeIntestine|Mucosa|ColonDescendens|Interstitial     Aciclovir
      91   Organism|LargeIntestine|Mucosa|ColonDescendens|Intracellular     Aciclovir
      92             Organism|LargeIntestine|Mucosa|ColonSigmoid|Plasma     Aciclovir
      93         Organism|LargeIntestine|Mucosa|ColonSigmoid|BloodCells     Aciclovir
      94       Organism|LargeIntestine|Mucosa|ColonSigmoid|Interstitial     Aciclovir
      95      Organism|LargeIntestine|Mucosa|ColonSigmoid|Intracellular     Aciclovir
      96                   Organism|LargeIntestine|Mucosa|Rectum|Plasma     Aciclovir
      97               Organism|LargeIntestine|Mucosa|Rectum|BloodCells     Aciclovir
      98             Organism|LargeIntestine|Mucosa|Rectum|Interstitial     Aciclovir
      99            Organism|LargeIntestine|Mucosa|Rectum|Intracellular     Aciclovir
      100                              Organism|Liver|Periportal|Plasma     Aciclovir
      101                          Organism|Liver|Periportal|BloodCells     Aciclovir
      102                        Organism|Liver|Periportal|Interstitial     Aciclovir
      103                       Organism|Liver|Periportal|Intracellular     Aciclovir
      104                             Organism|Liver|Pericentral|Plasma     Aciclovir
      105                         Organism|Liver|Pericentral|BloodCells     Aciclovir
      106                       Organism|Liver|Pericentral|Interstitial     Aciclovir
      107                      Organism|Liver|Pericentral|Intracellular     Aciclovir
      108                                          Organism|Lung|Plasma     Aciclovir
      109                                      Organism|Lung|BloodCells     Aciclovir
      110                                    Organism|Lung|Interstitial     Aciclovir
      111                                   Organism|Lung|Intracellular     Aciclovir
      112                                        Organism|Muscle|Plasma     Aciclovir
      113                                    Organism|Muscle|BloodCells     Aciclovir
      114                                  Organism|Muscle|Interstitial     Aciclovir
      115                                 Organism|Muscle|Intracellular     Aciclovir
      116                                      Organism|Pancreas|Plasma     Aciclovir
      117                                  Organism|Pancreas|BloodCells     Aciclovir
      118                                Organism|Pancreas|Interstitial     Aciclovir
      119                               Organism|Pancreas|Intracellular     Aciclovir
      120                                    Organism|PortalVein|Plasma     Aciclovir
      121                                Organism|PortalVein|BloodCells     Aciclovir
      122                                          Organism|Skin|Plasma     Aciclovir
      123                                      Organism|Skin|BloodCells     Aciclovir
      124                                    Organism|Skin|Interstitial     Aciclovir
      125                                   Organism|Skin|Intracellular     Aciclovir
      126                                        Organism|Spleen|Plasma     Aciclovir
      127                                    Organism|Spleen|BloodCells     Aciclovir
      128                                  Organism|Spleen|Interstitial     Aciclovir
      129                                 Organism|Spleen|Intracellular     Aciclovir
      130                                        Organism|Saliva|Saliva     Aciclovir
      131                                   Organism|Saliva|SalivaGland     Aciclovir
          Is Present Value Unit Scale Divisor Neg. Values Allowed
      1         TRUE     0 µmol             1               FALSE
      2         TRUE     0 µmol             1               FALSE
      3         TRUE     0 µmol             1               FALSE
      4         TRUE     0 µmol             1               FALSE
      5         TRUE     0 µmol             1               FALSE
      6         TRUE     0 µmol             1               FALSE
      7         TRUE     0 µmol             1               FALSE
      8         TRUE     0 µmol             1               FALSE
      9         TRUE     0 µmol             1               FALSE
      10        TRUE     0 µmol             1               FALSE
      11        TRUE     0 µmol             1               FALSE
      12        TRUE     0 µmol             1               FALSE
      13        TRUE     0 µmol             1               FALSE
      14        TRUE     0 µmol             1               FALSE
      15        TRUE     0 µmol             1               FALSE
      16        TRUE     0 µmol             1               FALSE
      17        TRUE     0 µmol             1               FALSE
      18        TRUE     0 µmol             1               FALSE
      19        TRUE     0 µmol             1               FALSE
      20        TRUE     0 µmol             1               FALSE
      21        TRUE     0 µmol             1               FALSE
      22        TRUE     0 µmol             1               FALSE
      23        TRUE     0 µmol             1               FALSE
      24        TRUE     0 µmol             1               FALSE
      25        TRUE     0 µmol             1               FALSE
      26        TRUE     0 µmol             1               FALSE
      27        TRUE     0 µmol             1               FALSE
      28        TRUE     0 µmol             1               FALSE
      29        TRUE     0 µmol             1               FALSE
      30        TRUE     0 µmol             1               FALSE
      31        TRUE     0 µmol             1               FALSE
      32        TRUE     0 µmol             1               FALSE
      33        TRUE     0 µmol             1               FALSE
      34        TRUE     0 µmol             1               FALSE
      35        TRUE     0 µmol             1               FALSE
      36        TRUE     0 µmol             1               FALSE
      37        TRUE     0 µmol             1               FALSE
      38        TRUE     0 µmol             1               FALSE
      39        TRUE     0 µmol             1               FALSE
      40        TRUE     0 µmol             1               FALSE
      41        TRUE     0 µmol             1               FALSE
      42        TRUE     0 µmol             1               FALSE
      43        TRUE     0 µmol             1               FALSE
      44        TRUE     0 µmol             1               FALSE
      45        TRUE     0 µmol             1               FALSE
      46        TRUE     0 µmol             1               FALSE
      47        TRUE     0 µmol             1               FALSE
      48        TRUE     0 µmol             1               FALSE
      49        TRUE     0 µmol             1               FALSE
      50        TRUE     0 µmol             1               FALSE
      51        TRUE     0 µmol             1               FALSE
      52        TRUE     0 µmol             1               FALSE
      53        TRUE     0 µmol             1               FALSE
      54        TRUE     0 µmol             1               FALSE
      55        TRUE     0 µmol             1               FALSE
      56        TRUE     0 µmol             1               FALSE
      57        TRUE     0 µmol             1               FALSE
      58        TRUE     0 µmol             1               FALSE
      59        TRUE     0 µmol             1               FALSE
      60        TRUE     0 µmol             1               FALSE
      61        TRUE     0 µmol             1               FALSE
      62        TRUE     0 µmol             1               FALSE
      63        TRUE     0 µmol             1               FALSE
      64        TRUE     0 µmol             1               FALSE
      65        TRUE     0 µmol             1               FALSE
      66        TRUE     0 µmol             1               FALSE
      67        TRUE     0 µmol             1               FALSE
      68        TRUE     0 µmol             1               FALSE
      69        TRUE     0 µmol             1               FALSE
      70        TRUE     0 µmol             1               FALSE
      71        TRUE     0 µmol             1               FALSE
      72        TRUE     0 µmol             1               FALSE
      73        TRUE     0 µmol             1               FALSE
      74        TRUE     0 µmol             1               FALSE
      75        TRUE     0 µmol             1               FALSE
      76        TRUE     0 µmol             1               FALSE
      77        TRUE     0 µmol             1               FALSE
      78        TRUE     0 µmol             1               FALSE
      79        TRUE     0 µmol             1               FALSE
      80        TRUE     0 µmol             1               FALSE
      81        TRUE     0 µmol             1               FALSE
      82        TRUE     0 µmol             1               FALSE
      83        TRUE     0 µmol             1               FALSE
      84        TRUE     0 µmol             1               FALSE
      85        TRUE     0 µmol             1               FALSE
      86        TRUE     0 µmol             1               FALSE
      87        TRUE     0 µmol             1               FALSE
      88        TRUE     0 µmol             1               FALSE
      89        TRUE     0 µmol             1               FALSE
      90        TRUE     0 µmol             1               FALSE
      91        TRUE     0 µmol             1               FALSE
      92        TRUE     0 µmol             1               FALSE
      93        TRUE     0 µmol             1               FALSE
      94        TRUE     0 µmol             1               FALSE
      95        TRUE     0 µmol             1               FALSE
      96        TRUE     0 µmol             1               FALSE
      97        TRUE     0 µmol             1               FALSE
      98        TRUE     0 µmol             1               FALSE
      99        TRUE     0 µmol             1               FALSE
      100       TRUE     0 µmol             1               FALSE
      101       TRUE     0 µmol             1               FALSE
      102       TRUE     0 µmol             1               FALSE
      103       TRUE     0 µmol             1               FALSE
      104       TRUE     0 µmol             1               FALSE
      105       TRUE     0 µmol             1               FALSE
      106       TRUE     0 µmol             1               FALSE
      107       TRUE     0 µmol             1               FALSE
      108       TRUE     0 µmol             1               FALSE
      109       TRUE     0 µmol             1               FALSE
      110       TRUE     0 µmol             1               FALSE
      111       TRUE     0 µmol             1               FALSE
      112       TRUE     0 µmol             1               FALSE
      113       TRUE     0 µmol             1               FALSE
      114       TRUE     0 µmol             1               FALSE
      115       TRUE     0 µmol             1               FALSE
      116       TRUE     0 µmol             1               FALSE
      117       TRUE     0 µmol             1               FALSE
      118       TRUE     0 µmol             1               FALSE
      119       TRUE     0 µmol             1               FALSE
      120       TRUE     0 µmol             1               FALSE
      121       TRUE     0 µmol             1               FALSE
      122       TRUE     0 µmol             1               FALSE
      123       TRUE     0 µmol             1               FALSE
      124       TRUE     0 µmol             1               FALSE
      125       TRUE     0 µmol             1               FALSE
      126       TRUE     0 µmol             1               FALSE
      127       TRUE     0 µmol             1               FALSE
      128       TRUE     0 µmol             1               FALSE
      129       TRUE     0 µmol             1               FALSE
      130       TRUE     0 µmol             1               FALSE
      131       TRUE     0 µmol             1                TRUE

# extendInitialConditions extends with all molecules if moleculeNames is NULL

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

# extendInitialConditions does not add new entries for existing molecules and compartments

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

# extendInitialConditions extends only with specified molecules

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

