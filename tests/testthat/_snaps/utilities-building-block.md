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

