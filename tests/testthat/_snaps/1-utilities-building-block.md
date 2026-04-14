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
                           Parameter Name      Value   Unit Value Origin
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
                          Container Path Molecule Name Is Present Value Unit
      136        Organism|Thyroid|Plasma             A       TRUE     0 µmol
      137        Organism|Thyroid|Plasma             B       TRUE     0 µmol
      138        Organism|Thyroid|Plasma        UGT2B6       TRUE     0 µmol
      139    Organism|Thyroid|BloodCells             A       TRUE     0 µmol
      140    Organism|Thyroid|BloodCells             B       TRUE     0 µmol
      141    Organism|Thyroid|BloodCells        UGT2B6       TRUE     0 µmol
      142  Organism|Thyroid|Interstitial             A       TRUE     0 µmol
      143  Organism|Thyroid|Interstitial             B       TRUE     0 µmol
      144  Organism|Thyroid|Interstitial        UGT2B6       TRUE     0 µmol
      145 Organism|Thyroid|Intracellular             A       TRUE     0 µmol
      146 Organism|Thyroid|Intracellular             B       TRUE     0 µmol
      147 Organism|Thyroid|Intracellular        UGT2B6       TRUE     0 µmol
      148      Organism|Thyroid|Endosome             A       TRUE     0 µmol
      149      Organism|Thyroid|Endosome             B       TRUE     0 µmol
      150      Organism|Thyroid|Endosome        UGT2B6       TRUE     0 µmol
      151         Organism|Thyroid|Lumen             A       TRUE     0 µmol
      152         Organism|Thyroid|Lumen             B       TRUE     0 µmol
      153         Organism|Thyroid|Lumen        UGT2B6       TRUE     0 µmol
          Scale Divisor Neg. Values Allowed
      136             1               FALSE
      137             1               FALSE
      138             1               FALSE
      139             1               FALSE
      140             1               FALSE
      141             1               FALSE
      142             1               FALSE
      143             1               FALSE
      144             1               FALSE
      145             1               FALSE
      146             1               FALSE
      147             1               FALSE
      148             1               FALSE
      149             1               FALSE
      150             1               FALSE
      151             1               FALSE
      152             1               FALSE
      153             1               FALSE

# extendInitialConditionsBB does not add new entries for existing molecules and compartments

    Code
      newPaths_df
    Output
                                                         Container Path Molecule Name
      136                                          Organism|Gallbladder        CYP3A4
      137                                   Organism|VenousBlood|Plasma        CYP3A4
      138                               Organism|VenousBlood|BloodCells        CYP3A4
      139                                 Organism|ArterialBlood|Plasma        CYP3A4
      140                             Organism|ArterialBlood|BloodCells        CYP3A4
      141                                          Organism|Bone|Plasma        CYP3A4
      142                                      Organism|Bone|BloodCells        CYP3A4
      143                                    Organism|Bone|Interstitial        CYP3A4
      144                                   Organism|Bone|Intracellular        CYP3A4
      145                                         Organism|Brain|Plasma        CYP3A4
      146                                     Organism|Brain|BloodCells        CYP3A4
      147                                   Organism|Brain|Interstitial        CYP3A4
      148                                  Organism|Brain|Intracellular        CYP3A4
      149                                           Organism|Fat|Plasma        CYP3A4
      150                                       Organism|Fat|BloodCells        CYP3A4
      151                                     Organism|Fat|Interstitial        CYP3A4
      152                                    Organism|Fat|Intracellular        CYP3A4
      153                                        Organism|Gonads|Plasma        CYP3A4
      154                                    Organism|Gonads|BloodCells        CYP3A4
      155                                  Organism|Gonads|Interstitial        CYP3A4
      156                                 Organism|Gonads|Intracellular        CYP3A4
      157                                         Organism|Heart|Plasma        CYP3A4
      158                                     Organism|Heart|BloodCells        CYP3A4
      159                                   Organism|Heart|Interstitial        CYP3A4
      160                                  Organism|Heart|Intracellular        CYP3A4
      161                                        Organism|Kidney|Plasma        CYP3A4
      162                                    Organism|Kidney|BloodCells        CYP3A4
      163                                  Organism|Kidney|Interstitial        CYP3A4
      164                                 Organism|Kidney|Intracellular        CYP3A4
      165                                         Organism|Kidney|Urine        CYP3A4
      166                                        Organism|Lumen|Stomach        CYP3A4
      167                                       Organism|Lumen|Duodenum        CYP3A4
      168                                   Organism|Lumen|UpperJejunum        CYP3A4
      169                                   Organism|Lumen|LowerJejunum        CYP3A4
      170                                     Organism|Lumen|UpperIleum        CYP3A4
      171                                     Organism|Lumen|LowerIleum        CYP3A4
      172                                         Organism|Lumen|Caecum        CYP3A4
      173                                 Organism|Lumen|ColonAscendens        CYP3A4
      174                               Organism|Lumen|ColonTransversum        CYP3A4
      175                                Organism|Lumen|ColonDescendens        CYP3A4
      176                                   Organism|Lumen|ColonSigmoid        CYP3A4
      177                                         Organism|Lumen|Rectum        CYP3A4
      178                                          Organism|Lumen|Feces        CYP3A4
      179                                       Organism|Stomach|Plasma        CYP3A4
      180                                   Organism|Stomach|BloodCells        CYP3A4
      181                                 Organism|Stomach|Interstitial        CYP3A4
      182                                Organism|Stomach|Intracellular        CYP3A4
      183                                Organism|SmallIntestine|Plasma        CYP3A4
      184                            Organism|SmallIntestine|BloodCells        CYP3A4
      185                          Organism|SmallIntestine|Interstitial        CYP3A4
      186                         Organism|SmallIntestine|Intracellular        CYP3A4
      187                Organism|SmallIntestine|Mucosa|Duodenum|Plasma        CYP3A4
      188            Organism|SmallIntestine|Mucosa|Duodenum|BloodCells        CYP3A4
      189          Organism|SmallIntestine|Mucosa|Duodenum|Interstitial        CYP3A4
      190         Organism|SmallIntestine|Mucosa|Duodenum|Intracellular        CYP3A4
      191            Organism|SmallIntestine|Mucosa|UpperJejunum|Plasma        CYP3A4
      192        Organism|SmallIntestine|Mucosa|UpperJejunum|BloodCells        CYP3A4
      193      Organism|SmallIntestine|Mucosa|UpperJejunum|Interstitial        CYP3A4
      194     Organism|SmallIntestine|Mucosa|UpperJejunum|Intracellular        CYP3A4
      195            Organism|SmallIntestine|Mucosa|LowerJejunum|Plasma        CYP3A4
      196        Organism|SmallIntestine|Mucosa|LowerJejunum|BloodCells        CYP3A4
      197      Organism|SmallIntestine|Mucosa|LowerJejunum|Interstitial        CYP3A4
      198     Organism|SmallIntestine|Mucosa|LowerJejunum|Intracellular        CYP3A4
      199              Organism|SmallIntestine|Mucosa|UpperIleum|Plasma        CYP3A4
      200          Organism|SmallIntestine|Mucosa|UpperIleum|BloodCells        CYP3A4
      201        Organism|SmallIntestine|Mucosa|UpperIleum|Interstitial        CYP3A4
      202       Organism|SmallIntestine|Mucosa|UpperIleum|Intracellular        CYP3A4
      203              Organism|SmallIntestine|Mucosa|LowerIleum|Plasma        CYP3A4
      204          Organism|SmallIntestine|Mucosa|LowerIleum|BloodCells        CYP3A4
      205        Organism|SmallIntestine|Mucosa|LowerIleum|Interstitial        CYP3A4
      206       Organism|SmallIntestine|Mucosa|LowerIleum|Intracellular        CYP3A4
      207                                Organism|LargeIntestine|Plasma        CYP3A4
      208                            Organism|LargeIntestine|BloodCells        CYP3A4
      209                          Organism|LargeIntestine|Interstitial        CYP3A4
      210                         Organism|LargeIntestine|Intracellular        CYP3A4
      211                  Organism|LargeIntestine|Mucosa|Caecum|Plasma        CYP3A4
      212              Organism|LargeIntestine|Mucosa|Caecum|BloodCells        CYP3A4
      213            Organism|LargeIntestine|Mucosa|Caecum|Interstitial        CYP3A4
      214           Organism|LargeIntestine|Mucosa|Caecum|Intracellular        CYP3A4
      215          Organism|LargeIntestine|Mucosa|ColonAscendens|Plasma        CYP3A4
      216      Organism|LargeIntestine|Mucosa|ColonAscendens|BloodCells        CYP3A4
      217    Organism|LargeIntestine|Mucosa|ColonAscendens|Interstitial        CYP3A4
      218   Organism|LargeIntestine|Mucosa|ColonAscendens|Intracellular        CYP3A4
      219        Organism|LargeIntestine|Mucosa|ColonTransversum|Plasma        CYP3A4
      220    Organism|LargeIntestine|Mucosa|ColonTransversum|BloodCells        CYP3A4
      221  Organism|LargeIntestine|Mucosa|ColonTransversum|Interstitial        CYP3A4
      222 Organism|LargeIntestine|Mucosa|ColonTransversum|Intracellular        CYP3A4
      223         Organism|LargeIntestine|Mucosa|ColonDescendens|Plasma        CYP3A4
      224     Organism|LargeIntestine|Mucosa|ColonDescendens|BloodCells        CYP3A4
      225   Organism|LargeIntestine|Mucosa|ColonDescendens|Interstitial        CYP3A4
      226  Organism|LargeIntestine|Mucosa|ColonDescendens|Intracellular        CYP3A4
      227            Organism|LargeIntestine|Mucosa|ColonSigmoid|Plasma        CYP3A4
      228        Organism|LargeIntestine|Mucosa|ColonSigmoid|BloodCells        CYP3A4
      229      Organism|LargeIntestine|Mucosa|ColonSigmoid|Interstitial        CYP3A4
      230     Organism|LargeIntestine|Mucosa|ColonSigmoid|Intracellular        CYP3A4
      231                  Organism|LargeIntestine|Mucosa|Rectum|Plasma        CYP3A4
      232              Organism|LargeIntestine|Mucosa|Rectum|BloodCells        CYP3A4
      233            Organism|LargeIntestine|Mucosa|Rectum|Interstitial        CYP3A4
      234           Organism|LargeIntestine|Mucosa|Rectum|Intracellular        CYP3A4
      235                              Organism|Liver|Periportal|Plasma        CYP3A4
      236                          Organism|Liver|Periportal|BloodCells        CYP3A4
      237                        Organism|Liver|Periportal|Interstitial        CYP3A4
      238                       Organism|Liver|Periportal|Intracellular        CYP3A4
      239                             Organism|Liver|Pericentral|Plasma        CYP3A4
      240                         Organism|Liver|Pericentral|BloodCells        CYP3A4
      241                       Organism|Liver|Pericentral|Interstitial        CYP3A4
      242                      Organism|Liver|Pericentral|Intracellular        CYP3A4
      243                                          Organism|Lung|Plasma        CYP3A4
      244                                      Organism|Lung|BloodCells        CYP3A4
      245                                    Organism|Lung|Interstitial        CYP3A4
      246                                   Organism|Lung|Intracellular        CYP3A4
      247                                        Organism|Muscle|Plasma        CYP3A4
      248                                    Organism|Muscle|BloodCells        CYP3A4
      249                                  Organism|Muscle|Interstitial        CYP3A4
      250                                 Organism|Muscle|Intracellular        CYP3A4
      251                                      Organism|Pancreas|Plasma        CYP3A4
      252                                  Organism|Pancreas|BloodCells        CYP3A4
      253                                Organism|Pancreas|Interstitial        CYP3A4
      254                               Organism|Pancreas|Intracellular        CYP3A4
      255                                    Organism|PortalVein|Plasma        CYP3A4
      256                                Organism|PortalVein|BloodCells        CYP3A4
      257                                          Organism|Skin|Plasma        CYP3A4
      258                                      Organism|Skin|BloodCells        CYP3A4
      259                                    Organism|Skin|Interstitial        CYP3A4
      260                                   Organism|Skin|Intracellular        CYP3A4
      261                                        Organism|Spleen|Plasma        CYP3A4
      262                                    Organism|Spleen|BloodCells        CYP3A4
      263                                  Organism|Spleen|Interstitial        CYP3A4
      264                                 Organism|Spleen|Intracellular        CYP3A4
      265                                        Organism|Saliva|Saliva        CYP3A4
      266                                   Organism|Saliva|SalivaGland        CYP3A4
          Is Present Value Unit Scale Divisor Neg. Values Allowed
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
      154       TRUE   NaN µmol             1               FALSE
      155       TRUE   NaN µmol             1               FALSE
      156       TRUE   NaN µmol             1               FALSE
      157       TRUE   NaN µmol             1               FALSE
      158       TRUE   NaN µmol             1               FALSE
      159       TRUE   NaN µmol             1               FALSE
      160       TRUE   NaN µmol             1               FALSE
      161       TRUE   NaN µmol             1               FALSE
      162       TRUE   NaN µmol             1               FALSE
      163       TRUE   NaN µmol             1               FALSE
      164       TRUE   NaN µmol             1               FALSE
      165       TRUE   NaN µmol             1               FALSE
      166       TRUE   NaN µmol             1               FALSE
      167       TRUE   NaN µmol             1               FALSE
      168       TRUE   NaN µmol             1               FALSE
      169       TRUE   NaN µmol             1               FALSE
      170       TRUE   NaN µmol             1               FALSE
      171       TRUE   NaN µmol             1               FALSE
      172       TRUE   NaN µmol             1               FALSE
      173       TRUE   NaN µmol             1               FALSE
      174       TRUE   NaN µmol             1               FALSE
      175       TRUE   NaN µmol             1               FALSE
      176       TRUE   NaN µmol             1               FALSE
      177       TRUE   NaN µmol             1               FALSE
      178       TRUE   NaN µmol             1               FALSE
      179       TRUE   NaN µmol             1               FALSE
      180       TRUE   NaN µmol             1               FALSE
      181       TRUE   NaN µmol             1               FALSE
      182       TRUE   NaN µmol             1               FALSE
      183       TRUE   NaN µmol             1               FALSE
      184       TRUE   NaN µmol             1               FALSE
      185       TRUE   NaN µmol             1               FALSE
      186       TRUE   NaN µmol             1               FALSE
      187       TRUE   NaN µmol             1               FALSE
      188       TRUE   NaN µmol             1               FALSE
      189       TRUE   NaN µmol             1               FALSE
      190       TRUE   NaN µmol             1               FALSE
      191       TRUE   NaN µmol             1               FALSE
      192       TRUE   NaN µmol             1               FALSE
      193       TRUE   NaN µmol             1               FALSE
      194       TRUE   NaN µmol             1               FALSE
      195       TRUE   NaN µmol             1               FALSE
      196       TRUE   NaN µmol             1               FALSE
      197       TRUE   NaN µmol             1               FALSE
      198       TRUE   NaN µmol             1               FALSE
      199       TRUE   NaN µmol             1               FALSE
      200       TRUE   NaN µmol             1               FALSE
      201       TRUE   NaN µmol             1               FALSE
      202       TRUE   NaN µmol             1               FALSE
      203       TRUE   NaN µmol             1               FALSE
      204       TRUE   NaN µmol             1               FALSE
      205       TRUE   NaN µmol             1               FALSE
      206       TRUE   NaN µmol             1               FALSE
      207       TRUE   NaN µmol             1               FALSE
      208       TRUE   NaN µmol             1               FALSE
      209       TRUE   NaN µmol             1               FALSE
      210       TRUE   NaN µmol             1               FALSE
      211       TRUE   NaN µmol             1               FALSE
      212       TRUE   NaN µmol             1               FALSE
      213       TRUE   NaN µmol             1               FALSE
      214       TRUE   NaN µmol             1               FALSE
      215       TRUE   NaN µmol             1               FALSE
      216       TRUE   NaN µmol             1               FALSE
      217       TRUE   NaN µmol             1               FALSE
      218       TRUE   NaN µmol             1               FALSE
      219       TRUE   NaN µmol             1               FALSE
      220       TRUE   NaN µmol             1               FALSE
      221       TRUE   NaN µmol             1               FALSE
      222       TRUE   NaN µmol             1               FALSE
      223       TRUE   NaN µmol             1               FALSE
      224       TRUE   NaN µmol             1               FALSE
      225       TRUE   NaN µmol             1               FALSE
      226       TRUE   NaN µmol             1               FALSE
      227       TRUE   NaN µmol             1               FALSE
      228       TRUE   NaN µmol             1               FALSE
      229       TRUE   NaN µmol             1               FALSE
      230       TRUE   NaN µmol             1               FALSE
      231       TRUE   NaN µmol             1               FALSE
      232       TRUE   NaN µmol             1               FALSE
      233       TRUE   NaN µmol             1               FALSE
      234       TRUE   NaN µmol             1               FALSE
      235       TRUE   NaN µmol             1               FALSE
      236       TRUE   NaN µmol             1               FALSE
      237       TRUE   NaN µmol             1               FALSE
      238       TRUE   NaN µmol             1               FALSE
      239       TRUE   NaN µmol             1               FALSE
      240       TRUE   NaN µmol             1               FALSE
      241       TRUE   NaN µmol             1               FALSE
      242       TRUE   NaN µmol             1               FALSE
      243       TRUE   NaN µmol             1               FALSE
      244       TRUE   NaN µmol             1               FALSE
      245       TRUE   NaN µmol             1               FALSE
      246       TRUE   NaN µmol             1               FALSE
      247       TRUE   NaN µmol             1               FALSE
      248       TRUE   NaN µmol             1               FALSE
      249       TRUE   NaN µmol             1               FALSE
      250       TRUE   NaN µmol             1               FALSE
      251       TRUE   NaN µmol             1               FALSE
      252       TRUE   NaN µmol             1               FALSE
      253       TRUE   NaN µmol             1               FALSE
      254       TRUE   NaN µmol             1               FALSE
      255       TRUE   NaN µmol             1               FALSE
      256       TRUE   NaN µmol             1               FALSE
      257       TRUE   NaN µmol             1               FALSE
      258       TRUE   NaN µmol             1               FALSE
      259       TRUE   NaN µmol             1               FALSE
      260       TRUE   NaN µmol             1               FALSE
      261       TRUE   NaN µmol             1               FALSE
      262       TRUE   NaN µmol             1               FALSE
      263       TRUE   NaN µmol             1               FALSE
      264       TRUE   NaN µmol             1               FALSE
      265       TRUE   NaN µmol             1               FALSE
      266       TRUE   NaN µmol             1               FALSE

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
                  Parameter Name Value Unit Value Origin
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
                  Parameter Name Value Unit Value Origin
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

