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
      138        Organism|Thyroid|Plasma        UGT2B7       TRUE   NaN µmol
      139        Organism|Thyroid|Plasma        CYP3A4       TRUE   NaN µmol
      140    Organism|Thyroid|BloodCells             A       TRUE     0 µmol
      141    Organism|Thyroid|BloodCells             B       TRUE     0 µmol
      142    Organism|Thyroid|BloodCells        UGT2B7       TRUE   NaN µmol
      143    Organism|Thyroid|BloodCells        CYP3A4       TRUE   NaN µmol
      144  Organism|Thyroid|Interstitial             A       TRUE     0 µmol
      145  Organism|Thyroid|Interstitial             B       TRUE     0 µmol
      146  Organism|Thyroid|Interstitial        UGT2B7       TRUE   NaN µmol
      147  Organism|Thyroid|Interstitial        CYP3A4       TRUE   NaN µmol
      148 Organism|Thyroid|Intracellular             A       TRUE     0 µmol
      149 Organism|Thyroid|Intracellular             B       TRUE     0 µmol
      150 Organism|Thyroid|Intracellular        UGT2B7       TRUE   NaN µmol
      151 Organism|Thyroid|Intracellular        CYP3A4       TRUE   NaN µmol
      152      Organism|Thyroid|Endosome             A       TRUE     0 µmol
      153      Organism|Thyroid|Endosome             B       TRUE     0 µmol
      154      Organism|Thyroid|Endosome        UGT2B7       TRUE   NaN µmol
      155      Organism|Thyroid|Endosome        CYP3A4       TRUE   NaN µmol
      156         Organism|Thyroid|Lumen             A       TRUE     0 µmol
      157         Organism|Thyroid|Lumen             B       TRUE     0 µmol
      158         Organism|Thyroid|Lumen        UGT2B7       TRUE   NaN µmol
      159         Organism|Thyroid|Lumen        CYP3A4       TRUE   NaN µmol
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
      154             1               FALSE
      155             1               FALSE
      156             1               FALSE
      157             1               FALSE
      158             1               FALSE
      159             1               FALSE

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
      1                                                 Organism|Gallbladder|A
      2                                                 Organism|Gallbladder|B
      3                                            Organism|Gallbladder|UGT2B7
      4                                            Organism|Gallbladder|CYP3A4
      5                                    Organism|ArterialBlood|BloodCells|A
      6                                    Organism|ArterialBlood|BloodCells|B
      7                               Organism|ArterialBlood|BloodCells|UGT2B7
      8                               Organism|ArterialBlood|BloodCells|CYP3A4
      9                                        Organism|ArterialBlood|Plasma|A
      10                                       Organism|ArterialBlood|Plasma|B
      11                                  Organism|ArterialBlood|Plasma|UGT2B7
      12                                  Organism|ArterialBlood|Plasma|CYP3A4
      13                                          Organism|Bone|Interstitial|A
      14                                          Organism|Bone|Interstitial|B
      15                                     Organism|Bone|Interstitial|UGT2B7
      16                                     Organism|Bone|Interstitial|CYP3A4
      17                                         Organism|Bone|Intracellular|A
      18                                         Organism|Bone|Intracellular|B
      19                                    Organism|Bone|Intracellular|UGT2B7
      20                                    Organism|Bone|Intracellular|UGT2B7
      21                                    Organism|Bone|Intracellular|UGT2B7
      22                                    Organism|Bone|Intracellular|CYP3A4
      23                                    Organism|Bone|Intracellular|CYP3A4
      24                                    Organism|Bone|Intracellular|CYP3A4
      25                                            Organism|Bone|BloodCells|A
      26                                            Organism|Bone|BloodCells|B
      27                                       Organism|Bone|BloodCells|UGT2B7
      28                                       Organism|Bone|BloodCells|CYP3A4
      29                                                Organism|Bone|Plasma|A
      30                                                Organism|Bone|Plasma|B
      31                                           Organism|Bone|Plasma|UGT2B7
      32                                           Organism|Bone|Plasma|CYP3A4
      33                                           Organism|Brain|BloodCells|A
      34                                           Organism|Brain|BloodCells|B
      35                                      Organism|Brain|BloodCells|UGT2B7
      36                                      Organism|Brain|BloodCells|CYP3A4
      37                                         Organism|Brain|Interstitial|A
      38                                         Organism|Brain|Interstitial|B
      39                                    Organism|Brain|Interstitial|UGT2B7
      40                                    Organism|Brain|Interstitial|CYP3A4
      41                                        Organism|Brain|Intracellular|A
      42                                        Organism|Brain|Intracellular|B
      43                                   Organism|Brain|Intracellular|UGT2B7
      44                                   Organism|Brain|Intracellular|UGT2B7
      45                                   Organism|Brain|Intracellular|UGT2B7
      46                                   Organism|Brain|Intracellular|CYP3A4
      47                                   Organism|Brain|Intracellular|CYP3A4
      48                                   Organism|Brain|Intracellular|CYP3A4
      49                                               Organism|Brain|Plasma|A
      50                                               Organism|Brain|Plasma|B
      51                                          Organism|Brain|Plasma|UGT2B7
      52                                          Organism|Brain|Plasma|CYP3A4
      53                                             Organism|Fat|BloodCells|A
      54                                             Organism|Fat|BloodCells|B
      55                                        Organism|Fat|BloodCells|UGT2B7
      56                                        Organism|Fat|BloodCells|CYP3A4
      57                                           Organism|Fat|Interstitial|A
      58                                           Organism|Fat|Interstitial|B
      59                                      Organism|Fat|Interstitial|UGT2B7
      60                                      Organism|Fat|Interstitial|CYP3A4
      61                                          Organism|Fat|Intracellular|A
      62                                          Organism|Fat|Intracellular|B
      63                                     Organism|Fat|Intracellular|UGT2B7
      64                                     Organism|Fat|Intracellular|UGT2B7
      65                                     Organism|Fat|Intracellular|UGT2B7
      66                                     Organism|Fat|Intracellular|CYP3A4
      67                                     Organism|Fat|Intracellular|CYP3A4
      68                                     Organism|Fat|Intracellular|CYP3A4
      69                                                 Organism|Fat|Plasma|A
      70                                                 Organism|Fat|Plasma|B
      71                                            Organism|Fat|Plasma|UGT2B7
      72                                            Organism|Fat|Plasma|CYP3A4
      73                                          Organism|Gonads|BloodCells|A
      74                                          Organism|Gonads|BloodCells|B
      75                                     Organism|Gonads|BloodCells|UGT2B7
      76                                     Organism|Gonads|BloodCells|CYP3A4
      77                                        Organism|Gonads|Interstitial|A
      78                                        Organism|Gonads|Interstitial|B
      79                                   Organism|Gonads|Interstitial|UGT2B7
      80                                   Organism|Gonads|Interstitial|CYP3A4
      81                                       Organism|Gonads|Intracellular|A
      82                                       Organism|Gonads|Intracellular|B
      83                                  Organism|Gonads|Intracellular|UGT2B7
      84                                  Organism|Gonads|Intracellular|UGT2B7
      85                                  Organism|Gonads|Intracellular|UGT2B7
      86                                  Organism|Gonads|Intracellular|CYP3A4
      87                                  Organism|Gonads|Intracellular|CYP3A4
      88                                  Organism|Gonads|Intracellular|CYP3A4
      89                                              Organism|Gonads|Plasma|A
      90                                              Organism|Gonads|Plasma|B
      91                                         Organism|Gonads|Plasma|UGT2B7
      92                                         Organism|Gonads|Plasma|CYP3A4
      93                                           Organism|Heart|BloodCells|A
      94                                           Organism|Heart|BloodCells|B
      95                                      Organism|Heart|BloodCells|UGT2B7
      96                                      Organism|Heart|BloodCells|CYP3A4
      97                                         Organism|Heart|Interstitial|A
      98                                         Organism|Heart|Interstitial|B
      99                                    Organism|Heart|Interstitial|UGT2B7
      100                                   Organism|Heart|Interstitial|CYP3A4
      101                                       Organism|Heart|Intracellular|A
      102                                       Organism|Heart|Intracellular|B
      103                                  Organism|Heart|Intracellular|UGT2B7
      104                                  Organism|Heart|Intracellular|UGT2B7
      105                                  Organism|Heart|Intracellular|UGT2B7
      106                                  Organism|Heart|Intracellular|CYP3A4
      107                                  Organism|Heart|Intracellular|CYP3A4
      108                                  Organism|Heart|Intracellular|CYP3A4
      109                                              Organism|Heart|Plasma|A
      110                                              Organism|Heart|Plasma|B
      111                                         Organism|Heart|Plasma|UGT2B7
      112                                         Organism|Heart|Plasma|CYP3A4
      113                                         Organism|Kidney|BloodCells|A
      114                                         Organism|Kidney|BloodCells|B
      115                                    Organism|Kidney|BloodCells|UGT2B7
      116                                    Organism|Kidney|BloodCells|CYP3A4
      117                                       Organism|Kidney|Interstitial|A
      118                                       Organism|Kidney|Interstitial|B
      119                                  Organism|Kidney|Interstitial|UGT2B7
      120                                  Organism|Kidney|Interstitial|CYP3A4
      121                                      Organism|Kidney|Intracellular|A
      122                                      Organism|Kidney|Intracellular|B
      123                                 Organism|Kidney|Intracellular|UGT2B7
      124                                 Organism|Kidney|Intracellular|UGT2B7
      125                                 Organism|Kidney|Intracellular|UGT2B7
      126                                 Organism|Kidney|Intracellular|CYP3A4
      127                                 Organism|Kidney|Intracellular|CYP3A4
      128                                 Organism|Kidney|Intracellular|CYP3A4
      129                                             Organism|Kidney|Plasma|A
      130                                             Organism|Kidney|Plasma|B
      131                                        Organism|Kidney|Plasma|UGT2B7
      132                                        Organism|Kidney|Plasma|CYP3A4
      133                                 Organism|LargeIntestine|BloodCells|A
      134                                 Organism|LargeIntestine|BloodCells|B
      135                            Organism|LargeIntestine|BloodCells|UGT2B7
      136                            Organism|LargeIntestine|BloodCells|CYP3A4
      137                               Organism|LargeIntestine|Interstitial|A
      138                               Organism|LargeIntestine|Interstitial|B
      139                          Organism|LargeIntestine|Interstitial|UGT2B7
      140                          Organism|LargeIntestine|Interstitial|CYP3A4
      141                              Organism|LargeIntestine|Intracellular|A
      142                              Organism|LargeIntestine|Intracellular|B
      143                         Organism|LargeIntestine|Intracellular|UGT2B7
      144                         Organism|LargeIntestine|Intracellular|UGT2B7
      145                         Organism|LargeIntestine|Intracellular|UGT2B7
      146                         Organism|LargeIntestine|Intracellular|CYP3A4
      147                         Organism|LargeIntestine|Intracellular|CYP3A4
      148                         Organism|LargeIntestine|Intracellular|CYP3A4
      149                                     Organism|LargeIntestine|Plasma|A
      150                                     Organism|LargeIntestine|Plasma|B
      151                                Organism|LargeIntestine|Plasma|UGT2B7
      152                                Organism|LargeIntestine|Plasma|CYP3A4
      153                   Organism|LargeIntestine|Mucosa|Caecum|BloodCells|A
      154                   Organism|LargeIntestine|Mucosa|Caecum|BloodCells|B
      155              Organism|LargeIntestine|Mucosa|Caecum|BloodCells|UGT2B7
      156              Organism|LargeIntestine|Mucosa|Caecum|BloodCells|CYP3A4
      157                 Organism|LargeIntestine|Mucosa|Caecum|Interstitial|A
      158                 Organism|LargeIntestine|Mucosa|Caecum|Interstitial|B
      159            Organism|LargeIntestine|Mucosa|Caecum|Interstitial|UGT2B7
      160            Organism|LargeIntestine|Mucosa|Caecum|Interstitial|CYP3A4
      161                Organism|LargeIntestine|Mucosa|Caecum|Intracellular|A
      162                Organism|LargeIntestine|Mucosa|Caecum|Intracellular|B
      163           Organism|LargeIntestine|Mucosa|Caecum|Intracellular|UGT2B7
      164           Organism|LargeIntestine|Mucosa|Caecum|Intracellular|UGT2B7
      165           Organism|LargeIntestine|Mucosa|Caecum|Intracellular|UGT2B7
      166           Organism|LargeIntestine|Mucosa|Caecum|Intracellular|CYP3A4
      167           Organism|LargeIntestine|Mucosa|Caecum|Intracellular|CYP3A4
      168           Organism|LargeIntestine|Mucosa|Caecum|Intracellular|CYP3A4
      169                       Organism|LargeIntestine|Mucosa|Caecum|Plasma|A
      170                       Organism|LargeIntestine|Mucosa|Caecum|Plasma|B
      171                  Organism|LargeIntestine|Mucosa|Caecum|Plasma|UGT2B7
      172                  Organism|LargeIntestine|Mucosa|Caecum|Plasma|CYP3A4
      173           Organism|LargeIntestine|Mucosa|ColonAscendens|BloodCells|A
      174           Organism|LargeIntestine|Mucosa|ColonAscendens|BloodCells|B
      175      Organism|LargeIntestine|Mucosa|ColonAscendens|BloodCells|UGT2B7
      176      Organism|LargeIntestine|Mucosa|ColonAscendens|BloodCells|CYP3A4
      177         Organism|LargeIntestine|Mucosa|ColonAscendens|Interstitial|A
      178         Organism|LargeIntestine|Mucosa|ColonAscendens|Interstitial|B
      179    Organism|LargeIntestine|Mucosa|ColonAscendens|Interstitial|UGT2B7
      180    Organism|LargeIntestine|Mucosa|ColonAscendens|Interstitial|CYP3A4
      181        Organism|LargeIntestine|Mucosa|ColonAscendens|Intracellular|A
      182        Organism|LargeIntestine|Mucosa|ColonAscendens|Intracellular|B
      183   Organism|LargeIntestine|Mucosa|ColonAscendens|Intracellular|UGT2B7
      184   Organism|LargeIntestine|Mucosa|ColonAscendens|Intracellular|UGT2B7
      185   Organism|LargeIntestine|Mucosa|ColonAscendens|Intracellular|UGT2B7
      186   Organism|LargeIntestine|Mucosa|ColonAscendens|Intracellular|CYP3A4
      187   Organism|LargeIntestine|Mucosa|ColonAscendens|Intracellular|CYP3A4
      188   Organism|LargeIntestine|Mucosa|ColonAscendens|Intracellular|CYP3A4
      189               Organism|LargeIntestine|Mucosa|ColonAscendens|Plasma|A
      190               Organism|LargeIntestine|Mucosa|ColonAscendens|Plasma|B
      191          Organism|LargeIntestine|Mucosa|ColonAscendens|Plasma|UGT2B7
      192          Organism|LargeIntestine|Mucosa|ColonAscendens|Plasma|CYP3A4
      193          Organism|LargeIntestine|Mucosa|ColonDescendens|BloodCells|A
      194          Organism|LargeIntestine|Mucosa|ColonDescendens|BloodCells|B
      195     Organism|LargeIntestine|Mucosa|ColonDescendens|BloodCells|UGT2B7
      196     Organism|LargeIntestine|Mucosa|ColonDescendens|BloodCells|CYP3A4
      197        Organism|LargeIntestine|Mucosa|ColonDescendens|Interstitial|A
      198        Organism|LargeIntestine|Mucosa|ColonDescendens|Interstitial|B
      199   Organism|LargeIntestine|Mucosa|ColonDescendens|Interstitial|UGT2B7
      200   Organism|LargeIntestine|Mucosa|ColonDescendens|Interstitial|CYP3A4
      201       Organism|LargeIntestine|Mucosa|ColonDescendens|Intracellular|A
      202       Organism|LargeIntestine|Mucosa|ColonDescendens|Intracellular|B
      203  Organism|LargeIntestine|Mucosa|ColonDescendens|Intracellular|UGT2B7
      204  Organism|LargeIntestine|Mucosa|ColonDescendens|Intracellular|UGT2B7
      205  Organism|LargeIntestine|Mucosa|ColonDescendens|Intracellular|UGT2B7
      206  Organism|LargeIntestine|Mucosa|ColonDescendens|Intracellular|CYP3A4
      207  Organism|LargeIntestine|Mucosa|ColonDescendens|Intracellular|CYP3A4
      208  Organism|LargeIntestine|Mucosa|ColonDescendens|Intracellular|CYP3A4
      209              Organism|LargeIntestine|Mucosa|ColonDescendens|Plasma|A
      210              Organism|LargeIntestine|Mucosa|ColonDescendens|Plasma|B
      211         Organism|LargeIntestine|Mucosa|ColonDescendens|Plasma|UGT2B7
      212         Organism|LargeIntestine|Mucosa|ColonDescendens|Plasma|CYP3A4
      213             Organism|LargeIntestine|Mucosa|ColonSigmoid|BloodCells|A
      214             Organism|LargeIntestine|Mucosa|ColonSigmoid|BloodCells|B
      215        Organism|LargeIntestine|Mucosa|ColonSigmoid|BloodCells|UGT2B7
      216        Organism|LargeIntestine|Mucosa|ColonSigmoid|BloodCells|CYP3A4
      217           Organism|LargeIntestine|Mucosa|ColonSigmoid|Interstitial|A
      218           Organism|LargeIntestine|Mucosa|ColonSigmoid|Interstitial|B
      219      Organism|LargeIntestine|Mucosa|ColonSigmoid|Interstitial|UGT2B7
      220      Organism|LargeIntestine|Mucosa|ColonSigmoid|Interstitial|CYP3A4
      221          Organism|LargeIntestine|Mucosa|ColonSigmoid|Intracellular|A
      222          Organism|LargeIntestine|Mucosa|ColonSigmoid|Intracellular|B
      223     Organism|LargeIntestine|Mucosa|ColonSigmoid|Intracellular|UGT2B7
      224     Organism|LargeIntestine|Mucosa|ColonSigmoid|Intracellular|UGT2B7
      225     Organism|LargeIntestine|Mucosa|ColonSigmoid|Intracellular|UGT2B7
      226     Organism|LargeIntestine|Mucosa|ColonSigmoid|Intracellular|CYP3A4
      227     Organism|LargeIntestine|Mucosa|ColonSigmoid|Intracellular|CYP3A4
      228     Organism|LargeIntestine|Mucosa|ColonSigmoid|Intracellular|CYP3A4
      229                 Organism|LargeIntestine|Mucosa|ColonSigmoid|Plasma|A
      230                 Organism|LargeIntestine|Mucosa|ColonSigmoid|Plasma|B
      231            Organism|LargeIntestine|Mucosa|ColonSigmoid|Plasma|UGT2B7
      232            Organism|LargeIntestine|Mucosa|ColonSigmoid|Plasma|CYP3A4
      233         Organism|LargeIntestine|Mucosa|ColonTransversum|BloodCells|A
      234         Organism|LargeIntestine|Mucosa|ColonTransversum|BloodCells|B
      235    Organism|LargeIntestine|Mucosa|ColonTransversum|BloodCells|UGT2B7
      236    Organism|LargeIntestine|Mucosa|ColonTransversum|BloodCells|CYP3A4
      237       Organism|LargeIntestine|Mucosa|ColonTransversum|Interstitial|A
      238       Organism|LargeIntestine|Mucosa|ColonTransversum|Interstitial|B
      239  Organism|LargeIntestine|Mucosa|ColonTransversum|Interstitial|UGT2B7
      240  Organism|LargeIntestine|Mucosa|ColonTransversum|Interstitial|CYP3A4
      241      Organism|LargeIntestine|Mucosa|ColonTransversum|Intracellular|A
      242      Organism|LargeIntestine|Mucosa|ColonTransversum|Intracellular|B
      243 Organism|LargeIntestine|Mucosa|ColonTransversum|Intracellular|UGT2B7
      244 Organism|LargeIntestine|Mucosa|ColonTransversum|Intracellular|UGT2B7
      245 Organism|LargeIntestine|Mucosa|ColonTransversum|Intracellular|UGT2B7
      246 Organism|LargeIntestine|Mucosa|ColonTransversum|Intracellular|CYP3A4
      247 Organism|LargeIntestine|Mucosa|ColonTransversum|Intracellular|CYP3A4
      248 Organism|LargeIntestine|Mucosa|ColonTransversum|Intracellular|CYP3A4
      249             Organism|LargeIntestine|Mucosa|ColonTransversum|Plasma|A
      250             Organism|LargeIntestine|Mucosa|ColonTransversum|Plasma|B
      251        Organism|LargeIntestine|Mucosa|ColonTransversum|Plasma|UGT2B7
      252        Organism|LargeIntestine|Mucosa|ColonTransversum|Plasma|CYP3A4
      253                   Organism|LargeIntestine|Mucosa|Rectum|BloodCells|A
      254                   Organism|LargeIntestine|Mucosa|Rectum|BloodCells|B
      255              Organism|LargeIntestine|Mucosa|Rectum|BloodCells|UGT2B7
      256              Organism|LargeIntestine|Mucosa|Rectum|BloodCells|CYP3A4
      257                 Organism|LargeIntestine|Mucosa|Rectum|Interstitial|A
      258                 Organism|LargeIntestine|Mucosa|Rectum|Interstitial|B
      259            Organism|LargeIntestine|Mucosa|Rectum|Interstitial|UGT2B7
      260            Organism|LargeIntestine|Mucosa|Rectum|Interstitial|CYP3A4
      261                Organism|LargeIntestine|Mucosa|Rectum|Intracellular|A
      262                Organism|LargeIntestine|Mucosa|Rectum|Intracellular|B
      263           Organism|LargeIntestine|Mucosa|Rectum|Intracellular|UGT2B7
      264           Organism|LargeIntestine|Mucosa|Rectum|Intracellular|UGT2B7
      265           Organism|LargeIntestine|Mucosa|Rectum|Intracellular|UGT2B7
      266           Organism|LargeIntestine|Mucosa|Rectum|Intracellular|CYP3A4
      267           Organism|LargeIntestine|Mucosa|Rectum|Intracellular|CYP3A4
      268           Organism|LargeIntestine|Mucosa|Rectum|Intracellular|CYP3A4
      269                       Organism|LargeIntestine|Mucosa|Rectum|Plasma|A
      270                       Organism|LargeIntestine|Mucosa|Rectum|Plasma|B
      271                  Organism|LargeIntestine|Mucosa|Rectum|Plasma|UGT2B7
      272                  Organism|LargeIntestine|Mucosa|Rectum|Plasma|CYP3A4
      273                              Organism|Liver|Pericentral|BloodCells|A
      274                              Organism|Liver|Pericentral|BloodCells|B
      275                         Organism|Liver|Pericentral|BloodCells|UGT2B7
      276                         Organism|Liver|Pericentral|BloodCells|CYP3A4
      277                            Organism|Liver|Pericentral|Interstitial|A
      278                            Organism|Liver|Pericentral|Interstitial|B
      279                       Organism|Liver|Pericentral|Interstitial|UGT2B7
      280                       Organism|Liver|Pericentral|Interstitial|CYP3A4
      281                           Organism|Liver|Pericentral|Intracellular|A
      282                           Organism|Liver|Pericentral|Intracellular|B
      283                      Organism|Liver|Pericentral|Intracellular|UGT2B7
      284                      Organism|Liver|Pericentral|Intracellular|UGT2B7
      285                      Organism|Liver|Pericentral|Intracellular|UGT2B7
      286                      Organism|Liver|Pericentral|Intracellular|CYP3A4
      287                      Organism|Liver|Pericentral|Intracellular|CYP3A4
      288                      Organism|Liver|Pericentral|Intracellular|CYP3A4
      289                                  Organism|Liver|Pericentral|Plasma|A
      290                                  Organism|Liver|Pericentral|Plasma|B
      291                             Organism|Liver|Pericentral|Plasma|UGT2B7
      292                             Organism|Liver|Pericentral|Plasma|CYP3A4
      293                               Organism|Liver|Periportal|BloodCells|A
      294                               Organism|Liver|Periportal|BloodCells|B
      295                          Organism|Liver|Periportal|BloodCells|UGT2B7
      296                          Organism|Liver|Periportal|BloodCells|CYP3A4
      297                             Organism|Liver|Periportal|Interstitial|A
      298                             Organism|Liver|Periportal|Interstitial|B
      299                        Organism|Liver|Periportal|Interstitial|UGT2B7
      300                        Organism|Liver|Periportal|Interstitial|CYP3A4
      301                            Organism|Liver|Periportal|Intracellular|A
      302                            Organism|Liver|Periportal|Intracellular|B
      303                       Organism|Liver|Periportal|Intracellular|UGT2B7
      304                       Organism|Liver|Periportal|Intracellular|UGT2B7
      305                       Organism|Liver|Periportal|Intracellular|UGT2B7
      306                       Organism|Liver|Periportal|Intracellular|CYP3A4
      307                       Organism|Liver|Periportal|Intracellular|CYP3A4
      308                       Organism|Liver|Periportal|Intracellular|CYP3A4
      309                                   Organism|Liver|Periportal|Plasma|A
      310                                   Organism|Liver|Periportal|Plasma|B
      311                              Organism|Liver|Periportal|Plasma|UGT2B7
      312                              Organism|Liver|Periportal|Plasma|CYP3A4
      313                                              Organism|Lumen|Caecum|A
      314                                              Organism|Lumen|Caecum|B
      315                                         Organism|Lumen|Caecum|UGT2B7
      316                                         Organism|Lumen|Caecum|CYP3A4
      317                                      Organism|Lumen|ColonAscendens|A
      318                                      Organism|Lumen|ColonAscendens|B
      319                                 Organism|Lumen|ColonAscendens|UGT2B7
      320                                 Organism|Lumen|ColonAscendens|CYP3A4
      321                                     Organism|Lumen|ColonDescendens|A
      322                                     Organism|Lumen|ColonDescendens|B
      323                                Organism|Lumen|ColonDescendens|UGT2B7
      324                                Organism|Lumen|ColonDescendens|CYP3A4
      325                                        Organism|Lumen|ColonSigmoid|A
      326                                        Organism|Lumen|ColonSigmoid|B
      327                                   Organism|Lumen|ColonSigmoid|UGT2B7
      328                                   Organism|Lumen|ColonSigmoid|CYP3A4
      329                                    Organism|Lumen|ColonTransversum|A
      330                                    Organism|Lumen|ColonTransversum|B
      331                               Organism|Lumen|ColonTransversum|UGT2B7
      332                               Organism|Lumen|ColonTransversum|CYP3A4
      333                                            Organism|Lumen|Duodenum|A
      334                                            Organism|Lumen|Duodenum|B
      335                                       Organism|Lumen|Duodenum|UGT2B7
      336                                       Organism|Lumen|Duodenum|CYP3A4
      337                                               Organism|Lumen|Feces|A
      338                                               Organism|Lumen|Feces|B
      339                                          Organism|Lumen|Feces|UGT2B7
      340                                          Organism|Lumen|Feces|CYP3A4
      341                                          Organism|Lumen|LowerIleum|A
      342                                          Organism|Lumen|LowerIleum|B
      343                                     Organism|Lumen|LowerIleum|UGT2B7
      344                                     Organism|Lumen|LowerIleum|CYP3A4
      345                                        Organism|Lumen|LowerJejunum|A
      346                                        Organism|Lumen|LowerJejunum|B
      347                                   Organism|Lumen|LowerJejunum|UGT2B7
      348                                   Organism|Lumen|LowerJejunum|CYP3A4
      349                                              Organism|Lumen|Rectum|A
      350                                              Organism|Lumen|Rectum|B
      351                                         Organism|Lumen|Rectum|UGT2B7
      352                                         Organism|Lumen|Rectum|CYP3A4
      353                                             Organism|Lumen|Stomach|A
      354                                             Organism|Lumen|Stomach|B
      355                                        Organism|Lumen|Stomach|UGT2B7
      356                                        Organism|Lumen|Stomach|CYP3A4
      357                                          Organism|Lumen|UpperIleum|A
      358                                          Organism|Lumen|UpperIleum|B
      359                                     Organism|Lumen|UpperIleum|UGT2B7
      360                                     Organism|Lumen|UpperIleum|CYP3A4
      361                                        Organism|Lumen|UpperJejunum|A
      362                                        Organism|Lumen|UpperJejunum|B
      363                                   Organism|Lumen|UpperJejunum|UGT2B7
      364                                   Organism|Lumen|UpperJejunum|CYP3A4
      365                                           Organism|Lung|BloodCells|A
      366                                           Organism|Lung|BloodCells|B
      367                                      Organism|Lung|BloodCells|UGT2B7
      368                                      Organism|Lung|BloodCells|CYP3A4
      369                                         Organism|Lung|Interstitial|A
      370                                         Organism|Lung|Interstitial|B
      371                                    Organism|Lung|Interstitial|UGT2B7
      372                                    Organism|Lung|Interstitial|CYP3A4
      373                                        Organism|Lung|Intracellular|A
      374                                        Organism|Lung|Intracellular|B
      375                                   Organism|Lung|Intracellular|UGT2B7
      376                                   Organism|Lung|Intracellular|UGT2B7
      377                                   Organism|Lung|Intracellular|UGT2B7
      378                                   Organism|Lung|Intracellular|CYP3A4
      379                                   Organism|Lung|Intracellular|CYP3A4
      380                                   Organism|Lung|Intracellular|CYP3A4
      381                                               Organism|Lung|Plasma|A
      382                                               Organism|Lung|Plasma|B
      383                                          Organism|Lung|Plasma|UGT2B7
      384                                          Organism|Lung|Plasma|CYP3A4
      385                                         Organism|Muscle|BloodCells|A
      386                                         Organism|Muscle|BloodCells|B
      387                                    Organism|Muscle|BloodCells|UGT2B7
      388                                    Organism|Muscle|BloodCells|CYP3A4
      389                                       Organism|Muscle|Interstitial|A
      390                                       Organism|Muscle|Interstitial|B
      391                                  Organism|Muscle|Interstitial|UGT2B7
      392                                  Organism|Muscle|Interstitial|CYP3A4
      393                                      Organism|Muscle|Intracellular|A
      394                                      Organism|Muscle|Intracellular|B
      395                                 Organism|Muscle|Intracellular|UGT2B7
      396                                 Organism|Muscle|Intracellular|UGT2B7
      397                                 Organism|Muscle|Intracellular|UGT2B7
      398                                 Organism|Muscle|Intracellular|CYP3A4
      399                                 Organism|Muscle|Intracellular|CYP3A4
      400                                 Organism|Muscle|Intracellular|CYP3A4
      401                                             Organism|Muscle|Plasma|A
      402                                             Organism|Muscle|Plasma|B
      403                                        Organism|Muscle|Plasma|UGT2B7
      404                                        Organism|Muscle|Plasma|CYP3A4
      405                                       Organism|Pancreas|BloodCells|A
      406                                       Organism|Pancreas|BloodCells|B
      407                                  Organism|Pancreas|BloodCells|UGT2B7
      408                                  Organism|Pancreas|BloodCells|CYP3A4
      409                                     Organism|Pancreas|Interstitial|A
      410                                     Organism|Pancreas|Interstitial|B
      411                                Organism|Pancreas|Interstitial|UGT2B7
      412                                Organism|Pancreas|Interstitial|CYP3A4
      413                                    Organism|Pancreas|Intracellular|A
      414                                    Organism|Pancreas|Intracellular|B
      415                               Organism|Pancreas|Intracellular|UGT2B7
      416                               Organism|Pancreas|Intracellular|UGT2B7
      417                               Organism|Pancreas|Intracellular|UGT2B7
      418                               Organism|Pancreas|Intracellular|CYP3A4
      419                               Organism|Pancreas|Intracellular|CYP3A4
      420                               Organism|Pancreas|Intracellular|CYP3A4
      421                                           Organism|Pancreas|Plasma|A
      422                                           Organism|Pancreas|Plasma|B
      423                                      Organism|Pancreas|Plasma|UGT2B7
      424                                      Organism|Pancreas|Plasma|CYP3A4
      425                                     Organism|PortalVein|BloodCells|A
      426                                     Organism|PortalVein|BloodCells|B
      427                                Organism|PortalVein|BloodCells|UGT2B7
      428                                Organism|PortalVein|BloodCells|CYP3A4
      429                                         Organism|PortalVein|Plasma|A
      430                                         Organism|PortalVein|Plasma|B
      431                                    Organism|PortalVein|Plasma|UGT2B7
      432                                    Organism|PortalVein|Plasma|CYP3A4
      433                                           Organism|Skin|BloodCells|A
      434                                           Organism|Skin|BloodCells|B
      435                                      Organism|Skin|BloodCells|UGT2B7
      436                                      Organism|Skin|BloodCells|CYP3A4
      437                                         Organism|Skin|Interstitial|A
      438                                         Organism|Skin|Interstitial|B
      439                                    Organism|Skin|Interstitial|UGT2B7
      440                                    Organism|Skin|Interstitial|CYP3A4
      441                                        Organism|Skin|Intracellular|A
      442                                        Organism|Skin|Intracellular|B
      443                                   Organism|Skin|Intracellular|UGT2B7
      444                                   Organism|Skin|Intracellular|UGT2B7
      445                                   Organism|Skin|Intracellular|UGT2B7
      446                                   Organism|Skin|Intracellular|CYP3A4
      447                                   Organism|Skin|Intracellular|CYP3A4
      448                                   Organism|Skin|Intracellular|CYP3A4
      449                                               Organism|Skin|Plasma|A
      450                                               Organism|Skin|Plasma|B
      451                                          Organism|Skin|Plasma|UGT2B7
      452                                          Organism|Skin|Plasma|CYP3A4
      453                                 Organism|SmallIntestine|BloodCells|A
      454                                 Organism|SmallIntestine|BloodCells|B
      455                            Organism|SmallIntestine|BloodCells|UGT2B7
      456                            Organism|SmallIntestine|BloodCells|CYP3A4
      457                               Organism|SmallIntestine|Interstitial|A
      458                               Organism|SmallIntestine|Interstitial|B
      459                          Organism|SmallIntestine|Interstitial|UGT2B7
      460                          Organism|SmallIntestine|Interstitial|CYP3A4
      461                              Organism|SmallIntestine|Intracellular|A
      462                              Organism|SmallIntestine|Intracellular|B
      463                         Organism|SmallIntestine|Intracellular|UGT2B7
      464                         Organism|SmallIntestine|Intracellular|UGT2B7
      465                         Organism|SmallIntestine|Intracellular|UGT2B7
      466                         Organism|SmallIntestine|Intracellular|CYP3A4
      467                         Organism|SmallIntestine|Intracellular|CYP3A4
      468                         Organism|SmallIntestine|Intracellular|CYP3A4
      469                                     Organism|SmallIntestine|Plasma|A
      470                                     Organism|SmallIntestine|Plasma|B
      471                                Organism|SmallIntestine|Plasma|UGT2B7
      472                                Organism|SmallIntestine|Plasma|CYP3A4
      473                 Organism|SmallIntestine|Mucosa|Duodenum|BloodCells|A
      474                 Organism|SmallIntestine|Mucosa|Duodenum|BloodCells|B
      475            Organism|SmallIntestine|Mucosa|Duodenum|BloodCells|UGT2B7
      476            Organism|SmallIntestine|Mucosa|Duodenum|BloodCells|CYP3A4
      477               Organism|SmallIntestine|Mucosa|Duodenum|Interstitial|A
      478               Organism|SmallIntestine|Mucosa|Duodenum|Interstitial|B
      479          Organism|SmallIntestine|Mucosa|Duodenum|Interstitial|UGT2B7
      480          Organism|SmallIntestine|Mucosa|Duodenum|Interstitial|CYP3A4
      481              Organism|SmallIntestine|Mucosa|Duodenum|Intracellular|A
      482              Organism|SmallIntestine|Mucosa|Duodenum|Intracellular|B
      483         Organism|SmallIntestine|Mucosa|Duodenum|Intracellular|UGT2B7
      484         Organism|SmallIntestine|Mucosa|Duodenum|Intracellular|UGT2B7
      485         Organism|SmallIntestine|Mucosa|Duodenum|Intracellular|UGT2B7
      486         Organism|SmallIntestine|Mucosa|Duodenum|Intracellular|CYP3A4
      487         Organism|SmallIntestine|Mucosa|Duodenum|Intracellular|CYP3A4
      488         Organism|SmallIntestine|Mucosa|Duodenum|Intracellular|CYP3A4
      489                     Organism|SmallIntestine|Mucosa|Duodenum|Plasma|A
      490                     Organism|SmallIntestine|Mucosa|Duodenum|Plasma|B
      491                Organism|SmallIntestine|Mucosa|Duodenum|Plasma|UGT2B7
      492                Organism|SmallIntestine|Mucosa|Duodenum|Plasma|CYP3A4
      493               Organism|SmallIntestine|Mucosa|LowerIleum|BloodCells|A
      494               Organism|SmallIntestine|Mucosa|LowerIleum|BloodCells|B
      495          Organism|SmallIntestine|Mucosa|LowerIleum|BloodCells|UGT2B7
      496          Organism|SmallIntestine|Mucosa|LowerIleum|BloodCells|CYP3A4
      497             Organism|SmallIntestine|Mucosa|LowerIleum|Interstitial|A
      498             Organism|SmallIntestine|Mucosa|LowerIleum|Interstitial|B
      499        Organism|SmallIntestine|Mucosa|LowerIleum|Interstitial|UGT2B7
      500        Organism|SmallIntestine|Mucosa|LowerIleum|Interstitial|CYP3A4
      501            Organism|SmallIntestine|Mucosa|LowerIleum|Intracellular|A
      502            Organism|SmallIntestine|Mucosa|LowerIleum|Intracellular|B
      503       Organism|SmallIntestine|Mucosa|LowerIleum|Intracellular|UGT2B7
      504       Organism|SmallIntestine|Mucosa|LowerIleum|Intracellular|UGT2B7
      505       Organism|SmallIntestine|Mucosa|LowerIleum|Intracellular|UGT2B7
      506       Organism|SmallIntestine|Mucosa|LowerIleum|Intracellular|CYP3A4
      507       Organism|SmallIntestine|Mucosa|LowerIleum|Intracellular|CYP3A4
      508       Organism|SmallIntestine|Mucosa|LowerIleum|Intracellular|CYP3A4
      509                   Organism|SmallIntestine|Mucosa|LowerIleum|Plasma|A
      510                   Organism|SmallIntestine|Mucosa|LowerIleum|Plasma|B
      511              Organism|SmallIntestine|Mucosa|LowerIleum|Plasma|UGT2B7
      512              Organism|SmallIntestine|Mucosa|LowerIleum|Plasma|CYP3A4
      513             Organism|SmallIntestine|Mucosa|LowerJejunum|BloodCells|A
      514             Organism|SmallIntestine|Mucosa|LowerJejunum|BloodCells|B
      515        Organism|SmallIntestine|Mucosa|LowerJejunum|BloodCells|UGT2B7
      516        Organism|SmallIntestine|Mucosa|LowerJejunum|BloodCells|CYP3A4
      517           Organism|SmallIntestine|Mucosa|LowerJejunum|Interstitial|A
      518           Organism|SmallIntestine|Mucosa|LowerJejunum|Interstitial|B
      519      Organism|SmallIntestine|Mucosa|LowerJejunum|Interstitial|UGT2B7
      520      Organism|SmallIntestine|Mucosa|LowerJejunum|Interstitial|CYP3A4
      521          Organism|SmallIntestine|Mucosa|LowerJejunum|Intracellular|A
      522          Organism|SmallIntestine|Mucosa|LowerJejunum|Intracellular|B
      523     Organism|SmallIntestine|Mucosa|LowerJejunum|Intracellular|UGT2B7
      524     Organism|SmallIntestine|Mucosa|LowerJejunum|Intracellular|UGT2B7
      525     Organism|SmallIntestine|Mucosa|LowerJejunum|Intracellular|UGT2B7
      526     Organism|SmallIntestine|Mucosa|LowerJejunum|Intracellular|CYP3A4
      527     Organism|SmallIntestine|Mucosa|LowerJejunum|Intracellular|CYP3A4
      528     Organism|SmallIntestine|Mucosa|LowerJejunum|Intracellular|CYP3A4
      529                 Organism|SmallIntestine|Mucosa|LowerJejunum|Plasma|A
      530                 Organism|SmallIntestine|Mucosa|LowerJejunum|Plasma|B
      531            Organism|SmallIntestine|Mucosa|LowerJejunum|Plasma|UGT2B7
      532            Organism|SmallIntestine|Mucosa|LowerJejunum|Plasma|CYP3A4
      533               Organism|SmallIntestine|Mucosa|UpperIleum|BloodCells|A
      534               Organism|SmallIntestine|Mucosa|UpperIleum|BloodCells|B
      535          Organism|SmallIntestine|Mucosa|UpperIleum|BloodCells|UGT2B7
      536          Organism|SmallIntestine|Mucosa|UpperIleum|BloodCells|CYP3A4
      537             Organism|SmallIntestine|Mucosa|UpperIleum|Interstitial|A
      538             Organism|SmallIntestine|Mucosa|UpperIleum|Interstitial|B
      539        Organism|SmallIntestine|Mucosa|UpperIleum|Interstitial|UGT2B7
      540        Organism|SmallIntestine|Mucosa|UpperIleum|Interstitial|CYP3A4
      541            Organism|SmallIntestine|Mucosa|UpperIleum|Intracellular|A
      542            Organism|SmallIntestine|Mucosa|UpperIleum|Intracellular|B
      543       Organism|SmallIntestine|Mucosa|UpperIleum|Intracellular|UGT2B7
      544       Organism|SmallIntestine|Mucosa|UpperIleum|Intracellular|UGT2B7
      545       Organism|SmallIntestine|Mucosa|UpperIleum|Intracellular|UGT2B7
      546       Organism|SmallIntestine|Mucosa|UpperIleum|Intracellular|CYP3A4
      547       Organism|SmallIntestine|Mucosa|UpperIleum|Intracellular|CYP3A4
      548       Organism|SmallIntestine|Mucosa|UpperIleum|Intracellular|CYP3A4
      549                   Organism|SmallIntestine|Mucosa|UpperIleum|Plasma|A
      550                   Organism|SmallIntestine|Mucosa|UpperIleum|Plasma|B
      551              Organism|SmallIntestine|Mucosa|UpperIleum|Plasma|UGT2B7
      552              Organism|SmallIntestine|Mucosa|UpperIleum|Plasma|CYP3A4
      553             Organism|SmallIntestine|Mucosa|UpperJejunum|BloodCells|A
      554             Organism|SmallIntestine|Mucosa|UpperJejunum|BloodCells|B
      555        Organism|SmallIntestine|Mucosa|UpperJejunum|BloodCells|UGT2B7
      556        Organism|SmallIntestine|Mucosa|UpperJejunum|BloodCells|CYP3A4
      557           Organism|SmallIntestine|Mucosa|UpperJejunum|Interstitial|A
      558           Organism|SmallIntestine|Mucosa|UpperJejunum|Interstitial|B
      559      Organism|SmallIntestine|Mucosa|UpperJejunum|Interstitial|UGT2B7
      560      Organism|SmallIntestine|Mucosa|UpperJejunum|Interstitial|CYP3A4
      561          Organism|SmallIntestine|Mucosa|UpperJejunum|Intracellular|A
      562          Organism|SmallIntestine|Mucosa|UpperJejunum|Intracellular|B
      563     Organism|SmallIntestine|Mucosa|UpperJejunum|Intracellular|UGT2B7
      564     Organism|SmallIntestine|Mucosa|UpperJejunum|Intracellular|UGT2B7
      565     Organism|SmallIntestine|Mucosa|UpperJejunum|Intracellular|UGT2B7
      566     Organism|SmallIntestine|Mucosa|UpperJejunum|Intracellular|CYP3A4
      567     Organism|SmallIntestine|Mucosa|UpperJejunum|Intracellular|CYP3A4
      568     Organism|SmallIntestine|Mucosa|UpperJejunum|Intracellular|CYP3A4
      569                 Organism|SmallIntestine|Mucosa|UpperJejunum|Plasma|A
      570                 Organism|SmallIntestine|Mucosa|UpperJejunum|Plasma|B
      571            Organism|SmallIntestine|Mucosa|UpperJejunum|Plasma|UGT2B7
      572            Organism|SmallIntestine|Mucosa|UpperJejunum|Plasma|CYP3A4
      573                                         Organism|Spleen|BloodCells|A
      574                                         Organism|Spleen|BloodCells|B
      575                                    Organism|Spleen|BloodCells|UGT2B7
      576                                    Organism|Spleen|BloodCells|CYP3A4
      577                                       Organism|Spleen|Interstitial|A
      578                                       Organism|Spleen|Interstitial|B
      579                                  Organism|Spleen|Interstitial|UGT2B7
      580                                  Organism|Spleen|Interstitial|CYP3A4
      581                                      Organism|Spleen|Intracellular|A
      582                                      Organism|Spleen|Intracellular|B
      583                                 Organism|Spleen|Intracellular|UGT2B7
      584                                 Organism|Spleen|Intracellular|UGT2B7
      585                                 Organism|Spleen|Intracellular|UGT2B7
      586                                 Organism|Spleen|Intracellular|CYP3A4
      587                                 Organism|Spleen|Intracellular|CYP3A4
      588                                 Organism|Spleen|Intracellular|CYP3A4
      589                                             Organism|Spleen|Plasma|A
      590                                             Organism|Spleen|Plasma|B
      591                                        Organism|Spleen|Plasma|UGT2B7
      592                                        Organism|Spleen|Plasma|CYP3A4
      593                                        Organism|Stomach|BloodCells|A
      594                                        Organism|Stomach|BloodCells|B
      595                                   Organism|Stomach|BloodCells|UGT2B7
      596                                   Organism|Stomach|BloodCells|CYP3A4
      597                                      Organism|Stomach|Interstitial|A
      598                                      Organism|Stomach|Interstitial|B
      599                                 Organism|Stomach|Interstitial|UGT2B7
      600                                 Organism|Stomach|Interstitial|CYP3A4
      601                                     Organism|Stomach|Intracellular|A
      602                                     Organism|Stomach|Intracellular|B
      603                                Organism|Stomach|Intracellular|UGT2B7
      604                                Organism|Stomach|Intracellular|UGT2B7
      605                                Organism|Stomach|Intracellular|UGT2B7
      606                                Organism|Stomach|Intracellular|CYP3A4
      607                                Organism|Stomach|Intracellular|CYP3A4
      608                                Organism|Stomach|Intracellular|CYP3A4
      609                                            Organism|Stomach|Plasma|A
      610                                            Organism|Stomach|Plasma|B
      611                                       Organism|Stomach|Plasma|UGT2B7
      612                                       Organism|Stomach|Plasma|CYP3A4
      613                                    Organism|VenousBlood|BloodCells|A
      614                                    Organism|VenousBlood|BloodCells|B
      615                               Organism|VenousBlood|BloodCells|UGT2B7
      616                               Organism|VenousBlood|BloodCells|CYP3A4
      617                                        Organism|VenousBlood|Plasma|A
      618                                        Organism|VenousBlood|Plasma|B
      619                                   Organism|VenousBlood|Plasma|UGT2B7
      620                                   Organism|VenousBlood|Plasma|CYP3A4
                            Parameter Name Value   Unit Value Origin
      1             LocalMoleculeParameter     0   µmol             
      2             LocalMoleculeParameter     0   µmol             
      3              Initial concentration     0 µmol/l             
      4              Initial concentration     0 µmol/l             
      5             LocalMoleculeParameter     0   µmol             
      6             LocalMoleculeParameter     0   µmol             
      7              Initial concentration     0 µmol/l             
      8              Initial concentration     0 µmol/l             
      9             LocalMoleculeParameter     0   µmol             
      10            LocalMoleculeParameter     0   µmol             
      11             Initial concentration     0 µmol/l             
      12             Initial concentration     0 µmol/l             
      13            LocalMoleculeParameter     0   µmol             
      14            LocalMoleculeParameter     0   µmol             
      15             Initial concentration     0 µmol/l             
      16             Initial concentration     0 µmol/l             
      17            LocalMoleculeParameter     0   µmol             
      18            LocalMoleculeParameter     0   µmol             
      19               Relative expression     0                    
      20             Initial concentration     0 µmol/l             
      21  Fraction expressed intracellular     1                    
      22               Relative expression     0                    
      23             Initial concentration     0 µmol/l             
      24  Fraction expressed intracellular     1                    
      25            LocalMoleculeParameter     0   µmol             
      26            LocalMoleculeParameter     0   µmol             
      27             Initial concentration     0 µmol/l             
      28             Initial concentration     0 µmol/l             
      29            LocalMoleculeParameter     0   µmol             
      30            LocalMoleculeParameter     0   µmol             
      31             Initial concentration     0 µmol/l             
      32             Initial concentration     0 µmol/l             
      33            LocalMoleculeParameter     0   µmol             
      34            LocalMoleculeParameter     0   µmol             
      35             Initial concentration     0 µmol/l             
      36             Initial concentration     0 µmol/l             
      37            LocalMoleculeParameter     0   µmol             
      38            LocalMoleculeParameter     0   µmol             
      39             Initial concentration     0 µmol/l             
      40             Initial concentration     0 µmol/l             
      41            LocalMoleculeParameter     0   µmol             
      42            LocalMoleculeParameter     0   µmol             
      43               Relative expression     0                    
      44             Initial concentration     0 µmol/l             
      45  Fraction expressed intracellular     1                    
      46               Relative expression     0                    
      47             Initial concentration     0 µmol/l             
      48  Fraction expressed intracellular     1                    
      49            LocalMoleculeParameter     0   µmol             
      50            LocalMoleculeParameter     0   µmol             
      51             Initial concentration     0 µmol/l             
      52             Initial concentration     0 µmol/l             
      53            LocalMoleculeParameter     0   µmol             
      54            LocalMoleculeParameter     0   µmol             
      55             Initial concentration     0 µmol/l             
      56             Initial concentration     0 µmol/l             
      57            LocalMoleculeParameter     0   µmol             
      58            LocalMoleculeParameter     0   µmol             
      59             Initial concentration     0 µmol/l             
      60             Initial concentration     0 µmol/l             
      61            LocalMoleculeParameter     0   µmol             
      62            LocalMoleculeParameter     0   µmol             
      63               Relative expression     0                    
      64             Initial concentration     0 µmol/l             
      65  Fraction expressed intracellular     1                    
      66               Relative expression     0                    
      67             Initial concentration     0 µmol/l             
      68  Fraction expressed intracellular     1                    
      69            LocalMoleculeParameter     0   µmol             
      70            LocalMoleculeParameter     0   µmol             
      71             Initial concentration     0 µmol/l             
      72             Initial concentration     0 µmol/l             
      73            LocalMoleculeParameter     0   µmol             
      74            LocalMoleculeParameter     0   µmol             
      75             Initial concentration     0 µmol/l             
      76             Initial concentration     0 µmol/l             
      77            LocalMoleculeParameter     0   µmol             
      78            LocalMoleculeParameter     0   µmol             
      79             Initial concentration     0 µmol/l             
      80             Initial concentration     0 µmol/l             
      81            LocalMoleculeParameter     0   µmol             
      82            LocalMoleculeParameter     0   µmol             
      83               Relative expression     0                    
      84             Initial concentration     0 µmol/l             
      85  Fraction expressed intracellular     1                    
      86               Relative expression     0                    
      87             Initial concentration     0 µmol/l             
      88  Fraction expressed intracellular     1                    
      89            LocalMoleculeParameter     0   µmol             
      90            LocalMoleculeParameter     0   µmol             
      91             Initial concentration     0 µmol/l             
      92             Initial concentration     0 µmol/l             
      93            LocalMoleculeParameter     0   µmol             
      94            LocalMoleculeParameter     0   µmol             
      95             Initial concentration     0 µmol/l             
      96             Initial concentration     0 µmol/l             
      97            LocalMoleculeParameter     0   µmol             
      98            LocalMoleculeParameter     0   µmol             
      99             Initial concentration     0 µmol/l             
      100            Initial concentration     0 µmol/l             
      101           LocalMoleculeParameter     0   µmol             
      102           LocalMoleculeParameter     0   µmol             
      103              Relative expression     0                    
      104            Initial concentration     0 µmol/l             
      105 Fraction expressed intracellular     1                    
      106              Relative expression     0                    
      107            Initial concentration     0 µmol/l             
      108 Fraction expressed intracellular     1                    
      109           LocalMoleculeParameter     0   µmol             
      110           LocalMoleculeParameter     0   µmol             
      111            Initial concentration     0 µmol/l             
      112            Initial concentration     0 µmol/l             
      113           LocalMoleculeParameter     0   µmol             
      114           LocalMoleculeParameter     0   µmol             
      115            Initial concentration     0 µmol/l             
      116            Initial concentration     0 µmol/l             
      117           LocalMoleculeParameter     0   µmol             
      118           LocalMoleculeParameter     0   µmol             
      119            Initial concentration     0 µmol/l             
      120            Initial concentration     0 µmol/l             
      121           LocalMoleculeParameter     0   µmol             
      122           LocalMoleculeParameter     0   µmol             
      123              Relative expression     0                    
      124            Initial concentration     0 µmol/l             
      125 Fraction expressed intracellular     1                    
      126              Relative expression     0                    
      127            Initial concentration     0 µmol/l             
      128 Fraction expressed intracellular     1                    
      129           LocalMoleculeParameter     0   µmol             
      130           LocalMoleculeParameter     0   µmol             
      131            Initial concentration     0 µmol/l             
      132            Initial concentration     0 µmol/l             
      133           LocalMoleculeParameter     0   µmol             
      134           LocalMoleculeParameter     0   µmol             
      135            Initial concentration     0 µmol/l             
      136            Initial concentration     0 µmol/l             
      137           LocalMoleculeParameter     0   µmol             
      138           LocalMoleculeParameter     0   µmol             
      139            Initial concentration     0 µmol/l             
      140            Initial concentration     0 µmol/l             
      141           LocalMoleculeParameter     0   µmol             
      142           LocalMoleculeParameter     0   µmol             
      143              Relative expression     0                    
      144            Initial concentration     0 µmol/l             
      145 Fraction expressed intracellular     1                    
      146              Relative expression     0                    
      147            Initial concentration     0 µmol/l             
      148 Fraction expressed intracellular     1                    
      149           LocalMoleculeParameter     0   µmol             
      150           LocalMoleculeParameter     0   µmol             
      151            Initial concentration     0 µmol/l             
      152            Initial concentration     0 µmol/l             
      153           LocalMoleculeParameter     0   µmol             
      154           LocalMoleculeParameter     0   µmol             
      155            Initial concentration     0 µmol/l             
      156            Initial concentration     0 µmol/l             
      157           LocalMoleculeParameter     0   µmol             
      158           LocalMoleculeParameter     0   µmol             
      159            Initial concentration     0 µmol/l             
      160            Initial concentration     0 µmol/l             
      161           LocalMoleculeParameter     0   µmol             
      162           LocalMoleculeParameter     0   µmol             
      163              Relative expression     0                    
      164            Initial concentration     0 µmol/l             
      165 Fraction expressed intracellular     1                    
      166              Relative expression     0                    
      167            Initial concentration     0 µmol/l             
      168 Fraction expressed intracellular     1                    
      169           LocalMoleculeParameter     0   µmol             
      170           LocalMoleculeParameter     0   µmol             
      171            Initial concentration     0 µmol/l             
      172            Initial concentration     0 µmol/l             
      173           LocalMoleculeParameter     0   µmol             
      174           LocalMoleculeParameter     0   µmol             
      175            Initial concentration     0 µmol/l             
      176            Initial concentration     0 µmol/l             
      177           LocalMoleculeParameter     0   µmol             
      178           LocalMoleculeParameter     0   µmol             
      179            Initial concentration     0 µmol/l             
      180            Initial concentration     0 µmol/l             
      181           LocalMoleculeParameter     0   µmol             
      182           LocalMoleculeParameter     0   µmol             
      183              Relative expression     0                    
      184            Initial concentration     0 µmol/l             
      185 Fraction expressed intracellular     1                    
      186              Relative expression     0                    
      187            Initial concentration     0 µmol/l             
      188 Fraction expressed intracellular     1                    
      189           LocalMoleculeParameter     0   µmol             
      190           LocalMoleculeParameter     0   µmol             
      191            Initial concentration     0 µmol/l             
      192            Initial concentration     0 µmol/l             
      193           LocalMoleculeParameter     0   µmol             
      194           LocalMoleculeParameter     0   µmol             
      195            Initial concentration     0 µmol/l             
      196            Initial concentration     0 µmol/l             
      197           LocalMoleculeParameter     0   µmol             
      198           LocalMoleculeParameter     0   µmol             
      199            Initial concentration     0 µmol/l             
      200            Initial concentration     0 µmol/l             
      201           LocalMoleculeParameter     0   µmol             
      202           LocalMoleculeParameter     0   µmol             
      203              Relative expression     0                    
      204            Initial concentration     0 µmol/l             
      205 Fraction expressed intracellular     1                    
      206              Relative expression     0                    
      207            Initial concentration     0 µmol/l             
      208 Fraction expressed intracellular     1                    
      209           LocalMoleculeParameter     0   µmol             
      210           LocalMoleculeParameter     0   µmol             
      211            Initial concentration     0 µmol/l             
      212            Initial concentration     0 µmol/l             
      213           LocalMoleculeParameter     0   µmol             
      214           LocalMoleculeParameter     0   µmol             
      215            Initial concentration     0 µmol/l             
      216            Initial concentration     0 µmol/l             
      217           LocalMoleculeParameter     0   µmol             
      218           LocalMoleculeParameter     0   µmol             
      219            Initial concentration     0 µmol/l             
      220            Initial concentration     0 µmol/l             
      221           LocalMoleculeParameter     0   µmol             
      222           LocalMoleculeParameter     0   µmol             
      223              Relative expression     0                    
      224            Initial concentration     0 µmol/l             
      225 Fraction expressed intracellular     1                    
      226              Relative expression     0                    
      227            Initial concentration     0 µmol/l             
      228 Fraction expressed intracellular     1                    
      229           LocalMoleculeParameter     0   µmol             
      230           LocalMoleculeParameter     0   µmol             
      231            Initial concentration     0 µmol/l             
      232            Initial concentration     0 µmol/l             
      233           LocalMoleculeParameter     0   µmol             
      234           LocalMoleculeParameter     0   µmol             
      235            Initial concentration     0 µmol/l             
      236            Initial concentration     0 µmol/l             
      237           LocalMoleculeParameter     0   µmol             
      238           LocalMoleculeParameter     0   µmol             
      239            Initial concentration     0 µmol/l             
      240            Initial concentration     0 µmol/l             
      241           LocalMoleculeParameter     0   µmol             
      242           LocalMoleculeParameter     0   µmol             
      243              Relative expression     0                    
      244            Initial concentration     0 µmol/l             
      245 Fraction expressed intracellular     1                    
      246              Relative expression     0                    
      247            Initial concentration     0 µmol/l             
      248 Fraction expressed intracellular     1                    
      249           LocalMoleculeParameter     0   µmol             
      250           LocalMoleculeParameter     0   µmol             
      251            Initial concentration     0 µmol/l             
      252            Initial concentration     0 µmol/l             
      253           LocalMoleculeParameter     0   µmol             
      254           LocalMoleculeParameter     0   µmol             
      255            Initial concentration     0 µmol/l             
      256            Initial concentration     0 µmol/l             
      257           LocalMoleculeParameter     0   µmol             
      258           LocalMoleculeParameter     0   µmol             
      259            Initial concentration     0 µmol/l             
      260            Initial concentration     0 µmol/l             
      261           LocalMoleculeParameter     0   µmol             
      262           LocalMoleculeParameter     0   µmol             
      263              Relative expression     0                    
      264            Initial concentration     0 µmol/l             
      265 Fraction expressed intracellular     1                    
      266              Relative expression     0                    
      267            Initial concentration     0 µmol/l             
      268 Fraction expressed intracellular     1                    
      269           LocalMoleculeParameter     0   µmol             
      270           LocalMoleculeParameter     0   µmol             
      271            Initial concentration     0 µmol/l             
      272            Initial concentration     0 µmol/l             
      273           LocalMoleculeParameter     0   µmol             
      274           LocalMoleculeParameter     0   µmol             
      275            Initial concentration     0 µmol/l             
      276            Initial concentration     0 µmol/l             
      277           LocalMoleculeParameter     0   µmol             
      278           LocalMoleculeParameter     0   µmol             
      279            Initial concentration     0 µmol/l             
      280            Initial concentration     0 µmol/l             
      281           LocalMoleculeParameter     0   µmol             
      282           LocalMoleculeParameter     0   µmol             
      283              Relative expression     0                    
      284            Initial concentration     0 µmol/l             
      285 Fraction expressed intracellular     1                    
      286              Relative expression     0                    
      287            Initial concentration     0 µmol/l             
      288 Fraction expressed intracellular     1                    
      289           LocalMoleculeParameter     0   µmol             
      290           LocalMoleculeParameter     0   µmol             
      291            Initial concentration     0 µmol/l             
      292            Initial concentration     0 µmol/l             
      293           LocalMoleculeParameter     0   µmol             
      294           LocalMoleculeParameter     0   µmol             
      295            Initial concentration     0 µmol/l             
      296            Initial concentration     0 µmol/l             
      297           LocalMoleculeParameter     0   µmol             
      298           LocalMoleculeParameter     0   µmol             
      299            Initial concentration     0 µmol/l             
      300            Initial concentration     0 µmol/l             
      301           LocalMoleculeParameter     0   µmol             
      302           LocalMoleculeParameter     0   µmol             
      303              Relative expression     0                    
      304            Initial concentration     0 µmol/l             
      305 Fraction expressed intracellular     1                    
      306              Relative expression     0                    
      307            Initial concentration     0 µmol/l             
      308 Fraction expressed intracellular     1                    
      309           LocalMoleculeParameter     0   µmol             
      310           LocalMoleculeParameter     0   µmol             
      311            Initial concentration     0 µmol/l             
      312            Initial concentration     0 µmol/l             
      313           LocalMoleculeParameter     0   µmol             
      314           LocalMoleculeParameter     0   µmol             
      315            Initial concentration     0 µmol/l             
      316            Initial concentration     0 µmol/l             
      317           LocalMoleculeParameter     0   µmol             
      318           LocalMoleculeParameter     0   µmol             
      319            Initial concentration     0 µmol/l             
      320            Initial concentration     0 µmol/l             
      321           LocalMoleculeParameter     0   µmol             
      322           LocalMoleculeParameter     0   µmol             
      323            Initial concentration     0 µmol/l             
      324            Initial concentration     0 µmol/l             
      325           LocalMoleculeParameter     0   µmol             
      326           LocalMoleculeParameter     0   µmol             
      327            Initial concentration     0 µmol/l             
      328            Initial concentration     0 µmol/l             
      329           LocalMoleculeParameter     0   µmol             
      330           LocalMoleculeParameter     0   µmol             
      331            Initial concentration     0 µmol/l             
      332            Initial concentration     0 µmol/l             
      333           LocalMoleculeParameter     0   µmol             
      334           LocalMoleculeParameter     0   µmol             
      335            Initial concentration     0 µmol/l             
      336            Initial concentration     0 µmol/l             
      337           LocalMoleculeParameter     0   µmol             
      338           LocalMoleculeParameter     0   µmol             
      339            Initial concentration     0 µmol/l             
      340            Initial concentration     0 µmol/l             
      341           LocalMoleculeParameter     0   µmol             
      342           LocalMoleculeParameter     0   µmol             
      343            Initial concentration     0 µmol/l             
      344            Initial concentration     0 µmol/l             
      345           LocalMoleculeParameter     0   µmol             
      346           LocalMoleculeParameter     0   µmol             
      347            Initial concentration     0 µmol/l             
      348            Initial concentration     0 µmol/l             
      349           LocalMoleculeParameter     0   µmol             
      350           LocalMoleculeParameter     0   µmol             
      351            Initial concentration     0 µmol/l             
      352            Initial concentration     0 µmol/l             
      353           LocalMoleculeParameter     0   µmol             
      354           LocalMoleculeParameter     0   µmol             
      355            Initial concentration     0 µmol/l             
      356            Initial concentration     0 µmol/l             
      357           LocalMoleculeParameter     0   µmol             
      358           LocalMoleculeParameter     0   µmol             
      359            Initial concentration     0 µmol/l             
      360            Initial concentration     0 µmol/l             
      361           LocalMoleculeParameter     0   µmol             
      362           LocalMoleculeParameter     0   µmol             
      363            Initial concentration     0 µmol/l             
      364            Initial concentration     0 µmol/l             
      365           LocalMoleculeParameter     0   µmol             
      366           LocalMoleculeParameter     0   µmol             
      367            Initial concentration     0 µmol/l             
      368            Initial concentration     0 µmol/l             
      369           LocalMoleculeParameter     0   µmol             
      370           LocalMoleculeParameter     0   µmol             
      371            Initial concentration     0 µmol/l             
      372            Initial concentration     0 µmol/l             
      373           LocalMoleculeParameter     0   µmol             
      374           LocalMoleculeParameter     0   µmol             
      375              Relative expression     0                    
      376            Initial concentration     0 µmol/l             
      377 Fraction expressed intracellular     1                    
      378              Relative expression     0                    
      379            Initial concentration     0 µmol/l             
      380 Fraction expressed intracellular     1                    
      381           LocalMoleculeParameter     0   µmol             
      382           LocalMoleculeParameter     0   µmol             
      383            Initial concentration     0 µmol/l             
      384            Initial concentration     0 µmol/l             
      385           LocalMoleculeParameter     0   µmol             
      386           LocalMoleculeParameter     0   µmol             
      387            Initial concentration     0 µmol/l             
      388            Initial concentration     0 µmol/l             
      389           LocalMoleculeParameter     0   µmol             
      390           LocalMoleculeParameter     0   µmol             
      391            Initial concentration     0 µmol/l             
      392            Initial concentration     0 µmol/l             
      393           LocalMoleculeParameter     0   µmol             
      394           LocalMoleculeParameter     0   µmol             
      395              Relative expression     0                    
      396            Initial concentration     0 µmol/l             
      397 Fraction expressed intracellular     1                    
      398              Relative expression     0                    
      399            Initial concentration     0 µmol/l             
      400 Fraction expressed intracellular     1                    
      401           LocalMoleculeParameter     0   µmol             
      402           LocalMoleculeParameter     0   µmol             
      403            Initial concentration     0 µmol/l             
      404            Initial concentration     0 µmol/l             
      405           LocalMoleculeParameter     0   µmol             
      406           LocalMoleculeParameter     0   µmol             
      407            Initial concentration     0 µmol/l             
      408            Initial concentration     0 µmol/l             
      409           LocalMoleculeParameter     0   µmol             
      410           LocalMoleculeParameter     0   µmol             
      411            Initial concentration     0 µmol/l             
      412            Initial concentration     0 µmol/l             
      413           LocalMoleculeParameter     0   µmol             
      414           LocalMoleculeParameter     0   µmol             
      415              Relative expression     0                    
      416            Initial concentration     0 µmol/l             
      417 Fraction expressed intracellular     1                    
      418              Relative expression     0                    
      419            Initial concentration     0 µmol/l             
      420 Fraction expressed intracellular     1                    
      421           LocalMoleculeParameter     0   µmol             
      422           LocalMoleculeParameter     0   µmol             
      423            Initial concentration     0 µmol/l             
      424            Initial concentration     0 µmol/l             
      425           LocalMoleculeParameter     0   µmol             
      426           LocalMoleculeParameter     0   µmol             
      427            Initial concentration     0 µmol/l             
      428            Initial concentration     0 µmol/l             
      429           LocalMoleculeParameter     0   µmol             
      430           LocalMoleculeParameter     0   µmol             
      431            Initial concentration     0 µmol/l             
      432            Initial concentration     0 µmol/l             
      433           LocalMoleculeParameter     0   µmol             
      434           LocalMoleculeParameter     0   µmol             
      435            Initial concentration     0 µmol/l             
      436            Initial concentration     0 µmol/l             
      437           LocalMoleculeParameter     0   µmol             
      438           LocalMoleculeParameter     0   µmol             
      439            Initial concentration     0 µmol/l             
      440            Initial concentration     0 µmol/l             
      441           LocalMoleculeParameter     0   µmol             
      442           LocalMoleculeParameter     0   µmol             
      443              Relative expression     0                    
      444            Initial concentration     0 µmol/l             
      445 Fraction expressed intracellular     1                    
      446              Relative expression     0                    
      447            Initial concentration     0 µmol/l             
      448 Fraction expressed intracellular     1                    
      449           LocalMoleculeParameter     0   µmol             
      450           LocalMoleculeParameter     0   µmol             
      451            Initial concentration     0 µmol/l             
      452            Initial concentration     0 µmol/l             
      453           LocalMoleculeParameter     0   µmol             
      454           LocalMoleculeParameter     0   µmol             
      455            Initial concentration     0 µmol/l             
      456            Initial concentration     0 µmol/l             
      457           LocalMoleculeParameter     0   µmol             
      458           LocalMoleculeParameter     0   µmol             
      459            Initial concentration     0 µmol/l             
      460            Initial concentration     0 µmol/l             
      461           LocalMoleculeParameter     0   µmol             
      462           LocalMoleculeParameter     0   µmol             
      463              Relative expression     0                    
      464            Initial concentration     0 µmol/l             
      465 Fraction expressed intracellular     1                    
      466              Relative expression     0                    
      467            Initial concentration     0 µmol/l             
      468 Fraction expressed intracellular     1                    
      469           LocalMoleculeParameter     0   µmol             
      470           LocalMoleculeParameter     0   µmol             
      471            Initial concentration     0 µmol/l             
      472            Initial concentration     0 µmol/l             
      473           LocalMoleculeParameter     0   µmol             
      474           LocalMoleculeParameter     0   µmol             
      475            Initial concentration     0 µmol/l             
      476            Initial concentration     0 µmol/l             
      477           LocalMoleculeParameter     0   µmol             
      478           LocalMoleculeParameter     0   µmol             
      479            Initial concentration     0 µmol/l             
      480            Initial concentration     0 µmol/l             
      481           LocalMoleculeParameter     0   µmol             
      482           LocalMoleculeParameter     0   µmol             
      483              Relative expression     0                    
      484            Initial concentration     0 µmol/l             
      485 Fraction expressed intracellular     1                    
      486              Relative expression     0                    
      487            Initial concentration     0 µmol/l             
      488 Fraction expressed intracellular     1                    
      489           LocalMoleculeParameter     0   µmol             
      490           LocalMoleculeParameter     0   µmol             
      491            Initial concentration     0 µmol/l             
      492            Initial concentration     0 µmol/l             
      493           LocalMoleculeParameter     0   µmol             
      494           LocalMoleculeParameter     0   µmol             
      495            Initial concentration     0 µmol/l             
      496            Initial concentration     0 µmol/l             
      497           LocalMoleculeParameter     0   µmol             
      498           LocalMoleculeParameter     0   µmol             
      499            Initial concentration     0 µmol/l             
      500            Initial concentration     0 µmol/l             
      501           LocalMoleculeParameter     0   µmol             
      502           LocalMoleculeParameter     0   µmol             
      503              Relative expression     0                    
      504            Initial concentration     0 µmol/l             
      505 Fraction expressed intracellular     1                    
      506              Relative expression     0                    
      507            Initial concentration     0 µmol/l             
      508 Fraction expressed intracellular     1                    
      509           LocalMoleculeParameter     0   µmol             
      510           LocalMoleculeParameter     0   µmol             
      511            Initial concentration     0 µmol/l             
      512            Initial concentration     0 µmol/l             
      513           LocalMoleculeParameter     0   µmol             
      514           LocalMoleculeParameter     0   µmol             
      515            Initial concentration     0 µmol/l             
      516            Initial concentration     0 µmol/l             
      517           LocalMoleculeParameter     0   µmol             
      518           LocalMoleculeParameter     0   µmol             
      519            Initial concentration     0 µmol/l             
      520            Initial concentration     0 µmol/l             
      521           LocalMoleculeParameter     0   µmol             
      522           LocalMoleculeParameter     0   µmol             
      523              Relative expression     0                    
      524            Initial concentration     0 µmol/l             
      525 Fraction expressed intracellular     1                    
      526              Relative expression     0                    
      527            Initial concentration     0 µmol/l             
      528 Fraction expressed intracellular     1                    
      529           LocalMoleculeParameter     0   µmol             
      530           LocalMoleculeParameter     0   µmol             
      531            Initial concentration     0 µmol/l             
      532            Initial concentration     0 µmol/l             
      533           LocalMoleculeParameter     0   µmol             
      534           LocalMoleculeParameter     0   µmol             
      535            Initial concentration     0 µmol/l             
      536            Initial concentration     0 µmol/l             
      537           LocalMoleculeParameter     0   µmol             
      538           LocalMoleculeParameter     0   µmol             
      539            Initial concentration     0 µmol/l             
      540            Initial concentration     0 µmol/l             
      541           LocalMoleculeParameter     0   µmol             
      542           LocalMoleculeParameter     0   µmol             
      543              Relative expression     0                    
      544            Initial concentration     0 µmol/l             
      545 Fraction expressed intracellular     1                    
      546              Relative expression     0                    
      547            Initial concentration     0 µmol/l             
      548 Fraction expressed intracellular     1                    
      549           LocalMoleculeParameter     0   µmol             
      550           LocalMoleculeParameter     0   µmol             
      551            Initial concentration     0 µmol/l             
      552            Initial concentration     0 µmol/l             
      553           LocalMoleculeParameter     0   µmol             
      554           LocalMoleculeParameter     0   µmol             
      555            Initial concentration     0 µmol/l             
      556            Initial concentration     0 µmol/l             
      557           LocalMoleculeParameter     0   µmol             
      558           LocalMoleculeParameter     0   µmol             
      559            Initial concentration     0 µmol/l             
      560            Initial concentration     0 µmol/l             
      561           LocalMoleculeParameter     0   µmol             
      562           LocalMoleculeParameter     0   µmol             
      563              Relative expression     0                    
      564            Initial concentration     0 µmol/l             
      565 Fraction expressed intracellular     1                    
      566              Relative expression     0                    
      567            Initial concentration     0 µmol/l             
      568 Fraction expressed intracellular     1                    
      569           LocalMoleculeParameter     0   µmol             
      570           LocalMoleculeParameter     0   µmol             
      571            Initial concentration     0 µmol/l             
      572            Initial concentration     0 µmol/l             
      573           LocalMoleculeParameter     0   µmol             
      574           LocalMoleculeParameter     0   µmol             
      575            Initial concentration     0 µmol/l             
      576            Initial concentration     0 µmol/l             
      577           LocalMoleculeParameter     0   µmol             
      578           LocalMoleculeParameter     0   µmol             
      579            Initial concentration     0 µmol/l             
      580            Initial concentration     0 µmol/l             
      581           LocalMoleculeParameter     0   µmol             
      582           LocalMoleculeParameter     0   µmol             
      583              Relative expression     0                    
      584            Initial concentration     0 µmol/l             
      585 Fraction expressed intracellular     1                    
      586              Relative expression     0                    
      587            Initial concentration     0 µmol/l             
      588 Fraction expressed intracellular     1                    
      589           LocalMoleculeParameter     0   µmol             
      590           LocalMoleculeParameter     0   µmol             
      591            Initial concentration     0 µmol/l             
      592            Initial concentration     0 µmol/l             
      593           LocalMoleculeParameter     0   µmol             
      594           LocalMoleculeParameter     0   µmol             
      595            Initial concentration     0 µmol/l             
      596            Initial concentration     0 µmol/l             
      597           LocalMoleculeParameter     0   µmol             
      598           LocalMoleculeParameter     0   µmol             
      599            Initial concentration     0 µmol/l             
      600            Initial concentration     0 µmol/l             
      601           LocalMoleculeParameter     0   µmol             
      602           LocalMoleculeParameter     0   µmol             
      603              Relative expression     0                    
      604            Initial concentration     0 µmol/l             
      605 Fraction expressed intracellular     1                    
      606              Relative expression     0                    
      607            Initial concentration     0 µmol/l             
      608 Fraction expressed intracellular     1                    
      609           LocalMoleculeParameter     0   µmol             
      610           LocalMoleculeParameter     0   µmol             
      611            Initial concentration     0 µmol/l             
      612            Initial concentration     0 µmol/l             
      613           LocalMoleculeParameter     0   µmol             
      614           LocalMoleculeParameter     0   µmol             
      615            Initial concentration     0 µmol/l             
      616            Initial concentration     0 µmol/l             
      617           LocalMoleculeParameter     0   µmol             
      618           LocalMoleculeParameter     0   µmol             
      619            Initial concentration     0 µmol/l             
      620            Initial concentration     0 µmol/l             

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

# addProteinExpressionToParameterValuesBB adds expression parameters for a single protein in a selected organ

    Code
      newPaths_df
    Output
                              Container Path                   Parameter Name Value
      1    Organism|Kidney|BloodCells|UGT2B7            Initial concentration   NaN
      2  Organism|Kidney|Interstitial|UGT2B7            Initial concentration   NaN
      3  Organism|Kidney|Interstitial|UGT2B7  Fraction expressed interstitial   NaN
      4 Organism|Kidney|Intracellular|UGT2B7              Relative expression     0
      5 Organism|Kidney|Intracellular|UGT2B7            Initial concentration   NaN
      6 Organism|Kidney|Intracellular|UGT2B7 Fraction expressed intracellular     1
      7        Organism|Kidney|Plasma|UGT2B7            Initial concentration   NaN
          Unit Value Origin
      1 µmol/l             
      2 µmol/l             
      3                    
      4                    
      5 µmol/l             
      6                    
      7 µmol/l             

