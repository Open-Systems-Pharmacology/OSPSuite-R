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
                          Container Path    Molecule Name Is Present Value Unit
      136        Organism|Thyroid|Plasma                A       TRUE     0 µmol
      137        Organism|Thyroid|Plasma                B       TRUE     0 µmol
      138        Organism|Thyroid|Plasma           UGT2B7       TRUE   NaN µmol
      139        Organism|Thyroid|Plasma           CYP3A4       TRUE   NaN µmol
      140        Organism|Thyroid|Plasma FloatingMolecule       TRUE     0 µmol
      141        Organism|Thyroid|Plasma   BindingPartner       TRUE   NaN µmol
      142        Organism|Thyroid|Plasma          OATP1B1       TRUE   NaN µmol
      143    Organism|Thyroid|BloodCells                A       TRUE     0 µmol
      144    Organism|Thyroid|BloodCells                B       TRUE     0 µmol
      145    Organism|Thyroid|BloodCells           UGT2B7       TRUE   NaN µmol
      146    Organism|Thyroid|BloodCells           CYP3A4       TRUE   NaN µmol
      147    Organism|Thyroid|BloodCells FloatingMolecule       TRUE     0 µmol
      148    Organism|Thyroid|BloodCells   BindingPartner       TRUE   NaN µmol
      149    Organism|Thyroid|BloodCells          OATP1B1       TRUE   NaN µmol
      150  Organism|Thyroid|Interstitial                A       TRUE     0 µmol
      151  Organism|Thyroid|Interstitial                B       TRUE     0 µmol
      152  Organism|Thyroid|Interstitial           UGT2B7       TRUE   NaN µmol
      153  Organism|Thyroid|Interstitial           CYP3A4       TRUE   NaN µmol
      154  Organism|Thyroid|Interstitial FloatingMolecule       TRUE     0 µmol
      155  Organism|Thyroid|Interstitial   BindingPartner       TRUE   NaN µmol
      156  Organism|Thyroid|Interstitial          OATP1B1       TRUE   NaN µmol
      157 Organism|Thyroid|Intracellular                A       TRUE     0 µmol
      158 Organism|Thyroid|Intracellular                B       TRUE     0 µmol
      159 Organism|Thyroid|Intracellular           UGT2B7       TRUE   NaN µmol
      160 Organism|Thyroid|Intracellular           CYP3A4       TRUE   NaN µmol
      161 Organism|Thyroid|Intracellular FloatingMolecule       TRUE     0 µmol
      162 Organism|Thyroid|Intracellular   BindingPartner       TRUE   NaN µmol
      163 Organism|Thyroid|Intracellular          OATP1B1       TRUE   NaN µmol
      164      Organism|Thyroid|Endosome                A       TRUE     0 µmol
      165      Organism|Thyroid|Endosome                B       TRUE     0 µmol
      166      Organism|Thyroid|Endosome           UGT2B7       TRUE   NaN µmol
      167      Organism|Thyroid|Endosome           CYP3A4       TRUE   NaN µmol
      168      Organism|Thyroid|Endosome FloatingMolecule       TRUE     0 µmol
      169      Organism|Thyroid|Endosome   BindingPartner       TRUE   NaN µmol
      170      Organism|Thyroid|Endosome          OATP1B1       TRUE   NaN µmol
      171         Organism|Thyroid|Lumen                A       TRUE     0 µmol
      172         Organism|Thyroid|Lumen                B       TRUE     0 µmol
      173         Organism|Thyroid|Lumen           UGT2B7       TRUE   NaN µmol
      174         Organism|Thyroid|Lumen           CYP3A4       TRUE   NaN µmol
      175         Organism|Thyroid|Lumen FloatingMolecule       TRUE     0 µmol
      176         Organism|Thyroid|Lumen   BindingPartner       TRUE   NaN µmol
      177         Organism|Thyroid|Lumen          OATP1B1       TRUE   NaN µmol
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
      160             1               FALSE
      161             1               FALSE
      162             1               FALSE
      163             1               FALSE
      164             1               FALSE
      165             1               FALSE
      166             1               FALSE
      167             1               FALSE
      168             1               FALSE
      169             1               FALSE
      170             1               FALSE
      171             1               FALSE
      172             1               FALSE
      173             1               FALSE
      174             1               FALSE
      175             1               FALSE
      176             1               FALSE
      177             1               FALSE

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
      1                                                         Organism|Gallbladder|A
      2                                                         Organism|Gallbladder|B
      3                                                    Organism|Gallbladder|UGT2B7
      4                                                    Organism|Gallbladder|CYP3A4
      5                                            Organism|Gallbladder|BindingPartner
      6                                                   Organism|Gallbladder|OATP1B1
      7                                            Organism|ArterialBlood|BloodCells|A
      8                                            Organism|ArterialBlood|BloodCells|B
      9                                       Organism|ArterialBlood|BloodCells|UGT2B7
      10                                      Organism|ArterialBlood|BloodCells|CYP3A4
      11                              Organism|ArterialBlood|BloodCells|BindingPartner
      12                                     Organism|ArterialBlood|BloodCells|OATP1B1
      13                                               Organism|ArterialBlood|Plasma|A
      14                                               Organism|ArterialBlood|Plasma|B
      15                                          Organism|ArterialBlood|Plasma|UGT2B7
      16                                          Organism|ArterialBlood|Plasma|CYP3A4
      17                                  Organism|ArterialBlood|Plasma|BindingPartner
      18                                         Organism|ArterialBlood|Plasma|OATP1B1
      19                                                  Organism|Bone|Interstitial|A
      20                                                  Organism|Bone|Interstitial|B
      21                                             Organism|Bone|Interstitial|UGT2B7
      22                                             Organism|Bone|Interstitial|CYP3A4
      23                                     Organism|Bone|Interstitial|BindingPartner
      24                                            Organism|Bone|Interstitial|OATP1B1
      25                                                 Organism|Bone|Intracellular|A
      26                                                 Organism|Bone|Intracellular|B
      27                                            Organism|Bone|Intracellular|UGT2B7
      28                                            Organism|Bone|Intracellular|UGT2B7
      29                                            Organism|Bone|Intracellular|UGT2B7
      30                                            Organism|Bone|Intracellular|CYP3A4
      31                                            Organism|Bone|Intracellular|CYP3A4
      32                                            Organism|Bone|Intracellular|CYP3A4
      33                                    Organism|Bone|Intracellular|BindingPartner
      34                                    Organism|Bone|Intracellular|BindingPartner
      35                                    Organism|Bone|Intracellular|BindingPartner
      36                                           Organism|Bone|Intracellular|OATP1B1
      37                                           Organism|Bone|Intracellular|OATP1B1
      38                                           Organism|Bone|Intracellular|OATP1B1
      39                                                    Organism|Bone|BloodCells|A
      40                                                    Organism|Bone|BloodCells|B
      41                                               Organism|Bone|BloodCells|UGT2B7
      42                                               Organism|Bone|BloodCells|CYP3A4
      43                                       Organism|Bone|BloodCells|BindingPartner
      44                                              Organism|Bone|BloodCells|OATP1B1
      45                                                        Organism|Bone|Plasma|A
      46                                                        Organism|Bone|Plasma|B
      47                                                   Organism|Bone|Plasma|UGT2B7
      48                                                   Organism|Bone|Plasma|CYP3A4
      49                                           Organism|Bone|Plasma|BindingPartner
      50                                                  Organism|Bone|Plasma|OATP1B1
      51                                                   Organism|Brain|BloodCells|A
      52                                                   Organism|Brain|BloodCells|B
      53                                              Organism|Brain|BloodCells|UGT2B7
      54                                              Organism|Brain|BloodCells|CYP3A4
      55                                      Organism|Brain|BloodCells|BindingPartner
      56                                             Organism|Brain|BloodCells|OATP1B1
      57                                                 Organism|Brain|Interstitial|A
      58                                                 Organism|Brain|Interstitial|B
      59                                            Organism|Brain|Interstitial|UGT2B7
      60                                            Organism|Brain|Interstitial|CYP3A4
      61                                    Organism|Brain|Interstitial|BindingPartner
      62                                           Organism|Brain|Interstitial|OATP1B1
      63                                                Organism|Brain|Intracellular|A
      64                                                Organism|Brain|Intracellular|B
      65                                           Organism|Brain|Intracellular|UGT2B7
      66                                           Organism|Brain|Intracellular|UGT2B7
      67                                           Organism|Brain|Intracellular|UGT2B7
      68                                           Organism|Brain|Intracellular|CYP3A4
      69                                           Organism|Brain|Intracellular|CYP3A4
      70                                           Organism|Brain|Intracellular|CYP3A4
      71                                   Organism|Brain|Intracellular|BindingPartner
      72                                   Organism|Brain|Intracellular|BindingPartner
      73                                   Organism|Brain|Intracellular|BindingPartner
      74                                          Organism|Brain|Intracellular|OATP1B1
      75                                          Organism|Brain|Intracellular|OATP1B1
      76                                                       Organism|Brain|Plasma|A
      77                                                       Organism|Brain|Plasma|B
      78                                                  Organism|Brain|Plasma|UGT2B7
      79                                                  Organism|Brain|Plasma|CYP3A4
      80                                          Organism|Brain|Plasma|BindingPartner
      81                                                 Organism|Brain|Plasma|OATP1B1
      82                                                 Organism|Brain|Plasma|OATP1B1
      83                                                     Organism|Fat|BloodCells|A
      84                                                     Organism|Fat|BloodCells|B
      85                                                Organism|Fat|BloodCells|UGT2B7
      86                                                Organism|Fat|BloodCells|CYP3A4
      87                                        Organism|Fat|BloodCells|BindingPartner
      88                                               Organism|Fat|BloodCells|OATP1B1
      89                                                   Organism|Fat|Interstitial|A
      90                                                   Organism|Fat|Interstitial|B
      91                                              Organism|Fat|Interstitial|UGT2B7
      92                                              Organism|Fat|Interstitial|CYP3A4
      93                                      Organism|Fat|Interstitial|BindingPartner
      94                                             Organism|Fat|Interstitial|OATP1B1
      95                                                  Organism|Fat|Intracellular|A
      96                                                  Organism|Fat|Intracellular|B
      97                                             Organism|Fat|Intracellular|UGT2B7
      98                                             Organism|Fat|Intracellular|UGT2B7
      99                                             Organism|Fat|Intracellular|UGT2B7
      100                                            Organism|Fat|Intracellular|CYP3A4
      101                                            Organism|Fat|Intracellular|CYP3A4
      102                                            Organism|Fat|Intracellular|CYP3A4
      103                                    Organism|Fat|Intracellular|BindingPartner
      104                                    Organism|Fat|Intracellular|BindingPartner
      105                                    Organism|Fat|Intracellular|BindingPartner
      106                                           Organism|Fat|Intracellular|OATP1B1
      107                                           Organism|Fat|Intracellular|OATP1B1
      108                                           Organism|Fat|Intracellular|OATP1B1
      109                                                        Organism|Fat|Plasma|A
      110                                                        Organism|Fat|Plasma|B
      111                                                   Organism|Fat|Plasma|UGT2B7
      112                                                   Organism|Fat|Plasma|CYP3A4
      113                                           Organism|Fat|Plasma|BindingPartner
      114                                                  Organism|Fat|Plasma|OATP1B1
      115                                                 Organism|Gonads|BloodCells|A
      116                                                 Organism|Gonads|BloodCells|B
      117                                            Organism|Gonads|BloodCells|UGT2B7
      118                                            Organism|Gonads|BloodCells|CYP3A4
      119                                    Organism|Gonads|BloodCells|BindingPartner
      120                                           Organism|Gonads|BloodCells|OATP1B1
      121                                               Organism|Gonads|Interstitial|A
      122                                               Organism|Gonads|Interstitial|B
      123                                          Organism|Gonads|Interstitial|UGT2B7
      124                                          Organism|Gonads|Interstitial|CYP3A4
      125                                  Organism|Gonads|Interstitial|BindingPartner
      126                                         Organism|Gonads|Interstitial|OATP1B1
      127                                              Organism|Gonads|Intracellular|A
      128                                              Organism|Gonads|Intracellular|B
      129                                         Organism|Gonads|Intracellular|UGT2B7
      130                                         Organism|Gonads|Intracellular|UGT2B7
      131                                         Organism|Gonads|Intracellular|UGT2B7
      132                                         Organism|Gonads|Intracellular|CYP3A4
      133                                         Organism|Gonads|Intracellular|CYP3A4
      134                                         Organism|Gonads|Intracellular|CYP3A4
      135                                 Organism|Gonads|Intracellular|BindingPartner
      136                                 Organism|Gonads|Intracellular|BindingPartner
      137                                 Organism|Gonads|Intracellular|BindingPartner
      138                                        Organism|Gonads|Intracellular|OATP1B1
      139                                        Organism|Gonads|Intracellular|OATP1B1
      140                                        Organism|Gonads|Intracellular|OATP1B1
      141                                                     Organism|Gonads|Plasma|A
      142                                                     Organism|Gonads|Plasma|B
      143                                                Organism|Gonads|Plasma|UGT2B7
      144                                                Organism|Gonads|Plasma|CYP3A4
      145                                        Organism|Gonads|Plasma|BindingPartner
      146                                               Organism|Gonads|Plasma|OATP1B1
      147                                                  Organism|Heart|BloodCells|A
      148                                                  Organism|Heart|BloodCells|B
      149                                             Organism|Heart|BloodCells|UGT2B7
      150                                             Organism|Heart|BloodCells|CYP3A4
      151                                     Organism|Heart|BloodCells|BindingPartner
      152                                            Organism|Heart|BloodCells|OATP1B1
      153                                                Organism|Heart|Interstitial|A
      154                                                Organism|Heart|Interstitial|B
      155                                           Organism|Heart|Interstitial|UGT2B7
      156                                           Organism|Heart|Interstitial|CYP3A4
      157                                   Organism|Heart|Interstitial|BindingPartner
      158                                          Organism|Heart|Interstitial|OATP1B1
      159                                               Organism|Heart|Intracellular|A
      160                                               Organism|Heart|Intracellular|B
      161                                          Organism|Heart|Intracellular|UGT2B7
      162                                          Organism|Heart|Intracellular|UGT2B7
      163                                          Organism|Heart|Intracellular|UGT2B7
      164                                          Organism|Heart|Intracellular|CYP3A4
      165                                          Organism|Heart|Intracellular|CYP3A4
      166                                          Organism|Heart|Intracellular|CYP3A4
      167                                  Organism|Heart|Intracellular|BindingPartner
      168                                  Organism|Heart|Intracellular|BindingPartner
      169                                  Organism|Heart|Intracellular|BindingPartner
      170                                         Organism|Heart|Intracellular|OATP1B1
      171                                         Organism|Heart|Intracellular|OATP1B1
      172                                         Organism|Heart|Intracellular|OATP1B1
      173                                                      Organism|Heart|Plasma|A
      174                                                      Organism|Heart|Plasma|B
      175                                                 Organism|Heart|Plasma|UGT2B7
      176                                                 Organism|Heart|Plasma|CYP3A4
      177                                         Organism|Heart|Plasma|BindingPartner
      178                                                Organism|Heart|Plasma|OATP1B1
      179                                                 Organism|Kidney|BloodCells|A
      180                                                 Organism|Kidney|BloodCells|B
      181                                            Organism|Kidney|BloodCells|UGT2B7
      182                                            Organism|Kidney|BloodCells|CYP3A4
      183                                    Organism|Kidney|BloodCells|BindingPartner
      184                                           Organism|Kidney|BloodCells|OATP1B1
      185                                               Organism|Kidney|Interstitial|A
      186                                               Organism|Kidney|Interstitial|B
      187                                          Organism|Kidney|Interstitial|UGT2B7
      188                                          Organism|Kidney|Interstitial|CYP3A4
      189                                  Organism|Kidney|Interstitial|BindingPartner
      190                                         Organism|Kidney|Interstitial|OATP1B1
      191                                              Organism|Kidney|Intracellular|A
      192                                              Organism|Kidney|Intracellular|B
      193                                         Organism|Kidney|Intracellular|UGT2B7
      194                                         Organism|Kidney|Intracellular|UGT2B7
      195                                         Organism|Kidney|Intracellular|UGT2B7
      196                                         Organism|Kidney|Intracellular|CYP3A4
      197                                         Organism|Kidney|Intracellular|CYP3A4
      198                                         Organism|Kidney|Intracellular|CYP3A4
      199                                 Organism|Kidney|Intracellular|BindingPartner
      200                                 Organism|Kidney|Intracellular|BindingPartner
      201                                 Organism|Kidney|Intracellular|BindingPartner
      202                                        Organism|Kidney|Intracellular|OATP1B1
      203                                        Organism|Kidney|Intracellular|OATP1B1
      204                                        Organism|Kidney|Intracellular|OATP1B1
      205                                                     Organism|Kidney|Plasma|A
      206                                                     Organism|Kidney|Plasma|B
      207                                                Organism|Kidney|Plasma|UGT2B7
      208                                                Organism|Kidney|Plasma|CYP3A4
      209                                        Organism|Kidney|Plasma|BindingPartner
      210                                               Organism|Kidney|Plasma|OATP1B1
      211                                         Organism|LargeIntestine|BloodCells|A
      212                                         Organism|LargeIntestine|BloodCells|B
      213                                    Organism|LargeIntestine|BloodCells|UGT2B7
      214                                    Organism|LargeIntestine|BloodCells|CYP3A4
      215                            Organism|LargeIntestine|BloodCells|BindingPartner
      216                                   Organism|LargeIntestine|BloodCells|OATP1B1
      217                                       Organism|LargeIntestine|Interstitial|A
      218                                       Organism|LargeIntestine|Interstitial|B
      219                                  Organism|LargeIntestine|Interstitial|UGT2B7
      220                                  Organism|LargeIntestine|Interstitial|CYP3A4
      221                          Organism|LargeIntestine|Interstitial|BindingPartner
      222                                 Organism|LargeIntestine|Interstitial|OATP1B1
      223                                      Organism|LargeIntestine|Intracellular|A
      224                                      Organism|LargeIntestine|Intracellular|B
      225                                 Organism|LargeIntestine|Intracellular|UGT2B7
      226                                 Organism|LargeIntestine|Intracellular|UGT2B7
      227                                 Organism|LargeIntestine|Intracellular|UGT2B7
      228                                 Organism|LargeIntestine|Intracellular|CYP3A4
      229                                 Organism|LargeIntestine|Intracellular|CYP3A4
      230                                 Organism|LargeIntestine|Intracellular|CYP3A4
      231                         Organism|LargeIntestine|Intracellular|BindingPartner
      232                         Organism|LargeIntestine|Intracellular|BindingPartner
      233                         Organism|LargeIntestine|Intracellular|BindingPartner
      234                                Organism|LargeIntestine|Intracellular|OATP1B1
      235                                Organism|LargeIntestine|Intracellular|OATP1B1
      236                                Organism|LargeIntestine|Intracellular|OATP1B1
      237                                             Organism|LargeIntestine|Plasma|A
      238                                             Organism|LargeIntestine|Plasma|B
      239                                        Organism|LargeIntestine|Plasma|UGT2B7
      240                                        Organism|LargeIntestine|Plasma|CYP3A4
      241                                Organism|LargeIntestine|Plasma|BindingPartner
      242                                       Organism|LargeIntestine|Plasma|OATP1B1
      243                           Organism|LargeIntestine|Mucosa|Caecum|BloodCells|A
      244                           Organism|LargeIntestine|Mucosa|Caecum|BloodCells|B
      245                      Organism|LargeIntestine|Mucosa|Caecum|BloodCells|UGT2B7
      246                      Organism|LargeIntestine|Mucosa|Caecum|BloodCells|CYP3A4
      247              Organism|LargeIntestine|Mucosa|Caecum|BloodCells|BindingPartner
      248                     Organism|LargeIntestine|Mucosa|Caecum|BloodCells|OATP1B1
      249                         Organism|LargeIntestine|Mucosa|Caecum|Interstitial|A
      250                         Organism|LargeIntestine|Mucosa|Caecum|Interstitial|B
      251                    Organism|LargeIntestine|Mucosa|Caecum|Interstitial|UGT2B7
      252                    Organism|LargeIntestine|Mucosa|Caecum|Interstitial|CYP3A4
      253            Organism|LargeIntestine|Mucosa|Caecum|Interstitial|BindingPartner
      254                   Organism|LargeIntestine|Mucosa|Caecum|Interstitial|OATP1B1
      255                        Organism|LargeIntestine|Mucosa|Caecum|Intracellular|A
      256                        Organism|LargeIntestine|Mucosa|Caecum|Intracellular|B
      257                   Organism|LargeIntestine|Mucosa|Caecum|Intracellular|UGT2B7
      258                   Organism|LargeIntestine|Mucosa|Caecum|Intracellular|UGT2B7
      259                   Organism|LargeIntestine|Mucosa|Caecum|Intracellular|UGT2B7
      260                   Organism|LargeIntestine|Mucosa|Caecum|Intracellular|CYP3A4
      261                   Organism|LargeIntestine|Mucosa|Caecum|Intracellular|CYP3A4
      262                   Organism|LargeIntestine|Mucosa|Caecum|Intracellular|CYP3A4
      263           Organism|LargeIntestine|Mucosa|Caecum|Intracellular|BindingPartner
      264           Organism|LargeIntestine|Mucosa|Caecum|Intracellular|BindingPartner
      265           Organism|LargeIntestine|Mucosa|Caecum|Intracellular|BindingPartner
      266                  Organism|LargeIntestine|Mucosa|Caecum|Intracellular|OATP1B1
      267                  Organism|LargeIntestine|Mucosa|Caecum|Intracellular|OATP1B1
      268                  Organism|LargeIntestine|Mucosa|Caecum|Intracellular|OATP1B1
      269                               Organism|LargeIntestine|Mucosa|Caecum|Plasma|A
      270                               Organism|LargeIntestine|Mucosa|Caecum|Plasma|B
      271                          Organism|LargeIntestine|Mucosa|Caecum|Plasma|UGT2B7
      272                          Organism|LargeIntestine|Mucosa|Caecum|Plasma|CYP3A4
      273                  Organism|LargeIntestine|Mucosa|Caecum|Plasma|BindingPartner
      274                         Organism|LargeIntestine|Mucosa|Caecum|Plasma|OATP1B1
      275                   Organism|LargeIntestine|Mucosa|ColonAscendens|BloodCells|A
      276                   Organism|LargeIntestine|Mucosa|ColonAscendens|BloodCells|B
      277              Organism|LargeIntestine|Mucosa|ColonAscendens|BloodCells|UGT2B7
      278              Organism|LargeIntestine|Mucosa|ColonAscendens|BloodCells|CYP3A4
      279      Organism|LargeIntestine|Mucosa|ColonAscendens|BloodCells|BindingPartner
      280             Organism|LargeIntestine|Mucosa|ColonAscendens|BloodCells|OATP1B1
      281                 Organism|LargeIntestine|Mucosa|ColonAscendens|Interstitial|A
      282                 Organism|LargeIntestine|Mucosa|ColonAscendens|Interstitial|B
      283            Organism|LargeIntestine|Mucosa|ColonAscendens|Interstitial|UGT2B7
      284            Organism|LargeIntestine|Mucosa|ColonAscendens|Interstitial|CYP3A4
      285    Organism|LargeIntestine|Mucosa|ColonAscendens|Interstitial|BindingPartner
      286           Organism|LargeIntestine|Mucosa|ColonAscendens|Interstitial|OATP1B1
      287                Organism|LargeIntestine|Mucosa|ColonAscendens|Intracellular|A
      288                Organism|LargeIntestine|Mucosa|ColonAscendens|Intracellular|B
      289           Organism|LargeIntestine|Mucosa|ColonAscendens|Intracellular|UGT2B7
      290           Organism|LargeIntestine|Mucosa|ColonAscendens|Intracellular|UGT2B7
      291           Organism|LargeIntestine|Mucosa|ColonAscendens|Intracellular|UGT2B7
      292           Organism|LargeIntestine|Mucosa|ColonAscendens|Intracellular|CYP3A4
      293           Organism|LargeIntestine|Mucosa|ColonAscendens|Intracellular|CYP3A4
      294           Organism|LargeIntestine|Mucosa|ColonAscendens|Intracellular|CYP3A4
      295   Organism|LargeIntestine|Mucosa|ColonAscendens|Intracellular|BindingPartner
      296   Organism|LargeIntestine|Mucosa|ColonAscendens|Intracellular|BindingPartner
      297   Organism|LargeIntestine|Mucosa|ColonAscendens|Intracellular|BindingPartner
      298          Organism|LargeIntestine|Mucosa|ColonAscendens|Intracellular|OATP1B1
      299          Organism|LargeIntestine|Mucosa|ColonAscendens|Intracellular|OATP1B1
      300          Organism|LargeIntestine|Mucosa|ColonAscendens|Intracellular|OATP1B1
      301                       Organism|LargeIntestine|Mucosa|ColonAscendens|Plasma|A
      302                       Organism|LargeIntestine|Mucosa|ColonAscendens|Plasma|B
      303                  Organism|LargeIntestine|Mucosa|ColonAscendens|Plasma|UGT2B7
      304                  Organism|LargeIntestine|Mucosa|ColonAscendens|Plasma|CYP3A4
      305          Organism|LargeIntestine|Mucosa|ColonAscendens|Plasma|BindingPartner
      306                 Organism|LargeIntestine|Mucosa|ColonAscendens|Plasma|OATP1B1
      307                  Organism|LargeIntestine|Mucosa|ColonDescendens|BloodCells|A
      308                  Organism|LargeIntestine|Mucosa|ColonDescendens|BloodCells|B
      309             Organism|LargeIntestine|Mucosa|ColonDescendens|BloodCells|UGT2B7
      310             Organism|LargeIntestine|Mucosa|ColonDescendens|BloodCells|CYP3A4
      311     Organism|LargeIntestine|Mucosa|ColonDescendens|BloodCells|BindingPartner
      312            Organism|LargeIntestine|Mucosa|ColonDescendens|BloodCells|OATP1B1
      313                Organism|LargeIntestine|Mucosa|ColonDescendens|Interstitial|A
      314                Organism|LargeIntestine|Mucosa|ColonDescendens|Interstitial|B
      315           Organism|LargeIntestine|Mucosa|ColonDescendens|Interstitial|UGT2B7
      316           Organism|LargeIntestine|Mucosa|ColonDescendens|Interstitial|CYP3A4
      317   Organism|LargeIntestine|Mucosa|ColonDescendens|Interstitial|BindingPartner
      318          Organism|LargeIntestine|Mucosa|ColonDescendens|Interstitial|OATP1B1
      319               Organism|LargeIntestine|Mucosa|ColonDescendens|Intracellular|A
      320               Organism|LargeIntestine|Mucosa|ColonDescendens|Intracellular|B
      321          Organism|LargeIntestine|Mucosa|ColonDescendens|Intracellular|UGT2B7
      322          Organism|LargeIntestine|Mucosa|ColonDescendens|Intracellular|UGT2B7
      323          Organism|LargeIntestine|Mucosa|ColonDescendens|Intracellular|UGT2B7
      324          Organism|LargeIntestine|Mucosa|ColonDescendens|Intracellular|CYP3A4
      325          Organism|LargeIntestine|Mucosa|ColonDescendens|Intracellular|CYP3A4
      326          Organism|LargeIntestine|Mucosa|ColonDescendens|Intracellular|CYP3A4
      327  Organism|LargeIntestine|Mucosa|ColonDescendens|Intracellular|BindingPartner
      328  Organism|LargeIntestine|Mucosa|ColonDescendens|Intracellular|BindingPartner
      329  Organism|LargeIntestine|Mucosa|ColonDescendens|Intracellular|BindingPartner
      330         Organism|LargeIntestine|Mucosa|ColonDescendens|Intracellular|OATP1B1
      331         Organism|LargeIntestine|Mucosa|ColonDescendens|Intracellular|OATP1B1
      332         Organism|LargeIntestine|Mucosa|ColonDescendens|Intracellular|OATP1B1
      333                      Organism|LargeIntestine|Mucosa|ColonDescendens|Plasma|A
      334                      Organism|LargeIntestine|Mucosa|ColonDescendens|Plasma|B
      335                 Organism|LargeIntestine|Mucosa|ColonDescendens|Plasma|UGT2B7
      336                 Organism|LargeIntestine|Mucosa|ColonDescendens|Plasma|CYP3A4
      337         Organism|LargeIntestine|Mucosa|ColonDescendens|Plasma|BindingPartner
      338                Organism|LargeIntestine|Mucosa|ColonDescendens|Plasma|OATP1B1
      339                     Organism|LargeIntestine|Mucosa|ColonSigmoid|BloodCells|A
      340                     Organism|LargeIntestine|Mucosa|ColonSigmoid|BloodCells|B
      341                Organism|LargeIntestine|Mucosa|ColonSigmoid|BloodCells|UGT2B7
      342                Organism|LargeIntestine|Mucosa|ColonSigmoid|BloodCells|CYP3A4
      343        Organism|LargeIntestine|Mucosa|ColonSigmoid|BloodCells|BindingPartner
      344               Organism|LargeIntestine|Mucosa|ColonSigmoid|BloodCells|OATP1B1
      345                   Organism|LargeIntestine|Mucosa|ColonSigmoid|Interstitial|A
      346                   Organism|LargeIntestine|Mucosa|ColonSigmoid|Interstitial|B
      347              Organism|LargeIntestine|Mucosa|ColonSigmoid|Interstitial|UGT2B7
      348              Organism|LargeIntestine|Mucosa|ColonSigmoid|Interstitial|CYP3A4
      349      Organism|LargeIntestine|Mucosa|ColonSigmoid|Interstitial|BindingPartner
      350             Organism|LargeIntestine|Mucosa|ColonSigmoid|Interstitial|OATP1B1
      351                  Organism|LargeIntestine|Mucosa|ColonSigmoid|Intracellular|A
      352                  Organism|LargeIntestine|Mucosa|ColonSigmoid|Intracellular|B
      353             Organism|LargeIntestine|Mucosa|ColonSigmoid|Intracellular|UGT2B7
      354             Organism|LargeIntestine|Mucosa|ColonSigmoid|Intracellular|UGT2B7
      355             Organism|LargeIntestine|Mucosa|ColonSigmoid|Intracellular|UGT2B7
      356             Organism|LargeIntestine|Mucosa|ColonSigmoid|Intracellular|CYP3A4
      357             Organism|LargeIntestine|Mucosa|ColonSigmoid|Intracellular|CYP3A4
      358             Organism|LargeIntestine|Mucosa|ColonSigmoid|Intracellular|CYP3A4
      359     Organism|LargeIntestine|Mucosa|ColonSigmoid|Intracellular|BindingPartner
      360     Organism|LargeIntestine|Mucosa|ColonSigmoid|Intracellular|BindingPartner
      361     Organism|LargeIntestine|Mucosa|ColonSigmoid|Intracellular|BindingPartner
      362            Organism|LargeIntestine|Mucosa|ColonSigmoid|Intracellular|OATP1B1
      363            Organism|LargeIntestine|Mucosa|ColonSigmoid|Intracellular|OATP1B1
      364            Organism|LargeIntestine|Mucosa|ColonSigmoid|Intracellular|OATP1B1
      365                         Organism|LargeIntestine|Mucosa|ColonSigmoid|Plasma|A
      366                         Organism|LargeIntestine|Mucosa|ColonSigmoid|Plasma|B
      367                    Organism|LargeIntestine|Mucosa|ColonSigmoid|Plasma|UGT2B7
      368                    Organism|LargeIntestine|Mucosa|ColonSigmoid|Plasma|CYP3A4
      369            Organism|LargeIntestine|Mucosa|ColonSigmoid|Plasma|BindingPartner
      370                   Organism|LargeIntestine|Mucosa|ColonSigmoid|Plasma|OATP1B1
      371                 Organism|LargeIntestine|Mucosa|ColonTransversum|BloodCells|A
      372                 Organism|LargeIntestine|Mucosa|ColonTransversum|BloodCells|B
      373            Organism|LargeIntestine|Mucosa|ColonTransversum|BloodCells|UGT2B7
      374            Organism|LargeIntestine|Mucosa|ColonTransversum|BloodCells|CYP3A4
      375    Organism|LargeIntestine|Mucosa|ColonTransversum|BloodCells|BindingPartner
      376           Organism|LargeIntestine|Mucosa|ColonTransversum|BloodCells|OATP1B1
      377               Organism|LargeIntestine|Mucosa|ColonTransversum|Interstitial|A
      378               Organism|LargeIntestine|Mucosa|ColonTransversum|Interstitial|B
      379          Organism|LargeIntestine|Mucosa|ColonTransversum|Interstitial|UGT2B7
      380          Organism|LargeIntestine|Mucosa|ColonTransversum|Interstitial|CYP3A4
      381  Organism|LargeIntestine|Mucosa|ColonTransversum|Interstitial|BindingPartner
      382         Organism|LargeIntestine|Mucosa|ColonTransversum|Interstitial|OATP1B1
      383              Organism|LargeIntestine|Mucosa|ColonTransversum|Intracellular|A
      384              Organism|LargeIntestine|Mucosa|ColonTransversum|Intracellular|B
      385         Organism|LargeIntestine|Mucosa|ColonTransversum|Intracellular|UGT2B7
      386         Organism|LargeIntestine|Mucosa|ColonTransversum|Intracellular|UGT2B7
      387         Organism|LargeIntestine|Mucosa|ColonTransversum|Intracellular|UGT2B7
      388         Organism|LargeIntestine|Mucosa|ColonTransversum|Intracellular|CYP3A4
      389         Organism|LargeIntestine|Mucosa|ColonTransversum|Intracellular|CYP3A4
      390         Organism|LargeIntestine|Mucosa|ColonTransversum|Intracellular|CYP3A4
      391 Organism|LargeIntestine|Mucosa|ColonTransversum|Intracellular|BindingPartner
      392 Organism|LargeIntestine|Mucosa|ColonTransversum|Intracellular|BindingPartner
      393 Organism|LargeIntestine|Mucosa|ColonTransversum|Intracellular|BindingPartner
      394        Organism|LargeIntestine|Mucosa|ColonTransversum|Intracellular|OATP1B1
      395        Organism|LargeIntestine|Mucosa|ColonTransversum|Intracellular|OATP1B1
      396        Organism|LargeIntestine|Mucosa|ColonTransversum|Intracellular|OATP1B1
      397                     Organism|LargeIntestine|Mucosa|ColonTransversum|Plasma|A
      398                     Organism|LargeIntestine|Mucosa|ColonTransversum|Plasma|B
      399                Organism|LargeIntestine|Mucosa|ColonTransversum|Plasma|UGT2B7
      400                Organism|LargeIntestine|Mucosa|ColonTransversum|Plasma|CYP3A4
      401        Organism|LargeIntestine|Mucosa|ColonTransversum|Plasma|BindingPartner
      402               Organism|LargeIntestine|Mucosa|ColonTransversum|Plasma|OATP1B1
      403                           Organism|LargeIntestine|Mucosa|Rectum|BloodCells|A
      404                           Organism|LargeIntestine|Mucosa|Rectum|BloodCells|B
      405                      Organism|LargeIntestine|Mucosa|Rectum|BloodCells|UGT2B7
      406                      Organism|LargeIntestine|Mucosa|Rectum|BloodCells|CYP3A4
      407              Organism|LargeIntestine|Mucosa|Rectum|BloodCells|BindingPartner
      408                     Organism|LargeIntestine|Mucosa|Rectum|BloodCells|OATP1B1
      409                         Organism|LargeIntestine|Mucosa|Rectum|Interstitial|A
      410                         Organism|LargeIntestine|Mucosa|Rectum|Interstitial|B
      411                    Organism|LargeIntestine|Mucosa|Rectum|Interstitial|UGT2B7
      412                    Organism|LargeIntestine|Mucosa|Rectum|Interstitial|CYP3A4
      413            Organism|LargeIntestine|Mucosa|Rectum|Interstitial|BindingPartner
      414                   Organism|LargeIntestine|Mucosa|Rectum|Interstitial|OATP1B1
      415                        Organism|LargeIntestine|Mucosa|Rectum|Intracellular|A
      416                        Organism|LargeIntestine|Mucosa|Rectum|Intracellular|B
      417                   Organism|LargeIntestine|Mucosa|Rectum|Intracellular|UGT2B7
      418                   Organism|LargeIntestine|Mucosa|Rectum|Intracellular|UGT2B7
      419                   Organism|LargeIntestine|Mucosa|Rectum|Intracellular|UGT2B7
      420                   Organism|LargeIntestine|Mucosa|Rectum|Intracellular|CYP3A4
      421                   Organism|LargeIntestine|Mucosa|Rectum|Intracellular|CYP3A4
      422                   Organism|LargeIntestine|Mucosa|Rectum|Intracellular|CYP3A4
      423           Organism|LargeIntestine|Mucosa|Rectum|Intracellular|BindingPartner
      424           Organism|LargeIntestine|Mucosa|Rectum|Intracellular|BindingPartner
      425           Organism|LargeIntestine|Mucosa|Rectum|Intracellular|BindingPartner
      426                  Organism|LargeIntestine|Mucosa|Rectum|Intracellular|OATP1B1
      427                  Organism|LargeIntestine|Mucosa|Rectum|Intracellular|OATP1B1
      428                  Organism|LargeIntestine|Mucosa|Rectum|Intracellular|OATP1B1
      429                               Organism|LargeIntestine|Mucosa|Rectum|Plasma|A
      430                               Organism|LargeIntestine|Mucosa|Rectum|Plasma|B
      431                          Organism|LargeIntestine|Mucosa|Rectum|Plasma|UGT2B7
      432                          Organism|LargeIntestine|Mucosa|Rectum|Plasma|CYP3A4
      433                  Organism|LargeIntestine|Mucosa|Rectum|Plasma|BindingPartner
      434                         Organism|LargeIntestine|Mucosa|Rectum|Plasma|OATP1B1
      435                                      Organism|Liver|Pericentral|BloodCells|A
      436                                      Organism|Liver|Pericentral|BloodCells|B
      437                                 Organism|Liver|Pericentral|BloodCells|UGT2B7
      438                                 Organism|Liver|Pericentral|BloodCells|CYP3A4
      439                         Organism|Liver|Pericentral|BloodCells|BindingPartner
      440                                Organism|Liver|Pericentral|BloodCells|OATP1B1
      441                                    Organism|Liver|Pericentral|Interstitial|A
      442                                    Organism|Liver|Pericentral|Interstitial|B
      443                               Organism|Liver|Pericentral|Interstitial|UGT2B7
      444                               Organism|Liver|Pericentral|Interstitial|CYP3A4
      445                       Organism|Liver|Pericentral|Interstitial|BindingPartner
      446                              Organism|Liver|Pericentral|Interstitial|OATP1B1
      447                                   Organism|Liver|Pericentral|Intracellular|A
      448                                   Organism|Liver|Pericentral|Intracellular|B
      449                              Organism|Liver|Pericentral|Intracellular|UGT2B7
      450                              Organism|Liver|Pericentral|Intracellular|UGT2B7
      451                              Organism|Liver|Pericentral|Intracellular|UGT2B7
      452                              Organism|Liver|Pericentral|Intracellular|CYP3A4
      453                              Organism|Liver|Pericentral|Intracellular|CYP3A4
      454                              Organism|Liver|Pericentral|Intracellular|CYP3A4
      455                      Organism|Liver|Pericentral|Intracellular|BindingPartner
      456                      Organism|Liver|Pericentral|Intracellular|BindingPartner
      457                      Organism|Liver|Pericentral|Intracellular|BindingPartner
      458                             Organism|Liver|Pericentral|Intracellular|OATP1B1
      459                             Organism|Liver|Pericentral|Intracellular|OATP1B1
      460                             Organism|Liver|Pericentral|Intracellular|OATP1B1
      461                                          Organism|Liver|Pericentral|Plasma|A
      462                                          Organism|Liver|Pericentral|Plasma|B
      463                                     Organism|Liver|Pericentral|Plasma|UGT2B7
      464                                     Organism|Liver|Pericentral|Plasma|CYP3A4
      465                             Organism|Liver|Pericentral|Plasma|BindingPartner
      466                                    Organism|Liver|Pericentral|Plasma|OATP1B1
      467                                       Organism|Liver|Periportal|BloodCells|A
      468                                       Organism|Liver|Periportal|BloodCells|B
      469                                  Organism|Liver|Periportal|BloodCells|UGT2B7
      470                                  Organism|Liver|Periportal|BloodCells|CYP3A4
      471                          Organism|Liver|Periportal|BloodCells|BindingPartner
      472                                 Organism|Liver|Periportal|BloodCells|OATP1B1
      473                                     Organism|Liver|Periportal|Interstitial|A
      474                                     Organism|Liver|Periportal|Interstitial|B
      475                                Organism|Liver|Periportal|Interstitial|UGT2B7
      476                                Organism|Liver|Periportal|Interstitial|CYP3A4
      477                        Organism|Liver|Periportal|Interstitial|BindingPartner
      478                               Organism|Liver|Periportal|Interstitial|OATP1B1
      479                                    Organism|Liver|Periportal|Intracellular|A
      480                                    Organism|Liver|Periportal|Intracellular|B
      481                               Organism|Liver|Periportal|Intracellular|UGT2B7
      482                               Organism|Liver|Periportal|Intracellular|UGT2B7
      483                               Organism|Liver|Periportal|Intracellular|UGT2B7
      484                               Organism|Liver|Periportal|Intracellular|CYP3A4
      485                               Organism|Liver|Periportal|Intracellular|CYP3A4
      486                               Organism|Liver|Periportal|Intracellular|CYP3A4
      487                       Organism|Liver|Periportal|Intracellular|BindingPartner
      488                       Organism|Liver|Periportal|Intracellular|BindingPartner
      489                       Organism|Liver|Periportal|Intracellular|BindingPartner
      490                              Organism|Liver|Periportal|Intracellular|OATP1B1
      491                              Organism|Liver|Periportal|Intracellular|OATP1B1
      492                              Organism|Liver|Periportal|Intracellular|OATP1B1
      493                                           Organism|Liver|Periportal|Plasma|A
      494                                           Organism|Liver|Periportal|Plasma|B
      495                                      Organism|Liver|Periportal|Plasma|UGT2B7
      496                                      Organism|Liver|Periportal|Plasma|CYP3A4
      497                              Organism|Liver|Periportal|Plasma|BindingPartner
      498                                     Organism|Liver|Periportal|Plasma|OATP1B1
      499                                                      Organism|Lumen|Caecum|A
      500                                                      Organism|Lumen|Caecum|B
      501                                                 Organism|Lumen|Caecum|UGT2B7
      502                                                 Organism|Lumen|Caecum|CYP3A4
      503                                         Organism|Lumen|Caecum|BindingPartner
      504                                                Organism|Lumen|Caecum|OATP1B1
      505                                              Organism|Lumen|ColonAscendens|A
      506                                              Organism|Lumen|ColonAscendens|B
      507                                         Organism|Lumen|ColonAscendens|UGT2B7
      508                                         Organism|Lumen|ColonAscendens|CYP3A4
      509                                 Organism|Lumen|ColonAscendens|BindingPartner
      510                                        Organism|Lumen|ColonAscendens|OATP1B1
      511                                             Organism|Lumen|ColonDescendens|A
      512                                             Organism|Lumen|ColonDescendens|B
      513                                        Organism|Lumen|ColonDescendens|UGT2B7
      514                                        Organism|Lumen|ColonDescendens|CYP3A4
      515                                Organism|Lumen|ColonDescendens|BindingPartner
      516                                       Organism|Lumen|ColonDescendens|OATP1B1
      517                                                Organism|Lumen|ColonSigmoid|A
      518                                                Organism|Lumen|ColonSigmoid|B
      519                                           Organism|Lumen|ColonSigmoid|UGT2B7
      520                                           Organism|Lumen|ColonSigmoid|CYP3A4
      521                                   Organism|Lumen|ColonSigmoid|BindingPartner
      522                                          Organism|Lumen|ColonSigmoid|OATP1B1
      523                                            Organism|Lumen|ColonTransversum|A
      524                                            Organism|Lumen|ColonTransversum|B
      525                                       Organism|Lumen|ColonTransversum|UGT2B7
      526                                       Organism|Lumen|ColonTransversum|CYP3A4
      527                               Organism|Lumen|ColonTransversum|BindingPartner
      528                                      Organism|Lumen|ColonTransversum|OATP1B1
      529                                                    Organism|Lumen|Duodenum|A
      530                                                    Organism|Lumen|Duodenum|B
      531                                               Organism|Lumen|Duodenum|UGT2B7
      532                                               Organism|Lumen|Duodenum|CYP3A4
      533                                       Organism|Lumen|Duodenum|BindingPartner
      534                                              Organism|Lumen|Duodenum|OATP1B1
      535                                                       Organism|Lumen|Feces|A
      536                                                       Organism|Lumen|Feces|B
      537                                                  Organism|Lumen|Feces|UGT2B7
      538                                                  Organism|Lumen|Feces|CYP3A4
      539                                          Organism|Lumen|Feces|BindingPartner
      540                                                 Organism|Lumen|Feces|OATP1B1
      541                                                  Organism|Lumen|LowerIleum|A
      542                                                  Organism|Lumen|LowerIleum|B
      543                                             Organism|Lumen|LowerIleum|UGT2B7
      544                                             Organism|Lumen|LowerIleum|CYP3A4
      545                                     Organism|Lumen|LowerIleum|BindingPartner
      546                                            Organism|Lumen|LowerIleum|OATP1B1
      547                                                Organism|Lumen|LowerJejunum|A
      548                                                Organism|Lumen|LowerJejunum|B
      549                                           Organism|Lumen|LowerJejunum|UGT2B7
      550                                           Organism|Lumen|LowerJejunum|CYP3A4
      551                                   Organism|Lumen|LowerJejunum|BindingPartner
      552                                          Organism|Lumen|LowerJejunum|OATP1B1
      553                                                      Organism|Lumen|Rectum|A
      554                                                      Organism|Lumen|Rectum|B
      555                                                 Organism|Lumen|Rectum|UGT2B7
      556                                                 Organism|Lumen|Rectum|CYP3A4
      557                                         Organism|Lumen|Rectum|BindingPartner
      558                                                Organism|Lumen|Rectum|OATP1B1
      559                                                     Organism|Lumen|Stomach|A
      560                                                     Organism|Lumen|Stomach|B
      561                                                Organism|Lumen|Stomach|UGT2B7
      562                                                Organism|Lumen|Stomach|CYP3A4
      563                                        Organism|Lumen|Stomach|BindingPartner
      564                                               Organism|Lumen|Stomach|OATP1B1
      565                                                  Organism|Lumen|UpperIleum|A
      566                                                  Organism|Lumen|UpperIleum|B
      567                                             Organism|Lumen|UpperIleum|UGT2B7
      568                                             Organism|Lumen|UpperIleum|CYP3A4
      569                                     Organism|Lumen|UpperIleum|BindingPartner
      570                                            Organism|Lumen|UpperIleum|OATP1B1
      571                                                Organism|Lumen|UpperJejunum|A
      572                                                Organism|Lumen|UpperJejunum|B
      573                                           Organism|Lumen|UpperJejunum|UGT2B7
      574                                           Organism|Lumen|UpperJejunum|CYP3A4
      575                                   Organism|Lumen|UpperJejunum|BindingPartner
      576                                          Organism|Lumen|UpperJejunum|OATP1B1
      577                                                   Organism|Lung|BloodCells|A
      578                                                   Organism|Lung|BloodCells|B
      579                                              Organism|Lung|BloodCells|UGT2B7
      580                                              Organism|Lung|BloodCells|CYP3A4
      581                                      Organism|Lung|BloodCells|BindingPartner
      582                                             Organism|Lung|BloodCells|OATP1B1
      583                                                 Organism|Lung|Interstitial|A
      584                                                 Organism|Lung|Interstitial|B
      585                                            Organism|Lung|Interstitial|UGT2B7
      586                                            Organism|Lung|Interstitial|CYP3A4
      587                                    Organism|Lung|Interstitial|BindingPartner
      588                                           Organism|Lung|Interstitial|OATP1B1
      589                                                Organism|Lung|Intracellular|A
      590                                                Organism|Lung|Intracellular|B
      591                                           Organism|Lung|Intracellular|UGT2B7
      592                                           Organism|Lung|Intracellular|UGT2B7
      593                                           Organism|Lung|Intracellular|UGT2B7
      594                                           Organism|Lung|Intracellular|CYP3A4
      595                                           Organism|Lung|Intracellular|CYP3A4
      596                                           Organism|Lung|Intracellular|CYP3A4
      597                                   Organism|Lung|Intracellular|BindingPartner
      598                                   Organism|Lung|Intracellular|BindingPartner
      599                                   Organism|Lung|Intracellular|BindingPartner
      600                                          Organism|Lung|Intracellular|OATP1B1
      601                                          Organism|Lung|Intracellular|OATP1B1
      602                                          Organism|Lung|Intracellular|OATP1B1
      603                                                       Organism|Lung|Plasma|A
      604                                                       Organism|Lung|Plasma|B
      605                                                  Organism|Lung|Plasma|UGT2B7
      606                                                  Organism|Lung|Plasma|CYP3A4
      607                                          Organism|Lung|Plasma|BindingPartner
      608                                                 Organism|Lung|Plasma|OATP1B1
      609                                                 Organism|Muscle|BloodCells|A
      610                                                 Organism|Muscle|BloodCells|B
      611                                            Organism|Muscle|BloodCells|UGT2B7
      612                                            Organism|Muscle|BloodCells|CYP3A4
      613                                    Organism|Muscle|BloodCells|BindingPartner
      614                                           Organism|Muscle|BloodCells|OATP1B1
      615                                               Organism|Muscle|Interstitial|A
      616                                               Organism|Muscle|Interstitial|B
      617                                          Organism|Muscle|Interstitial|UGT2B7
      618                                          Organism|Muscle|Interstitial|CYP3A4
      619                                  Organism|Muscle|Interstitial|BindingPartner
      620                                         Organism|Muscle|Interstitial|OATP1B1
      621                                              Organism|Muscle|Intracellular|A
      622                                              Organism|Muscle|Intracellular|B
      623                                         Organism|Muscle|Intracellular|UGT2B7
      624                                         Organism|Muscle|Intracellular|UGT2B7
      625                                         Organism|Muscle|Intracellular|UGT2B7
      626                                         Organism|Muscle|Intracellular|CYP3A4
      627                                         Organism|Muscle|Intracellular|CYP3A4
      628                                         Organism|Muscle|Intracellular|CYP3A4
      629                                 Organism|Muscle|Intracellular|BindingPartner
      630                                 Organism|Muscle|Intracellular|BindingPartner
      631                                 Organism|Muscle|Intracellular|BindingPartner
      632                                        Organism|Muscle|Intracellular|OATP1B1
      633                                        Organism|Muscle|Intracellular|OATP1B1
      634                                        Organism|Muscle|Intracellular|OATP1B1
      635                                                     Organism|Muscle|Plasma|A
      636                                                     Organism|Muscle|Plasma|B
      637                                                Organism|Muscle|Plasma|UGT2B7
      638                                                Organism|Muscle|Plasma|CYP3A4
      639                                        Organism|Muscle|Plasma|BindingPartner
      640                                               Organism|Muscle|Plasma|OATP1B1
      641                                               Organism|Pancreas|BloodCells|A
      642                                               Organism|Pancreas|BloodCells|B
      643                                          Organism|Pancreas|BloodCells|UGT2B7
      644                                          Organism|Pancreas|BloodCells|CYP3A4
      645                                  Organism|Pancreas|BloodCells|BindingPartner
      646                                         Organism|Pancreas|BloodCells|OATP1B1
      647                                             Organism|Pancreas|Interstitial|A
      648                                             Organism|Pancreas|Interstitial|B
      649                                        Organism|Pancreas|Interstitial|UGT2B7
      650                                        Organism|Pancreas|Interstitial|CYP3A4
      651                                Organism|Pancreas|Interstitial|BindingPartner
      652                                       Organism|Pancreas|Interstitial|OATP1B1
      653                                            Organism|Pancreas|Intracellular|A
      654                                            Organism|Pancreas|Intracellular|B
      655                                       Organism|Pancreas|Intracellular|UGT2B7
      656                                       Organism|Pancreas|Intracellular|UGT2B7
      657                                       Organism|Pancreas|Intracellular|UGT2B7
      658                                       Organism|Pancreas|Intracellular|CYP3A4
      659                                       Organism|Pancreas|Intracellular|CYP3A4
      660                                       Organism|Pancreas|Intracellular|CYP3A4
      661                               Organism|Pancreas|Intracellular|BindingPartner
      662                               Organism|Pancreas|Intracellular|BindingPartner
      663                               Organism|Pancreas|Intracellular|BindingPartner
      664                                      Organism|Pancreas|Intracellular|OATP1B1
      665                                      Organism|Pancreas|Intracellular|OATP1B1
      666                                      Organism|Pancreas|Intracellular|OATP1B1
      667                                                   Organism|Pancreas|Plasma|A
      668                                                   Organism|Pancreas|Plasma|B
      669                                              Organism|Pancreas|Plasma|UGT2B7
      670                                              Organism|Pancreas|Plasma|CYP3A4
      671                                      Organism|Pancreas|Plasma|BindingPartner
      672                                             Organism|Pancreas|Plasma|OATP1B1
      673                                             Organism|PortalVein|BloodCells|A
      674                                             Organism|PortalVein|BloodCells|B
      675                                        Organism|PortalVein|BloodCells|UGT2B7
      676                                        Organism|PortalVein|BloodCells|CYP3A4
      677                                Organism|PortalVein|BloodCells|BindingPartner
      678                                       Organism|PortalVein|BloodCells|OATP1B1
      679                                                 Organism|PortalVein|Plasma|A
      680                                                 Organism|PortalVein|Plasma|B
      681                                            Organism|PortalVein|Plasma|UGT2B7
      682                                            Organism|PortalVein|Plasma|CYP3A4
      683                                    Organism|PortalVein|Plasma|BindingPartner
      684                                           Organism|PortalVein|Plasma|OATP1B1
      685                                                   Organism|Skin|BloodCells|A
      686                                                   Organism|Skin|BloodCells|B
      687                                              Organism|Skin|BloodCells|UGT2B7
      688                                              Organism|Skin|BloodCells|CYP3A4
      689                                      Organism|Skin|BloodCells|BindingPartner
      690                                             Organism|Skin|BloodCells|OATP1B1
      691                                                 Organism|Skin|Interstitial|A
      692                                                 Organism|Skin|Interstitial|B
      693                                            Organism|Skin|Interstitial|UGT2B7
      694                                            Organism|Skin|Interstitial|CYP3A4
      695                                    Organism|Skin|Interstitial|BindingPartner
      696                                           Organism|Skin|Interstitial|OATP1B1
      697                                                Organism|Skin|Intracellular|A
      698                                                Organism|Skin|Intracellular|B
      699                                           Organism|Skin|Intracellular|UGT2B7
      700                                           Organism|Skin|Intracellular|UGT2B7
      701                                           Organism|Skin|Intracellular|UGT2B7
      702                                           Organism|Skin|Intracellular|CYP3A4
      703                                           Organism|Skin|Intracellular|CYP3A4
      704                                           Organism|Skin|Intracellular|CYP3A4
      705                                   Organism|Skin|Intracellular|BindingPartner
      706                                   Organism|Skin|Intracellular|BindingPartner
      707                                   Organism|Skin|Intracellular|BindingPartner
      708                                          Organism|Skin|Intracellular|OATP1B1
      709                                          Organism|Skin|Intracellular|OATP1B1
      710                                          Organism|Skin|Intracellular|OATP1B1
      711                                                       Organism|Skin|Plasma|A
      712                                                       Organism|Skin|Plasma|B
      713                                                  Organism|Skin|Plasma|UGT2B7
      714                                                  Organism|Skin|Plasma|CYP3A4
      715                                          Organism|Skin|Plasma|BindingPartner
      716                                                 Organism|Skin|Plasma|OATP1B1
      717                                         Organism|SmallIntestine|BloodCells|A
      718                                         Organism|SmallIntestine|BloodCells|B
      719                                    Organism|SmallIntestine|BloodCells|UGT2B7
      720                                    Organism|SmallIntestine|BloodCells|CYP3A4
      721                            Organism|SmallIntestine|BloodCells|BindingPartner
      722                                   Organism|SmallIntestine|BloodCells|OATP1B1
      723                                       Organism|SmallIntestine|Interstitial|A
      724                                       Organism|SmallIntestine|Interstitial|B
      725                                  Organism|SmallIntestine|Interstitial|UGT2B7
      726                                  Organism|SmallIntestine|Interstitial|CYP3A4
      727                          Organism|SmallIntestine|Interstitial|BindingPartner
      728                                 Organism|SmallIntestine|Interstitial|OATP1B1
      729                                      Organism|SmallIntestine|Intracellular|A
      730                                      Organism|SmallIntestine|Intracellular|B
      731                                 Organism|SmallIntestine|Intracellular|UGT2B7
      732                                 Organism|SmallIntestine|Intracellular|UGT2B7
      733                                 Organism|SmallIntestine|Intracellular|UGT2B7
      734                                 Organism|SmallIntestine|Intracellular|CYP3A4
      735                                 Organism|SmallIntestine|Intracellular|CYP3A4
      736                                 Organism|SmallIntestine|Intracellular|CYP3A4
      737                         Organism|SmallIntestine|Intracellular|BindingPartner
      738                         Organism|SmallIntestine|Intracellular|BindingPartner
      739                         Organism|SmallIntestine|Intracellular|BindingPartner
      740                                Organism|SmallIntestine|Intracellular|OATP1B1
      741                                Organism|SmallIntestine|Intracellular|OATP1B1
      742                                Organism|SmallIntestine|Intracellular|OATP1B1
      743                                             Organism|SmallIntestine|Plasma|A
      744                                             Organism|SmallIntestine|Plasma|B
      745                                        Organism|SmallIntestine|Plasma|UGT2B7
      746                                        Organism|SmallIntestine|Plasma|CYP3A4
      747                                Organism|SmallIntestine|Plasma|BindingPartner
      748                                       Organism|SmallIntestine|Plasma|OATP1B1
      749                         Organism|SmallIntestine|Mucosa|Duodenum|BloodCells|A
      750                         Organism|SmallIntestine|Mucosa|Duodenum|BloodCells|B
      751                    Organism|SmallIntestine|Mucosa|Duodenum|BloodCells|UGT2B7
      752                    Organism|SmallIntestine|Mucosa|Duodenum|BloodCells|CYP3A4
      753            Organism|SmallIntestine|Mucosa|Duodenum|BloodCells|BindingPartner
      754                   Organism|SmallIntestine|Mucosa|Duodenum|BloodCells|OATP1B1
      755                       Organism|SmallIntestine|Mucosa|Duodenum|Interstitial|A
      756                       Organism|SmallIntestine|Mucosa|Duodenum|Interstitial|B
      757                  Organism|SmallIntestine|Mucosa|Duodenum|Interstitial|UGT2B7
      758                  Organism|SmallIntestine|Mucosa|Duodenum|Interstitial|CYP3A4
      759          Organism|SmallIntestine|Mucosa|Duodenum|Interstitial|BindingPartner
      760                 Organism|SmallIntestine|Mucosa|Duodenum|Interstitial|OATP1B1
      761                      Organism|SmallIntestine|Mucosa|Duodenum|Intracellular|A
      762                      Organism|SmallIntestine|Mucosa|Duodenum|Intracellular|B
      763                 Organism|SmallIntestine|Mucosa|Duodenum|Intracellular|UGT2B7
      764                 Organism|SmallIntestine|Mucosa|Duodenum|Intracellular|UGT2B7
      765                 Organism|SmallIntestine|Mucosa|Duodenum|Intracellular|UGT2B7
      766                 Organism|SmallIntestine|Mucosa|Duodenum|Intracellular|CYP3A4
      767                 Organism|SmallIntestine|Mucosa|Duodenum|Intracellular|CYP3A4
      768                 Organism|SmallIntestine|Mucosa|Duodenum|Intracellular|CYP3A4
      769         Organism|SmallIntestine|Mucosa|Duodenum|Intracellular|BindingPartner
      770         Organism|SmallIntestine|Mucosa|Duodenum|Intracellular|BindingPartner
      771         Organism|SmallIntestine|Mucosa|Duodenum|Intracellular|BindingPartner
      772                Organism|SmallIntestine|Mucosa|Duodenum|Intracellular|OATP1B1
      773                Organism|SmallIntestine|Mucosa|Duodenum|Intracellular|OATP1B1
      774                Organism|SmallIntestine|Mucosa|Duodenum|Intracellular|OATP1B1
      775                             Organism|SmallIntestine|Mucosa|Duodenum|Plasma|A
      776                             Organism|SmallIntestine|Mucosa|Duodenum|Plasma|B
      777                        Organism|SmallIntestine|Mucosa|Duodenum|Plasma|UGT2B7
      778                        Organism|SmallIntestine|Mucosa|Duodenum|Plasma|CYP3A4
      779                Organism|SmallIntestine|Mucosa|Duodenum|Plasma|BindingPartner
      780                       Organism|SmallIntestine|Mucosa|Duodenum|Plasma|OATP1B1
      781                       Organism|SmallIntestine|Mucosa|LowerIleum|BloodCells|A
      782                       Organism|SmallIntestine|Mucosa|LowerIleum|BloodCells|B
      783                  Organism|SmallIntestine|Mucosa|LowerIleum|BloodCells|UGT2B7
      784                  Organism|SmallIntestine|Mucosa|LowerIleum|BloodCells|CYP3A4
      785          Organism|SmallIntestine|Mucosa|LowerIleum|BloodCells|BindingPartner
      786                 Organism|SmallIntestine|Mucosa|LowerIleum|BloodCells|OATP1B1
      787                     Organism|SmallIntestine|Mucosa|LowerIleum|Interstitial|A
      788                     Organism|SmallIntestine|Mucosa|LowerIleum|Interstitial|B
      789                Organism|SmallIntestine|Mucosa|LowerIleum|Interstitial|UGT2B7
      790                Organism|SmallIntestine|Mucosa|LowerIleum|Interstitial|CYP3A4
      791        Organism|SmallIntestine|Mucosa|LowerIleum|Interstitial|BindingPartner
      792               Organism|SmallIntestine|Mucosa|LowerIleum|Interstitial|OATP1B1
      793                    Organism|SmallIntestine|Mucosa|LowerIleum|Intracellular|A
      794                    Organism|SmallIntestine|Mucosa|LowerIleum|Intracellular|B
      795               Organism|SmallIntestine|Mucosa|LowerIleum|Intracellular|UGT2B7
      796               Organism|SmallIntestine|Mucosa|LowerIleum|Intracellular|UGT2B7
      797               Organism|SmallIntestine|Mucosa|LowerIleum|Intracellular|UGT2B7
      798               Organism|SmallIntestine|Mucosa|LowerIleum|Intracellular|CYP3A4
      799               Organism|SmallIntestine|Mucosa|LowerIleum|Intracellular|CYP3A4
      800               Organism|SmallIntestine|Mucosa|LowerIleum|Intracellular|CYP3A4
      801       Organism|SmallIntestine|Mucosa|LowerIleum|Intracellular|BindingPartner
      802       Organism|SmallIntestine|Mucosa|LowerIleum|Intracellular|BindingPartner
      803       Organism|SmallIntestine|Mucosa|LowerIleum|Intracellular|BindingPartner
      804              Organism|SmallIntestine|Mucosa|LowerIleum|Intracellular|OATP1B1
      805              Organism|SmallIntestine|Mucosa|LowerIleum|Intracellular|OATP1B1
      806              Organism|SmallIntestine|Mucosa|LowerIleum|Intracellular|OATP1B1
      807                           Organism|SmallIntestine|Mucosa|LowerIleum|Plasma|A
      808                           Organism|SmallIntestine|Mucosa|LowerIleum|Plasma|B
      809                      Organism|SmallIntestine|Mucosa|LowerIleum|Plasma|UGT2B7
      810                      Organism|SmallIntestine|Mucosa|LowerIleum|Plasma|CYP3A4
      811              Organism|SmallIntestine|Mucosa|LowerIleum|Plasma|BindingPartner
      812                     Organism|SmallIntestine|Mucosa|LowerIleum|Plasma|OATP1B1
      813                     Organism|SmallIntestine|Mucosa|LowerJejunum|BloodCells|A
      814                     Organism|SmallIntestine|Mucosa|LowerJejunum|BloodCells|B
      815                Organism|SmallIntestine|Mucosa|LowerJejunum|BloodCells|UGT2B7
      816                Organism|SmallIntestine|Mucosa|LowerJejunum|BloodCells|CYP3A4
      817        Organism|SmallIntestine|Mucosa|LowerJejunum|BloodCells|BindingPartner
      818               Organism|SmallIntestine|Mucosa|LowerJejunum|BloodCells|OATP1B1
      819                   Organism|SmallIntestine|Mucosa|LowerJejunum|Interstitial|A
      820                   Organism|SmallIntestine|Mucosa|LowerJejunum|Interstitial|B
      821              Organism|SmallIntestine|Mucosa|LowerJejunum|Interstitial|UGT2B7
      822              Organism|SmallIntestine|Mucosa|LowerJejunum|Interstitial|CYP3A4
      823      Organism|SmallIntestine|Mucosa|LowerJejunum|Interstitial|BindingPartner
      824             Organism|SmallIntestine|Mucosa|LowerJejunum|Interstitial|OATP1B1
      825                  Organism|SmallIntestine|Mucosa|LowerJejunum|Intracellular|A
      826                  Organism|SmallIntestine|Mucosa|LowerJejunum|Intracellular|B
      827             Organism|SmallIntestine|Mucosa|LowerJejunum|Intracellular|UGT2B7
      828             Organism|SmallIntestine|Mucosa|LowerJejunum|Intracellular|UGT2B7
      829             Organism|SmallIntestine|Mucosa|LowerJejunum|Intracellular|UGT2B7
      830             Organism|SmallIntestine|Mucosa|LowerJejunum|Intracellular|CYP3A4
      831             Organism|SmallIntestine|Mucosa|LowerJejunum|Intracellular|CYP3A4
      832             Organism|SmallIntestine|Mucosa|LowerJejunum|Intracellular|CYP3A4
      833     Organism|SmallIntestine|Mucosa|LowerJejunum|Intracellular|BindingPartner
      834     Organism|SmallIntestine|Mucosa|LowerJejunum|Intracellular|BindingPartner
      835     Organism|SmallIntestine|Mucosa|LowerJejunum|Intracellular|BindingPartner
      836            Organism|SmallIntestine|Mucosa|LowerJejunum|Intracellular|OATP1B1
      837            Organism|SmallIntestine|Mucosa|LowerJejunum|Intracellular|OATP1B1
      838            Organism|SmallIntestine|Mucosa|LowerJejunum|Intracellular|OATP1B1
      839                         Organism|SmallIntestine|Mucosa|LowerJejunum|Plasma|A
      840                         Organism|SmallIntestine|Mucosa|LowerJejunum|Plasma|B
      841                    Organism|SmallIntestine|Mucosa|LowerJejunum|Plasma|UGT2B7
      842                    Organism|SmallIntestine|Mucosa|LowerJejunum|Plasma|CYP3A4
      843            Organism|SmallIntestine|Mucosa|LowerJejunum|Plasma|BindingPartner
      844                   Organism|SmallIntestine|Mucosa|LowerJejunum|Plasma|OATP1B1
      845                       Organism|SmallIntestine|Mucosa|UpperIleum|BloodCells|A
      846                       Organism|SmallIntestine|Mucosa|UpperIleum|BloodCells|B
      847                  Organism|SmallIntestine|Mucosa|UpperIleum|BloodCells|UGT2B7
      848                  Organism|SmallIntestine|Mucosa|UpperIleum|BloodCells|CYP3A4
      849          Organism|SmallIntestine|Mucosa|UpperIleum|BloodCells|BindingPartner
      850                 Organism|SmallIntestine|Mucosa|UpperIleum|BloodCells|OATP1B1
      851                     Organism|SmallIntestine|Mucosa|UpperIleum|Interstitial|A
      852                     Organism|SmallIntestine|Mucosa|UpperIleum|Interstitial|B
      853                Organism|SmallIntestine|Mucosa|UpperIleum|Interstitial|UGT2B7
      854                Organism|SmallIntestine|Mucosa|UpperIleum|Interstitial|CYP3A4
      855        Organism|SmallIntestine|Mucosa|UpperIleum|Interstitial|BindingPartner
      856               Organism|SmallIntestine|Mucosa|UpperIleum|Interstitial|OATP1B1
      857                    Organism|SmallIntestine|Mucosa|UpperIleum|Intracellular|A
      858                    Organism|SmallIntestine|Mucosa|UpperIleum|Intracellular|B
      859               Organism|SmallIntestine|Mucosa|UpperIleum|Intracellular|UGT2B7
      860               Organism|SmallIntestine|Mucosa|UpperIleum|Intracellular|UGT2B7
      861               Organism|SmallIntestine|Mucosa|UpperIleum|Intracellular|UGT2B7
      862               Organism|SmallIntestine|Mucosa|UpperIleum|Intracellular|CYP3A4
      863               Organism|SmallIntestine|Mucosa|UpperIleum|Intracellular|CYP3A4
      864               Organism|SmallIntestine|Mucosa|UpperIleum|Intracellular|CYP3A4
      865       Organism|SmallIntestine|Mucosa|UpperIleum|Intracellular|BindingPartner
      866       Organism|SmallIntestine|Mucosa|UpperIleum|Intracellular|BindingPartner
      867       Organism|SmallIntestine|Mucosa|UpperIleum|Intracellular|BindingPartner
      868              Organism|SmallIntestine|Mucosa|UpperIleum|Intracellular|OATP1B1
      869              Organism|SmallIntestine|Mucosa|UpperIleum|Intracellular|OATP1B1
      870              Organism|SmallIntestine|Mucosa|UpperIleum|Intracellular|OATP1B1
      871                           Organism|SmallIntestine|Mucosa|UpperIleum|Plasma|A
      872                           Organism|SmallIntestine|Mucosa|UpperIleum|Plasma|B
      873                      Organism|SmallIntestine|Mucosa|UpperIleum|Plasma|UGT2B7
      874                      Organism|SmallIntestine|Mucosa|UpperIleum|Plasma|CYP3A4
      875              Organism|SmallIntestine|Mucosa|UpperIleum|Plasma|BindingPartner
      876                     Organism|SmallIntestine|Mucosa|UpperIleum|Plasma|OATP1B1
      877                     Organism|SmallIntestine|Mucosa|UpperJejunum|BloodCells|A
      878                     Organism|SmallIntestine|Mucosa|UpperJejunum|BloodCells|B
      879                Organism|SmallIntestine|Mucosa|UpperJejunum|BloodCells|UGT2B7
      880                Organism|SmallIntestine|Mucosa|UpperJejunum|BloodCells|CYP3A4
      881        Organism|SmallIntestine|Mucosa|UpperJejunum|BloodCells|BindingPartner
      882               Organism|SmallIntestine|Mucosa|UpperJejunum|BloodCells|OATP1B1
      883                   Organism|SmallIntestine|Mucosa|UpperJejunum|Interstitial|A
      884                   Organism|SmallIntestine|Mucosa|UpperJejunum|Interstitial|B
      885              Organism|SmallIntestine|Mucosa|UpperJejunum|Interstitial|UGT2B7
      886              Organism|SmallIntestine|Mucosa|UpperJejunum|Interstitial|CYP3A4
      887      Organism|SmallIntestine|Mucosa|UpperJejunum|Interstitial|BindingPartner
      888             Organism|SmallIntestine|Mucosa|UpperJejunum|Interstitial|OATP1B1
      889                  Organism|SmallIntestine|Mucosa|UpperJejunum|Intracellular|A
      890                  Organism|SmallIntestine|Mucosa|UpperJejunum|Intracellular|B
      891             Organism|SmallIntestine|Mucosa|UpperJejunum|Intracellular|UGT2B7
      892             Organism|SmallIntestine|Mucosa|UpperJejunum|Intracellular|UGT2B7
      893             Organism|SmallIntestine|Mucosa|UpperJejunum|Intracellular|UGT2B7
      894             Organism|SmallIntestine|Mucosa|UpperJejunum|Intracellular|CYP3A4
      895             Organism|SmallIntestine|Mucosa|UpperJejunum|Intracellular|CYP3A4
      896             Organism|SmallIntestine|Mucosa|UpperJejunum|Intracellular|CYP3A4
      897     Organism|SmallIntestine|Mucosa|UpperJejunum|Intracellular|BindingPartner
      898     Organism|SmallIntestine|Mucosa|UpperJejunum|Intracellular|BindingPartner
      899     Organism|SmallIntestine|Mucosa|UpperJejunum|Intracellular|BindingPartner
      900            Organism|SmallIntestine|Mucosa|UpperJejunum|Intracellular|OATP1B1
      901            Organism|SmallIntestine|Mucosa|UpperJejunum|Intracellular|OATP1B1
      902            Organism|SmallIntestine|Mucosa|UpperJejunum|Intracellular|OATP1B1
      903                         Organism|SmallIntestine|Mucosa|UpperJejunum|Plasma|A
      904                         Organism|SmallIntestine|Mucosa|UpperJejunum|Plasma|B
      905                    Organism|SmallIntestine|Mucosa|UpperJejunum|Plasma|UGT2B7
      906                    Organism|SmallIntestine|Mucosa|UpperJejunum|Plasma|CYP3A4
      907            Organism|SmallIntestine|Mucosa|UpperJejunum|Plasma|BindingPartner
      908                   Organism|SmallIntestine|Mucosa|UpperJejunum|Plasma|OATP1B1
      909                                                 Organism|Spleen|BloodCells|A
      910                                                 Organism|Spleen|BloodCells|B
      911                                            Organism|Spleen|BloodCells|UGT2B7
      912                                            Organism|Spleen|BloodCells|CYP3A4
      913                                    Organism|Spleen|BloodCells|BindingPartner
      914                                           Organism|Spleen|BloodCells|OATP1B1
      915                                               Organism|Spleen|Interstitial|A
      916                                               Organism|Spleen|Interstitial|B
      917                                          Organism|Spleen|Interstitial|UGT2B7
      918                                          Organism|Spleen|Interstitial|CYP3A4
      919                                  Organism|Spleen|Interstitial|BindingPartner
      920                                         Organism|Spleen|Interstitial|OATP1B1
      921                                              Organism|Spleen|Intracellular|A
      922                                              Organism|Spleen|Intracellular|B
      923                                         Organism|Spleen|Intracellular|UGT2B7
      924                                         Organism|Spleen|Intracellular|UGT2B7
      925                                         Organism|Spleen|Intracellular|UGT2B7
      926                                         Organism|Spleen|Intracellular|CYP3A4
      927                                         Organism|Spleen|Intracellular|CYP3A4
      928                                         Organism|Spleen|Intracellular|CYP3A4
      929                                 Organism|Spleen|Intracellular|BindingPartner
      930                                 Organism|Spleen|Intracellular|BindingPartner
      931                                 Organism|Spleen|Intracellular|BindingPartner
      932                                        Organism|Spleen|Intracellular|OATP1B1
      933                                        Organism|Spleen|Intracellular|OATP1B1
      934                                        Organism|Spleen|Intracellular|OATP1B1
      935                                                     Organism|Spleen|Plasma|A
      936                                                     Organism|Spleen|Plasma|B
      937                                                Organism|Spleen|Plasma|UGT2B7
      938                                                Organism|Spleen|Plasma|CYP3A4
      939                                        Organism|Spleen|Plasma|BindingPartner
      940                                               Organism|Spleen|Plasma|OATP1B1
      941                                                Organism|Stomach|BloodCells|A
      942                                                Organism|Stomach|BloodCells|B
      943                                           Organism|Stomach|BloodCells|UGT2B7
      944                                           Organism|Stomach|BloodCells|CYP3A4
      945                                   Organism|Stomach|BloodCells|BindingPartner
      946                                          Organism|Stomach|BloodCells|OATP1B1
      947                                              Organism|Stomach|Interstitial|A
      948                                              Organism|Stomach|Interstitial|B
      949                                         Organism|Stomach|Interstitial|UGT2B7
      950                                         Organism|Stomach|Interstitial|CYP3A4
      951                                 Organism|Stomach|Interstitial|BindingPartner
      952                                        Organism|Stomach|Interstitial|OATP1B1
      953                                             Organism|Stomach|Intracellular|A
      954                                             Organism|Stomach|Intracellular|B
      955                                        Organism|Stomach|Intracellular|UGT2B7
      956                                        Organism|Stomach|Intracellular|UGT2B7
      957                                        Organism|Stomach|Intracellular|UGT2B7
      958                                        Organism|Stomach|Intracellular|CYP3A4
      959                                        Organism|Stomach|Intracellular|CYP3A4
      960                                        Organism|Stomach|Intracellular|CYP3A4
      961                                Organism|Stomach|Intracellular|BindingPartner
      962                                Organism|Stomach|Intracellular|BindingPartner
      963                                Organism|Stomach|Intracellular|BindingPartner
      964                                       Organism|Stomach|Intracellular|OATP1B1
      965                                       Organism|Stomach|Intracellular|OATP1B1
      966                                       Organism|Stomach|Intracellular|OATP1B1
      967                                                    Organism|Stomach|Plasma|A
      968                                                    Organism|Stomach|Plasma|B
      969                                               Organism|Stomach|Plasma|UGT2B7
      970                                               Organism|Stomach|Plasma|CYP3A4
      971                                       Organism|Stomach|Plasma|BindingPartner
      972                                              Organism|Stomach|Plasma|OATP1B1
      973                                            Organism|VenousBlood|BloodCells|A
      974                                            Organism|VenousBlood|BloodCells|B
      975                                       Organism|VenousBlood|BloodCells|UGT2B7
      976                                       Organism|VenousBlood|BloodCells|CYP3A4
      977                               Organism|VenousBlood|BloodCells|BindingPartner
      978                                      Organism|VenousBlood|BloodCells|OATP1B1
      979                                                Organism|VenousBlood|Plasma|A
      980                                                Organism|VenousBlood|Plasma|B
      981                                           Organism|VenousBlood|Plasma|UGT2B7
      982                                           Organism|VenousBlood|Plasma|CYP3A4
      983                                   Organism|VenousBlood|Plasma|BindingPartner
      984                                          Organism|VenousBlood|Plasma|OATP1B1
                                     Parameter Name Value   Unit Value Origin
      1                      LocalMoleculeParameter     0   µmol             
      2                      LocalMoleculeParameter     0   µmol             
      3                       Initial concentration     0 µmol/l             
      4                       Initial concentration     0 µmol/l             
      5                       Initial concentration     0 µmol/l             
      6                       Initial concentration     0 µmol/l             
      7                      LocalMoleculeParameter     0   µmol             
      8                      LocalMoleculeParameter     0   µmol             
      9                       Initial concentration     0 µmol/l             
      10                      Initial concentration     0 µmol/l             
      11                      Initial concentration     0 µmol/l             
      12                      Initial concentration     0 µmol/l             
      13                     LocalMoleculeParameter     0   µmol             
      14                     LocalMoleculeParameter     0   µmol             
      15                      Initial concentration     0 µmol/l             
      16                      Initial concentration     0 µmol/l             
      17                      Initial concentration     0 µmol/l             
      18                      Initial concentration     0 µmol/l             
      19                     LocalMoleculeParameter     0   µmol             
      20                     LocalMoleculeParameter     0   µmol             
      21                      Initial concentration     0 µmol/l             
      22                      Initial concentration     0 µmol/l             
      23                      Initial concentration     0 µmol/l             
      24                      Initial concentration     0 µmol/l             
      25                     LocalMoleculeParameter     0   µmol             
      26                     LocalMoleculeParameter     0   µmol             
      27                        Relative expression     0                    
      28                      Initial concentration     0 µmol/l             
      29           Fraction expressed intracellular     1                    
      30                        Relative expression     0                    
      31                      Initial concentration     0 µmol/l             
      32           Fraction expressed intracellular     1                    
      33                        Relative expression     0                    
      34                      Initial concentration     0 µmol/l             
      35           Fraction expressed intracellular     1                    
      36                        Relative expression     0                    
      37                      Initial concentration     0 µmol/l             
      38                  Fraction expressed apical     0                    
      39                     LocalMoleculeParameter     0   µmol             
      40                     LocalMoleculeParameter     0   µmol             
      41                      Initial concentration     0 µmol/l             
      42                      Initial concentration     0 µmol/l             
      43                      Initial concentration     0 µmol/l             
      44                      Initial concentration     0 µmol/l             
      45                     LocalMoleculeParameter     0   µmol             
      46                     LocalMoleculeParameter     0   µmol             
      47                      Initial concentration     0 µmol/l             
      48                      Initial concentration     0 µmol/l             
      49                      Initial concentration     0 µmol/l             
      50                      Initial concentration     0 µmol/l             
      51                     LocalMoleculeParameter     0   µmol             
      52                     LocalMoleculeParameter     0   µmol             
      53                      Initial concentration     0 µmol/l             
      54                      Initial concentration     0 µmol/l             
      55                      Initial concentration     0 µmol/l             
      56                      Initial concentration     0 µmol/l             
      57                     LocalMoleculeParameter     0   µmol             
      58                     LocalMoleculeParameter     0   µmol             
      59                      Initial concentration     0 µmol/l             
      60                      Initial concentration     0 µmol/l             
      61                      Initial concentration     0 µmol/l             
      62                      Initial concentration     0 µmol/l             
      63                     LocalMoleculeParameter     0   µmol             
      64                     LocalMoleculeParameter     0   µmol             
      65                        Relative expression     0                    
      66                      Initial concentration     0 µmol/l             
      67           Fraction expressed intracellular     1                    
      68                        Relative expression     0                    
      69                      Initial concentration     0 µmol/l             
      70           Fraction expressed intracellular     1                    
      71                        Relative expression     0                    
      72                      Initial concentration     0 µmol/l             
      73           Fraction expressed intracellular     1                    
      74                        Relative expression     0                    
      75                      Initial concentration     0 µmol/l             
      76                     LocalMoleculeParameter     0   µmol             
      77                     LocalMoleculeParameter     0   µmol             
      78                      Initial concentration     0 µmol/l             
      79                      Initial concentration     0 µmol/l             
      80                      Initial concentration     0 µmol/l             
      81                      Initial concentration     0 µmol/l             
      82  Fraction expressed at blood brain barrier     1                    
      83                     LocalMoleculeParameter     0   µmol             
      84                     LocalMoleculeParameter     0   µmol             
      85                      Initial concentration     0 µmol/l             
      86                      Initial concentration     0 µmol/l             
      87                      Initial concentration     0 µmol/l             
      88                      Initial concentration     0 µmol/l             
      89                     LocalMoleculeParameter     0   µmol             
      90                     LocalMoleculeParameter     0   µmol             
      91                      Initial concentration     0 µmol/l             
      92                      Initial concentration     0 µmol/l             
      93                      Initial concentration     0 µmol/l             
      94                      Initial concentration     0 µmol/l             
      95                     LocalMoleculeParameter     0   µmol             
      96                     LocalMoleculeParameter     0   µmol             
      97                        Relative expression     0                    
      98                      Initial concentration     0 µmol/l             
      99           Fraction expressed intracellular     1                    
      100                       Relative expression     0                    
      101                     Initial concentration     0 µmol/l             
      102          Fraction expressed intracellular     1                    
      103                       Relative expression     0                    
      104                     Initial concentration     0 µmol/l             
      105          Fraction expressed intracellular     1                    
      106                       Relative expression     0                    
      107                     Initial concentration     0 µmol/l             
      108                 Fraction expressed apical     0                    
      109                    LocalMoleculeParameter     0   µmol             
      110                    LocalMoleculeParameter     0   µmol             
      111                     Initial concentration     0 µmol/l             
      112                     Initial concentration     0 µmol/l             
      113                     Initial concentration     0 µmol/l             
      114                     Initial concentration     0 µmol/l             
      115                    LocalMoleculeParameter     0   µmol             
      116                    LocalMoleculeParameter     0   µmol             
      117                     Initial concentration     0 µmol/l             
      118                     Initial concentration     0 µmol/l             
      119                     Initial concentration     0 µmol/l             
      120                     Initial concentration     0 µmol/l             
      121                    LocalMoleculeParameter     0   µmol             
      122                    LocalMoleculeParameter     0   µmol             
      123                     Initial concentration     0 µmol/l             
      124                     Initial concentration     0 µmol/l             
      125                     Initial concentration     0 µmol/l             
      126                     Initial concentration     0 µmol/l             
      127                    LocalMoleculeParameter     0   µmol             
      128                    LocalMoleculeParameter     0   µmol             
      129                       Relative expression     0                    
      130                     Initial concentration     0 µmol/l             
      131          Fraction expressed intracellular     1                    
      132                       Relative expression     0                    
      133                     Initial concentration     0 µmol/l             
      134          Fraction expressed intracellular     1                    
      135                       Relative expression     0                    
      136                     Initial concentration     0 µmol/l             
      137          Fraction expressed intracellular     1                    
      138                       Relative expression     0                    
      139                     Initial concentration     0 µmol/l             
      140                 Fraction expressed apical     0                    
      141                    LocalMoleculeParameter     0   µmol             
      142                    LocalMoleculeParameter     0   µmol             
      143                     Initial concentration     0 µmol/l             
      144                     Initial concentration     0 µmol/l             
      145                     Initial concentration     0 µmol/l             
      146                     Initial concentration     0 µmol/l             
      147                    LocalMoleculeParameter     0   µmol             
      148                    LocalMoleculeParameter     0   µmol             
      149                     Initial concentration     0 µmol/l             
      150                     Initial concentration     0 µmol/l             
      151                     Initial concentration     0 µmol/l             
      152                     Initial concentration     0 µmol/l             
      153                    LocalMoleculeParameter     0   µmol             
      154                    LocalMoleculeParameter     0   µmol             
      155                     Initial concentration     0 µmol/l             
      156                     Initial concentration     0 µmol/l             
      157                     Initial concentration     0 µmol/l             
      158                     Initial concentration     0 µmol/l             
      159                    LocalMoleculeParameter     0   µmol             
      160                    LocalMoleculeParameter     0   µmol             
      161                       Relative expression     0                    
      162                     Initial concentration     0 µmol/l             
      163          Fraction expressed intracellular     1                    
      164                       Relative expression     0                    
      165                     Initial concentration     0 µmol/l             
      166          Fraction expressed intracellular     1                    
      167                       Relative expression     0                    
      168                     Initial concentration     0 µmol/l             
      169          Fraction expressed intracellular     1                    
      170                       Relative expression     0                    
      171                     Initial concentration     0 µmol/l             
      172                 Fraction expressed apical     0                    
      173                    LocalMoleculeParameter     0   µmol             
      174                    LocalMoleculeParameter     0   µmol             
      175                     Initial concentration     0 µmol/l             
      176                     Initial concentration     0 µmol/l             
      177                     Initial concentration     0 µmol/l             
      178                     Initial concentration     0 µmol/l             
      179                    LocalMoleculeParameter     0   µmol             
      180                    LocalMoleculeParameter     0   µmol             
      181                     Initial concentration     0 µmol/l             
      182                     Initial concentration     0 µmol/l             
      183                     Initial concentration     0 µmol/l             
      184                     Initial concentration     0 µmol/l             
      185                    LocalMoleculeParameter     0   µmol             
      186                    LocalMoleculeParameter     0   µmol             
      187                     Initial concentration     0 µmol/l             
      188                     Initial concentration     0 µmol/l             
      189                     Initial concentration     0 µmol/l             
      190                     Initial concentration     0 µmol/l             
      191                    LocalMoleculeParameter     0   µmol             
      192                    LocalMoleculeParameter     0   µmol             
      193                       Relative expression     0                    
      194                     Initial concentration     0 µmol/l             
      195          Fraction expressed intracellular     1                    
      196                       Relative expression     0                    
      197                     Initial concentration     0 µmol/l             
      198          Fraction expressed intracellular     1                    
      199                       Relative expression     0                    
      200                     Initial concentration     0 µmol/l             
      201          Fraction expressed intracellular     1                    
      202                       Relative expression     0                    
      203                     Initial concentration     0 µmol/l             
      204                 Fraction expressed apical     0                    
      205                    LocalMoleculeParameter     0   µmol             
      206                    LocalMoleculeParameter     0   µmol             
      207                     Initial concentration     0 µmol/l             
      208                     Initial concentration     0 µmol/l             
      209                     Initial concentration     0 µmol/l             
      210                     Initial concentration     0 µmol/l             
      211                    LocalMoleculeParameter     0   µmol             
      212                    LocalMoleculeParameter     0   µmol             
      213                     Initial concentration     0 µmol/l             
      214                     Initial concentration     0 µmol/l             
      215                     Initial concentration     0 µmol/l             
      216                     Initial concentration     0 µmol/l             
      217                    LocalMoleculeParameter     0   µmol             
      218                    LocalMoleculeParameter     0   µmol             
      219                     Initial concentration     0 µmol/l             
      220                     Initial concentration     0 µmol/l             
      221                     Initial concentration     0 µmol/l             
      222                     Initial concentration     0 µmol/l             
      223                    LocalMoleculeParameter     0   µmol             
      224                    LocalMoleculeParameter     0   µmol             
      225                       Relative expression     0                    
      226                     Initial concentration     0 µmol/l             
      227          Fraction expressed intracellular     1                    
      228                       Relative expression     0                    
      229                     Initial concentration     0 µmol/l             
      230          Fraction expressed intracellular     1                    
      231                       Relative expression     0                    
      232                     Initial concentration     0 µmol/l             
      233          Fraction expressed intracellular     1                    
      234                       Relative expression     0                    
      235                     Initial concentration     0 µmol/l             
      236                 Fraction expressed apical     0                    
      237                    LocalMoleculeParameter     0   µmol             
      238                    LocalMoleculeParameter     0   µmol             
      239                     Initial concentration     0 µmol/l             
      240                     Initial concentration     0 µmol/l             
      241                     Initial concentration     0 µmol/l             
      242                     Initial concentration     0 µmol/l             
      243                    LocalMoleculeParameter     0   µmol             
      244                    LocalMoleculeParameter     0   µmol             
      245                     Initial concentration     0 µmol/l             
      246                     Initial concentration     0 µmol/l             
      247                     Initial concentration     0 µmol/l             
      248                     Initial concentration     0 µmol/l             
      249                    LocalMoleculeParameter     0   µmol             
      250                    LocalMoleculeParameter     0   µmol             
      251                     Initial concentration     0 µmol/l             
      252                     Initial concentration     0 µmol/l             
      253                     Initial concentration     0 µmol/l             
      254                     Initial concentration     0 µmol/l             
      255                    LocalMoleculeParameter     0   µmol             
      256                    LocalMoleculeParameter     0   µmol             
      257                       Relative expression     0                    
      258                     Initial concentration     0 µmol/l             
      259          Fraction expressed intracellular     1                    
      260                       Relative expression     0                    
      261                     Initial concentration     0 µmol/l             
      262          Fraction expressed intracellular     1                    
      263                       Relative expression     0                    
      264                     Initial concentration     0 µmol/l             
      265          Fraction expressed intracellular     1                    
      266                       Relative expression     0                    
      267                     Initial concentration     0 µmol/l             
      268                 Fraction expressed apical     0                    
      269                    LocalMoleculeParameter     0   µmol             
      270                    LocalMoleculeParameter     0   µmol             
      271                     Initial concentration     0 µmol/l             
      272                     Initial concentration     0 µmol/l             
      273                     Initial concentration     0 µmol/l             
      274                     Initial concentration     0 µmol/l             
      275                    LocalMoleculeParameter     0   µmol             
      276                    LocalMoleculeParameter     0   µmol             
      277                     Initial concentration     0 µmol/l             
      278                     Initial concentration     0 µmol/l             
      279                     Initial concentration     0 µmol/l             
      280                     Initial concentration     0 µmol/l             
      281                    LocalMoleculeParameter     0   µmol             
      282                    LocalMoleculeParameter     0   µmol             
      283                     Initial concentration     0 µmol/l             
      284                     Initial concentration     0 µmol/l             
      285                     Initial concentration     0 µmol/l             
      286                     Initial concentration     0 µmol/l             
      287                    LocalMoleculeParameter     0   µmol             
      288                    LocalMoleculeParameter     0   µmol             
      289                       Relative expression     0                    
      290                     Initial concentration     0 µmol/l             
      291          Fraction expressed intracellular     1                    
      292                       Relative expression     0                    
      293                     Initial concentration     0 µmol/l             
      294          Fraction expressed intracellular     1                    
      295                       Relative expression     0                    
      296                     Initial concentration     0 µmol/l             
      297          Fraction expressed intracellular     1                    
      298                       Relative expression     0                    
      299                     Initial concentration     0 µmol/l             
      300                 Fraction expressed apical     0                    
      301                    LocalMoleculeParameter     0   µmol             
      302                    LocalMoleculeParameter     0   µmol             
      303                     Initial concentration     0 µmol/l             
      304                     Initial concentration     0 µmol/l             
      305                     Initial concentration     0 µmol/l             
      306                     Initial concentration     0 µmol/l             
      307                    LocalMoleculeParameter     0   µmol             
      308                    LocalMoleculeParameter     0   µmol             
      309                     Initial concentration     0 µmol/l             
      310                     Initial concentration     0 µmol/l             
      311                     Initial concentration     0 µmol/l             
      312                     Initial concentration     0 µmol/l             
      313                    LocalMoleculeParameter     0   µmol             
      314                    LocalMoleculeParameter     0   µmol             
      315                     Initial concentration     0 µmol/l             
      316                     Initial concentration     0 µmol/l             
      317                     Initial concentration     0 µmol/l             
      318                     Initial concentration     0 µmol/l             
      319                    LocalMoleculeParameter     0   µmol             
      320                    LocalMoleculeParameter     0   µmol             
      321                       Relative expression     0                    
      322                     Initial concentration     0 µmol/l             
      323          Fraction expressed intracellular     1                    
      324                       Relative expression     0                    
      325                     Initial concentration     0 µmol/l             
      326          Fraction expressed intracellular     1                    
      327                       Relative expression     0                    
      328                     Initial concentration     0 µmol/l             
      329          Fraction expressed intracellular     1                    
      330                       Relative expression     0                    
      331                     Initial concentration     0 µmol/l             
      332                 Fraction expressed apical     0                    
      333                    LocalMoleculeParameter     0   µmol             
      334                    LocalMoleculeParameter     0   µmol             
      335                     Initial concentration     0 µmol/l             
      336                     Initial concentration     0 µmol/l             
      337                     Initial concentration     0 µmol/l             
      338                     Initial concentration     0 µmol/l             
      339                    LocalMoleculeParameter     0   µmol             
      340                    LocalMoleculeParameter     0   µmol             
      341                     Initial concentration     0 µmol/l             
      342                     Initial concentration     0 µmol/l             
      343                     Initial concentration     0 µmol/l             
      344                     Initial concentration     0 µmol/l             
      345                    LocalMoleculeParameter     0   µmol             
      346                    LocalMoleculeParameter     0   µmol             
      347                     Initial concentration     0 µmol/l             
      348                     Initial concentration     0 µmol/l             
      349                     Initial concentration     0 µmol/l             
      350                     Initial concentration     0 µmol/l             
      351                    LocalMoleculeParameter     0   µmol             
      352                    LocalMoleculeParameter     0   µmol             
      353                       Relative expression     0                    
      354                     Initial concentration     0 µmol/l             
      355          Fraction expressed intracellular     1                    
      356                       Relative expression     0                    
      357                     Initial concentration     0 µmol/l             
      358          Fraction expressed intracellular     1                    
      359                       Relative expression     0                    
      360                     Initial concentration     0 µmol/l             
      361          Fraction expressed intracellular     1                    
      362                       Relative expression     0                    
      363                     Initial concentration     0 µmol/l             
      364                 Fraction expressed apical     0                    
      365                    LocalMoleculeParameter     0   µmol             
      366                    LocalMoleculeParameter     0   µmol             
      367                     Initial concentration     0 µmol/l             
      368                     Initial concentration     0 µmol/l             
      369                     Initial concentration     0 µmol/l             
      370                     Initial concentration     0 µmol/l             
      371                    LocalMoleculeParameter     0   µmol             
      372                    LocalMoleculeParameter     0   µmol             
      373                     Initial concentration     0 µmol/l             
      374                     Initial concentration     0 µmol/l             
      375                     Initial concentration     0 µmol/l             
      376                     Initial concentration     0 µmol/l             
      377                    LocalMoleculeParameter     0   µmol             
      378                    LocalMoleculeParameter     0   µmol             
      379                     Initial concentration     0 µmol/l             
      380                     Initial concentration     0 µmol/l             
      381                     Initial concentration     0 µmol/l             
      382                     Initial concentration     0 µmol/l             
      383                    LocalMoleculeParameter     0   µmol             
      384                    LocalMoleculeParameter     0   µmol             
      385                       Relative expression     0                    
      386                     Initial concentration     0 µmol/l             
      387          Fraction expressed intracellular     1                    
      388                       Relative expression     0                    
      389                     Initial concentration     0 µmol/l             
      390          Fraction expressed intracellular     1                    
      391                       Relative expression     0                    
      392                     Initial concentration     0 µmol/l             
      393          Fraction expressed intracellular     1                    
      394                       Relative expression     0                    
      395                     Initial concentration     0 µmol/l             
      396                 Fraction expressed apical     0                    
      397                    LocalMoleculeParameter     0   µmol             
      398                    LocalMoleculeParameter     0   µmol             
      399                     Initial concentration     0 µmol/l             
      400                     Initial concentration     0 µmol/l             
      401                     Initial concentration     0 µmol/l             
      402                     Initial concentration     0 µmol/l             
      403                    LocalMoleculeParameter     0   µmol             
      404                    LocalMoleculeParameter     0   µmol             
      405                     Initial concentration     0 µmol/l             
      406                     Initial concentration     0 µmol/l             
      407                     Initial concentration     0 µmol/l             
      408                     Initial concentration     0 µmol/l             
      409                    LocalMoleculeParameter     0   µmol             
      410                    LocalMoleculeParameter     0   µmol             
      411                     Initial concentration     0 µmol/l             
      412                     Initial concentration     0 µmol/l             
      413                     Initial concentration     0 µmol/l             
      414                     Initial concentration     0 µmol/l             
      415                    LocalMoleculeParameter     0   µmol             
      416                    LocalMoleculeParameter     0   µmol             
      417                       Relative expression     0                    
      418                     Initial concentration     0 µmol/l             
      419          Fraction expressed intracellular     1                    
      420                       Relative expression     0                    
      421                     Initial concentration     0 µmol/l             
      422          Fraction expressed intracellular     1                    
      423                       Relative expression     0                    
      424                     Initial concentration     0 µmol/l             
      425          Fraction expressed intracellular     1                    
      426                       Relative expression     0                    
      427                     Initial concentration     0 µmol/l             
      428                 Fraction expressed apical     0                    
      429                    LocalMoleculeParameter     0   µmol             
      430                    LocalMoleculeParameter     0   µmol             
      431                     Initial concentration     0 µmol/l             
      432                     Initial concentration     0 µmol/l             
      433                     Initial concentration     0 µmol/l             
      434                     Initial concentration     0 µmol/l             
      435                    LocalMoleculeParameter     0   µmol             
      436                    LocalMoleculeParameter     0   µmol             
      437                     Initial concentration     0 µmol/l             
      438                     Initial concentration     0 µmol/l             
      439                     Initial concentration     0 µmol/l             
      440                     Initial concentration     0 µmol/l             
      441                    LocalMoleculeParameter     0   µmol             
      442                    LocalMoleculeParameter     0   µmol             
      443                     Initial concentration     0 µmol/l             
      444                     Initial concentration     0 µmol/l             
      445                     Initial concentration     0 µmol/l             
      446                     Initial concentration     0 µmol/l             
      447                    LocalMoleculeParameter     0   µmol             
      448                    LocalMoleculeParameter     0   µmol             
      449                       Relative expression     0                    
      450                     Initial concentration     0 µmol/l             
      451          Fraction expressed intracellular     1                    
      452                       Relative expression     0                    
      453                     Initial concentration     0 µmol/l             
      454          Fraction expressed intracellular     1                    
      455                       Relative expression     0                    
      456                     Initial concentration     0 µmol/l             
      457          Fraction expressed intracellular     1                    
      458                       Relative expression     0                    
      459                     Initial concentration     0 µmol/l             
      460                 Fraction expressed apical     0                    
      461                    LocalMoleculeParameter     0   µmol             
      462                    LocalMoleculeParameter     0   µmol             
      463                     Initial concentration     0 µmol/l             
      464                     Initial concentration     0 µmol/l             
      465                     Initial concentration     0 µmol/l             
      466                     Initial concentration     0 µmol/l             
      467                    LocalMoleculeParameter     0   µmol             
      468                    LocalMoleculeParameter     0   µmol             
      469                     Initial concentration     0 µmol/l             
      470                     Initial concentration     0 µmol/l             
      471                     Initial concentration     0 µmol/l             
      472                     Initial concentration     0 µmol/l             
      473                    LocalMoleculeParameter     0   µmol             
      474                    LocalMoleculeParameter     0   µmol             
      475                     Initial concentration     0 µmol/l             
      476                     Initial concentration     0 µmol/l             
      477                     Initial concentration     0 µmol/l             
      478                     Initial concentration     0 µmol/l             
      479                    LocalMoleculeParameter     0   µmol             
      480                    LocalMoleculeParameter     0   µmol             
      481                       Relative expression     0                    
      482                     Initial concentration     0 µmol/l             
      483          Fraction expressed intracellular     1                    
      484                       Relative expression     0                    
      485                     Initial concentration     0 µmol/l             
      486          Fraction expressed intracellular     1                    
      487                       Relative expression     0                    
      488                     Initial concentration     0 µmol/l             
      489          Fraction expressed intracellular     1                    
      490                       Relative expression     0                    
      491                     Initial concentration     0 µmol/l             
      492                 Fraction expressed apical     0                    
      493                    LocalMoleculeParameter     0   µmol             
      494                    LocalMoleculeParameter     0   µmol             
      495                     Initial concentration     0 µmol/l             
      496                     Initial concentration     0 µmol/l             
      497                     Initial concentration     0 µmol/l             
      498                     Initial concentration     0 µmol/l             
      499                    LocalMoleculeParameter     0   µmol             
      500                    LocalMoleculeParameter     0   µmol             
      501                     Initial concentration     0 µmol/l             
      502                     Initial concentration     0 µmol/l             
      503                     Initial concentration     0 µmol/l             
      504                     Initial concentration     0 µmol/l             
      505                    LocalMoleculeParameter     0   µmol             
      506                    LocalMoleculeParameter     0   µmol             
      507                     Initial concentration     0 µmol/l             
      508                     Initial concentration     0 µmol/l             
      509                     Initial concentration     0 µmol/l             
      510                     Initial concentration     0 µmol/l             
      511                    LocalMoleculeParameter     0   µmol             
      512                    LocalMoleculeParameter     0   µmol             
      513                     Initial concentration     0 µmol/l             
      514                     Initial concentration     0 µmol/l             
      515                     Initial concentration     0 µmol/l             
      516                     Initial concentration     0 µmol/l             
      517                    LocalMoleculeParameter     0   µmol             
      518                    LocalMoleculeParameter     0   µmol             
      519                     Initial concentration     0 µmol/l             
      520                     Initial concentration     0 µmol/l             
      521                     Initial concentration     0 µmol/l             
      522                     Initial concentration     0 µmol/l             
      523                    LocalMoleculeParameter     0   µmol             
      524                    LocalMoleculeParameter     0   µmol             
      525                     Initial concentration     0 µmol/l             
      526                     Initial concentration     0 µmol/l             
      527                     Initial concentration     0 µmol/l             
      528                     Initial concentration     0 µmol/l             
      529                    LocalMoleculeParameter     0   µmol             
      530                    LocalMoleculeParameter     0   µmol             
      531                     Initial concentration     0 µmol/l             
      532                     Initial concentration     0 µmol/l             
      533                     Initial concentration     0 µmol/l             
      534                     Initial concentration     0 µmol/l             
      535                    LocalMoleculeParameter     0   µmol             
      536                    LocalMoleculeParameter     0   µmol             
      537                     Initial concentration     0 µmol/l             
      538                     Initial concentration     0 µmol/l             
      539                     Initial concentration     0 µmol/l             
      540                     Initial concentration     0 µmol/l             
      541                    LocalMoleculeParameter     0   µmol             
      542                    LocalMoleculeParameter     0   µmol             
      543                     Initial concentration     0 µmol/l             
      544                     Initial concentration     0 µmol/l             
      545                     Initial concentration     0 µmol/l             
      546                     Initial concentration     0 µmol/l             
      547                    LocalMoleculeParameter     0   µmol             
      548                    LocalMoleculeParameter     0   µmol             
      549                     Initial concentration     0 µmol/l             
      550                     Initial concentration     0 µmol/l             
      551                     Initial concentration     0 µmol/l             
      552                     Initial concentration     0 µmol/l             
      553                    LocalMoleculeParameter     0   µmol             
      554                    LocalMoleculeParameter     0   µmol             
      555                     Initial concentration     0 µmol/l             
      556                     Initial concentration     0 µmol/l             
      557                     Initial concentration     0 µmol/l             
      558                     Initial concentration     0 µmol/l             
      559                    LocalMoleculeParameter     0   µmol             
      560                    LocalMoleculeParameter     0   µmol             
      561                     Initial concentration     0 µmol/l             
      562                     Initial concentration     0 µmol/l             
      563                     Initial concentration     0 µmol/l             
      564                     Initial concentration     0 µmol/l             
      565                    LocalMoleculeParameter     0   µmol             
      566                    LocalMoleculeParameter     0   µmol             
      567                     Initial concentration     0 µmol/l             
      568                     Initial concentration     0 µmol/l             
      569                     Initial concentration     0 µmol/l             
      570                     Initial concentration     0 µmol/l             
      571                    LocalMoleculeParameter     0   µmol             
      572                    LocalMoleculeParameter     0   µmol             
      573                     Initial concentration     0 µmol/l             
      574                     Initial concentration     0 µmol/l             
      575                     Initial concentration     0 µmol/l             
      576                     Initial concentration     0 µmol/l             
      577                    LocalMoleculeParameter     0   µmol             
      578                    LocalMoleculeParameter     0   µmol             
      579                     Initial concentration     0 µmol/l             
      580                     Initial concentration     0 µmol/l             
      581                     Initial concentration     0 µmol/l             
      582                     Initial concentration     0 µmol/l             
      583                    LocalMoleculeParameter     0   µmol             
      584                    LocalMoleculeParameter     0   µmol             
      585                     Initial concentration     0 µmol/l             
      586                     Initial concentration     0 µmol/l             
      587                     Initial concentration     0 µmol/l             
      588                     Initial concentration     0 µmol/l             
      589                    LocalMoleculeParameter     0   µmol             
      590                    LocalMoleculeParameter     0   µmol             
      591                       Relative expression     0                    
      592                     Initial concentration     0 µmol/l             
      593          Fraction expressed intracellular     1                    
      594                       Relative expression     0                    
      595                     Initial concentration     0 µmol/l             
      596          Fraction expressed intracellular     1                    
      597                       Relative expression     0                    
      598                     Initial concentration     0 µmol/l             
      599          Fraction expressed intracellular     1                    
      600                       Relative expression     0                    
      601                     Initial concentration     0 µmol/l             
      602                 Fraction expressed apical     0                    
      603                    LocalMoleculeParameter     0   µmol             
      604                    LocalMoleculeParameter     0   µmol             
      605                     Initial concentration     0 µmol/l             
      606                     Initial concentration     0 µmol/l             
      607                     Initial concentration     0 µmol/l             
      608                     Initial concentration     0 µmol/l             
      609                    LocalMoleculeParameter     0   µmol             
      610                    LocalMoleculeParameter     0   µmol             
      611                     Initial concentration     0 µmol/l             
      612                     Initial concentration     0 µmol/l             
      613                     Initial concentration     0 µmol/l             
      614                     Initial concentration     0 µmol/l             
      615                    LocalMoleculeParameter     0   µmol             
      616                    LocalMoleculeParameter     0   µmol             
      617                     Initial concentration     0 µmol/l             
      618                     Initial concentration     0 µmol/l             
      619                     Initial concentration     0 µmol/l             
      620                     Initial concentration     0 µmol/l             
      621                    LocalMoleculeParameter     0   µmol             
      622                    LocalMoleculeParameter     0   µmol             
      623                       Relative expression     0                    
      624                     Initial concentration     0 µmol/l             
      625          Fraction expressed intracellular     1                    
      626                       Relative expression     0                    
      627                     Initial concentration     0 µmol/l             
      628          Fraction expressed intracellular     1                    
      629                       Relative expression     0                    
      630                     Initial concentration     0 µmol/l             
      631          Fraction expressed intracellular     1                    
      632                       Relative expression     0                    
      633                     Initial concentration     0 µmol/l             
      634                 Fraction expressed apical     0                    
      635                    LocalMoleculeParameter     0   µmol             
      636                    LocalMoleculeParameter     0   µmol             
      637                     Initial concentration     0 µmol/l             
      638                     Initial concentration     0 µmol/l             
      639                     Initial concentration     0 µmol/l             
      640                     Initial concentration     0 µmol/l             
      641                    LocalMoleculeParameter     0   µmol             
      642                    LocalMoleculeParameter     0   µmol             
      643                     Initial concentration     0 µmol/l             
      644                     Initial concentration     0 µmol/l             
      645                     Initial concentration     0 µmol/l             
      646                     Initial concentration     0 µmol/l             
      647                    LocalMoleculeParameter     0   µmol             
      648                    LocalMoleculeParameter     0   µmol             
      649                     Initial concentration     0 µmol/l             
      650                     Initial concentration     0 µmol/l             
      651                     Initial concentration     0 µmol/l             
      652                     Initial concentration     0 µmol/l             
      653                    LocalMoleculeParameter     0   µmol             
      654                    LocalMoleculeParameter     0   µmol             
      655                       Relative expression     0                    
      656                     Initial concentration     0 µmol/l             
      657          Fraction expressed intracellular     1                    
      658                       Relative expression     0                    
      659                     Initial concentration     0 µmol/l             
      660          Fraction expressed intracellular     1                    
      661                       Relative expression     0                    
      662                     Initial concentration     0 µmol/l             
      663          Fraction expressed intracellular     1                    
      664                       Relative expression     0                    
      665                     Initial concentration     0 µmol/l             
      666                 Fraction expressed apical     0                    
      667                    LocalMoleculeParameter     0   µmol             
      668                    LocalMoleculeParameter     0   µmol             
      669                     Initial concentration     0 µmol/l             
      670                     Initial concentration     0 µmol/l             
      671                     Initial concentration     0 µmol/l             
      672                     Initial concentration     0 µmol/l             
      673                    LocalMoleculeParameter     0   µmol             
      674                    LocalMoleculeParameter     0   µmol             
      675                     Initial concentration     0 µmol/l             
      676                     Initial concentration     0 µmol/l             
      677                     Initial concentration     0 µmol/l             
      678                     Initial concentration     0 µmol/l             
      679                    LocalMoleculeParameter     0   µmol             
      680                    LocalMoleculeParameter     0   µmol             
      681                     Initial concentration     0 µmol/l             
      682                     Initial concentration     0 µmol/l             
      683                     Initial concentration     0 µmol/l             
      684                     Initial concentration     0 µmol/l             
      685                    LocalMoleculeParameter     0   µmol             
      686                    LocalMoleculeParameter     0   µmol             
      687                     Initial concentration     0 µmol/l             
      688                     Initial concentration     0 µmol/l             
      689                     Initial concentration     0 µmol/l             
      690                     Initial concentration     0 µmol/l             
      691                    LocalMoleculeParameter     0   µmol             
      692                    LocalMoleculeParameter     0   µmol             
      693                     Initial concentration     0 µmol/l             
      694                     Initial concentration     0 µmol/l             
      695                     Initial concentration     0 µmol/l             
      696                     Initial concentration     0 µmol/l             
      697                    LocalMoleculeParameter     0   µmol             
      698                    LocalMoleculeParameter     0   µmol             
      699                       Relative expression     0                    
      700                     Initial concentration     0 µmol/l             
      701          Fraction expressed intracellular     1                    
      702                       Relative expression     0                    
      703                     Initial concentration     0 µmol/l             
      704          Fraction expressed intracellular     1                    
      705                       Relative expression     0                    
      706                     Initial concentration     0 µmol/l             
      707          Fraction expressed intracellular     1                    
      708                       Relative expression     0                    
      709                     Initial concentration     0 µmol/l             
      710                 Fraction expressed apical     0                    
      711                    LocalMoleculeParameter     0   µmol             
      712                    LocalMoleculeParameter     0   µmol             
      713                     Initial concentration     0 µmol/l             
      714                     Initial concentration     0 µmol/l             
      715                     Initial concentration     0 µmol/l             
      716                     Initial concentration     0 µmol/l             
      717                    LocalMoleculeParameter     0   µmol             
      718                    LocalMoleculeParameter     0   µmol             
      719                     Initial concentration     0 µmol/l             
      720                     Initial concentration     0 µmol/l             
      721                     Initial concentration     0 µmol/l             
      722                     Initial concentration     0 µmol/l             
      723                    LocalMoleculeParameter     0   µmol             
      724                    LocalMoleculeParameter     0   µmol             
      725                     Initial concentration     0 µmol/l             
      726                     Initial concentration     0 µmol/l             
      727                     Initial concentration     0 µmol/l             
      728                     Initial concentration     0 µmol/l             
      729                    LocalMoleculeParameter     0   µmol             
      730                    LocalMoleculeParameter     0   µmol             
      731                       Relative expression     0                    
      732                     Initial concentration     0 µmol/l             
      733          Fraction expressed intracellular     1                    
      734                       Relative expression     0                    
      735                     Initial concentration     0 µmol/l             
      736          Fraction expressed intracellular     1                    
      737                       Relative expression     0                    
      738                     Initial concentration     0 µmol/l             
      739          Fraction expressed intracellular     1                    
      740                       Relative expression     0                    
      741                     Initial concentration     0 µmol/l             
      742                 Fraction expressed apical     0                    
      743                    LocalMoleculeParameter     0   µmol             
      744                    LocalMoleculeParameter     0   µmol             
      745                     Initial concentration     0 µmol/l             
      746                     Initial concentration     0 µmol/l             
      747                     Initial concentration     0 µmol/l             
      748                     Initial concentration     0 µmol/l             
      749                    LocalMoleculeParameter     0   µmol             
      750                    LocalMoleculeParameter     0   µmol             
      751                     Initial concentration     0 µmol/l             
      752                     Initial concentration     0 µmol/l             
      753                     Initial concentration     0 µmol/l             
      754                     Initial concentration     0 µmol/l             
      755                    LocalMoleculeParameter     0   µmol             
      756                    LocalMoleculeParameter     0   µmol             
      757                     Initial concentration     0 µmol/l             
      758                     Initial concentration     0 µmol/l             
      759                     Initial concentration     0 µmol/l             
      760                     Initial concentration     0 µmol/l             
      761                    LocalMoleculeParameter     0   µmol             
      762                    LocalMoleculeParameter     0   µmol             
      763                       Relative expression     0                    
      764                     Initial concentration     0 µmol/l             
      765          Fraction expressed intracellular     1                    
      766                       Relative expression     0                    
      767                     Initial concentration     0 µmol/l             
      768          Fraction expressed intracellular     1                    
      769                       Relative expression     0                    
      770                     Initial concentration     0 µmol/l             
      771          Fraction expressed intracellular     1                    
      772                       Relative expression     0                    
      773                     Initial concentration     0 µmol/l             
      774                 Fraction expressed apical     0                    
      775                    LocalMoleculeParameter     0   µmol             
      776                    LocalMoleculeParameter     0   µmol             
      777                     Initial concentration     0 µmol/l             
      778                     Initial concentration     0 µmol/l             
      779                     Initial concentration     0 µmol/l             
      780                     Initial concentration     0 µmol/l             
      781                    LocalMoleculeParameter     0   µmol             
      782                    LocalMoleculeParameter     0   µmol             
      783                     Initial concentration     0 µmol/l             
      784                     Initial concentration     0 µmol/l             
      785                     Initial concentration     0 µmol/l             
      786                     Initial concentration     0 µmol/l             
      787                    LocalMoleculeParameter     0   µmol             
      788                    LocalMoleculeParameter     0   µmol             
      789                     Initial concentration     0 µmol/l             
      790                     Initial concentration     0 µmol/l             
      791                     Initial concentration     0 µmol/l             
      792                     Initial concentration     0 µmol/l             
      793                    LocalMoleculeParameter     0   µmol             
      794                    LocalMoleculeParameter     0   µmol             
      795                       Relative expression     0                    
      796                     Initial concentration     0 µmol/l             
      797          Fraction expressed intracellular     1                    
      798                       Relative expression     0                    
      799                     Initial concentration     0 µmol/l             
      800          Fraction expressed intracellular     1                    
      801                       Relative expression     0                    
      802                     Initial concentration     0 µmol/l             
      803          Fraction expressed intracellular     1                    
      804                       Relative expression     0                    
      805                     Initial concentration     0 µmol/l             
      806                 Fraction expressed apical     0                    
      807                    LocalMoleculeParameter     0   µmol             
      808                    LocalMoleculeParameter     0   µmol             
      809                     Initial concentration     0 µmol/l             
      810                     Initial concentration     0 µmol/l             
      811                     Initial concentration     0 µmol/l             
      812                     Initial concentration     0 µmol/l             
      813                    LocalMoleculeParameter     0   µmol             
      814                    LocalMoleculeParameter     0   µmol             
      815                     Initial concentration     0 µmol/l             
      816                     Initial concentration     0 µmol/l             
      817                     Initial concentration     0 µmol/l             
      818                     Initial concentration     0 µmol/l             
      819                    LocalMoleculeParameter     0   µmol             
      820                    LocalMoleculeParameter     0   µmol             
      821                     Initial concentration     0 µmol/l             
      822                     Initial concentration     0 µmol/l             
      823                     Initial concentration     0 µmol/l             
      824                     Initial concentration     0 µmol/l             
      825                    LocalMoleculeParameter     0   µmol             
      826                    LocalMoleculeParameter     0   µmol             
      827                       Relative expression     0                    
      828                     Initial concentration     0 µmol/l             
      829          Fraction expressed intracellular     1                    
      830                       Relative expression     0                    
      831                     Initial concentration     0 µmol/l             
      832          Fraction expressed intracellular     1                    
      833                       Relative expression     0                    
      834                     Initial concentration     0 µmol/l             
      835          Fraction expressed intracellular     1                    
      836                       Relative expression     0                    
      837                     Initial concentration     0 µmol/l             
      838                 Fraction expressed apical     0                    
      839                    LocalMoleculeParameter     0   µmol             
      840                    LocalMoleculeParameter     0   µmol             
      841                     Initial concentration     0 µmol/l             
      842                     Initial concentration     0 µmol/l             
      843                     Initial concentration     0 µmol/l             
      844                     Initial concentration     0 µmol/l             
      845                    LocalMoleculeParameter     0   µmol             
      846                    LocalMoleculeParameter     0   µmol             
      847                     Initial concentration     0 µmol/l             
      848                     Initial concentration     0 µmol/l             
      849                     Initial concentration     0 µmol/l             
      850                     Initial concentration     0 µmol/l             
      851                    LocalMoleculeParameter     0   µmol             
      852                    LocalMoleculeParameter     0   µmol             
      853                     Initial concentration     0 µmol/l             
      854                     Initial concentration     0 µmol/l             
      855                     Initial concentration     0 µmol/l             
      856                     Initial concentration     0 µmol/l             
      857                    LocalMoleculeParameter     0   µmol             
      858                    LocalMoleculeParameter     0   µmol             
      859                       Relative expression     0                    
      860                     Initial concentration     0 µmol/l             
      861          Fraction expressed intracellular     1                    
      862                       Relative expression     0                    
      863                     Initial concentration     0 µmol/l             
      864          Fraction expressed intracellular     1                    
      865                       Relative expression     0                    
      866                     Initial concentration     0 µmol/l             
      867          Fraction expressed intracellular     1                    
      868                       Relative expression     0                    
      869                     Initial concentration     0 µmol/l             
      870                 Fraction expressed apical     0                    
      871                    LocalMoleculeParameter     0   µmol             
      872                    LocalMoleculeParameter     0   µmol             
      873                     Initial concentration     0 µmol/l             
      874                     Initial concentration     0 µmol/l             
      875                     Initial concentration     0 µmol/l             
      876                     Initial concentration     0 µmol/l             
      877                    LocalMoleculeParameter     0   µmol             
      878                    LocalMoleculeParameter     0   µmol             
      879                     Initial concentration     0 µmol/l             
      880                     Initial concentration     0 µmol/l             
      881                     Initial concentration     0 µmol/l             
      882                     Initial concentration     0 µmol/l             
      883                    LocalMoleculeParameter     0   µmol             
      884                    LocalMoleculeParameter     0   µmol             
      885                     Initial concentration     0 µmol/l             
      886                     Initial concentration     0 µmol/l             
      887                     Initial concentration     0 µmol/l             
      888                     Initial concentration     0 µmol/l             
      889                    LocalMoleculeParameter     0   µmol             
      890                    LocalMoleculeParameter     0   µmol             
      891                       Relative expression     0                    
      892                     Initial concentration     0 µmol/l             
      893          Fraction expressed intracellular     1                    
      894                       Relative expression     0                    
      895                     Initial concentration     0 µmol/l             
      896          Fraction expressed intracellular     1                    
      897                       Relative expression     0                    
      898                     Initial concentration     0 µmol/l             
      899          Fraction expressed intracellular     1                    
      900                       Relative expression     0                    
      901                     Initial concentration     0 µmol/l             
      902                 Fraction expressed apical     0                    
      903                    LocalMoleculeParameter     0   µmol             
      904                    LocalMoleculeParameter     0   µmol             
      905                     Initial concentration     0 µmol/l             
      906                     Initial concentration     0 µmol/l             
      907                     Initial concentration     0 µmol/l             
      908                     Initial concentration     0 µmol/l             
      909                    LocalMoleculeParameter     0   µmol             
      910                    LocalMoleculeParameter     0   µmol             
      911                     Initial concentration     0 µmol/l             
      912                     Initial concentration     0 µmol/l             
      913                     Initial concentration     0 µmol/l             
      914                     Initial concentration     0 µmol/l             
      915                    LocalMoleculeParameter     0   µmol             
      916                    LocalMoleculeParameter     0   µmol             
      917                     Initial concentration     0 µmol/l             
      918                     Initial concentration     0 µmol/l             
      919                     Initial concentration     0 µmol/l             
      920                     Initial concentration     0 µmol/l             
      921                    LocalMoleculeParameter     0   µmol             
      922                    LocalMoleculeParameter     0   µmol             
      923                       Relative expression     0                    
      924                     Initial concentration     0 µmol/l             
      925          Fraction expressed intracellular     1                    
      926                       Relative expression     0                    
      927                     Initial concentration     0 µmol/l             
      928          Fraction expressed intracellular     1                    
      929                       Relative expression     0                    
      930                     Initial concentration     0 µmol/l             
      931          Fraction expressed intracellular     1                    
      932                       Relative expression     0                    
      933                     Initial concentration     0 µmol/l             
      934                 Fraction expressed apical     0                    
      935                    LocalMoleculeParameter     0   µmol             
      936                    LocalMoleculeParameter     0   µmol             
      937                     Initial concentration     0 µmol/l             
      938                     Initial concentration     0 µmol/l             
      939                     Initial concentration     0 µmol/l             
      940                     Initial concentration     0 µmol/l             
      941                    LocalMoleculeParameter     0   µmol             
      942                    LocalMoleculeParameter     0   µmol             
      943                     Initial concentration     0 µmol/l             
      944                     Initial concentration     0 µmol/l             
      945                     Initial concentration     0 µmol/l             
      946                     Initial concentration     0 µmol/l             
      947                    LocalMoleculeParameter     0   µmol             
      948                    LocalMoleculeParameter     0   µmol             
      949                     Initial concentration     0 µmol/l             
      950                     Initial concentration     0 µmol/l             
      951                     Initial concentration     0 µmol/l             
      952                     Initial concentration     0 µmol/l             
      953                    LocalMoleculeParameter     0   µmol             
      954                    LocalMoleculeParameter     0   µmol             
      955                       Relative expression     0                    
      956                     Initial concentration     0 µmol/l             
      957          Fraction expressed intracellular     1                    
      958                       Relative expression     0                    
      959                     Initial concentration     0 µmol/l             
      960          Fraction expressed intracellular     1                    
      961                       Relative expression     0                    
      962                     Initial concentration     0 µmol/l             
      963          Fraction expressed intracellular     1                    
      964                       Relative expression     0                    
      965                     Initial concentration     0 µmol/l             
      966                 Fraction expressed apical     0                    
      967                    LocalMoleculeParameter     0   µmol             
      968                    LocalMoleculeParameter     0   µmol             
      969                     Initial concentration     0 µmol/l             
      970                     Initial concentration     0 µmol/l             
      971                     Initial concentration     0 µmol/l             
      972                     Initial concentration     0 µmol/l             
      973                    LocalMoleculeParameter     0   µmol             
      974                    LocalMoleculeParameter     0   µmol             
      975                     Initial concentration     0 µmol/l             
      976                     Initial concentration     0 µmol/l             
      977                     Initial concentration     0 µmol/l             
      978                     Initial concentration     0 µmol/l             
      979                    LocalMoleculeParameter     0   µmol             
      980                    LocalMoleculeParameter     0   µmol             
      981                     Initial concentration     0 µmol/l             
      982                     Initial concentration     0 µmol/l             
      983                     Initial concentration     0 µmol/l             
      984                     Initial concentration     0 µmol/l             

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

