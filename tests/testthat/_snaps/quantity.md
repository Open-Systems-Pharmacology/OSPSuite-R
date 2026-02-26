# It can print Quantity

    Code
      quantity$print()
    Output
      <Quantity>
        * Quantity Type: Parameter
        * Path: Organism|Liver|Intracellular|Volume
        * Value: 1.59 [l]
        * Value Origin: Publication-Willmann S, Lippert J, Sevestre M, Solodenko J,
        Fois F, Schmitt W. PK-Sim®: a physiologically based pharmacokinetic
        ‘whole-body’ model. Biosilico. 2003; 1 (4): 121-124.
        http://dx.doi.org/10.1016/S1478-5382%2803%2902342-4
      
      -- Formula --
      
        * isExplicit: TRUE
        * formula: f_cell*V
        * Value overrides formula: FALSE

# It prints the Scientific value of the Quantity

    Code
      print(quantity)
    Output
      <Quantity>
        * Quantity Type: Parameter
        * Path: Organism|Liver|Intracellular|Volume
        * Value: 1.00e-03 [l]
        * Value Origin: Publication-Willmann S, Lippert J, Sevestre M, Solodenko J,
        Fois F, Schmitt W. PK-Sim®: a physiologically based pharmacokinetic
        ‘whole-body’ model. Biosilico. 2003; 1 (4): 121-124.
        http://dx.doi.org/10.1016/S1478-5382%2803%2902342-4
      
      -- Formula --
      
        * isExplicit: TRUE
        * formula: f_cell*V
        * Value overrides formula: TRUE

---

    Code
      print(quantity)
    Output
      <Quantity>
        * Quantity Type: Parameter
        * Path: Organism|Liver|Intracellular|Volume
        * Value: 2.00 [l]
        * Value Origin: Publication-Willmann S, Lippert J, Sevestre M, Solodenko J,
        Fois F, Schmitt W. PK-Sim®: a physiologically based pharmacokinetic
        ‘whole-body’ model. Biosilico. 2003; 1 (4): 121-124.
        http://dx.doi.org/10.1016/S1478-5382%2803%2902342-4
      
      -- Formula --
      
        * isExplicit: TRUE
        * formula: f_cell*V
        * Value overrides formula: TRUE

---

    Code
      print(quantity)
    Output
      <Quantity>
        * Quantity Type: Parameter
        * Path: Organism|Liver|Intracellular|Volume
        * Value: 1.00e+04 [l]
        * Value Origin: Publication-Willmann S, Lippert J, Sevestre M, Solodenko J,
        Fois F, Schmitt W. PK-Sim®: a physiologically based pharmacokinetic
        ‘whole-body’ model. Biosilico. 2003; 1 (4): 121-124.
        http://dx.doi.org/10.1016/S1478-5382%2803%2902342-4
      
      -- Formula --
      
        * isExplicit: TRUE
        * formula: f_cell*V
        * Value overrides formula: TRUE

---

    Code
      print(quantity)
    Output
      <Quantity>
        * Quantity Type: Parameter
        * Path: Organism|Liver|Intracellular|Volume
        * Value: 1.00e+04 [l]
        * Value Origin: Publication-Willmann S, Lippert J, Sevestre M, Solodenko J,
        Fois F, Schmitt W. PK-Sim®: a physiologically based pharmacokinetic
        ‘whole-body’ model. Biosilico. 2003; 1 (4): 121-124.
        http://dx.doi.org/10.1016/S1478-5382%2803%2902342-4
      
      -- Formula --
      
        * isExplicit: TRUE
        * formula: f_cell*V
        * Value overrides formula: TRUE

# It prints the NaN value of the Quantity

    Code
      quantity$print()
    Output
      <Quantity>
        * Quantity Type: Parameter
        * Path: AADAC|Lipophilicity
        * Value: NaN [Log Units]
      
      -- Formula --
      
        * isConstant: TRUE

