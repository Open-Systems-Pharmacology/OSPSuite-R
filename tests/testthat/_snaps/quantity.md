# It can print Quantity

    Code
      quantity$print()
    Message
      <Quantity>
        * Quantity Type: Parameter
        * Path: Organism|Liver|Intracellular|Volume
        * Value: 1.59 [l]
      
      -- Formula --
      
        * isExplicit: TRUE
        * formula: f_cell*V
        * Value overrides formula: FALSE

# It prints the Scientific value of the Quantity

    Code
      print(quantity)
    Message
      <Quantity>
        * Quantity Type: Parameter
        * Path: Organism|Liver|Intracellular|Volume
        * Value: 1.00e-03 [l]
      
      -- Formula --
      
        * isExplicit: TRUE
        * formula: f_cell*V
        * Value overrides formula: TRUE

---

    Code
      print(quantity)
    Message
      <Quantity>
        * Quantity Type: Parameter
        * Path: Organism|Liver|Intracellular|Volume
        * Value: 2.00 [l]
      
      -- Formula --
      
        * isExplicit: TRUE
        * formula: f_cell*V
        * Value overrides formula: TRUE

---

    Code
      print(quantity)
    Message
      <Quantity>
        * Quantity Type: Parameter
        * Path: Organism|Liver|Intracellular|Volume
        * Value: 1.00e+04 [l]
      
      -- Formula --
      
        * isExplicit: TRUE
        * formula: f_cell*V
        * Value overrides formula: TRUE

---

    Code
      print(quantity)
    Message
      <Quantity>
        * Quantity Type: Parameter
        * Path: Organism|Liver|Intracellular|Volume
        * Value: 1.00e+04 [l]
      
      -- Formula --
      
        * isExplicit: TRUE
        * formula: f_cell*V
        * Value overrides formula: TRUE

# It prints the NaN value of the Quantity

    Code
      quantity$print()
    Message
      <Quantity>
        * Quantity Type: Parameter
        * Path: AADAC|Lipophilicity
        * Value: NaN [Log Units]
      
      -- Formula --
      
        * isConstant: TRUE

