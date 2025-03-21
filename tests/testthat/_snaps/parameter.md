# It can print parameters

    Code
      volumeParameter$print()
    Message
      <Parameter>
        * Quantity Type: Parameter
        * Path: Organism|Liver|Volume
        * Value: 2.38 [l]
      
      -- Formula --
      
        * isDistributed: TRUE

---

    Code
      formulaParameter$print()
    Message
      <Parameter>
        * Quantity Type: Parameter
        * Path: Organism|Weight
        * Value: 73.00 [kg]
      
      -- Formula --
      
        * isExplicit: TRUE
        * formula: Weight_blood + Weight_tissue
        * Value overrides formula: FALSE

---

    Code
      constantParameter$print()
    Message
      <Parameter>
        * Quantity Type: Parameter
        * Path: Organism|Age
        * Value: 30.00 [year(s)]
      
      -- Formula --
      
        * isConstant: TRUE

---

    Code
      tableParameter$print()
    Message
      <Parameter>
        * Quantity Type: Parameter
        * Path: Organism|TableParameter
        * Value: 1.00 [1/min]
      
      -- Formula --
      
        * isTable: TRUE
        * XDimension: Time
        * UseDerivedValues: FALSE
      
      -- Table values ----------------------------------------------------------------
    Output
        x= 0, y= 1, restartSolver= FALSE
        x= 10, y= 2, restartSolver= FALSE
        x= 30, y= 3, restartSolver= FALSE
        x= 40, y= 4, restartSolver= FALSE
    Message
        * Value overrides formula: FALSE

---

    Code
      rhsParameter$print()
    Message
      <Parameter>
        * Quantity Type: Parameter
        * Path: Organism|RHSParameter
        * Value: 15.00 [l]
      
      -- Formula --
      
        * isConstant: TRUE
      
      -- State variable --
      
        * isStateVariable: TRUE
      
      -- RHSFormula 
        * isExplicit: TRUE
        * formula: Q*Volume*Time

