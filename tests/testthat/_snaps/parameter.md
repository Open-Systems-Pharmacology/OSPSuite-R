# It can print parameters

    Code
      volumeParameter$print()
    Output
      <Parameter>
        * Quantity Type: Parameter
        * Path: Organism|Liver|Volume
        * Value: 10.00 [l]
      
      -- Formula --
      
        * isConstant: TRUE

---

    Code
      formulaParameter$print()
    Output
      <Parameter>
        * Quantity Type: Parameter
        * Path: Organism|FormulaParameter
        * Value: 2.00 [l/min]
      
      -- Formula --
      
        * isExplicit: TRUE
        * formula: 1*2
        * Value overrides formula: FALSE

---

    Code
      constantParameter$print()
    Output
      <Parameter>
        * Quantity Type: Parameter
        * Path: Organism|Q
        * Value: 10.00 [(l/min)²]
      
      -- Formula --
      
        * isConstant: TRUE

---

    Code
      tableParameter$print()
    Output
      <Parameter>
        * Quantity Type: Parameter
        * Path: Organism|TableParameter
        * Value: 1.00 [1/min]
      
      -- Formula --
      
        * isTable: TRUE
        * XDimension: Time
        * UseDerivedValues: FALSE
      
      -- Table values ----------------------------------------------------------------
        x= 0, y= 1, restartSolver= FALSE
        x= 10, y= 2, restartSolver= FALSE
        x= 30, y= 3, restartSolver= FALSE
        x= 40, y= 4, restartSolver= FALSE
        * Value overrides formula: FALSE

---

    Code
      rhsParameter$print()
    Output
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

