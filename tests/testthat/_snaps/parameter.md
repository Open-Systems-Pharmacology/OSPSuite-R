# It can print parameters

    Code
      volumeParameter$print()
    Output
      <Parameter>
        * Quantity Type: Parameter
        * Path: Organism|Liver|Volume
        * Value: 2.38 [l]
        * Value Origin: Publication-Schlender J-F, Meyer M, et al. Development of a
        Whole-Body Physiologically Based Pharmacokinetic Approach to Assess the
        Pharmacokinetics of Drugs in Elderly Individuals. Clinical Pharmacokinetics.
        2016;55(12):1573-1589. doi:10.1007/s40262-016-0422-3
      
      -- Formula --
      
        * isDistributed: TRUE

---

    Code
      formulaParameter$print()
    Output
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
    Output
      <Parameter>
        * Quantity Type: Parameter
        * Path: Organism|Age
        * Value: 30.00 [year(s)]
      
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

