# Test_Project.mbp3 — Structure

Fixture: `globalTestMoBiProject` in [tests/testthat/helper-for-tests.R](../../testthat/helper-for-tests.R).
Regen: `Rscript tests/data/MoBiProject/introspect_test_project.R`.

## Modules

| Name | isPKSim | merge | PV BBs | IC BBs | Molecules BB (known) | SpatialStructure |
|---|---|---|---|---|---|---|
| `TestModule` | FALSE | Extend | `Parameter Values` (empty) | `Initial Conditions` (empty) | `A`, `B`, `UGT2B7`, `CYP3A4` | standard PK-Sim human (organs: Kidney, Liver, Bone, Brain, Fat, Gallbladder, Gonads, Heart, LargeIntestine, SmallIntestine, Lung, Muscle, Pancreas, Skin, Spleen, Stomach, VenousBlood, ArterialBlood, PortalVein, Lumen) |

| `ExtModule_3IC_3PV` | FALSE | Extend | `PV1`, `PV2`, `PV3` (all empty) | `IC1`, `IC2`, `IC3` (all empty) | ? | ? |
| `ExtModule_noIC_noPV` | FALSE | Extend | none | none | ? | ? |

Molecules BB contents not enumerable via public R API; table lists known entries only.

Protein configuration (verified via `AddProteinExpressionParameters`):
- `UGT2B7` — configured as protein, works with `UGT2B7|Human|Healthy` profile.
- `CYP3A4` — configured as protein, works with `CYP3A4|Human|Healthy` profile.

## Individuals

| Name | rows |
|---|---|
| `DefaultIndividual` | 723 (standard human physiology) |

## Expression Profiles

| Name | Molecule | expression params | initial conditions |
|---|---|---|---|
| `UGT2B7|Human|Healthy` | `UGT2B7` | 262 | 153 |
| `CYP3A4|Human|Healthy` | `CYP3A4` | 262 | 153 |

## Spatial structure (level-2 / level-3 container names, from expression profile paths)

Level-2 (organ): `VenousBlood`, `ArterialBlood`, `Bone`, `Brain`, `Fat`, `Gonads`, `Heart`, `Kidney`, `Lumen`, `Stomach`, `SmallIntestine`, `LargeIntestine`, `Liver`, `Lung`, `Muscle`, `Pancreas`, `PortalVein`, `Skin`, `Spleen`. TestModule additionally has `Gallbladder` (seen in local-molecule-parameter snapshots).

Level-3 (compartment): `Plasma`, `BloodCells`, `Interstitial`, `Intracellular`, `Endosome`, `Mucosa`, `Periportal`, `Pericentral`; lumen: `Stomach`, `Duodenum`, `UpperJejunum`, `LowerJejunum`, `UpperIleum`, `LowerIleum`, `Caecum`, `ColonAscendens`, `ColonTransversum`, `ColonDescendens`, `ColonSigmoid`, `Rectum`.

Valid `organPaths`: `"Organism|<level-2>"` (e.g. `"Organism|Kidney"`, `"Organism|Bone"`).

## Expression parameter names

`Initial concentration`, `Fraction expressed interstitial`, `Relative expression`, `Fraction expressed intracellular`, `Reference concentration`, `t1/2 (liver)`, `t1/2 (intestine)`, `Disease factor`, `Ontogeny factor GI table`, `Ontogeny factor table`, `Ontogeny factor`, `Ontogeny factor GI`, `Relative expression in blood cells`, `Fraction expressed in blood cells`, `Fraction expressed in blood cells membrane`, `Relative expression in plasma`, `Relative expression in vascular endothelium`, `Fraction expressed in endosomes`, `Fraction expressed on plasma-side membrane of vascular endothelium`, `Fraction expressed on tissue-side membrane of vascular endothelium`.

## Recipes

```r
project   <- globalTestMoBiProject
module    <- project$getModules("TestModule")[[1]]
pvBB      <- module$getParameterValuesBBs()[[1]]         # empty
icBB      <- module$getInitialConditionsBBs()[[1]]       # empty
pv2       <- project$getModules("ExtModule_3IC_3PV")[[1]]$getParameterValuesBBs("PV2")[[1]]
emptyMod  <- project$getModules("ExtModule_noIC_noPV")[[1]]
cyp3a4    <- project$getExpressionProfiles("CYP3A4|Human|Healthy")[[1]]
ugt2b7    <- project$getExpressionProfiles("UGT2B7|Human|Healthy")[[1]]
ind       <- project$getIndividual("DefaultIndividual")

# Fresh project for mutating tests
freshProject <- loadMoBiProject(getTestDataFilePath("MoBiProject/Test_Project.mbp3"))
```

## Rules

- `globalTestMoBiProject` loaded once; BBs mutable. Reload for mutating tests.
- For `addProteinExpressionToParameterValuesBB` tests: use TestModule + molecule `UGT2B7` + profile `UGT2B7|Human|Healthy` (or `CYP3A4` + `CYP3A4|Human|Healthy`). Both verified working. Pass `referenceExpressionProfiles` as a plain (unnamed) list; molecule matching is done by each profile's `MoleculeName`.
- `ExtModule_noIC_noPV`: for missing-BB error paths.
