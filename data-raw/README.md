# Regenerating the example and test files

The files in `inst/extdata/` are generated with the OSP Suite .NET libraries that
ship in `inst/lib/`. When those libraries are updated (new suite version, new
`MoBi.R`/`PKSim.R` pin in `shared/DependencyManager/src/DependencyManager.csproj`),
the generated files can fall out of step with them. The scripts in this folder
rebuild the files for which the procedure is known.

`data-raw/` is listed in `.Rbuildignore`, so nothing here ships with the package.

## Scripts

| Script | Rebuilds |
| --- | --- |
| `regenerate-aciclovir-pkml.R` | `inst/extdata/Aciclovir.pkml`, from `Aciclovir_snapshot.json` |
| `regenerate-ind-and-pop-snapshot.R` | `inst/extdata/ind_and_pop_snapshot.json` |
| `regenerate-pk-analyses-csv.R` | `inst/extdata/PKAnalyses.csv` |
| `regenerate-pop-csv.R` | `inst/extdata/pop.csv` |

Run them from the repository root, in the order listed. `PKAnalyses.csv` is
derived from `Aciclovir.pkml`, so the simulation has to be rebuilt first.

```
Rscript data-raw/regenerate-aciclovir-pkml.R
Rscript data-raw/regenerate-ind-and-pop-snapshot.R
Rscript data-raw/regenerate-pk-analyses-csv.R
Rscript data-raw/regenerate-pop-csv.R
```

Do not add `--vanilla`: it skips `.Rprofile`, so renv never activates and the
wrong `rSharp` is used (see `AGENTS.md`).

## Before you start

1. Update `inst/lib/` first (see "How `inst/lib` is populated" in `AGENTS.md`).
   Every script here runs against the binaries in `inst/lib/`, so the suite
   version must already be the new one.
2. Expect large diffs. Both the PKML export and the snapshot export write new
   internal object ids on every run, so a regenerated file differs from the
   committed one over its whole length even when the model is unchanged. Review
   the parts that carry meaning (simulation name, outputs, parameter values,
   version attributes) rather than the id churn.
3. Check the result before committing:

   ```r
   devtools::test()
   devtools::build_vignettes()
   ```

   Several tests compare against the values in these files, and the vignettes
   print them.

## What each script does, and what to watch

### `Aciclovir.pkml`

The simulation is described by `data-raw/Aciclovir_snapshot.json`, a PK-Sim
project snapshot that is kept under version control. The script loads it, lets
PK-Sim rebuild the simulation with the current libraries, and writes the
simulation out as PKML.

That JSON was extracted from the PKML itself: a PKML exported by PK-Sim carries
the project snapshot inside it, base64 encoded, in `<Snapshot>` elements. Three
of them are present, one for the project, one for the individual and one for the
expression profile, and the project one is identified by its
`"ApplicationName": "PK-Sim"` entry. The script still contains that extraction
step, but only as a bootstrap for the case where the JSON is missing.

**A simulation loaded from a snapshot carries no embedded snapshot of its own.**
`loadSimulationsFromSnapshot()` builds the simulation from the snapshot content,
and nothing is left to write back into the `<Snapshot>` elements, so the PKML
this script writes has three empty ones and shrinks from about 82600 to about
69400 lines.

`saveSimulation()` itself preserves the snapshots. A PKML that is read with
`loadSimulation()` and written back keeps all three filled, on the first
roundtrip and on every later one; the file only loses a few dozen lines of
formatting. The loss described above comes from the snapshot route, not from the
export. Two consequences:

- `data-raw/Aciclovir_snapshot.json` is the only remaining description of this
  simulation that does not depend on the suite version. Do not delete it, and
  commit it whenever the simulation changes.
- If the example file should keep its embedded snapshot (which is what the
  committed one has today), do the regeneration in the PK-Sim user interface
  instead: load `data-raw/Aciclovir_snapshot.json` as a project, then export the
  single simulation to `inst/extdata/Aciclovir.pkml`. Use the script when the
  embedded snapshot does not matter to you, or when no PK-Sim installation is at
  hand.

Afterwards, the simulation must still be named `Vergin 1995 IV` and carry both
outputs:

```
Organism|PeripheralVenousBlood|Aciclovir|Plasma (Peripheral Venous Blood)
Organism|VenousBlood|Plasma|Aciclovir|Plasma Unbound
```

### `ind_and_pop_snapshot.json`

The script converts the snapshot to a PK-Sim project with the current libraries,
then converts that project back to a snapshot. Both directions run through the
PK-Sim converter, so the file comes out in the format the new version writes.

It uses `runSimulations = TRUE`. The committed snapshot has `"HasResults": true`
for its two simulations, and only a project whose simulations were run carries
that flag back into the exported snapshot. Without the flag, the regenerated file
differs from the committed one in those two entries.

`tests/testthat/test-1-utilities-snapshots.R` expects two simulations, one of
them a population of 6 individuals with aging data, so check those numbers after
the regeneration.

### `PKAnalyses.csv`

The script runs the simulation from `Aciclovir.pkml`, calculates the PK
parameters, and exports them. The CSV is written by OSPSuite.Core
(`PKAnalysisTask.ExportPKAnalysesToCSV`), not by R, which is why it has a UTF-8
byte order mark, CRLF line endings, and `µmol/l` style units.

Two things to know about the diff:

- One block of 14 parameters is written per output path, so the row count follows
  the output selections of the PKML. The simulation currently has two outputs,
  which gives 28 data rows plus the header.
- The number formatting depends on the .NET version, not on the model. Values
  written by the .NET Framework era libraries have up to 15 digits
  (`50.2527198791504`), current .NET writes the shortest round trip form of the
  same number (`50.25272`). A diff that only changes digit counts is expected.

`vignettes/pk-analysis.Rmd` also calls `exportPKAnalysesToCSV` on the installed
copy of this file, so building the vignettes overwrites it in the installed
package library. That does not touch the repository copy.

### `pop.csv`

The script creates a European population of 10 individuals with a fixed seed and
writes it with `exportPopulationToCSV()`, so the file follows the parameter
structure of the current libraries.

The file that this replaced came from PK-Sim 7.3 and was exported by PK-Sim
itself, not by this package. It began with

```
#Project: Undefined
#PK-Sim version: 7.3.0 - Build 0
```

and held 10 individuals of a population named `pop_10`. `exportPopulationToCSV()`
writes the table with `utils::write.csv()`, so the new file has no comment header,
and the population structure differs in four ways that the tests depend on:

- 121 columns against 100. The new ones are `Organism|BSA` and the
  `Organism|Lumen|...` pH and bile salt parameters.
- The covariates are `Gender` and `Population`. `RaceIndex` and
  `Population Name` are gone, so a population loaded from this file reports two
  covariates instead of three.
- `Gender` holds `MALE` and `FEMALE` instead of `1` and `2`.
- The column names carry no unit suffix. The old file had
  `Organism|Weight [kg]`, the new one has `Organism|Weight`.

The individuals themselves come from the seed in the script, so changing the seed
replaces all of them. That changes the results of every population simulation in
the test suite, and the snapshot of
`tests/testthat/_snaps/utilities-simulation-results.md` has to be accepted again:

```r
testthat::snapshot_accept("utilities-simulation-results", path = "tests/testthat")
```

## Files without a documented procedure

The remaining files in `inst/extdata/` are not covered here. If you regenerate
one of them, add a script and a section to this folder.

`CompiledDataSet.xlsx`, `ObsDataAciclovir_1.pkml`, `ObsDataAciclovir_2.pkml`,
`ObsDataAciclovir_3.pkml`, `SAResult.csv`, `SimResults.csv`,
`TH_QST_Platform.mbp3`, `Thyroid.pkml`, `aging_data.csv`,
`dataImporterConfiguration.xml`, `res.csv`, `sa.csv`, `simple.pkml`,
`test_snapshot.json`.
