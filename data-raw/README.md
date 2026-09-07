# Regenerating the example and test files

The files in `inst/extdata/` are generated with the OSP Suite .NET libraries that
ship in `inst/lib/`. When those libraries are updated (new suite version, new
`MoBi.R`/`PKSim.R` pin in `shared/DependencyManager/src/DependencyManager.csproj`),
the generated files can fall out of step with them. The scripts in this folder
rebuild the files for which the procedure is known.

`data-raw/` ships with the package, so the procedure travels with the sources it
belongs to and can be repeated on every major update of the libraries.

## Scripts

| Script | Rebuilds |
| --- | --- |
| `regenerate-aciclovir-pkml.R` | `inst/extdata/Aciclovir.pkml` |
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
2. Check the result before committing:

   ```r
   devtools::test()
   devtools::build_vignettes()
   ```

   Several tests compare against the values in these files, and the vignettes
   print them.

## What each script does, and what to watch

### `Aciclovir.pkml`

The simulation is described by the PK-Sim project snapshot that the PKML carries
inside it, base64 encoded, in a `<Snapshot>` element. Three of them are present,
one for the project, one for the individual and one for the expression profile,
and the project one is identified by its `"ApplicationName": "PK-Sim"` entry. The
script extracts it, writes it to `data-raw/Aciclovir_snapshot.json`, lets PK-Sim
rebuild the simulation from it with the current libraries, and writes the
simulation out as PKML.

**A simulation loaded from a snapshot carries no embedded snapshot of its own.**
`loadSimulationsFromSnapshot()` builds the simulation from the snapshot content,
and the PKML the script writes loses the snapshots. That is reported as a bug in
[OSPSuite-R#2029](https://github.com/Open-Systems-Pharmacology/OSPSuite-R/issues/2029).

`saveSimulation()` itself preserves the snapshots. A PKML that is read with
`loadSimulation()` and written back keeps all three filled, on the first
roundtrip and on every later one.

Until that bug is fixed, two things follow:

- `data-raw/Aciclovir_snapshot.json` is committed, because after one run of the
  script the PKML has nothing left to extract. The script then falls back to that
  file, and says so. Commit the JSON again whenever the simulation changes.
- To keep the embedded snapshot in the example file, which is what the committed
  one has today, do the regeneration in the PK-Sim user interface: load
  `data-raw/Aciclovir_snapshot.json` as a project, then export the single
  simulation to `inst/extdata/Aciclovir.pkml`.

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

### `PKAnalyses.csv`

The script runs the simulation from `Aciclovir.pkml`, calculates the PK
parameters, and exports them. The CSV is written by OSPSuite.Core
(`PKAnalysisTask.ExportPKAnalysesToCSV`), not by R, which is why it has a UTF-8
byte order mark, CRLF line endings, and `µmol/l` style units.

`vignettes/pk-analysis.Rmd` also calls `exportPKAnalysesToCSV` on the installed
copy of this file, so building the vignettes overwrites it in the installed
package library. That does not touch the repository copy.

### `pop.csv`

The script creates a European population of 10 individuals with a fixed seed and
writes it with `exportPopulationToCSV()`, so the file follows the parameter
structure of the current libraries.

The individuals themselves come from the seed in the script, so changing the seed
replaces all of them and with them the results of every population simulation.

## Files without a documented procedure

The remaining files in `inst/extdata/` are not covered here. If you regenerate
one of them, add a script and a section to this folder.

`CompiledDataSet.xlsx`, `ObsDataAciclovir_1.pkml`, `ObsDataAciclovir_2.pkml`,
`ObsDataAciclovir_3.pkml`, `SAResult.csv`, `SimResults.csv`,
`TH_QST_Platform.mbp3`, `Thyroid.pkml`, `aging_data.csv`,
`dataImporterConfiguration.xml`, `res.csv`, `sa.csv`, `simple.pkml`,
`test_snapshot.json`.
