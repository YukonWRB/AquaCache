# Patch 66 catalogue review

Reviewed on 2026-10-14 against `testdb` on `10.250.12.154`, after confirming
revision 65 and the independent reporting-basis/speciation/target schema.
The initial snapshot contained **1,455 parameters**, zero import mappings, 67 guidelines,
and 13 guideline-rule inputs. Production and the development `aquacache`
database were not migration targets.

A final read-only recheck found an additional parameter created concurrently by
another UI verification session (`ZZ review parameter 20261014`). It has no
scientific analyte definition and is not a duplicate. It is retained and included
in the audit, which now accounts for **all 1,456 current parameters**. Other
sessions also populated mappings during verification; tests isolate their own
fixtures and compare rollback state while retaining relation locks.

## Files and operation

- `../DEV_patch_66.R` follows the existing sourced-patch convention: an existing
  `con`, postgres maintenance role, exact prior revision, one owned transaction,
  final constraint checks, revision/package-version update, COMMIT on success,
  and complete ROLLBACK on error. Sourcing the patch **applies it**.
- `parameters.tsv` is an ordered, name-based migration manifest. It has no
  cluster-specific parameter, unit, fraction, matrix, or chemistry IDs.
- `cleanup.R` contains the one-time migration implementation and uses the
  installed reusable `revise_parameter_catalogue_entry()` migration helper.
- `catalogue_review.tsv` accounts for every parameter in the inspected snapshot.
  Its IDs are evidence for that snapshot only and are never read by the patch.

The manifest contains 113 rules, including three source aliases absent from the
initial snapshot. The current projected catalogue has **1,430 parameters: 26 merges and 84 name
updates**. A missing source is skipped. A missing canonical target causes the
source to be renamed and qualified, preserving its ID. An empty mapping
catalogue does not require fictitious mappings or source-profile records.

## Scientific decisions

The reviewed b+j, j+k and b+j+k fluoranthene pools each get one parameter. They
remain distinct from individual isomers and from one another. The two stated
2,4-/2,5-dichlorophenol mixture aliases are consolidated. Tonalide, true colour,
the below-top-of-casing water-level alias, and the methylnaphthalene all-isomer
pool are consolidated. Generic pheophytin is combined with pheophytin a only
when its recorded CAS explicitly identifies pheophytin a.

Arsenic, selenium, chromium and iron forms move to explicit chemical-form
metadata. Total sulfur carries an explicit all-species chemical scope; it does
not imply a physical sample fraction. Known acidity pH endpoints become fixed
endpoint metadata. Standard, five-day and explicitly non-standard CBOD share
one measurand while retaining different analytical conditions. No unknown
temperature, numeric reporting expression, physical fraction or dry/wet mass
basis is guessed. Sediment carbon/nitrogen retain the solid matrix and recorded
preparation; canonical parameters gain a solid unit only if one is absent.

These definitions remain separate:

- Native chemicals and isotopically labelled standards, including L-suffixed
  BDE congeners. Labelled congeners get explicit `13C12-labelled` names.
- Analyte concentration and percent surrogate recovery.
- Radionuclide mass concentration and activity concentration. Their existing
  units are made explicit in the names; no Bq-to-mass conversion is attempted.
- MPN and CFU enumeration, raw organism counts and volume-normalized counts,
  and water-sample versus string-wound-filter enumerations.
- Individual PFAS chain lengths, individual isomers, isotope mass numbers,
  sieve sizes, different petroleum carbon ranges, and molecular-formula targets.
- Reactive silica versus generic silica; corrected, uncorrected and unspecified
  chlorophyll determinations; dissolved concentration versus mass load;
  water levels with different reference points; LC50, survival and LT50.
- A defined summed alkylated-isomer pool and a methyl compound whose isomer
  scope is unspecified. The methylnaphthalene all-isomer CAS is the reviewed
  exception. Likewise, unqualified phenols/phenolics lack sufficient method
  evidence to assert that two determinations have identical analytical recovery.

The last distinction is an interpretation limit, not evidence that the names
represent interchangeable data. The catalogue review records these uncertain
scopes instead of silently inventing a method or composition. A shared CAS or
small spelling distance alone does not justify a merge. Residual shared-CAS
groups were checked for explicit isotope, method or measured-quantity differences.
CAS values corrupted into dates and ambiguous source labels are retained for
separate evidence-based correction; they are not automatically repaired.

## Cross-instance safeguards

Names resolve case-insensitively after trimming; ambiguous matches abort.
All references move using current database IDs. Existing group/subgroup links
and advisory chemistry suggestions coalesce through the package helper.
Identity collisions abort; no conflicting result is discarded. Custom condition
requirements that would be weakened by a merge abort for explicit review.

Unit aliases differing only in spelling can align without changing numbers.
For genuinely different scales, the patch only normalizes mappings when the
source has no numeric observations, criteria or other non-metadata references.
It accepts source-correct and already-target-correct conversion factors and
rejects custom factors/offsets. It never guesses whether an existing stored
number has already been converted. Such instances stop atomically and need a
separately reviewed numeric migration. A previously implicit single matrix is
retained when joining a target supporting multiple matrices.

The patch locks affected relations and temporarily suspends only result/mapping
chemistry-requiredness triggers, allowing historical unknowns to remain unknown
after relocation. It restores their original enabled modes. Mapping immutability
is suspended only during each explicit historical mapping correction and by the
shared reference migrator. Audit, unit, FK and result-identity checks remain in
force. Final checks reject duplicate normalized names and an unrestored mapping
guard. Source-match JSON, mapping IDs and mapping-set identities are preserved.

Relational references are covered. Raw audit/import payloads remain historical
evidence. Custom SQL/JSON containing embedded numeric parameter IDs is not a
foreign key and cannot be safely rewritten by a general catalogue patch.

## Source evidence

- [PubChem: Tonalide](https://pubchem.ncbi.nlm.nih.gov/compound/tonalide)
- [PubChem: pheophytin a](https://pubchem.ncbi.nlm.nih.gov/compound/Pheophytin-a)
- [ACGIH: methylnaphthalene, all isomers](https://www.acgih.org/methylnaphthalene-all-isomers/)
- [EPA Method 1614A, Table 1: native and labelled BDE congeners](https://www.epa.gov/sites/production/files/2015-08/documents/method_1614a_2010.pdf)
- [EPA selenium criteria: selenite/selenate oxidation states](https://www.epa.gov/sites/default/files/2019-03/documents/ambient-wqc-selenium-1987.pdf)
- [EPA carbonaceous BOD catalogue](https://cdxapps.epa.gov/oms-substance-registry-services/substance-details/1640614)
- [PubChem: perfluoropentanoic acid](https://pubchem.ncbi.nlm.nih.gov/compound/75921)
- [PubChem: perfluoroheptanesulfonic acid](https://pubchem.ncbi.nlm.nih.gov/compound/375-92-8)
- [German Environmental Specimen Bank: bisphenol A](https://www.umweltprobenbank.de/en/documents/profiles/analytes/18564)
- [NERC vocabulary: ibuprofen](https://vocab.nerc.ac.uk/collection/S27/current/CS004256/1/)
- [PubChem: DEET](https://pubchem.ncbi.nlm.nih.gov/compound/DEET)
- [PubChem: EPTC](https://pubchem.ncbi.nlm.nih.gov/compounds/12968)
- [PubChem: arsenobetaine](https://pubchem.ncbi.nlm.nih.gov/compound/Arsenobetaine)
- [EPA: different methods for phenols/phenolics](https://www.epa.gov/cwa-methods/approved-cwa-test-methods-organic-compounds)

## Verification

The complete real catalogue passed a rollback-only execution of the patch,
including immediate deferred-constraint validation, the expected catalogue
count, and exact restoration of the original parameter rows after rollback.
Both focused test files passed on testdb: **49 assertions, zero failures,
zero skips and zero test warnings**. They include published mappings, legacy
acidity results, missing sources/targets, numeric-reinterpretation rejection,
and result-identity collisions. The full-catalogue check compares all reviewed
canonical names and verifies no normalized-name duplicates remain. A savepoint
under retained relation locks verifies exact rollback restoration without
confusing other sessions' concurrent edits with the patch's changes.

Run the checks using the current checkout and explicit testdb opt-in:

```r
readRenviron("C:/Users/gtdelapl/Documents/.Renviron")
pkgload::load_all(".", quiet = TRUE)
Sys.setenv(AQUACACHE_PATCH66_DATABASE = "testdb")
testthat::test_file("tests/testthat/test-patch66-cleanup.R")
testthat::test_file("tests/testthat/test-patch66-failures.R")
```

These checks do not constitute a committed Patch 66 application. They leave
testdb at revision 65. Identity sequence increments from rollback fixtures may
survive, as with other PostgreSQL integration tests.
