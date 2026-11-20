# Patch 72

Deploy AquaCache and YGwater/AquaLink together with the database patch. Pause
processes using the previous packages for the cutover, then restart them with
the updated packages. This includes workers that load installed R packages.

[The rename manifest](column_names.R) lists the table columns changed by the
patch. Existing view columns retain their names. Historical continuous QC
and correction queries can read intervals recorded before and after the patch.
The patch migrates saved import profile identity fields and default keys to
the same names used by both R packages. On a cluster that already has the
Patch 72 schema, source the patch again to update these profile settings.

`discrete.samples.depth_height_m` is a signed offset in metres from the local
ground or water surface, or from the top of the well casing where applicable.
Use positive values above that reference, negative values below it, zero at the
reference, and NULL when unknown. The patch preserves existing values and signs.

`discrete.import_parameter_mappings.protocol_method_id` optionally assigns a
method to a result. It references `discrete.protocols_methods.protocol_method_id`,
the same catalogue used by `discrete.results.protocol_method_id`.
