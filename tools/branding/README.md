# Personal package hex artwork

These logos use the CTTIR v5 vector renderer and bundled Inter/JetBrains Mono
fonts from `CTTIR/hexmakR`. The shared hex geometry, transparent exterior,
name placement, fitted typography, shadows, and light/dark/monochrome exports
follow that renderer. Package motifs are defined in `personal-motifs.R`.

- Games: violet `#7956B5`, shared by the eight game packages.
- foldbenchR: embedding green `#146554`, matching the CTTIR embedding family.
- synesthR: creative/consensus rose `#91617B`, matching that CTTIR family.

Run `Rscript --vanilla tools/branding/render-personal.R <output-directory>`.
Font configuration is local to the rendering process. No installed fonts or
system settings are changed. The light and dark lockups are used on the
website and in package READMEs; icon exports omit lettering at small sizes.

The motif is the identifying cue in grayscale and under colour-vision changes.
All exports are reproducible vectors; package names retain their exact case.
