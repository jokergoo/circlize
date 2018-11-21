
The .R files generate the trace for the iteration report section in the help/manual.

They were run in OS X 10.10.3 with the following bash commands

export R_LIBS=../../../nleqslv.Rcheck
for k in *.R; do R CMD BATCH --no-timing $k ; done

in the directory <path to nleqslv package-source>/inst/iterationreport.

If they are run elsewhere you shouldn't need the export command.
If these are run in a different OS or with another version of R results may be marginally different.
If they are very different then I would like to be informed.

The iteration reports are excerpts from the .Rout files and slightly modified with
intercolumn some whitespace removed.
The actual reports may be extracted from the .Rout file with this bash command

for file in *.Rout ; do  gawk '/  Iter /, /^[$]x/  { if($0 != "$x" ) print }' $file > $(basename $file).txt ; done

Some editing will be needed before including the reports in the documentation.
