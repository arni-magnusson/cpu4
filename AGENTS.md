# Convert base R plots to ggplot format

This project is about modifying existing R scripts that are part of reproducible
workflows in TAF format.

1. The project repository is at https://github.com/arni-magnusson/cpu4 and the
   local working copy is at ~/git/arni-magnusson/cpu4.

2. The work requires running R with the current version of TAF and ggplot2 that
   are available on CRAN.

3. Read the taf-workflow and taf-ggplot skills from
   https://github.com/ices-taf-dev/ai.

4. Use consistent style between the ggplots and search R scripts inside the
   project repository for existing ggplot code. If it exists, it means it has
   passed a human review and you should use similar style attributes when
   developing other ggplots: color palette, text size, text alignment, spacing
   around titles, etc.

5. The developer will instruct you which workflow to focus on. The work inside
   each workflow involves two steps. In the first step, you convert the plotting
   code from base R graphics to ggplot, checking TAF workflow integrity and also
   checking whether the resulting PNG files look good. In the second step, you
   work with the developer on incremental tweaks to the format until the
   developer approved the final plot format.

6. The processor order should always be the chronological order: 3740, 10875,
   1165, 285. Newest processor at the top, oldest processor at the bottom.

7. Each workflow is isolated, so TAF scripts should not read or write in other
   workflows.
