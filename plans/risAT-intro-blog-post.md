# risAT Intro Blog Post Plan

<!--
Editing note:

Add comments directly below the section or bullet you want to change.
Recommended styles:

[RS: your comment here]

or

HTML comment style, written as a normal Markdown comment in the body:
RS: your comment here

You can also replace text directly if you already know the preferred wording.
-->

## Summary

Revise `posts/2026-06-30-risAT-intro/index.qmd` into an introductory post for `{risAT}` that matches the blog's existing package-post style: conversational framing, practical code examples, visible caveats, folded code, and a mix of package overview plus substantive data angles.

The post will present `{risAT}` as a tidyverse-friendly wrapper for the Austrian RIS OGD REST API v2.6, focused on the `/Judikatur` case-law endpoint. Sources checked: [risAT docs](https://werkstattcodes.github.io/risAT/) and [GitHub README](https://github.com/werkstattcodes/risAT/).

## Key Changes

- Keep the current draft as the base, but tighten the intro and remove scattered test queries.
- Add a clearer "what this package helps with" framing for researchers, journalists, students, and political scientists. [RS: also members of the legal profession]
- Keep the wrapper overview table for `ris_search_case_law()`, `ris_search_vfgh()`, `ris_search_vwgh()`, `ris_search_bvwg()`, `ris_search_dsk()`, and `ris_search_gbk()`.
- Add an explicit caveat that `{risAT}` is experimental, version `0.0.0.9000`, not affiliated with RIS/BKA, and result counts reflect RIS search behavior.

## Examples

- Use three compact human-rights/political-science mini-cases:
  - Asylum and protection: BVwG/VwGH searches around `Asyl`, `AsylG 2005`, or subsidiary protection, with a simple year-count plot.
  - Privacy and surveillance: DSK/DSB decisions using queries like `DSGVO`, `Videoüberwachung`, or `Auskunftsrecht`.
  - Equality and discrimination: GBK searches using `discrimination_ground`, e.g. gender, ethnicity, age, disability, or sexual orientation.
- For each example, show one query, a small `select()`/`count()` output, and one sentence on why the query matters substantively.
- Keep interpretation modest: the post introduces research possibilities, not a full legal/political-science analysis.

[RS: there should be also an example with for "Folter" and "Nonrefoulement"; and "Klimawandel". When makeing searches with the "query" attribute, these searches can be long since they are full text searches; delimit the date range of these searches.]
## Quarto/Code Details

- Use the existing YAML conventions: `date-modified`, `reference-location: margin`, folded code, `ragg_png`, categories including `Austria`, `legal`, and `risAT`.
- Use cached or `eval: false` chunks for API-heavy calls where needed, so the post remains reproducible without slow live requests on every render.
- Add `fig-cap` and `fig-alt` to the main plot.
- Use `content_urls` unnesting to demonstrate how readers can reach RIS HTML/PDF records.

## Test Plan

- Static check the `.qmd` for valid YAML, chunk labels, and no duplicate/leftover exploratory code.
- If local prerequisites are installed later, render the post with Quarto and confirm all evaluated chunks run.
- Current local limitation to account for: `risAT`, tidyverse packages, and `quarto` are not available on this machine, so implementation should either install prerequisites first or keep API chunks unevaluated until render verification is possible.

## Assumptions

- Default angle: "intro plus examples," not a full standalone empirical analysis.
- Default structure: 5 mini-cases rather than one deep dive.
- The existing draft path remains the target post.
- each example should also have either a good graph or a nice reactable. 
