# Integrate contested bills as unambiguous positions only, with a sensitivity ladder

## Context

The post excludes 962 non-regular vote rows (`getrennte Abstimmung`, `wechselnde Mehrheit`,
`mehrstimmig`, `namentliche Abstimmung`). Those are disproportionately contested bills — split and
voted in parts — so the analysis omits exactly the cases where position change is most likely and
then measures position change. That is selection on something correlated with the outcome, and it is
the strongest objection to the post's central claim.

An earlier plan proposed admitting these votes with **Split-across-bill** and **Split-within-party**
as additional position values. An independent review overturned that, and also overturned the
subsequent recommendation to abandon integration entirely. Both were wrong, for documented reasons:

- The validation that gated the work was **circular**. In `validate.R`, `supporters` derives from
  `filter(actual == "For")` on the same key `inferred` joins on, so `actual == inferred` is a
  tautology and the `WRONG` branch is unreachable. "99.59% correct" measured nothing. **This claim
  must not appear in the post.** The salvageable outputs are the 0.41% record-omission rate and the
  5.9% explicit-absence rate.
- The "skewed stage gain" objection was empirically false: pairs gained are near-symmetric
  (+2,301 committee→2nd, +2,397 2nd→3rd).
- The real threat is different. Non-regular share by stage is **committee 11.1%, second 22.9%,
  third 3.1%** — bills are split at second reading and voted whole at third. So a party recorded as
  Split at second reading *must* resolve to a single position at third, producing transitions that
  reflect a change of voting object, not a change of mind. Treating Split as a position takes
  2nd→3rd from 0.31% to 6.52% and collapses "share of movement at the first transition" from 92.2%
  to 59.9% — destroying the central finding by notational artefact.
- The 27% Split-across-bill figure is largely **manufactured** by the `teils einstimmig → __ALL__`
  heuristic, which fires on 210 of 692 parseable rows and labels every party absent from the `dafür`
  list as "split" whenever the bill contained any unanimous clause. Do not publish it.

## Approach

**Split is missing data on the outcome, not a position.** If a party voted differently on different
parts, its position on the bill is undefined and not comparable across stages. Keep only party-bill
positions that are identical across every part; drop the rest.

Target specification (**variant 3**): parser fixes + zero-unrecognised-token rows only + Split
treated as missing. Adds ~1,896 unambiguous positions (334 committee, 1,467 second, 141 third) and
~1,200–1,400 pairs per transition, giving **4.18% / 0.40%, 90.7% at the first transition** against a
3.90% / 0.31%, 92.2% baseline.

That is the result worth publishing: admitting the contested bills raises the committee→second rate
by roughly a quarter in relative terms and leaves the conclusion intact.

## Changes

All in `posts/2026-06-08-ParlAT-votingPatterns-2/index.qmd`. Parser prototype to port and fix:
`scratchpad/parser.R`.

### 1. Parser fixes (all confirmed by the review)

- **Alternation order**: `(tlw\.?|teilw\.?|teilweise)` strips "teilw" from "teilweise" leaving
  `eise` — ICU alternation is leftmost-first. Reorder to `teilweise|teilw\.?|tlw\.?`.
- **Newline anchor**: `dafür[:.]\s*(.+)$` returns `NA` whenever a newline follows the party list.
  Add `dotall`/`multiline` handling. Some of the 288 "unusable" rows are only this.
- **Multiple votes per text**: `str_match` takes the first `dafür:` and discards the rest.
- **Case-branch early return**: a text with both an `SVfLg` block and a `dafür:` list, a
  `teils einstimmig` clause, or an absence marker silently ignores everything but the block.
- **`einstimmig` with no `dafür` list**: 61 rows are discarded by the `NULL` return preceding the
  `einstimmig` check, though they are the easiest rows in the corpus (everyone For).

### 2. Admission rules

- Reject any row containing an unrecognised token. Contamination is **directionally biased toward
  finding flips** (`"V dagegen: F"` swallows a supporter, who is then inferred Against), so this
  must be a hard gate, asserted — a future notation variant must fail loudly, never become an
  "Against".
- Classify a party only where its position is identical across all parts; otherwise missing.
- Keep the `teils einstimmig → __ALL__` heuristic: under this design it only decides which rows are
  dropped, and it errs conservatively.
- Use seating windows (first-to-last vote date per party per period) for the supporters-only
  inference, as already implemented.

### 3. Presentation — parallel section, per the earlier decision

Keep the existing binary analysis intact. Add a section presenting the augmented result plus a
sensitivity ladder, so the reader sees how much the conclusion depends on the exclusions:

| variant | description |
|---|---|
| 0 | published baseline |
| 2 | Split as missing, all parseable rows |
| **3** | **as 2, zero-junk rows only — the headline augmented result** |
| 4 | as 3, absence-marked rows dropped |
| 5 | as 3, no silence inference at all |
| 1 | Split as a position — shown **labelled as an artefact**, to demonstrate why split votes cannot be coerced into a position |

Variant 1 earns its place precisely because it fails: it is the clearest way to show the reader that
the 2nd→3rd jump to 6.52% is a change of voting object, not of mind.

### 4. Corrections to existing prose

- Retire any "validated at 99.59%" framing. Replace with the 0.41% record-omission rate and the
  5.9% explicit-absence rate.
- Note in the methods that 18 `getrennte Abstimmung` and 8 `wechselnde Mehrheit` rows are already
  classified `regular` because they carry a `, dagegen:` separator — the regular/non-regular
  boundary is not clean, and those rows are currently read as whole-bill positions.
- Parse statistics must be computed on `items_scope_pairs` (962 non-regular rows), not `items_scope`
  (989) — the analysis runs on the de-duplicated table.

## Verification

1. Chunk syntax check (`scratchpad/syn.R`), then a clean `quarto render`.
2. **Assert the admission gate**: no unrecognised token may reach a position. Failing loudly is the
   entire point.
3. **Reproduce the ladder**: the rendered variants must match 3.90/0.31, 4.85/0.43, 4.18/0.40,
   4.00/0.40, 3.95/0.31, 10.37/6.52. Any divergence means the port differs from the prototype.
4. Assert the baseline is unchanged — the existing binary analysis must still produce 3.90% / 0.31%
   and 92.2%, proving the augmentation is additive.
5. Confirm the six existing figures are byte-identical.
