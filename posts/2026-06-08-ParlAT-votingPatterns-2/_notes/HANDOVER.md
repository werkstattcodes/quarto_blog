# Handover: split-vote integration (open work)

Written 2026-08-30 to carry this work between machines. `_notes/` is underscore-prefixed so
Quarto ignores it at render, like `_data/` and `_img/`.

**Start here, then read `split-votes-plan.md` in this directory.**

## Where the post stands

Committed and rendering cleanly on branch `voting-patterns-all_stages`. Scope: Regierungsvorlagen
(`item = "RV"`) only, GP XX–XXVIII, 2,710 bills. Headline: **3.90%** of comparable party–bill pairs
change position between committee and 2nd reading vs **0.31%** between 2nd and 3rd; **92.2%** of all
movement sits at the first transition; moves toward support outnumber withdrawals 2.3:1; 359 of 360
changes in the three-stage subset come from opposition parties.

## The open question

The analysis uses only votes whose text matches `Dafür: <parties>, dagegen: <parties>`. That
excludes 962 non-regular rows — `getrennte Abstimmung` (bill voted in parts), `wechselnde Mehrheit`
(different majorities per part), `mehrstimmig`, `namentliche Abstimmung`.

Those are disproportionately **contested** bills, so the post excludes exactly the cases where
position change is most likely and then measures position change. That is the strongest objection to
its central claim, and it is currently handled by a stated limitation rather than by including them.

## What was investigated

Notation forms across the non-regular rows: `dafür: S, V` supporters-only **56%**; no party names
**34%**; both lists **5.5%**; case-encoded `SVfLg` (upper = for, lower = against) **4.7%**.

A prototype parser (`split-vote-parser-PROTOTYPE.R`) handles case-encoding, `bzw.` / `teils … teils`
part separators, implicit unanimous parts, and `tlw.` within-party markers. It parses 701 of 989
rows, but **134 of those (19.1%) still contain unrecognised tokens** and it has known bugs — see the
plan.

## Corrections from an independent review — read these before trusting anything above

1. **The validation was circular.** A test claiming the "unnamed party = voted against" rule was
   "99.59% correct" proved nothing: it built the supporter set from the truth it was predicting, so
   agreement was a tautology and the failure branch was unreachable. **Never cite that number.**
   Salvageable instead: a **0.41%** record-omission rate and a **5.9%** explicit-absence rate
   (`nicht anwesend` etc.), both defensible as stated.

2. **Treating Split as a position destroys the finding, by artefact.** Non-regular share by stage is
   committee 11.1%, second reading **22.9%**, third **3.1%** — bills are split at second reading and
   voted whole at third. So a party recorded as Split at second *must* resolve at third, which is a
   change of voting object, not of mind. Full integration moves 2nd→3rd from 0.31% to **6.52%** and
   collapses the 92.2% headline to **59.9%**.

3. **Do not publish "27% of positions were Split-across-bill."** It is largely manufactured by the
   `teils einstimmig → __ALL__` heuristic, which fires on 210 of 692 parseable rows and labels every
   party absent from the `dafür` list as "split" whenever a bill contained any unanimous clause.

4. **A "skewed stage gain" objection raised earlier was wrong.** Pairs gained are near-symmetric:
   +2,301 committee→2nd, +2,397 2nd→3rd.

## The recommended path

Treat **Split as missing data on the outcome, not as a position** — if a party voted differently on
different parts, its position on the bill is undefined. Keep only positions identical across every
part; drop the rest. Sensitivity ladder, already computed:

| variant | c→2nd | 2nd→3rd | share at 1st |
|---|---|---|---|
| 0 published baseline | 3.90% (n=9,082) | 0.31% (n=9,695) | 92.2% |
| 1 Split as a category | 10.37% | 6.52% | 59.9% |
| 2 Split as missing | 4.85% | 0.43% | 91.3% |
| **3 as 2, zero-junk rows only** | **4.18% (n=10,303)** | **0.40% (n=11,138)** | **90.7%** |
| 4 as 3, absence-marked dropped | 4.00% | 0.40% | 90.3% |
| 5 as 3, no silence inference | 3.95% | 0.31% | 92.1% |

Publish **variant 3** as the augmented result with the others as sensitivity, and variant 1
explicitly labelled as an artefact demonstrating why split votes cannot be coerced into a position.
The point this makes: admitting contested bills raises the flip rate by about a quarter in relative
terms and **leaves the conclusion intact**.

## Reference: how ParlAT vote texts encode positions

(Also saved as a local Claude memory on the original machine, which does **not** sync — this is the
travelling copy.)

- `dafür: S, V` — supporters only. Unlisted parties are implicitly against, but see the absence
  caveat above.
- Case-encoded `SVfLg` — **upper case = for, lower case = against**, one letter per party.
- `bzw.` and `teils … teils` separate the party sets for **different parts of the bill**. A party in
  some part-sets but not others has no single position.
- `tlw.` / `teilw.` / `teilweise` marks a split **within** a party (individual MPs) — a different
  phenomenon from a party split across provisions.
- `namentliche Abstimmung` carries only aggregate counts (`abgegebene Stimmen: 137, davon
  Ja-Stimmen: 109`); per-MP records live elsewhere in the Parliament's data.
- **Presence must be a seating window** (first-to-last vote date per party per period), never
  "appears anywhere in the period" — otherwise STRONACH is recorded as opposing ~993 bills it was
  not in parliament for (it formed mid-GP XXIV; same issue for BZÖ/FPÖ in GP XXII).
- **Category labels are partly a grammar artefact**: the classifier matches the literal
  `wechselnde Mehrheit`, so `wechselnde Mehrheiten` matches but `mit wechselnden Mehrheiten`
  (dative) does not and falls through to `getrennte Abstimmung`.
- The regular/non-regular boundary is not clean: 18 `getrennte Abstimmung` and 8
  `wechselnde Mehrheit` rows already classify as `regular` because they carry a `, dagegen:`
  separator, and are read as whole-bill positions.
- Compute parse statistics on `items_scope_pairs` (962 non-regular rows), not `items_scope` (989) —
  the analysis runs on the de-duplicated table.

## Also worth knowing

- `ggtext`/`gridtext` renders a small tag set only. **Markdown backticks become `<code>` and abort
  the render** — use quotes in plot subtitles.
- `_data/*.rds` is gitignored, so the caches do **not** travel. On a new machine, regenerate them by
  flipping the `eval: false` fetch chunks (`fetch-items`, `fetch-item-details`, `fetch-aub-details`)
  to `true` once and rendering; each writes its own cache inline. The RV fetch is quick; item
  details took ~5 minutes for 2,710 bills at 4 workers.
