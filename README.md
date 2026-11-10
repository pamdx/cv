# CV

This is where I generate my CV in HTML and PDF, adapted from Nick Strayer's great [data-driven CV](https://github.com/nstrayer/cv).

The layout is Europass-inspired (single column, dates on the left, blue section headings, CEFR language grid).

## Build

Run `render_cv.r` (R >= 4.2 with `pagedown`, `googlesheets4`, `dplyr`, `tidyr`, `purrr`, `stringr`, `lubridate`, `glue`, `magrittr`). It reads the Google Sheet and writes `cv.html` and `cv.pdf`.

## Data (Google Sheet)

- Tabs: `entries`, `text_blocks` (rows `title`, `subtitle` and `intro`), `contact_info`, `it_skills` (`skill`, `level` as text, e.g. "Proficient user"), `language_skills` (`language`, `level` as a CEFR level A1-C2).
- Entry `section` values used: `experience`, `education`, `publications`, `conferences`, `certifications`.
- In `entries`, the `loc` column holds the institution/organiser and `institution` the place (as in the original template output).
- Dates: `start` = earlier date, `end` = later date. An empty `end` with a `start` is shown as "Present"; `current` too.
