# This file contains all the code needed to parse and print various sections of your CV
# from data. Feel free to tweak it as you desire!

#' Build interactive network logo
#'
#' Constructs a network based on your position data to be used as a logo.
#' Interactive in HTML version and static in the PDF version. Notes are entries,
#' colored by section and connected if they occurred in the same year
#'
#' @param position_data position data from your `CV_Printer` class.
#'
#' @return Interactive force-directed layout network of your CV data
#' @export
build_network_logo <- function(position_data){
  
  positions <- position_data %>%
    dplyr::mutate(
      id = dplyr::row_number(),
      title = stringr::str_remove_all(title, '(\\(.+?\\))|(\\[)|(\\])'),
      section = stringr::str_replace_all(section, "_", " ") %>% stringr::str_to_title()
    )
  
  combination_indices <- function(n){
    rep_counts <- (n:1) - 1
    dplyr::tibble(
      a = rep(1:n, times = rep_counts),
      b = purrr::flatten_int( purrr::map(rep_counts, ~{tail(1:n, .x)}) )
    )
  }
  current_year <- lubridate::year(lubridate::ymd(Sys.Date()))
  edges <- positions %>%
    dplyr::select(id, start_year, end_year) %>%
    dplyr::mutate(
      end_year = ifelse(end_year > current_year, current_year, end_year),
      start_year = ifelse(start_year > current_year, current_year, start_year)
    ) %>%
    purrr::pmap_dfr(function(id, start_year, end_year){
      dplyr::tibble(
        year = start_year:end_year,
        id = id
      )
    }) %>%
    dplyr::group_by(year) %>%
    tidyr::nest() %>%
    dplyr::rename(ids_for_year = data) %>%
    purrr::pmap_dfr(function(year, ids_for_year){
      combination_indices(nrow(ids_for_year)) %>%
        dplyr::transmute(
          year = year,
          source = ids_for_year$id[a],
          target = ids_for_year$id[b]
        )
    })
  
  network_data <- list(nodes = dplyr::select(positions, -in_resume,-timeline),
                       edges = edges) %>%
    jsonlite::toJSON()
  
  viz_script <- readr::read_file(system.file("js/cv_network.js", package = "datadrivencv"))
  
  glue::glue(
    "<script id = \"data_for_network\" type = \"application/json\">",
    "{network_data}",
    "</script>",
    "<script src=\"https://cdnjs.cloudflare.com/ajax/libs/d3/5.16.0/d3.min.js\"></script>",
    "<svg style = \"width: 100%; height:320px; margin-top: -125px;\" id = \"cv_network_viz\"></svg>",
    "<script>",
    "{viz_script}",
    "</script>"
  )
  
}

#' Create a CV_Printer object.
#'
#' @param data_location Path of the spreadsheets holding all your data. This can be
#'   either a URL to a google sheet with multiple sheets containing the four
#'   data types or a path to a folder containing four `.csv`s with the neccesary
#'   data.
#' @param source_location Where is the code to build your CV hosted?
#' @param pdf_mode Is the output being rendered into a pdf? Aka do links need
#'   to be stripped?
#' @param sheet_is_publicly_readable If you're using google sheets for data,
#'   is the sheet publicly available? (Makes authorization easier.)
#' @return A new `CV_Printer` object.
create_CV_object <-  function(data_location,
                              pdf_mode = FALSE,
                              sheet_is_publicly_readable = TRUE) {

  cv <- list(
    pdf_mode = pdf_mode,
    links = c()
  )

  is_google_sheets_location <- stringr::str_detect(data_location, "docs\\.google\\.com")

  if(is_google_sheets_location){
    if(sheet_is_publicly_readable){
      # This tells google sheets to not try and authenticate. Note that this will only
      # work if your sheet has sharing set to "anyone with link can view"
      googlesheets4::gs4_deauth()
    } else {
      # My info is in a public sheet so there's no need to do authentication but if you want
      # to use a private sheet, then this is the way you need to do it.
      # designate project-specific cache so we can render Rmd without problems
      options(gargle_oauth_cache = ".secrets")
    }

    read_gsheet <- function(sheet_id){
      googlesheets4::read_sheet(data_location, sheet = sheet_id, skip = 1, col_types = "c")
    }
    cv$entries_data  <- read_gsheet(sheet_id = "entries")
    cv$skills        <- read_gsheet(sheet_id = "it_skills")
    cv$languages     <- read_gsheet(sheet_id = "language_skills")
    cv$text_blocks   <- read_gsheet(sheet_id = "text_blocks")
    cv$contact_info  <- read_gsheet(sheet_id = "contact_info")
  } else {
    # Want to go old-school with csvs?
    cv$entries_data <- readr::read_csv(paste0(data_location, "entries.csv"), skip = 1)
    cv$skills       <- readr::read_csv(paste0(data_location, "it_skills.csv"), skip = 1)
    cv$languages    <- readr::read_csv(paste0(data_location, "language_skills.csv"), skip = 1)
    cv$text_blocks  <- readr::read_csv(paste0(data_location, "text_blocks.csv"), skip = 1)
    cv$contact_info <- readr::read_csv(paste0(data_location, "contact_info.csv"), skip = 1)
  }


  extract_year <- function(dates){
    date_year <- stringr::str_extract(dates, "(20|19)[0-9]{2}")
    date_year[is.na(date_year)] <- lubridate::year(lubridate::ymd(Sys.Date())) + 10

    date_year
  }

  parse_dates <- function(dates){

    date_month <- stringr::str_extract(dates, "(\\w+|\\d+)(?=(\\s|\\/|-)(20|19)[0-9]{2})")
    date_month[is.na(date_month)] <- "1"

    paste("1", date_month, extract_year(dates), sep = "-") %>%
      lubridate::dmy()
  }

  # "current"/"present" (any case) are displayed as "Present"
  format_date_label <- function(x){
    ifelse(tolower(x) %in% c("current", "present", "now"), "Present", as.character(x))
  }

  # Clean up entries dataframe to format we need it for printing
  cv$entries_data %<>%
    tidyr::unite(
      tidyr::starts_with('description'),
      col = "description_bullets",
      sep = "\n- ",
      na.rm = TRUE
    ) %>%
    dplyr::mutate(
      description_bullets = ifelse(description_bullets != "", paste0("- ", description_bullets), ""),
      start = ifelse(start == "NULL", NA, start),
      end = ifelse(end == "NULL", NA, end),
      start_year = extract_year(start),
      end_year = extract_year(end),
      no_start = is.na(start),
      has_start = !no_start,
      no_end = is.na(end),
      has_end = !no_end,
      timeline = dplyr::case_when(
        no_start  & no_end  ~ "",
        no_start  & has_end ~ format_date_label(end),
        has_start & no_end  ~ paste(start, "\u2013", "Present"),
        start == end        ~ as.character(start),
        TRUE                ~ paste(start, "\u2013", format_date_label(end))
      )
    ) %>%
    dplyr::arrange(desc(parse_dates(end))) %>%
    dplyr::mutate_all(~ ifelse(is.na(.), 'N/A', .)) %>% 
    dplyr::filter(as.logical(in_resume)) # ADDED BY PIERRE: only include entries with TRUE under in_resume column
  cv
}


# Remove links from a text block and add to internal list
sanitize_links <- function(cv, text){
  if(cv$pdf_mode){
    link_titles <- stringr::str_extract_all(text, '(?<=\\[).+?(?=\\])')[[1]]
    link_destinations <- stringr::str_extract_all(text, '(?<=\\().+?(?=\\))')[[1]]

    n_links <- length(cv$links)
    n_new_links <- length(link_titles)

    if(n_new_links > 0){
      # add links to links array
      cv$links <- c(cv$links, link_destinations)

      # Build map of link destination to superscript
      link_superscript_mappings <- purrr::set_names(
        paste0("<sup>", (1:n_new_links) + n_links, "</sup>"),
        paste0("(", link_destinations, ")")
      )

      # Replace the link destination and remove square brackets for title
      text <- text %>%
        stringr::str_replace_all(stringr::fixed(link_superscript_mappings)) %>%
        stringr::str_replace_all('\\[(.+?)\\]', "\\1")
    }
  }

  list(cv = cv, text = text)
}


#' @description Take a position data frame and the section id desired and prints the section to markdown.
#' @param section_id ID of the entries section to be printed as encoded by the `section` column of the `entries` table
print_section <- function(cv, section_id, glue_template = "default"){

  if(glue_template == "default"){
    glue_template <- "
### {title}

{loc}

{institution}

{timeline}

{description_bullets}
\n\n\n"
  }

  section_data <- dplyr::filter(cv$entries_data, section == section_id)

  # Take entire entries data frame and removes the links in descending order
  # so links for the same position are right next to each other in number.
  for(i in 1:nrow(section_data)){
    for(col in c('title', 'description_bullets')){
      strip_res <- sanitize_links(cv, section_data[i, col])
      section_data[i, col] <- strip_res$text
      cv <- strip_res$cv
    }
  }

  print(glue::glue_data(section_data, glue_template))

  invisible(strip_res$cv)
}



#' @description Prints out text block identified by a given label.
#' @param label ID of the text block to print as encoded in `label` column of `text_blocks` table.
print_text_block <- function(cv, label){
  text_block <- dplyr::filter(cv$text_blocks, loc == label) %>%
    dplyr::pull(text)

  strip_res <- sanitize_links(cv, text_block)

  cat(strip_res$text)

  invisible(strip_res$cv)
}



#' @description Construct a bar chart of skills
#' @param out_of The relative maximum for skills. Used to set what a fully filled in skill bar is.
print_skill_bars <- function(cv, out_of = 5, bar_color = "#969696", bar_background = "#d9d9d9", glue_template = "default"){

  if(glue_template == "default"){
    glue_template <- "
<div
  class = 'skill-bar'
  style = \"background:linear-gradient(to right,
                                      {bar_color} {width_percent}%,
                                      {bar_background} {width_percent}% 100%)\"
>{skill}</div>"
  }
  cv$skills %>%
    dplyr::mutate(width_percent = round(100*as.numeric(level)/out_of)) %>%
    glue::glue_data(glue_template) %>%
    print()

  invisible(cv)
}



#' @description List of all links in document labeled by their superscript integer.
print_links <- function(cv) {
  n_links <- length(cv$links)
  if (n_links > 0) {
    cat("
Links {data-icon=link}
--------------------------------------------------------------------------------

<br>


")

    purrr::walk2(cv$links, 1:n_links, function(link, index) {
      print(glue::glue('{index}. {link}'))
    })
  }

  invisible(cv)
}



#' @description Contact information section with icons
print_contact_info <- function(cv){
  glue::glue_data(
    cv$contact_info,
    "- <i class='fa fa-{icon}'></i> {contact}"
  ) %>% print()

  invisible(cv)
}



# ------------------------------------------------------------------------------
# Europass-inspired layout
# Everything below prints raw HTML (no blank lines inside a block so pandoc
# leaves it alone). Styling lives in dd_cv.css.
# ------------------------------------------------------------------------------

#' Escape HTML and convert markdown links / bold to HTML
md_inline <- function(x){
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\\[([^\\]]+)\\]\\(([^)]+)\\)", "<a href=\"\\2\">\\1</a>", x, perl = TRUE)
  x <- gsub("\\*\\*([^*]+)\\*\\*", "<strong>\\1</strong>", x, perl = TRUE)
  x
}

# TRUE for NA / "" / "N/A"
is_blank <- function(x) is.na(x) | trimws(x) == "" | x == "N/A"

# Turn the "- a\n- b" bullet string built in create_CV_object() into a vector
split_bullets <- function(x){
  if (is_blank(x)) return(character(0))
  b <- strsplit(sub("^- ", "", x), "\n- ", fixed = TRUE)[[1]]
  b[nzchar(trimws(b))]
}

europass_entry <- function(dates, body_html){
  paste0("<div class='ep-entry'><div class='ep-dates'>", dates, "</div>",
         "<div class='ep-body'>", body_html, "</div></div>")
}

#' One item of the contact line. HTML: the sheet's link as is. PDF (printing): the email
#' spelled out and URLs shortened (no protocol / www / trailing slash / .html), still clickable.
contact_item <- function(x, pdf_mode){
  pat <- "\\[([^\\]]+)\\]\\(([^)]+)\\)"
  if (!pdf_mode || !grepl(pat, x, perl = TRUE)) return(md_inline(x))
  url <- sub(paste0("^.*", pat, ".*$"), "\\2", x, perl = TRUE)
  shown <- if (grepl("^mailto:", url)) {
    sub("^mailto:", "", url)
  } else {
    url %>% sub("^https?://(www\\.)?", "", .) %>% sub("/$", "", .) %>% sub("\\.html?$", "", .)
  }
  paste0("<a href=\"", gsub("&", "&amp;", url, fixed = TRUE), "\">", gsub("&", "&amp;", shown, fixed = TRUE), "</a>")
}

#' Escape a string for use inside a CSS "..." string
css_string <- function(x) gsub("\"", "\\\"", gsub("\\", "\\\\", x, fixed = TRUE), fixed = TRUE)

#' Escape a string for use inside a JavaScript "..." string
js_string <- function(x) gsub("<", "\\u003c", css_string(x), fixed = TRUE)

#' Title, subtitle (rows `title` and `subtitle` of the text_blocks sheet) and contact line.
#' Also sets the page footer text from the title.
print_europass_header <- function(cv){
  get_block <- function(label){
    v <- dplyr::filter(cv$text_blocks, loc == label) %>% dplyr::pull(text)
    if (length(v) == 0 || is.na(v[1])) "" else v[1]
  }
  title    <- get_block("title")
  subtitle <- get_block("subtitle")

  contact <- paste0("<span>", vapply(cv$contact_info$contact, contact_item, character(1), pdf_mode = cv$pdf_mode), "</span>",
                    collapse = " <span class='ep-sep'>&bull;</span> ")
  cat(paste0(
    "<style>@page { @bottom-left { content: \"Curriculum vitae \u2013 ", css_string(title), "\"; } }</style>\n",
    # browser tab title (and PDF metadata title): "CV - <title>"
    "<script>document.title = \"CV - ", js_string(title), "\";</script>\n",
    "<header class='ep-header'>\n",
    "<div class='ep-name'>", md_inline(title), "</div>\n",
    if (nzchar(subtitle)) paste0("<div class='ep-headline'>", md_inline(subtitle), "</div>\n") else "",
    "<div class='ep-contact'>", contact, "</div>\n",
    "</header>\n\n"))
  invisible(cv)
}

#' PDF mode: turn <a href="url">label</a> into label<sup>n</sup> and record the url in cv$links
#' (the numbered list is printed by print_europass_links()). Same url = same number.
footnote_links <- function(cv, h){
  pat <- "<a href=\"([^\"]+)\">([^<]*)</a>"
  m <- gregexpr(pat, h, perl = TRUE)[[1]]
  if (m[1] == -1) return(list(cv = cv, html = h))
  for (k in regmatches(h, list(m))[[1]]) {
    url   <- sub(pat, "\\1", k, perl = TRUE)
    label <- sub(pat, "\\2", k, perl = TRUE)
    idx   <- match(url, cv$links)
    if (is.na(idx)) { cv$links <- c(cv$links, url); idx <- length(cv$links) }
    h <- sub(k, paste0(label, "<sup>", idx, "</sup>"), h, fixed = TRUE)
  }
  list(cv = cv, html = h)
}

#' Markdown -> HTML for a vector of strings; in PDF mode links become footnotes
inline_html <- function(cv, x){
  out <- character(length(x))
  for (i in seq_along(x)) {
    h <- md_inline(x[i])
    if (cv$pdf_mode) {
      res <- footnote_links(cv, h)
      cv <- res$cv
      h  <- res$html
    }
    out[i] <- h
  }
  list(cv = cv, html = out)
}

#' Free-text block (e.g. the "intro" row of the text_blocks sheet)
#' Use with `%<>%` so collected links are kept.
print_europass_text <- function(cv, label, heading){
  txt <- dplyr::filter(cv$text_blocks, loc == label) %>% dplyr::pull(text)
  res <- inline_html(cv, txt)
  cat(paste0("<section class='ep-section'>\n<h2>", heading, "</h2>\n<p class='ep-summary'>",
             res$html, "</p>\n</section>\n\n"))
  invisible(res$cv)
}

#' One section of the `entries` sheet (experience, education, ...)
#' Use with `%<>%` so collected links are kept.
print_europass_section <- function(cv, section_id, heading){
  d <- dplyr::filter(cv$entries_data, section == section_id)
  if (nrow(d) == 0) return(invisible(cv))

  entries <- character(nrow(d))
  for (i in seq_len(nrow(d))) {
    # In the sheet, `loc` holds the institution/organiser and `institution` the place
    sub_line <- c(d$loc[i], d$institution[i])
    sub_line <- sub_line[!is_blank(sub_line)]
    res <- inline_html(cv, sub_line); cv <- res$cv
    sub_line <- paste(res$html, collapse = " \u00b7 ")

    res <- inline_html(cv, d$title[i]); cv <- res$cv
    title_html <- res$html

    bullets <- split_bullets(d$description_bullets[i])
    res <- inline_html(cv, bullets); cv <- res$cv
    bullets <- res$html

    entries[i] <- europass_entry(
      d$timeline[i],
      paste0("<h3>", title_html, "</h3>",
             if (nzchar(sub_line)) paste0("<div class='ep-sub'>", sub_line, "</div>") else "",
             if (length(bullets) > 0) paste0("<ul>", paste0("<li>", bullets, "</li>", collapse = ""), "</ul>") else "")
    )
  }

  cat(paste0("<section class='ep-section'>\n<h2>", heading, "</h2>\n",
             paste(entries, collapse = "\n"), "\n</section>\n\n"))
  invisible(cv)
}

#' Numbered list of all links collected in PDF mode (matches the superscripts)
print_europass_links <- function(cv, heading = "Links"){
  if (length(cv$links) > 0) {
    items <- paste0("<li><a href=\"", cv$links, "\">", cv$links, "</a></li>", collapse = "")
    cat(paste0("<section class='ep-section'>\n<h2>", heading, "</h2>\n<ol class='ep-links'>",
               items, "</ol>\n</section>\n\n"))
  }
  invisible(cv)
}

#' Europass-style language grid from the `language_skills` sheet (language, level)
#' `level` is a CEFR level (A1-C2); the same level is shown for every skill column.
print_europass_languages <- function(cv, heading = "Language skills"){
  langs <- as.data.frame(cv$languages, stringsAsFactors = FALSE)

  rows <- vapply(seq_len(nrow(langs)), function(i){
    paste0("<tr><td class='lang'>", md_inline(langs$language[i]), "</td>",
           strrep(paste0("<td>", md_inline(langs$level[i]), "</td>"), 5),
           "</tr>")
  }, character(1))

  table_html <- paste0(
    "<table class='ep-langs'><thead>",
    "<tr><th></th><th>Listening</th><th>Reading</th><th>Spoken interaction</th><th>Spoken production</th><th>Writing</th></tr>",
    "</thead><tbody>", paste(rows, collapse = ""), "</tbody></table>")

  cat(paste0("<section class='ep-section'>\n<h2>", heading, "</h2>\n",
             europass_entry("Language(s)", table_html), "\n</section>\n\n"))
  invisible(cv)
}

#' IT skills from the `it_skills` sheet (skill, level), grouped by level in sheet order
#' `level` is used as the row label (e.g. "Proficient user", "Independent user", "Basic user").
print_europass_skills <- function(cv, heading = "IT skills"){
  sk <- cv$skills
  rows <- vapply(unique(sk$level), function(l){
    paste0("<div class='ep-skill-row'><div class='ep-skill-level'>", md_inline(l), "</div>",
           "<div class='ep-skill-list'>",
           paste(md_inline(sk$skill[sk$level == l]), collapse = " <span class='ep-dot'>&middot;</span> "),
           "</div></div>")
  }, character(1))
  cat(paste0("<section class='ep-section'>\n<h2>", heading, "</h2>\n",
             europass_entry("IT skills", paste(rows, collapse = "")),
             "\n</section>\n\n"))
  invisible(cv)
}
