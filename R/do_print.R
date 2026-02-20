#' Print dropout / test results in a compact, report-ready string
#'
#' @param x An object of class do_stats (from compute_stats) or do_chisq (from do_chisq).
#' @param ... Additional arguments passed to methods.
#' @param as_markdown Boolean. Should the output be formatted for a Markdown document (e.g. Quarto)? Defaults to FALSE.
#' @param print Boolean. Should the output be formatted for printing (e.g. to store in a character vector)? Defaults to FALSE.
#' @returns Summary of the input object as either a string, markdown-ready or console output.
#' @export
#' 
#' @examples
#' do_stats <- compute_stats(df = add_dropout_idx(dropRdemo, 3:54),
#' by_cond = "experimental_condition",
#' no_of_vars = 52)
#' 
#' do_print(do_stats)
#' 

do_print <- function(x, ..., as_markdown = FALSE, print = FALSE) {
  UseMethod("do_print")
}


# ---- internal helpers (keep these unexported) ----

.do_fmt_pct <- function(p, digits = 1) {
  ifelse(is.na(p), "NA", paste0(formatC(100 * p, format = "f", digits = digits), "%"))
}
.do_fmt_num <- function(x, digits = 2) {
  ifelse(is.na(x), "NA", formatC(x, format = "f", digits = digits))
}

.do_md_escape <- function(s) {
  # minimal: escape underscores so they don't italicize in markdown
  gsub("_", "\\\\_", s, fixed = TRUE)
}

.do_md_bold <- function(label) paste0("**", label, "**")



.do_finalize <- function(out, print = FALSE) {
  if (isTRUE(print)) cat(out, "\n")
  out
}

# ---- do_stats method ----
# expects compute_stats() output columns:
# q_idx, condition, pct_remain (and others)
#' @rdname do_print
#' @param x An object of class do_stats (from `compute_stats`).
#'
#' @param as_markdown Boolean. Should the output be formatted for a Markdown document (e.g. Quarto)? Defaults to FALSE.
#' @param item Numeric item index. Default NULL = last item for do_stats.
#' @param conditions Optional vector of conditions to include (ignored for totals unless you include them).
#' @param digits_pct Digits for percentages.
#' @param ... Additional arguments
#'
#' @export
do_print.do_stats <- function(x,
                              as_markdown = FALSE,
                              item = NULL,
                              conditions = NULL,
                              digits_pct = 1,
                              ...) {
  
  # pick default item = last observed q_idx
  if (is.null(item)) item <- max(x$q_idx, na.rm = TRUE)
  
  df <- x[x$q_idx == item, , drop = FALSE]
  if (!is.null(conditions)) df <- df[df$condition %in% conditions, , drop = FALSE]
  
  if (nrow(df) == 0) {
    return(sprintf("No dropout stats found for item %s.", item))
  }
  
  # dropout proportion up to item = 1 - pct_remain
  df$dropout_pct <- 1 - df$pct_remain
  
  # If multiple conditions, report each. If single row, just one number.
  # if (length(unique(df$condition)) > 1) {
  #   parts <- paste0(df$condition, "=", .do_fmt_pct(df$dropout_pct, digits_pct))
  #   sprintf("dropout up to item %s: %s.", item, paste(parts, collapse = ", "))
  # } else {
  #   sprintf("dropout up to item %s: %s.",
  #           item, .do_fmt_pct(df$dropout_pct[1], digits_pct))
  # }
  
  if (length(unique(df$condition)) > 1) {
    parts <- paste0(df$condition, "=", .do_fmt_pct(df$dropout_pct, digits_pct))
    parts <- paste(parts, collapse = ", ")
    out <- sprintf("dropout up to item %s: %s.", item, parts)
  } else {
    out <- sprintf("dropout up to item %s: %s.", item, .do_fmt_pct(df$dropout_pct[1], digits_pct))
  }
  
  if (isTRUE(as_markdown)) {
    out <- sprintf("%s up to item %s: %s",
                   .do_md_bold("dropout"),
                   item,
                   sub("^dropout at item [0-9]+:\\s*", "", out)) # keep it simple
    out <- .do_md_escape(out)
  }
  
  .do_finalize(out, print)
}

# ---- do_chisq method ----
# expects do_chisq() output:
# list(
#   "Test result" = <htest>,
#   "Dropout at item k" = <table/matrix/data.frame with dropout & remaining>
# )
#' @rdname do_print
#' @param x An object of class do_chisq (from `do_chisq`).
#'
#' @param as_markdown Boolean. Should the output be formatted for a Markdown document (e.g. Quarto)? Defaults to FALSE.
#' @param digits_pct Digits for percentages.
#' @param digits_stat Digits for test statistic.
#' @param ... Additional arguments.
#'
#' @importFrom stats xtabs
#' @export
do_print.do_chi <- function(x,
                            as_markdown = FALSE,
                            digits_pct = 1,
                            digits_stat = 2,
                            ...) {
  
  # Resolve global varaible warnings
  participants <- Freq <- NULL
  
  # 1) grab htest
  tst <- x[["Test result"]]
  if (is.null(tst) || !inherits(tst, "htest")) {
    return("do_print.do_chisq: couldn't find a valid 'Test result' htest element.")
  }
  
  # 2) find the dropout table element (name starts with "Dropout up to item")
  tab_name <- names(x)[grepl("^Dropout up to item", names(x))]
  if (length(tab_name) == 0) {
    return("do_print.do_chisq: couldn't find a 'Dropout up to item ...' element.")
  }
  tab <- x[[tab_name[1]]]
  
  # tab is shown in the docs as:
  #            participants
  # conditions dropout remaining
  # ...
  # We'll try to be robust to data.frame/matrix.
  tab_df <- as.data.frame(tab, stringsAsFactors = FALSE)
  
  # If the table came in with rownames as conditions, preserve them:
  if (!"conditions" %in% names(tab_df) && !is.null(rownames(tab_df))) {
    tab_df$conditions <- rownames(tab_df)
  }
  
  # tab_wide <- tidyr::pivot_wider(tab_df, names_from = participants, values_from = Freq)
  tab_wide <- as.data.frame.matrix(
    xtabs(Freq ~ conditions + participants, data = tab_df)
  )
  
  tab_wide$dropout_pct <- with(tab_wide, dropout / (dropout + remaining))
  # pct_str <- paste0(tab_wide$conditions, "=", .do_fmt_pct(tab_wide$dropout_pct, digits_pct))
  pct_str <- paste0(rownames(tab_wide), "=", .do_fmt_pct(tab_wide$dropout_pct, digits_pct))
  pct_str <- paste(pct_str, collapse = ", ")
  
  # 3) format chi-square line
  chi <- unname(tst$statistic)
  degf  <- unname(tst$parameter)  # may be NA for simulated p
  p   <- tst$p.value
  
  p_str <- if (is.na(p)) "NA" else if (p < .001) "< .001" else .do_fmt_num(p, digits = 3)
  
  # extract item number from the name if possible
  item <- sub("^Dropout up to item\\s*", "", tab_name[1])
  
  # handle degf NA gracefully
  degf_str <- if (is.na(degf)) "df NA" else as.character(degf)
  
  # sprintf("item %s: χ²(%s) = %s, p %s; dropout: %s.",
  #         item, degf_str, .do_fmt_num(chi, digits_stat),
  #         if (startsWith(p_str, "<")) p_str else paste0("= ", p_str),
  #         pct_str)
  
  if (!isTRUE(as_markdown)) {
    # out <- sprintf("item %s: X²(%s) = %s, p %s; dropout: %s.", \u03C7\u00B2
    out <- sprintf("item %s: X^2(%s) = %s, p %s; dropout: %s.",
                   item, degf_str, .do_fmt_num(chi, digits_stat),
                   if (startsWith(p_str, "<")) p_str else paste0("= ", p_str),
                   pct_str)
  } else {
    # Quarto/Markdown friendly (inline math + italic p)
    chi_part <- sprintf("$\\chi^2$(%s) = %s", degf_str, .do_fmt_num(chi, digits_stat))
    p_part   <- if (startsWith(p_str, "<")) sprintf("*p* %s", p_str) else sprintf("*p* = %s", p_str)
    
    out <- sprintf("%s %s; %s: %s",
                   chi_part, p_part,
                   .do_md_bold("dropout"),
                   pct_str)
    
    out <- .do_md_escape(out)
  }
  
  .do_finalize(out, print)
}

#' @export
do_print.default <- function(x, ..., as_markdown = FALSE, print = FALSE) {
  
  cls_str <- paste(class(x), collapse = ", ")

  out <- sprintf(
    paste0(
      "do_print() does not know how to handle objects of class: %s.",
      "Supported inputs:",
      " - compute_stats() output (class 'do_stats')\n",
      " - do_chisq() output (class 'do_chisq')"
    ),
    cls_str
  )
  .do_finalize(out, print)
  
}

