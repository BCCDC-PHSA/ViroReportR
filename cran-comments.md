## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
  
## Resubmission (1.0.4)

This is a patch release (1.0.4) addressing issues identified during CRAN checks on Debian.

### Changes

- Fixed report rendering to avoid writing to the installed package directory.
  The R Markdown template is now copied to a temporary directory before rendering.
- Ensured all logical constants use TRUE/FALSE instead of T/F.
- Improved rendering environment isolation to avoid dependency on the global environment.
- Minor robustness improvements in examples.

### Testing

All checks pass locally with:

    devtools::check(cran = TRUE)

No ERRORs, WARNINGs, or significant NOTEs remain.

Thank you for your review.
