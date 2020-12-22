
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rmdprotectr

<!-- badges: start -->

<!-- badges: end -->

The goal of `rmdprotectr` is to password protect Rmarkdown HTML
documents.

> This package implements **very simple** password protection for static
> pages or whole websites with no server configuration required: you can
> use Dropbox, Amazon S3 or any generic hosting service to host a
> private, password protected site.

## Installation

You can install the development version of rmdprotectr from
[GitHub](https://github.com/favstats/rmdprotectr) with:

``` r
remotes::install_github("rmdprotectr")
```

## How to Use

``` r
library(rmdprotectr)

## provide a path to your .Rmd and it will be rendered in the correct folder
protect_rmd("test.Rmd", pw = "somefancypw")

## make sure your Rmd is set to knit to HTML!
```

You are set\! Push to private GitHub repo and activate GitHub pages or
whatever hosting service you prefer\!

## Example

Check this [repo](https://favstats.github.io/pwtest/) for an example
implementation.

You can also test the password protection by going to this [GitHub pages
site](https://favstats.github.io/pwtest/). The password is: `password`.

## Things to consider

  - If your hosting service offers directory listing, a visitor can
    bypass the protection.
  - there’s no protection against brute force attack. Pick a very long
    and hard to guess password.
  - Pasting the link directly to someone will bypass the login

## Other ways to password protect your Rmd

If you are looking for a more secure encryption method for your
Rmarkdowns you may want to take a look at
[{encryptRmd}](https://github.com/dirkschumacher/encryptedRmd/).

You can also check out this [StackOverflow
question](https://stackoverflow.com/questions/61379250/bookdown-password-protect-a-single-page-chapter-in-html)
for an interesting solution that works with Rmarkdown parameters. You
can also probably expand this to work with query parameters which is at
least as secure as the solution presented here (not a whole lot).

## Credit

Credit to
[matteobrusa](https://github.com/matteobrusa/Password-protection-for-static-pages)
and
[scottishstoater](https://github.com/scottishstoater/protected-github-pages)
for coding most of the custom HTML required for this to work.
