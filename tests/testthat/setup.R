# tests/testthat/setup.R

# first the packages
# formerly in helper-lib.R
library(R2jags)
library(runjags)

testdata_dir <- testthat::test_path("../testdata")
dir.create(testdata_dir, showWarnings = FALSE, recursive = TRUE)

# Make TESTDATA_DIR available to all scripts AND tests
assign("TESTDATA_DIR", testdata_dir, envir = .GlobalEnv)

# Stan-based models (rstan, rstanarm, brms) are slow to compile and depend on
# the C++ toolchain, so only fit them off CRAN. Matches testthat::skip_on_cran().
RUN_STAN <- interactive() || identical(Sys.getenv("NOT_CRAN"), "true")
assign("RUN_STAN", RUN_STAN, envir = .GlobalEnv)

# Fit a Stan-based model, but warn instead of stopping the whole test run if it
# fails. Returns NULL on failure; tests that need the model then skip.
fit_or_warn <- function(label, expr) {
  n_sinks <- sink.number()
  tryCatch(
    expr,
    error = function(e) {
      # rstan can leave output/message sinks open when compilation fails
      while (sink.number() > n_sinks) sink()
      if (sink.number(type = "message") != 2L) sink(type = "message")
      
      err <- conditionMessage(e)
      pkg_version <- function(pkg) {
        tryCatch(as.character(utils::packageVersion(pkg)),
                 error = function(e) "not installed")
      }
      pkgs <- unique(c(label, "rstan", "StanHeaders"))
      versions <- paste0(
        "R ", getRversion(), ", ",
        paste(pkgs, vapply(pkgs, pkg_version, character(1)), collapse = ", ")
      )
      
      if (grepl("Syntax error|parsing error|Semantic error", err)) {
        advice <- paste0(
          "Stan could not parse the model code in BayesPostEst's test setup. ",
          "This is a problem in the test code, not your setup. Please report ",
          "it at https://github.com/ShanaScogin/BayesPostEst/issues ",
          "and include the versions listed below."
        )
      } else {
        to_update <- unique(c("rstan", "StanHeaders", label,
                              "RcppEigen", "BH", "RcppParallel"))
        install_cmd <- paste0("install.packages(c(",
                              paste0("\"", to_update, "\"", collapse = ", "),
                              "))")
        hidden <- if (grepl("invalid connection", err)) {
          paste0(
            "The error below ('invalid connection') is not the real problem: ",
            "the C++ compile failed, and rstan's cleanup then hid the ",
            "compiler's message. The last step below shows the real error.\n"
          )
        } else {
          ""
        }
        advice <- paste0(
          hidden,
          "This is usually a C++ toolchain problem, for example rstan or ",
          "StanHeaders being older than your compiler. Updating often fixes ",
          "it:\n",
          "  * Update R if you are not on a recent release. CRAN only builds ",
          "new package binaries for recent R versions, so an older R can be ",
          "stuck with old Stan packages.\n",
          "  * Then update the Stan packages: ", install_cmd, "\n",
          "  * Make sure your compiler tools match your R version (Rtools on ",
          "Windows, the Xcode command line tools on macOS).\n",
          "  * To test your Stan setup on its own and see the full compiler ",
          "output, run: rstan::stan_model(model_code = \"parameters { real y; } ",
          "model { y ~ normal(0, 1); }\", verbose = TRUE)"
        )
      }
      
      warning(
        "Could not fit the ", label, " test model, so the tests that use it ",
        "will be skipped. BayesPostEst itself does not compile Stan models, ",
        "so only these tests are affected.\n\n",
        advice, "\n\n",
        "Versions: ", versions, "\n",
        "Start of the original error: ", substr(err, 1, 300),
        call. = FALSE
      )
      NULL
    }
  )
}
assign("fit_or_warn", fit_or_warn, envir = .GlobalEnv)

# Source sim_data.R first
source(testthat::test_path("setup-data/sim_data.R"))

# Source the rest of the setup-data scripts
r_scripts <- list.files(
  testthat::test_path("setup-data"),
  pattern = "\\.R$",
  full.names = TRUE
)
r_scripts <- setdiff(r_scripts, testthat::test_path("setup-data/sim_data.R"))
lapply(r_scripts, source)