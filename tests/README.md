Tests and Coverage
================
05 January, 2025 22:28:15

- [Coverage](#coverage)
- [Unit Tests](#unit-tests)

This output is created by
[covrpage](https://github.com/yonicd/covrpage).

## Coverage

Coverage summary is created using the
[covr](https://github.com/r-lib/covr) package.

| Object                                              | Coverage (%) |
|:----------------------------------------------------|:------------:|
| RosyREDCap                                          |     5.71     |
| [R/app_config.R](../R/app_config.R)                 |     0.00     |
| [R/app_server.R](../R/app_server.R)                 |     0.00     |
| [R/app_settings.R](../R/app_settings.R)             |     0.00     |
| [R/app_ui.R](../R/app_ui.R)                         |     0.00     |
| [R/DB_diagram.R](../R/DB_diagram.R)                 |     0.00     |
| [R/DB_helpers.R](../R/DB_helpers.R)                 |     0.00     |
| [R/DB_summarize.R](../R/DB_summarize.R)             |     0.00     |
| [R/DB_to_and_from_dir.R](../R/DB_to_and_from_dir.R) |     0.00     |
| [R/DB_transform.R](../R/DB_transform.R)             |     0.00     |
| [R/DB_update.R](../R/DB_update.R)                   |     0.00     |
| [R/REDCap_API.R](../R/REDCap_API.R)                 |     0.00     |
| [R/REDCap_files.R](../R/REDCap_files.R)             |     0.00     |
| [R/REDCap_upload.R](../R/REDCap_upload.R)           |     0.00     |
| [R/utils.R](../R/utils.R)                           |     0.00     |
| [R/utils-RosyUtils.R](../R/utils-RosyUtils.R)       |     8.08     |
| [R/REDCap_tokens.R](../R/REDCap_tokens.R)           |    32.20     |
| [R/zzz.R](../R/zzz.R)                               |    33.33     |
| [R/projects.R](../R/projects.R)                     |    61.96     |
| [R/DB_setup.R](../R/DB_setup.R)                     |    77.03     |
| [R/cache.R](../R/cache.R)                           |    100.00    |

<br>

## Unit Tests

Unit Test summary is created using the
[testthat](https://github.com/r-lib/testthat) package.

| file | n | time | error | failed | skipped | warning | icon |
|:---|---:|---:|---:|---:|---:|---:|:---|
| [test-cache.R](testthat/test-cache.R) | 17 | 0.077 | 0 | 0 | 0 | 0 |  |
| [test-DB_setup.R](testthat/test-DB_setup.R) | 52 | 0.664 | 0 | 0 | 1 | 0 | 🔶 |
| [test-projects.R](testthat/test-projects.R) | 12 | 0.057 | 0 | 0 | 0 | 0 |  |
| [test-REDCap_tokens.R](testthat/test-REDCap_tokens.R) | 26 | 0.126 | 0 | 0 | 0 | 0 |  |

<details open>
<summary>
Show Detailed Test Results
</summary>

| file | context | test | status | n | time | icon |
|:---|:---|:---|:---|---:|---:|:---|
| [test-cache.R](testthat/test-cache.R#L2) | cache | hoardr cache exsists | PASS | 2 | 0.011 |  |
| [test-cache.R](testthat/test-cache.R#L16) | cache | fake_cache sets and clears | PASS | 7 | 0.026 |  |
| [test-cache.R](testthat/test-cache.R#L40) | cache | cache_projects_exists, cache_clear works | PASS | 8 | 0.040 |  |
| [test-DB_setup.R](testthat/test-DB_setup.R#L3) | DB_setup | test_dir works | PASS | 3 | 0.012 |  |
| [test-DB_setup.R](testthat/test-DB_setup.R#L12) | DB_setup | setup_DB creates a valid DB object and valid directory | PASS | 22 | 0.187 |  |
| [test-DB_setup.R](testthat/test-DB_setup.R#L61_L63) | DB_setup | works | SKIPPED | 1 | 0.003 | 🔶 |
| [test-DB_setup.R](testthat/test-DB_setup.R#L85) | DB_setup | save_DB doesn’t save if it’s blank but will save and cache if valid, also loads | PASS | 11 | 0.281 |  |
| [test-DB_setup.R](testthat/test-DB_setup.R#L128) | DB_setup | set_dir creates a new directory if it does not exist | PASS | 3 | 0.036 |  |
| [test-DB_setup.R](testthat/test-DB_setup.R#L136) | DB_setup | set_dir handles existing directory correctly | PASS | 3 | 0.034 |  |
| [test-DB_setup.R](testthat/test-DB_setup.R#L141) | DB_setup | set_dir throws an error for invalid directory path | PASS | 1 | 0.015 |  |
| [test-DB_setup.R](testthat/test-DB_setup.R#L148) | DB_setup | set_dir creates missing internal directories | PASS | 3 | 0.037 |  |
| [test-DB_setup.R](testthat/test-DB_setup.R#L157) | DB_setup | set_dir stops if user chooses not to create directory | PASS | 2 | 0.020 |  |
| [test-DB_setup.R](testthat/test-DB_setup.R#L167) | DB_setup | set_dir validates the directory structure | PASS | 3 | 0.039 |  |
| [test-projects.R](testthat/test-projects.R#L4) | projects | get_projects is df and has appropriate columns | PASS | 4 | 0.019 |  |
| [test-projects.R](testthat/test-projects.R#L26) | projects | check_folder_for_projects works | PASS | 8 | 0.038 |  |
| [test-REDCap_tokens.R](testthat/test-REDCap_tokens.R#L2) | REDCap_tokens | internal constants are correct | PASS | 5 | 0.022 |  |
| [test-REDCap_tokens.R](testthat/test-REDCap_tokens.R#L9) | REDCap_tokens | get_test_token works correctly | PASS | 7 | 0.051 |  |
| [test-REDCap_tokens.R](testthat/test-REDCap_tokens.R#L18) | REDCap_tokens | is_valid_REDCap_token respects the rules of 32L hexidecimal | PASS | 14 | 0.053 |  |

| Failed | Warning | Skipped |
|:-------|:--------|:--------|
| 🛑     | ⚠️      | 🔶      |

</details>
<details>
<summary>
Session Info
</summary>

| Field    | Value                        |
|:---------|:-----------------------------|
| Version  | R version 4.4.1 (2024-06-14) |
| Platform | x86_64-apple-darwin20        |
| Running  | macOS Monterey 12.7.6        |
| Language | en_US                        |
| Timezone | America/New_York             |

| Package  | Version |
|:---------|:--------|
| testthat | 3.2.2   |
| covr     | 3.6.4   |
| covrpage | 0.2     |

</details>
<!--- Final Status : skipped/warning --->
