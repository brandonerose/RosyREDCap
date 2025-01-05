Tests and Coverage
================
05 January, 2025 15:22:56

- [Coverage](#coverage)
- [Unit Tests](#unit-tests)

This output is created by
[covrpage](https://github.com/yonicd/covrpage).

## Coverage

Coverage summary is created using the
[covr](https://github.com/r-lib/covr) package.

| Object                                              | Coverage (%) |
|:----------------------------------------------------|:------------:|
| RosyREDCap                                          |     0.02     |
| [R/app_config.R](../R/app_config.R)                 |     0.00     |
| [R/app_server.R](../R/app_server.R)                 |     0.00     |
| [R/app_settings.R](../R/app_settings.R)             |     0.00     |
| [R/app_ui.R](../R/app_ui.R)                         |     0.00     |
| [R/cache.R](../R/cache.R)                           |     0.00     |
| [R/DB_diagram.R](../R/DB_diagram.R)                 |     0.00     |
| [R/DB_helpers.R](../R/DB_helpers.R)                 |     0.00     |
| [R/DB_setup.R](../R/DB_setup.R)                     |     0.00     |
| [R/DB_summarize.R](../R/DB_summarize.R)             |     0.00     |
| [R/DB_to_and_from_dir.R](../R/DB_to_and_from_dir.R) |     0.00     |
| [R/DB_transform.R](../R/DB_transform.R)             |     0.00     |
| [R/DB_update.R](../R/DB_update.R)                   |     0.00     |
| [R/projects.R](../R/projects.R)                     |     0.00     |
| [R/REDCap_API.R](../R/REDCap_API.R)                 |     0.00     |
| [R/REDCap_files.R](../R/REDCap_files.R)             |     0.00     |
| [R/REDCap_tokens.R](../R/REDCap_tokens.R)           |     0.00     |
| [R/REDCap_upload.R](../R/REDCap_upload.R)           |     0.00     |
| [R/utils-RosyUtils.R](../R/utils-RosyUtils.R)       |     0.00     |
| [R/utils.R](../R/utils.R)                           |     0.00     |
| [R/zzz.R](../R/zzz.R)                               |    33.33     |

<br>

## Unit Tests

Unit Test summary is created using the
[testthat](https://github.com/r-lib/testthat) package.

| file | n | time | error | failed | skipped | warning | icon |
|:---|---:|---:|---:|---:|---:|---:|:---|
| [test-app_config.R](testthat/test-app_config.R) | 1 | 0.068 | 0 | 0 | 0 | 0 |  |
| [test-app_server.R](testthat/test-app_server.R) | 1 | 0.005 | 0 | 0 | 0 | 0 |  |
| [test-app_settings.R](testthat/test-app_settings.R) | 1 | 0.006 | 0 | 0 | 0 | 0 |  |
| [test-app_ui.R](testthat/test-app_ui.R) | 1 | 0.005 | 0 | 0 | 0 | 0 |  |
| [test-cache.R](testthat/test-cache.R) | 16 | 0.100 | 0 | 0 | 0 | 0 |  |
| [test-DB_diagram.R](testthat/test-DB_diagram.R) | 1 | 0.006 | 0 | 0 | 0 | 0 |  |
| [test-DB_helpers.R](testthat/test-DB_helpers.R) | 1 | 0.006 | 0 | 0 | 0 | 0 |  |
| [test-DB_setup.R](testthat/test-DB_setup.R) | 34 | 0.539 | 0 | 0 | 1 | 0 | 🔶 |
| [test-DB_to_and_from_dir.R](testthat/test-DB_to_and_from_dir.R) | 1 | 0.006 | 0 | 0 | 0 | 0 |  |
| [test-DB_transform.R](testthat/test-DB_transform.R) | 1 | 0.006 | 0 | 0 | 0 | 0 |  |
| [test-DB_update.R](testthat/test-DB_update.R) | 1 | 0.007 | 0 | 0 | 0 | 0 |  |
| [test-projects.R](testthat/test-projects.R) | 4 | 0.024 | 0 | 0 | 0 | 0 |  |
| [test-REDCap_API.R](testthat/test-REDCap_API.R) | 1 | 0.006 | 0 | 0 | 0 | 0 |  |
| [test-REDCap_files.R](testthat/test-REDCap_files.R) | 1 | 0.007 | 0 | 0 | 0 | 0 |  |
| [test-REDCap_tokens.R](testthat/test-REDCap_tokens.R) | 1 | 0.005 | 0 | 0 | 0 | 0 |  |
| [test-REDCap_upload.R](testthat/test-REDCap_upload.R) | 1 | 0.005 | 0 | 0 | 0 | 0 |  |

<details open>
<summary>
Show Detailed Test Results
</summary>

| file | context | test | status | n | time | icon |
|:---|:---|:---|:---|---:|---:|:---|
| [test-app_config.R](testthat/test-app_config.R#L2) | app_config | multiplication works | PASS | 1 | 0.068 |  |
| [test-app_server.R](testthat/test-app_server.R#L2) | app_server | multiplication works | PASS | 1 | 0.005 |  |
| [test-app_settings.R](testthat/test-app_settings.R#L2) | app_settings | multiplication works | PASS | 1 | 0.006 |  |
| [test-app_ui.R](testthat/test-app_ui.R#L2) | app_ui | multiplication works | PASS | 1 | 0.005 |  |
| [test-cache.R](testthat/test-cache.R#L2) | cache | hoardr cache exsists | PASS | 2 | 0.010 |  |
| [test-cache.R](testthat/test-cache.R#L16) | cache | fake_cache sets and clears | PASS | 7 | 0.050 |  |
| [test-cache.R](testthat/test-cache.R#L40) | cache | cache_projects_exists, cache_clear works | PASS | 7 | 0.040 |  |
| [test-DB_diagram.R](testthat/test-DB_diagram.R#L2) | DB_diagram | multiplication works | PASS | 1 | 0.006 |  |
| [test-DB_helpers.R](testthat/test-DB_helpers.R#L2) | DB_helpers | multiplication works | PASS | 1 | 0.006 |  |
| [test-DB_setup.R](testthat/test-DB_setup.R#L3) | DB_setup | test_dir works | PASS | 3 | 0.012 |  |
| [test-DB_setup.R](testthat/test-DB_setup.R#L12) | DB_setup | setup_DB creates a valid DB object and valid directory | PASS | 19 | 0.187 |  |
| [test-DB_setup.R](testthat/test-DB_setup.R#L57_L59) | DB_setup | works | SKIPPED | 1 | 0.004 | 🔶 |
| [test-DB_setup.R](testthat/test-DB_setup.R#L81) | DB_setup | save_DB doesn’t save if it’s blank but will save and cache if valid, also loads | PASS | 11 | 0.336 |  |
| [test-DB_to_and_from_dir.R](testthat/test-DB_to_and_from_dir.R#L2) | DB_to_and_from_dir | multiplication works | PASS | 1 | 0.006 |  |
| [test-DB_transform.R](testthat/test-DB_transform.R#L2) | DB_transform | multiplication works | PASS | 1 | 0.006 |  |
| [test-DB_update.R](testthat/test-DB_update.R#L2) | DB_update | multiplication works | PASS | 1 | 0.007 |  |
| [test-projects.R](testthat/test-projects.R#L4) | projects | get_projects is df and has appropriate columns | PASS | 4 | 0.024 |  |
| [test-REDCap_API.R](testthat/test-REDCap_API.R#L2) | REDCap_API | multiplication works | PASS | 1 | 0.006 |  |
| [test-REDCap_files.R](testthat/test-REDCap_files.R#L2) | REDCap_files | multiplication works | PASS | 1 | 0.007 |  |
| [test-REDCap_tokens.R](testthat/test-REDCap_tokens.R#L2) | REDCap_tokens | multiplication works | PASS | 1 | 0.005 |  |
| [test-REDCap_upload.R](testthat/test-REDCap_upload.R#L2) | REDCap_upload | multiplication works | PASS | 1 | 0.005 |  |

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
