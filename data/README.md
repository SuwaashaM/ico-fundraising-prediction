# Data

The coursework dataset (`LUBS5990M_courseworkData_202526.csv`) is not redistributable and is not included.

To run on real data, either:

- place the CSV at `data/raw/LUBS5990M_courseworkData_202526.csv`, or
- point to it: `ICO_DATA_PATH=/path/to/file.csv Rscript run_analysis.R`

The file must contain these columns (checked at load time): `goal`, `sold_tokens`, `whitelist`, `kyc`, `bonus`, `ERC20`, `mvp`, `link_white_paper`, `linkedin_link`, `github_link`, `website`, `ico_start`, `ico_end`, `pre_ico_start`, `pre_ico_end`, `country`, `distributed_in_ico`, `price_usd`, `rating`, `teamsize`, `token_for_sale`.

No data? Run `Rscript run_analysis.R --demo` to exercise the whole pipeline on synthetic data. Demo metrics are not meaningful.
