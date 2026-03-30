# AGENTS.md

## Project Overview
This is an R Shiny based dashboard application for subnational tailoring of
malaria strategies and interventions (SNT). The application provides interactive
visualizations for intervention scenarios, cost analysis, and impact mapping
using the Rhino framework.

## Project Structure
- `app/` - Main application directory
  - `logic/` - Business logic modules (data processing, calculations, utilities)
  - `view/` - UI components and panels
  - `data/` - SQLite database and shapefiles
  - `static/` - Static assets (images, HTML, JS, CSS)
- `tests/` - Test files using testthat framework
- Configuration files for CI/CD, linting, and R environment

## Dev Environment Tips
- This is an R project using the Rhino framework
- Main entry point: `app/main.R`
- Database connection: SQLite database in `app/data/input_data.sqlite`
- Use `.Rprofile` for R environment configuration
- The project uses `{renv}` for package management
- Linting configured with `{lintr}` (see `.lintr` file)
- Editor configuration in `.editorconfig` and `.dir-locals.el`

## Testing Instructions
- Tests are located in `tests/testthat/`
- Run tests with: `rhino::test_r()`, `devtools::test()` or `testthat::test_dir("tests/testthat/")`
- CI configuration in `.github/workflows/rhino-test.yml`
- Each logic module should have corresponding tests
- Test files follow pattern `test-<module_name>.R`

## Key Modules and Their Functions

### Data Processing
- `get_filtered_data.R` - Main data filtering and retrieval
- `convert_col_types.R` - Data type conversion utilities
- `variablesManager.R` - Variable management and configuration

### Visualization
- `plots.R` - Plot generation functions
- `maps.R` - Map visualization logic
- `color_palettes.R` - Color scheme definitions

### Business Logic
- `costs.R` - Cost calculation logic
- `generate_rules.R` - Rule generation for interventions
- `get_intervention_combo.R` - Intervention combination logic
- `sort_interventions.R` - Intervention sorting algorithms

### UI Components
- Panel files (`panel_1.R` through `panel_4.R`) - UI panels
- `scenario_customization.R` - Scenario customization interface
- `costs.R`, `impact_map.R`, `intervention_map.R` - Visualization views

## Code Style Guidelines
- Follow R tidyverse style conventions
- Use meaningful variable and function names
- Add roxygen2 documentation for functions
- Keep functions focused and modular
- Use error handling with `tryCatch()` where appropriate

## Working with Data
- Primary data source: SQLite database
- Shapefile data for geographic visualizations
- Use `get_connection.R` for database connections
- Always close connections after use
- Filter data using `get_filtered_data.R` functions

## Commit Guidelines
- See `commit_messages.md`

## Common Tasks
- Adding new interventions: Update `logic/generate_rules.R` and related modules
- New visualizations: Add to `logic/plots.R` or create new view modules
- Database changes: Update `logic/get_filtered_data.R` and test accordingly
- UI modifications: Edit relevant files in `app/view/`

## Dependencies
- Check `renv.lock` for exact package versions
- Core dependencies: Rhino, Shiny, DBI, RSQLite, dplyr, ggplot2, leaflet
- Use `renv::restore()` to ensure consistent environment
