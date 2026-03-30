# Database Setup

## Initial Configuration

The following pragmas must be applied **before** enabling WAL mode and
**before** inserting data:

```sql
PRAGMA page_size = 32768;
```

## Runtime Pragmas

Applied via connection pool on each new connection:

| Pragma         | Value       | Purpose                                                |
|----------------|-------------|--------------------------------------------------------|
| `journal_mode` | WAL         | Write-Ahead Logging for concurrent reads during writes |
| `busy_timeout` | 5000        | Wait up to 5 seconds when database is locked           |
| `synchronous`  | normal      | Reduced fsync calls (safe with WAL)                    |
| `temp_store`   | memory      | Store temporary tables in RAM                          |
| `mmap_size`    | 30000000000 | Memory-map up to ~30GB of database file                |
| `page_size`    | 32768       | 32KB pages (no effect after DB creation)               |
| `cache_size`   | -65536      | 64MB page cache (negative = KB)                        |

## Creating Indexes

Run the following SQL commands to create performance indexes:

```bash
sqlite3 app/data/input_data.sqlite <<'EOF'
-- Single-column indexes
CREATE INDEX IF NOT EXISTS idx_plan ON data(plan);
CREATE INDEX IF NOT EXISTS idx_scenario_name ON data(scenario_name);
CREATE INDEX IF NOT EXISTS idx_admin_1 ON data(admin_1);
CREATE INDEX IF NOT EXISTS idx_admin_2 ON data(admin_2);
CREATE INDEX IF NOT EXISTS idx_age_group ON data(age_group);
CREATE INDEX IF NOT EXISTS idx_risk_stratum ON data(risk_stratum);
CREATE INDEX IF NOT EXISTS idx_year ON data(year);

-- Composite indexes
CREATE INDEX IF NOT EXISTS idx_plan_scenario ON data(plan, scenario_name);
CREATE INDEX IF NOT EXISTS idx_admin_combo ON data(admin_1, admin_2);

-- Update query planner statistics
ANALYZE;
EOF
```

## Verification

Check that 9 indexes exist on the `data` table:

```bash
sqlite3 app/data/input_data.sqlite "SELECT COUNT(*) FROM sqlite_master WHERE type='index' AND tbl_name='data';"
```

## Create views

```bash
# All scenarios
sqlite3 app/data/input_data.sqlite <<'EOF'
CREATE VIEW IF NOT EXISTS all_scenarios AS
SELECT DISTINCT scenario_name FROM data WHERE plan != 'Customized';
EOF

# NSP scenarios
sqlite3 app/data/input_data.sqlite <<'EOF'
CREATE VIEW IF NOT EXISTS scenarios AS
SELECT DISTINCT scenario_name FROM data WHERE plan == 'NSP';
EOF

# References
sqlite3 app/data/input_data.sqlite <<'EOF'
CREATE VIEW IF NOT EXISTS references AS
SELECT DISTINCT scenario_name FROM data WHERE plan == 'BAU';
EOF

# Admin names
sqlite3 app/data/input_data.sqlite <<'EOF'
CREATE VIEW IF NOT EXISTS admins AS
SELECT DISTINCT admin_1, admin_2 FROM data;
EOF

# Risk strata
sqlite3 app/data/input_data.sqlite <<'EOF'
CREATE VIEW IF NOT EXISTS risk_strata AS
SELECT DISTINCT risk_stratum FROM data;
EOF

# Age groups
sqlite3 app/data/input_data.sqlite <<'EOF'
CREATE VIEW IF NOT EXISTS age_groups AS
SELECT DISTINCT age_group FROM data;
EOF

# Year range
sqlite3 app/data/input_data.sqlite <<'EOF'
CREATE VIEW IF NOT EXISTS year_range AS
SELECT DISTINCT year FROM data ORDER BY year ASC;
EOF

# Intervention data pool

# NOTE: This one is a bit tricky, because we need to know the intervention
#   columns beforehand. We can get them via
sqlite3 app/data/input_data.sqlite <<'EOF'
SELECT name FROM pragma_table_info('data')
WHERE name LIKE 'deployed_int_%';
EOF

sqlite3 app/data/input_data.sqlite <<'EOF'
CREATE VIEW IF NOT EXISTS int_data_pool AS
SELECT DISTINCT scenario_name, admin_2, scenario_name, admin_1, admin_2, risk_stratum, age_group, year,
deployed_int_LSM,
deployed_int_IPTSc,
deployed_int_IRS,
deployed_int_Vaccine,
deployed_int_ICCM,
deployed_int_CM,
deployed_int_STD_Nets,
deployed_int_PBO_Nets,
deployed_int_IG2_Nets,
deployed_int_PMC,
deployed_int_SMC
FROM data;
EOF

```
