# SNT Dashboard example

## Setup workflow

### Creating a new country repo

```bash
# 1. Clone the shared repo
git clone git@gitlab.com:org/snt-dashboard-shared.git snt-dashboard-newcountry
cd snt-dashboard-newcountry

# 2. Change the origin to the new country repo
git remote rename origin shared
git remote add origin git@gitlab.com:org/snt-dashboard-newcountry.git
git push -u origin main

# 3. Create country-specific files from examples
cp config.example.yml config.yml
cp app/main.example.R app/main.R

# 4. Edit config.yml and app/main.R for the country
#    - Set country_name, admin_mapping, plans, etc. in config.yml
#    - Adjust imports/panels in app/main.R if needed

# 5. Commit country-specific files
git add config.yml app/main.R
git commit -m "feat: Add country configuration for <Country>"
git push origin main

# 6. Deliver the database (out-of-band, not via git)
#    Place app/data/input_data.sqlite
```

### Pulling shared updates into a country repo

```bash
git fetch shared
git merge shared/main
# Only files that exist in BOTH repos can conflict.
# config.yml and app/main.R are country-only, so they are never touched.
```
