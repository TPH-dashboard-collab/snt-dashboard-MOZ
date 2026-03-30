# Scenario Customization Module Documentation

## Overview

The scenario customization module provides an **intervention-first** approach to creating custom malaria intervention scenarios. Users select which districts receive each intervention, and the system handles conflicts and missing data combinations automatically.

## High-Level Flow

```
┌─────────────────────────────────────────────────────────────────┐
│                     USER STARTS SESSION                         │
└─────────────────────┬───────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────────┐
│  1. SELECT BASE PLAN & REFERENCE                                │
│     (e.g., NSP vs BAU)                                          │
└─────────────────────┬───────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────────┐
│  2. FETCH DATA                                                  │
│     - int_data_pool: Available intervention combinations        │
│     - custom_scen_data: Base scenario data                      │
│     - custom_cf_data: Reference data                            │
│     (Filtered by aggregation level: National/Regional/Strata)   │
└─────────────────────┬───────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────────┐
│  3. INITIALIZE INTERVENTION MAPPING                             │
│     Load districts from base plan for each intervention         │
│     e.g., {CM: [District_A, District_B], IRS: [District_C]}     │
└─────────────────────┬───────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────────┐
│  4. USER OPENS MODAL & EDITS SELECTIONS                         │
│     - 11 virtualSelectInput dropdowns (one per intervention)    │
│     - Grouped by region for easy multi-select                   │
│     - Filtered by aggregation level                             │
└─────────────────────┬───────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────────┐
│  5. VALIDATION (Debounced 1000ms)                               │
│     For each district:                                          │
│       a) Detect conflicts (e.g., CM + ICCM)                     │
│       b) Resolve conflicts (keep higher priority)               │
│       c) Find best match using Hamming distance                 │
│       d) Store validation results                               │
└─────────────────────┬───────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────────┐
│  6. DISPLAY VALIDATION STATUS                                   │
│     - Compact summary: Valid / Fallback / Conflict counts       │
│     - Modal with full report on click                           │
└─────────────────────┬───────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────────┐
│  7. USER CLICKS "APPLY SELECTIONS"                              │
└─────────────────────┬───────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────────┐
│  8. DATA ASSEMBLY (Lazy Loading)                                │
│     a) Collect unique scenario names needed                     │
│     b) Fetch only those scenarios from DB (not all 386!)        │
│     c) Build custom data for each district                      │
│     d) Combine with reference                                   │
└─────────────────────┬───────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────────┐
│  9. UPDATE VISUALIZATIONS                                       │
│     - Trigger custom_data_run_trigger()                         │
│     - Other modules react to new custom data                    │
└─────────────────────────────────────────────────────────────────┘
```

## Key Components

### 1. Data Structures

#### Intervention-District Mapping
```
variables$session_state$intervention_district_mapping
{
  "deployed_int_CM": ["District_A", "District_B", "District_C"],
  "deployed_int_STD_Nets": ["District_A", "District_B"],
  "deployed_int_IRS": ["District_B", "District_D"],
  ... (11 interventions total)
}
```

#### Validation State
```
district_validation_state()
{
  "District_A": {
    has_conflict: TRUE,
    conflict_type: "case_management",
    details: "CM and ICCM cannot be deployed together",
    original_selection: ["deployed_int_CM", "deployed_int_ICCM"],
    resolved_selection: ["deployed_int_CM"],
    has_fallback: FALSE
  },
  "District_B": {
    has_conflict: FALSE,
    has_fallback: TRUE,
    hamming_distance: 1,
    original_combo: "CM + STD_Nets + Vaccine",
    matched_combo: "CM + STD_Nets"
  },
  ...
}
```

### 2. Core Functions

#### Data Fetching Functions

**`int_data_pool_task_fn()`**
- **What**: Fetches available intervention combinations from database
- **When**: On module initialization and aggregation level change
- **Why**: Need to know what combinations exist for best-match algorithm
- **Filters**: age_group, year=2026, aggregation level
- **Returns**: data.table with columns: admin_2, scenario_name, deployed_int_*

**`custom_data_task_fn()`**
- **What**: Fetches scenario data from database
- **When**: On Apply button click (lazy loading)
- **Why**: Build the final custom scenario dataset
- **Filters**: age_group, scenario_name, aggregation level
- **Returns**: Full data.table for selected scenarios

#### Validation Functions (from `app/logic/`)

**`intervention_conflicts$detect_conflicts(interventions, district_name)`**
- **What**: Checks if selected interventions violate mutual exclusivity rules
- **Rules**:
  - Case management: CM vs ICCM (only one allowed)
  - Net types: STD_Nets vs PBO_Nets vs IG2_Nets (only one allowed)
  - Preventive chemo: PMC vs SMC (only one allowed)
- **Returns**:
  ```
  {
    has_conflict: TRUE/FALSE,
    conflict_groups: {...},
    details: "..."
  }
  ```

**`intervention_conflicts$resolve_conflicts(interventions, conflicts, priorities)`**
- **What**: Removes lower-priority conflicting interventions
- **How**: Keep intervention with highest priority value
- **Example**: If CM (priority=10) and ICCM (priority=9) conflict → keep CM
- **Returns**: Resolved intervention list

**`combination_matcher$find_best_match(district_name, target_interventions, int_data_pool_dt, priority_weights)`**
- **What**: Finds closest available intervention combination
- **Algorithm**: Hamming distance (count of differing interventions)
- **Tiebreaker**: Priority-weighted scoring if multiple matches
- **Returns**:
  ```
  {
    matched_data: data.table row with best combo,
    distance: Integer (0 = exact match),
    exact_match: TRUE/FALSE,
    original_target: {...},
    matched_interventions: {...}
  }
  ```

### 3. Reactive Flow Detail

```
┌──────────────────────────────────────────────────────────────────┐
│                     REACTIVE DEPENDENCIES                        │
└──────────────────────────────────────────────────────────────────┘

filtered_admins()
    │
    ├─ Depends on: variables$admins, agg_level, region_selected,
    │              strata_selected, int_data_pool() (for strata)
    │
    ├─ Used by: intervention_selectors, intervention_summary,
    │           validation, data_assembly
    │
    └─ Updates: When aggregation level changes

intervention_district_mapping
    │
    ├─ Updated by: User selections in modal
    │
    ├─ Triggers: validation_trigger (debounced 1000ms)
    │
    └─ Used by: validation, intervention_summary

validation_trigger()
    │
    ├─ Debounced: 1000ms (prevents excessive recalculation)
    │
    ├─ Triggers: Main validation observer
    │
    └─ Updates: district_validation_state, resolved_district_data

resolved_district_data
    │
    ├─ Contains: Best-match scenario for each district
    │
    └─ Used by: Data assembly on Apply button
```

## Detailed Process Flows

### A. Validation Process

```
┌─────────────────────────────────────────────────────────────────┐
│  FOR EACH DISTRICT (filtered by aggregation level)              │
└─────────────────┬───────────────────────────────────────────────┘
                  │
                  ▼
         ┌────────────────────┐
         │ Which interventions│
         │ are selected for   │
         │ this district?     │
         └────────┬───────────┘
                  │
                  ▼
         ┌────────────────────┐     NO         ┌──────────────────┐
         │ Any interventions? ├───────────────►│ Mark as "no      │
         │                    │                │ intervention"    │
         └────────┬───────────┘                │ requested        │
                  │ YES                        └──────────────────┘
                  ▼
         ┌────────────────────┐
         │ detect_conflicts() │
         │                    │
         └────────┬───────────┘
                  │
        ┌─────────┴──────────┐
        │                    │
        ▼                    ▼
  ┌──────────┐        ┌──────────┐
  │ CONFLICT │        │ NO       │
  │ FOUND    │        │ CONFLICT │
  └────┬─────┘        └────┬─────┘
       │                   │
       ▼                   │
  ┌────────────────┐       │
  │ resolve_       │       │
  │ conflicts()    │       │
  │ Keep highest   │       │
  │ priority       │       │
  └────┬───────────┘       │
       │                   │
       └─────────┬─────────┘
                 │
                 ▼
         ┌────────────────────┐
         │ create_            │
         │ intervention_list()│
         │ Convert to         │
         │ TRUE/FALSE vector  │
         └────────┬───────────┘
                  │
                  ▼
         ┌────────────────────┐
         │ find_best_match()  │
         │ Search int_data_   │
         │ pool for closest   │
         │ combination        │
         └────────┬───────────┘
                  │
        ┌─────────┴──────────┐
        │                    │
        ▼                    ▼
  ┌──────────┐         ┌──────────┐
  │ EXACT    │         │ FALLBACK │
  │ MATCH    │         │ NEEDED   │
  │ distance=0│        │ distance>0│
  └────┬─────┘         └────┬─────┘
       │                    │
       └─────────┬──────────┘
                 │
                 ▼
         ┌────────────────────┐
         │ Store validation   │
         │ results and matched│
         │ data for district  │
         └────────────────────┘
```

### B. Best-Match Algorithm (Hamming Distance)

```
Target:     [1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1]  (CM, STD_Nets, Vaccine)
            │  │  │  │  │  │  │  │  │  │  │
Candidate1: [1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0]  Distance = 1 ✓
            │  │  │  │  │  │  │  │  │  │  X   (missing Vaccine)

Candidate2: [1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 1]  Distance = 1 ✓
            │  X  │  │  │  │  │  │  │  │  │   (extra ICCM)

Candidate3: [0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1]  Distance = 1 ✓
            X  │  │  │  │  │  │  │  │  │  │   (missing CM)

→ Multiple matches with distance=1
→ Use priority tiebreaker: sum(priority × intervention_present)
→ Candidate1 score: 10(CM) + 8(STD_Nets) = 18
→ Candidate2 score: 10(CM) + 9(ICCM) + 8(STD_Nets) + 5(Vaccine) = 32 ✓ WINNER
→ Candidate3 score: 8(STD_Nets) + 5(Vaccine) = 13
```

### C. Data Assembly (Lazy Loading)

```
┌─────────────────────────────────────────────────────────────────┐
│  STEP 1: COLLECT REQUIRED SCENARIOS                             │
└─────────────────┬───────────────────────────────────────────────┘
                  │
                  ▼
    FOR EACH DISTRICT (filtered):
      ┌────────────────────────┐
      │ Get matched scenario   │
      │ from resolved_district_│
      │ data                   │
      └────────┬───────────────┘
               │
      ┌────────┴────────┐
      │                 │
      ▼                 ▼
┌──────────┐      ┌──────────┐
│ Has      │      │ No match │
│ matched  │      │ Use base │
│ scenario │      │ scenario │
└────┬─────┘      └────┬─────┘
     │                 │
     └────────┬────────┘
              │
              ▼
      ┌────────────────┐
      │ Add scenario   │
      │ name to list   │
      └────────────────┘

    DEDUPLICATE scenario list
    → Typically 5-50 scenarios (vs 386 total!)

┌─────────────────────────────────────────────────────────────────┐
│  STEP 2: FETCH ONLY REQUIRED SCENARIOS                          │
└─────────────────┬───────────────────────────────────────────────┘
                  │
                  ▼
    custom_data_task_fn(
      scenario_name = required_scenarios  ← KEY OPTIMIZATION!
    )

    Fetches ~2-10% of total data

┌─────────────────────────────────────────────────────────────────┐
│  STEP 3: BUILD CUSTOM DATASET                                   │
└─────────────────┬───────────────────────────────────────────────┘
                  │
                  ▼
    FOR EACH DISTRICT (filtered):
      ┌────────────────────────┐
      │ Get scenario name      │
      └────────┬───────────────┘
               │
               ▼
      ┌────────────────────────┐
      │ Extract full data for  │
      │ district from pool     │
      └────────┬───────────────┘
               │
               ▼
      ┌────────────────────────┐
      │ Append to new_custom_  │
      │ data with rbindlist    │
      └────────────────────────┘

    Update custom_scen_data()
    Combine with custom_cf_data()
    Trigger visualization updates
```

## Aggregation Level Filtering

The module respects the selected aggregation level throughout:

```
┌─────────────────────────────────────────────────────────────────┐
│                    AGGREGATION LEVELS                           │
└─────────────────────────────────────────────────────────────────┘

NATIONAL
  ├─ Shows: All 183 districts
  ├─ Filters: None (full dataset)
  └─ Use case: Country-wide analysis

REGIONAL
  ├─ Shows: Districts from selected regions only
  ├─ Filters: admin_1 IN (selected_regions)
  ├─ Example: "Arusha", "Dar es Salaam" → ~20 districts
  └─ Use case: Regional comparison

RISK STRATA
  ├─ Shows: Districts from selected strata only
  ├─ Filters: risk_stratum IN (selected_strata)
  ├─ Example: "High burden" → ~40 districts
  └─ Use case: Risk-stratified interventions

Implementation:
  filtered_admins() reactive
    ├─ Filters variables$admins by aggregation level
    ├─ For strata: Uses int_data_pool() (already filtered by DB)
    └─ Used by: selectors, summary, validation, data assembly
```

## Performance Optimizations

### 1. Debounced Validation
```
User types in selector → 100ms
User types again       → 200ms  ┐
User types again       → 300ms  │ No validation yet
User types again       → 400ms  │
User stops typing      → 1400ms ┘ → VALIDATION RUNS ONCE
```

### 2. Lazy Loading
```
OLD APPROACH:
  Fetch all 386 scenarios upfront
  → ~15-30 seconds
  → ~500 MB data

NEW APPROACH:
  User customizes 20 districts
  → Need 5-10 unique scenarios
  → Fetch only those
  → ~1-2 seconds
  → ~10-50 MB data

SPEEDUP: 10-30x faster! 🚀
```

### 3. Efficient Data Structures
```
intervention_district_mapping: list (O(1) access)
int_data_pool: data.table (indexed lookups)
resolved_district_data: reactiveValues (lazy evaluation)
```

## Error Handling

### Missing Combinations
- **Issue**: User selects combo not in database
- **Solution**: Find best match using Hamming distance
- **User Feedback**: Yellow warning with distance info

### Conflicts
- **Issue**: User selects CM + ICCM (mutually exclusive)
- **Solution**: Automatically keep highest priority (CM)
- **User Feedback**: Red alert showing original → resolved

### Empty Selections
- **Issue**: User clears all interventions for a district
- **Solution**: Search for "all FALSE" scenario, or use closest match
- **User Feedback**: Shows as "No interventions" with match info

### No Data for Aggregation
- **Issue**: Selected region has no districts in database
- **Solution**: int_data_pool returns empty, UI shows loading
- **User Feedback**: "No data available" message

## UI Components

### Modal Layout
```
┌────────────────────────────────────────────────────────────────┐
│  Configure Intervention Assignments                   [X]      │
├────────────────────────────────────────────────────────────────┤
│                                                                │
│  [Clear All Interventions]                                     │
│                                                                │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐     │
│  │   CM     │  │   ICCM   │  │ STD_Nets │  │ PBO_Nets │     │
│  │          │  │          │  │          │  │          │     │
│  │ [Select] │  │ [Select] │  │ [Select] │  │ [Select] │     │
│  └──────────┘  └──────────┘  └──────────┘  └──────────┘     │
│                                                                │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐     │
│  │ IG2_Nets │  │   IRS    │  │  IPTSc   │  │   LSM    │     │
│  │          │  │          │  │          │  │          │     │
│  │ [Select] │  │ [Select] │  │ [Select] │  │ [Select] │     │
│  └──────────┘  └──────────┘  └──────────┘  └──────────┘     │
│                                                                │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐                    │
│  │   PMC    │  │   SMC    │  │ Vaccine  │                    │
│  │          │  │          │  │          │                    │
│  │ [Select] │  │ [Select] │  │ [Select] │                    │
│  └──────────┘  └──────────┘  └──────────┘                    │
│                                                                │
├────────────────────────────────────────────────────────────────┤
│                                      [Close]  [Apply Changes]  │
└────────────────────────────────────────────────────────────────┘
```

### Validation Summary
```
┌────────────────────────────────────────────────────────────────┐
│  Validation Status                                             │
├────────────────────────────────────────────────────────────────┤
│                                                                │
│       ✓              ⚠              ✗                          │
│      150            20              13                         │
│     Valid        Fallback       Conflict                       │
│                                                                │
│  [View Detailed Report]                                        │
└────────────────────────────────────────────────────────────────┘
```

## Troubleshooting

### Validation not updating
- Check: debounce timer (wait 1 second after last change)
- Check: int_data_pool loaded (see data pool loading indicator)

### Districts missing from selectors
- Check: aggregation level filter
- Check: int_data_pool filtered correctly (risk strata needs DB data)

### Apply button slow
- Expected: 1-5 seconds depending on number of unique scenarios
- Check debug panel: see how many scenarios being fetched
- Optimize: select similar interventions for multiple districts

### Validation shows all fallbacks
- Possible: Database doesn't have requested combinations
- Check: Are you selecting unusual combos?
- Review: int_data_pool to see available combinations

## Future Enhancements

- [ ] Bulk district operations (select all in region)
- [ ] Intervention dependency rules (e.g., Vaccine requires CM)
- [ ] Save/load custom scenarios
- [ ] Undo/redo functionality
- [ ] Performance: Cache validation results
- [ ] UI: Show district count in intervention headers
- [ ] Export validation report to CSV
