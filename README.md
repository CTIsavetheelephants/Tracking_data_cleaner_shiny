<<<<<<< HEAD
# Elephant Tracking Data Cleaner

A Shiny app for cleaning elephant GPS collar data. Upload raw CSV files, flag erroneous fixes, review and remove them, and export a cleaned dataset with audit files.

## Getting started

### Requirements

- R (≥ 4.1)
- RStudio

### Installation

1. Clone or download this repository
2. Open the `.Rproj` file in RStudio
3. Restore package dependencies:

```r
renv::restore()
```

4. Run the app:

```r
shiny::runApp()
```

---

## Usage

### Step 1: Data Ingestion

1. **Upload CSV file(s)** — upload one or more raw collar data files. The app reads each file and detects its columns automatically.
2. **Per-file settings** — for each file, confirm whether timestamps are in UTC, whether to filter to elephant rows only, and optionally apply a pre-filter on any column (e.g. to subset by site or collar type).
3. **Study area name** — enter the name of your study area.
4. **Name corrections (optional)** — upload a two-column CSV mapping raw collar names to canonical elephant names, then select which column is the raw name and which is the corrected name.
5. **Prepare data** — click to run standardisation: column names are standardised, coordinates validated, timestamps parsed, timezone detected, data projected to UTM, and duplicate fixes removed.
6. **Post-preparation options (optional)** — remove individuals entirely, rename individuals, or correct sex assignments.
7. Click **Complete → Step 2** when satisfied.

---

### Step 2: Region Assignment *(optional)*

1. The map shows a centroid point for each individual.
2. Click individual points or draw a rectangle on the map to select individuals.
3. Type or select a region name and click **Assign selected**. Repeat for other regions.
4. Use **Bulk assign all unassigned** to assign remaining individuals to one region in one step.
5. Click **Complete** (or **Skip** if region assignment is not needed).

---

### Step 3: Flag

1. **Spatial filters (optional)**
   - Draw a bounding box on the map to flag points outside your study area.
   - Click **Place HQ marker**, click the map to set a headquarters location, and set a buffer radius to flag points near HQ (e.g. camp fixes before deployment).
2. **Speed thresholds** — adjust the vehicle speed and airborne speed thresholds if needed (defaults are 25 km/h and 150 km/h).
3. **Pre-deployment detection** — set the maximum elephant speed, minimum consecutive clean fixes, and maximum pre-deployment window to identify fixes before the collar was properly deployed.
4. **Immobility detection** — set the cluster radius, minimum duration, minimum fixes, and end-of-track fraction to flag potential collar drops or deaths.
5. **Erroneous shift detection** — set the distance threshold and maximum time gap to flag fixes where an animal appears to jump far away and immediately return.
6. Click **Run flagging** — a summary table shows how many fixes were flagged under each category.
7. Click **Complete → Step 4** when satisfied (or **Clear flagging** to adjust thresholds and re-run).

---

### Step 4: Flag Review

1. Select an individual from the dropdown (or use the arrow buttons to step through).
2. The map shows the full track with flagged points highlighted by colour (vehicle, airborne, pre-deployment, immobility, shift episode).
3. Click a point to see its flag type and speed in a popup.
4. Use the checkboxes to choose which flag types to remove for this individual.
5. Click **Confirm & advance to next** to save removals and move to the next individual.
6. Use **Undo removals for this individual** to reverse if needed.
7. Use **Remove individual & advance** to drop an entire individual from the dataset.
8. Click **Complete review → Step 5** when all individuals have been reviewed.

---

### Step 5: Manual Cleaning

1. Select an individual from the dropdown.
2. The map shows the post-flag track for that individual.
3. **Click individual points** on the map to select them (turns red). Click again to deselect.
4. **Draw a rectangle** on the map to select multiple points at once.
5. Use **Select all points before/after selected** to extend the selection to the start or end of the track.
6. Click **Add selected to removal list** to queue those points for removal.
7. Use **Undo last addition** if you make a mistake.
8. Work through all individuals, then click **Complete clean → Step 6**.

---

### Step 6: Export

1. **Flagged removals CSV** — records all fixes removed during the automated flagging and review steps, with flag type. Rename the file if needed and download.
2. **Manual removals CSV** — records all fixes removed during manual cleaning. Rename and download.
3. **Cleaned dataset** — download the final cleaned data as an **RDS file** (for use in R) and/or a **CSV file**. Filenames are pre-populated from your study area name.
4. Click **Complete → Dashboard** when done.

---

### Dashboard

A summary view showing:

- Key statistics: number of individuals, raw fixes, fixes removed, clean fixes, and median fix interval
- A Gantt chart of temporal coverage per individual
- A heatmap of fix counts by individual and month
- A processing summary table per individual
=======
