Elephant Tracking Data Cleaner
A Shiny app for cleaning elephant GPS collar data. Upload raw CSV files, flag erroneous fixes, review and remove them, and export a cleaned dataset with audit files.

Getting started
Requirements
R (≥ 4.1)
RStudio
Installation
Clone or download this repository
Open ElephantTrackingDataCleaner.Rproj in RStudio
Restore package dependencies:
renv::restore()
Run the app:
shiny::runApp()


Usage
Step 1: Data Ingestion
Upload CSV file(s) — upload one or more raw collar data files. The app reads each file and detects its columns automatically.
Per-file settings — for each file, confirm whether timestamps are in UTC, whether to filter to elephant rows only, and optionally apply a pre-filter on any column (e.g. to subset by site or collar type).
Study area name — enter the name of your study area.
Name corrections (optional) — upload a two-column CSV mapping raw collar names to canonical elephant names, then select which column is the raw name and which is the corrected name.
Prepare data — click to run standardisation: column names are standardised, coordinates validated, timestamps parsed, timezone detected, data projected to UTM, and duplicate fixes removed.
Post-preparation options (optional) — remove individuals entirely, rename individuals, or correct sex assignments.
Click Complete → Step 2 when satisfied.
Step 2: Region Assignment (optional)
The map shows a centroid point for each individual.
Click individual points or draw a rectangle on the map to select individuals.
Type or select a region name and click Assign selected. Repeat for other regions.
Use Bulk assign all unassigned to assign remaining individuals to one region in one step.
Click Complete (or Skip if region assignment is not needed).
Step 3: Flag
Spatial filters (optional)
Draw a bounding box on the map to flag points outside your study area.
Click Place HQ marker, click the map to set a headquarters location, and set a buffer radius to flag points near HQ (e.g. camp fixes before deployment).
Speed thresholds — adjust the vehicle speed and airborne speed thresholds if needed (defaults are 25 km/h and 150 km/h).
Pre-deployment detection — set the maximum elephant speed, minimum consecutive clean fixes, and maximum pre-deployment window to identify fixes before the collar was properly deployed.
Immobility detection — set the cluster radius, minimum duration, minimum fixes, and end-of-track fraction to flag potential collar drops or deaths.
Erroneous shift detection — set the distance threshold and maximum time gap to flag fixes where an animal appears to jump far away and immediately return.
Click Run flagging — a summary table shows how many fixes were flagged under each category.
Click Complete → Step 4 when satisfied (or Clear flagging to adjust thresholds and re-run).
Step 4: Flag Review
Select an individual from the dropdown (or use the arrow buttons to step through).
The map shows the full track with flagged points highlighted by colour (vehicle, airborne, pre-deployment, immobility, shift episode).
Click a point to see its flag type and speed in a popup.
Use the checkboxes to choose which flag types to remove for this individual.
Click Confirm & advance to next to save removals and move to the next individual.
Use Undo removals for this individual to reverse if needed.
Use Remove individual & advance to drop an entire individual from the dataset.
Click Complete review → Step 5 when all individuals have been reviewed.
Step 5: Manual Cleaning
Select an individual from the dropdown.
The map shows the post-flag track for that individual.
Click individual points on the map to select them (turns red). Click again to deselect.
Draw a rectangle on the map to select multiple points at once.
Use Select all points before/after selected to extend the selection to the start or end of the track.
Click Add selected to removal list to queue those points for removal.
Use Undo last addition if you make a mistake.
Work through all individuals, then click Complete clean → Step 6.
Step 6: Export
Flagged removals CSV — records all fixes removed during the automated flagging and review steps, with flag type. Rename the file if needed and download.
Manual removals CSV — records all fixes removed during manual cleaning. Rename and download.
Cleaned dataset — download the final cleaned data as an RDS file (for use in R) and/or a CSV file. Filenames are pre-populated from your study area name.
Click Complete → Dashboard when done.
Dashboard
A summary view showing:

Key statistics: number of individuals, raw fixes, fixes removed, clean fixes, and median fix interval
A Gantt chart of temporal coverage per individual
A heatmap of fix counts by individual and month
A processing summary table per individual
