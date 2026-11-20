# run_all.R: creates every table and figure from regdata0.dta
# Run it from the folder that contains regdata0.dta

dir.create("outputs", showWarnings = FALSE)

# Replication and Extension 1 (R)
scripts = c("01_Table1.R", "02_Table2.R", "03_Table3.R", "04_Table7.R",
            "05_Figure1.R", "06_Figure2.R", "07_Figure3.R", "08_Figure4.R",
            "10_Extension1.R")
for (f in scripts) source(f, local = new.env())

# Extension 2 (Python): needs numpy, pandas, scikit-learn, matplotlib
system2("python", "11_Extension2.py")

# Formatted tables (markdown file)
knitr::knit("09_Format_tables.Rmd", output = "outputs/tables.md")

