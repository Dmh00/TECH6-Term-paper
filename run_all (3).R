
# 12_Run_all.R: creates every table and figure from regdata0.dta

# Check if we are in the right folder
if (!file.exists("regdata0.dta")) stop("Put regdata0.dta in this folder and run from here")
dir.create("outputs", showWarnings = FALSE)

# check that all R packages are installed
needed = c("haven","estimatr","ggplot2","gridExtra","rdrobust","rddensity","knitr","rmarkdown")
missing = needed[!sapply(needed, requireNamespace,quietly=TRUE)]
if (length(missing)>0) stop("Install first: install.packages(c(\"",paste(missing))

RUN_PYTHON = TRUE

# Replication: table and figures
for (f in c("01_Table1.R","02_Table2.R","03_Table3.R","04_Table7.R",
            "05_Figure1.R","06_Figure2.R","07_Figure3.R","08_Figure4.R"))
source(f,local=new.env())

# Extension 1 (R)
cat("\n============= 10_Extension ===============")
source("10_Extension1.R",local=new.env())

# Extension 2 (Python): needs numpy, pandas, scikit-learn, matplotlib
if (RUN_PYTHON) {
  cat("\n============= 11_Extension2.py ===============")
  source("11_Extension2.py",local=new.env())
  py = Sys.which("python)")
  if (py=="") py = Sys.which("python3")
  if (system2(py,"11_Extension2.py")!=0) stop("11_Extension2.py failed")
}
cat("\n============= 11_Extension2.py ===============")
if (rmarkdown::pandoc_available()) {
  rmarkdown::render("09_Format_tables.Rmd", output_format="html_document",
                    output_file="tables.html",output_dir="outputs",quiet=TRUE)
  
}

