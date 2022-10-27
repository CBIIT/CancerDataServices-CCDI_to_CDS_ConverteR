# CancerDataServices-CCDI_to_CDS_ConverteR
This script will take a CCDI submission template and transform it to a flattened CDS submission template.

Run the following command in a terminal where R is installed for help.
```
Rscript --vanilla CCDI_to_CDS_converteR.R -h
Usage: CCDI_to_CDS_converteR.R [options]

CCDI_to_CDS_converteR v2.0.0

Options:
	-f CHARACTER, --file=CHARACTER
		CCDI submission template dataset file (.xlsx)

	-t CHARACTER, --template=CHARACTER
		CDS submission template file (.xlsx)

	-h, --help
		Show this help message and exit
```

There is also a set of example files located in the Test_Files directory and the following command can be executed:
```
Rscript --vanilla CCDI_to_CDS_converteR.R -f Test_Files/EXAMPLE_ccdi_Submission_Template.xlsx -t Test_Files/EXAMPLE_CDS_submission_metadata_template.xlsx
```
