# runOPM
This readme was copied from the spe7 project, and will be edited, eventually.

This project is a learning exercise for myself.  The plan is:

1. Download SPE21221.  This is the simulation comparative solution project number 7, which deals with modeling horizontal wells.
2. Use tesseract-ocr / manual editing to digitize the tables
3. Use engauge to digitize plots
	- Is this "fair use", or will I be in violation?
4. Import the data into R and plot it up
5. Create an OPM ***flow*** deck for each of the 8 cases
	- Versions with, and without, LGR would be nice.
	- I don't know if flow supports LGR,  yet
6. Run the decks and retrieve the output into R
7. Plot the flow results up and compare with the SPE results.
8. Consider approaching the OPM developers about including this in their opm-data github project. They don't appear to have any test decks which use the LGR functionality.  Unfortunately, since I do not have access to an Eclipse license, someone else would need to run the decks for comparison.

This work will force me to learn how to do a number of things:

1. How to construct an LGR in ***flow***.
2. How to pull eclipse style ouput into R.
	- This is expected to be done with the ERT Python libraries
3. It would be nice if some batch/queue system could be set up for later work with OPM and R
4. The R/OPM interaction functionality will eventually need to be put into an R package, so keep this in mind

 SPE21221 is nearly 30 years old, and the figures are of remarkably poor quality, with a great deal of overplotting.  The point of the digitizing is to create a cloud of data points that show reasonable ranges for the expected output of the OPM simulation models.  There is no pretense that I have actualy captured the full data sets displayed in the plots.  Output data from the simulation models going through the middle of the cloud would support the idea that I have done a reasonable job of implementing the model, as specified in the paper.  If someone with access to an Eclipse license were able to run the decks, this could be a possible data set to minimally test the LGR functionality of ***flow***, as none of the other data sets supplied on the OPM website use this feature.