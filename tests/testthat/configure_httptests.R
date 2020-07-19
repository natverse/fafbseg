#This file contains details on how to cache http requests before using them in mock apis for offline testing
library(httptest)
library(fafbseg)

#Step 1: Set the folder locations..
testpath = paste0('./',test_path())
.mockPaths(testpath)


#Step 2: Start capturing http data from now..

start_capturing()


#Now add test cases for each file..

#For test file : test-flywire.R
      #For test case : "check return type/err handles from flywire"
flywire_fetch("https://globalv1.flywire-daf.com/nglstate/123",return="text")
flywire_fetch("https://globalv1.flywire-daf.com/nglstate/5747205470158848",return="parsed")
flywire_fetch("https://globalv1.flywire-daf.com/nglstate/5747205470158848",return="text")


#Step 4: Stop the capture
stop_capturing()

#Step 5: Rename the captured folder structure like below..
unlink(paste0(testpath, "/api/", sep=""), recursive = TRUE)
old_folderstruct <- paste0(testpath, "/globalv1.flywire-daf.com/.", sep="")
new_folderstruct <- paste0(testpath, "/api/", sep="")
dir.create(new_folderstruct)
new_folderstruct <- paste0(testpath, "/api/globalv1.flywire-daf.com/", sep="")
dir.create(new_folderstruct)
file.copy(old_folderstruct, new_folderstruct, recursive=TRUE)
unlink(paste0(testpath, "/globalv1.flywire-daf.com", sep=""), recursive = TRUE)



