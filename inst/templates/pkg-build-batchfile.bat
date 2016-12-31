@echo off

REM Clean up Artifacts (from previous unsuccessful installs)
del __PKG__*.gz
del __PKG__*.zip
rmdir /s /q __PKG__.Rcheck

REM Build and Check Package
R CMD build __PKG__
R CMD check __PKG__*.gz

REM Install Package
R CMD INSTALL --build __PKG__

echo "done"
