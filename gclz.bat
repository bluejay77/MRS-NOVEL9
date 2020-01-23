@echo off
REM
REM 1. Modify all instances of "x.y.z" in following lines to match 
REM    actual path to the file or directory indicated.
REM 2. Change all instances of 'ANSI' to 'CLtL1' if you are using the CLtL1 build
REM 3. Insert this file in "C:\Program Files\GCL-x.y.z-ANSI\bin" or "C:\Program Files\GCL-x.y.z-CLtL1\bin"
REM
set C_INCLUDE_PATH=C:\Progra~1\GCL-2.6.9-ANSI\lib\gcl-2.6.9\h
path C:\Progra~1\GCL-2.6.9-ANSI\mingw\bin;%PATH%
C:\Progra~1\GCL-2.6.9-ANSI\lib\gcl-2.6.9\unixport\saved_ansi_gcl.exe -dir C:/Progra~1/GCL-2.6.9-ANSI/lib/gcl-2.6.9/unixport/ -libdir C:/Progra~1/GCL-2.6.9-ANSI/lib/gcl-2.6.9/ 
