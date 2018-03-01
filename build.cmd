@echo off
cls

.paket\paket.bootstrapper.exe
.paket\paket.exe install

packages\FAKE\tools\FAKE.exe build.fsx %*
