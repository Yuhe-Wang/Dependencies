@echo off
copy ..\*.h .
copy ..\view\* .
del /q Makefile
del /q view.pro
del /q view.pro.user
copy ..\..\lib\win2k-cl-debug\egs_config1.h .
copy ..\dso\win2k-cl\egspp.lib .