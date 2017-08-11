@echo off
echo DSEP =/>egs++\config.mak
echo my_machine =win2k-cl>>egs++\config.mak
echo HEN_HOUSE =%cd:\=/%/>>egs++\config.mak
echo SPEC_DIR = $(HEN_HOUSE)$(DSEP)specs$(DSEP)>>egs++\config.mak
echo ABS_DSO = $(HEN_HOUSE)$(DSEP)egs++$(DSEP)dso$(DSEP)$(my_machine)$(DSEP)>>egs++\config.mak

echo DSEP =/>egs++\config-debug.mak
echo my_machine =win2k-cl-debug>>egs++\config-debug.mak
echo HEN_HOUSE =%cd:\=/%/>>egs++\config-debug.mak
echo SPEC_DIR = $(HEN_HOUSE)$(DSEP)specs$(DSEP)>>egs++\config-debug.mak
echo ABS_DSO = $(HEN_HOUSE)$(DSEP)egs++$(DSEP)dso$(DSEP)$(my_machine)$(DSEP)>>egs++\config-debug.mak