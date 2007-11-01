set DELPHIBIN=C:\Programme\Borland\BDS\4.0\Bin

L:
cd L:\PROJECT\SOFTWARE\Benjamin\CNA_Includes
del *.res

%DELPHIBIN%\brcc32.exe german.rc
%DELPHIBIN%\brcc32.exe english.rc
