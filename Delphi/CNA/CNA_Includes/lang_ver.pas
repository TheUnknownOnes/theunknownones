//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
{$IFDEF DEBUG_ON}
  {$IFDEF LANG_GERMAN}
    {$R '..\CNA_Includes\german_debug.res'}
  {$ELSE}
    {$R '..\CNA_Includes\english_debug.res'}
  {$ENDIF}
{$ELSE}
  {$IFDEF LANG_GERMAN}
    {$R '..\CNA_Includes\german.res'}
  {$ELSE}
    {$R '..\CNA_Includes\english.res'}
  {$ENDIF}
{$ENDIF}