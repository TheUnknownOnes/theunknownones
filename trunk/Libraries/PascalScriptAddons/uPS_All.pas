unit uPS_All;

interface

uses
  uPSRuntime,
  uPSCompiler,
  uPS_Classes,
  uPS_CSV,
  uPS_DateUtils,
  uPS_System,
  uPS_SysUtils,
  uPS_Types,
  uPS_UIB,
  uPS_Windows;

procedure PS_Register_All_C(ACompiler : TPSPascalCompiler);
procedure PS_Register_All_R(AExec : TPSExec; ARCi : TPSRuntimeClassImporter);

implementation

procedure PS_Register_All_C(ACompiler : TPSPascalCompiler);
begin
  PS_Register_Types_C(ACompiler);
  PS_Register_System_C(ACompiler);
  PS_Register_Classes_C(ACompiler);
  PS_Register_Windows_C(ACompiler);
  PS_Register_SysUtils_C(ACompiler);
  PS_Register_DateUtils_C(ACompiler);
  PS_Register_UIB_C(ACompiler);
  PS_Register_CSV_C(ACompiler);
end;

procedure PS_Register_All_R(AExec : TPSExec; ARCi : TPSRuntimeClassImporter);
begin
  PS_Register_Types_R(AExec, ARCi);
  PS_Register_System_R(AExec, ARCi);
  PS_Register_Windows_R(AExec, ARCi);
  PS_Register_Classes_R(AExec, ARCi);
  PS_Register_SysUtils_R(AExec, ARCi);
  PS_Register_DateUtils_R(AExec, ARCi);
  PS_Register_UIB_R(AExec, ARCi);
  PS_Register_CSV_R(AExec, ARCi);
end;

end.
