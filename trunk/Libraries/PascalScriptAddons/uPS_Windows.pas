unit uPS_Windows;

interface

uses
  uPSCompiler,
  uPSRuntime,
  SysUtils,
  Windows;

procedure PS_Register_Windows_C(ACompiler : TPSPascalCompiler);
procedure PS_Register_Windows_R(AExec : TPSExec; ARCi : TPSRuntimeClassImporter);

implementation

function _CopyFile(AOldName : String; ANewName : String; AFailIfExists : Boolean) : Boolean;
begin
  Result := CopyFileW(PWideChar(WideString(AOldName)), PWideChar(WideString(ANewName)), AFailIfExists);
end;

function _MoveFile(AOldName : String; ANewName : String) : Boolean;
begin
  Result := MoveFileW(PWideChar(WideString(AOldName)), PWideChar(WideString(ANewName)));
end;

procedure PS_Register_Windows_C(ACompiler : TPSPascalCompiler);
begin
  ACompiler.AddTypeCopyN('DWORD', 'Cardinal');
  ACompiler.AddTypeCopyN('TFilename', 'String');
  ACompiler.AddConstantN('MAX_PATH', 'Integer').SetInt(260);
  ACompiler.AddTypeS('TFileTime', 'record dwLowDateTime: DWORD; dwHighDateTime: DWORD; end;');

  ACompiler.AddDelphiFunction('procedure Sleep(AMilliseconds : Cardinal)');

  ACompiler.AddDelphiFunction('function CopyFile(AOldName : String; ANewName : String; AFailIfExists : Boolean) : Boolean');
  ACompiler.AddDelphiFunction('function MoveFile(AOldName : String; ANewName : String) : Boolean');
end;

procedure PS_Register_Windows_R(AExec : TPSExec; ARCi : TPSRuntimeClassImporter);
begin
  AExec.RegisterDelphiFunction(@Sleep, 'Sleep', cdPascal);
  AExec.RegisterDelphiFunction(@_CopyFile, 'CopyFile', cdRegister);
  AExec.RegisterDelphiFunction(@_MoveFile, 'MoveFile', cdRegister);
end;

end.
