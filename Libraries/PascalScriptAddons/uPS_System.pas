unit uPS_System;

interface

uses
  uPSCompiler,
  uPSRuntime;

procedure PS_Register_System_C(ACompiler : TPSPascalCompiler);
procedure PS_Register_System_R(AExec : TPSExec; ARCi : TPSRuntimeClassImporter);

implementation

procedure PS_Register_System_C(ACompiler : TPSPascalCompiler);
var
  pscTObject : TPSCompileTimeClass;
begin
  pscTObject := ACompiler.AddClass(nil, TObject);
  with pscTObject do
  begin
    RegisterMethod('constructor Create;');
    RegisterMethod('procedure Free;');
  end;
end;

procedure PS_Register_System_R(AExec : TPSExec; ARCi : TPSRuntimeClassImporter);
begin
  with ARCi.Add(TObject) do
  begin
    RegisterConstructor(@TObject.Create, 'Create');
    RegisterMethod(@TObject.Free, 'Free');
  end;
end;

end.
