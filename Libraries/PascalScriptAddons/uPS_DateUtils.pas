unit uPS_DateUtils;

interface

uses
  uPSCompiler,
  uPSRuntime,
  SysUtils,
  DateUtils,
  Windows;

procedure PS_Register_DateUtils_C(ACompiler : TPSPascalCompiler);
procedure PS_Register_DateUtils_R(AExec : TPSExec; ARCi : TPSRuntimeClassImporter);

implementation

procedure PS_Register_DateUtils_C(ACompiler : TPSPascalCompiler);
begin
  ACompiler.AddDelphiFunction('function IncYear(const AValue: TDateTime; const ANumberOfYears: Integer): TDateTime;');
  // function IncMonth is in SysUtils
  ACompiler.AddDelphiFunction('function IncWeek(const AValue: TDateTime; const ANumberOfWeeks: Integer): TDateTime;');
  ACompiler.AddDelphiFunction('function IncDay(const AValue: TDateTime; const ANumberOfDays: Integer): TDateTime;');
  ACompiler.AddDelphiFunction('function IncHour(const AValue: TDateTime; const ANumberOfHours: Int64): TDateTime;');
  ACompiler.AddDelphiFunction('function IncMinute(const AValue: TDateTime; const ANumberOfMinutes: Int64): TDateTime;');
  ACompiler.AddDelphiFunction('function IncSecond(const AValue: TDateTime; const ANumberOfSeconds: Int64): TDateTime;');
  ACompiler.AddDelphiFunction('function IncMilliSecond(const AValue: TDateTime; const ANumberOfMilliSeconds: Int64): TDateTime;');

  ACompiler.AddDelphiFunction('function EncodeDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word): TDateTime;');
  ACompiler.AddDelphiFunction('procedure DecodeDateTime(const AValue: TDateTime; out AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: Word); ');
end;

procedure PS_Register_DateUtils_R(AExec : TPSExec; ARCi : TPSRuntimeClassImporter);
begin
  AExec.RegisterDelphiFunction(@IncYear, 'IncYear', cdRegister);
  // function IncMonth is in SysUtils
  AExec.RegisterDelphiFunction(@IncWeek, 'IncWeek', cdRegister);
  AExec.RegisterDelphiFunction(@IncDay, 'IncDay', cdRegister);
  AExec.RegisterDelphiFunction(@IncHour, 'IncHour', cdRegister);
  AExec.RegisterDelphiFunction(@IncMinute, 'IncMinute', cdRegister);
  AExec.RegisterDelphiFunction(@IncSecond, 'IncSecond', cdRegister);
  AExec.RegisterDelphiFunction(@IncMilliSecond, 'IncMilliSecond', cdRegister);

  AExec.RegisterDelphiFunction(@EncodeDateTime, 'EncodeDateTime', cdRegister);
  AExec.RegisterDelphiFunction(@DecodeDateTime, 'DecodeDateTime', cdRegister);
end;

end.
