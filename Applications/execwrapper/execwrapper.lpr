program execwrapper;

{ We need this tool for modified calls to the binutils,
  because there is no way to pass arguments to them while compiling
  with lazarus/freepascal.
}

{$mode objfpc}{$H+}

{.$DEFINE Debug}

uses
  Classes, SysUtils, IniFiles;

var
  path,
  current_exe,
  target,
  params : String;
  config : TIniFile;
  config_file : String;
  idx : Integer;

function GetValue(AName : String; ADefault : String = '') : String;
begin
  Result := EmptyStr;

  Result := GetEnvironmentVariable(current_exe + '_' + AName);

  if (Result = EmptyStr) and Assigned(config) then
    Result := config.ReadString(current_exe, AName, ADefault);

  if Result = EmptyStr then
     Result := ADefault;
end;

procedure AddParam(AParam : String);
begin
  if AParam = EmptyStr then
    exit;

  if params <> EmptyStr then
    params := params + ' ';
  params := params + AParam;
end;

procedure Log(AText : String);
begin
  WriteLn('execwrapper: ' + AText);
end;

begin
  try
    current_exe := ExtractFileName(ParamStr(0));
    path := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
    config := nil;
    params := EmptyStr;

    {$IFDEF Debug}
    Log('using envrionment variables ' + current_exe + '_[target|preparams|params|postparams]');
    {$ENDIF}

    config_file := path + 'execwrapper.ini';
    if FileExists(config_file) then
    begin
      config := TIniFile.Create(config_file);
      {$IFDEF Debug}
      Log('using config "' + config_file + '"');
      {$ENDIF}
    end;

    target := path + GetValue('target', ChangeFileExt(current_exe, '.orig.exe'));
    if not FileExists(target) then
      raise Exception.Create(target + ' doesn''t exist!')
    else
      {$IFDEF Debug}
      Log('target is "' + target + '"');
      {$ENDIF}

    AddParam(GetValue('preparams'));
    AddParam(GetValue('params'));

    for idx := 1 to Paramcount do
      AddParam(ParamStr(idx));

    AddParam(GetValue('postparams'));

    if Assigned(config) then
      config.Free;

    {$IFDEF Debug}
    Log('params are "' + params + '"');
    {$ENDIF}

    idx := ExecuteProcess(target, params, [ExecInheritsHandles]);
    Halt(idx);
  except
    on E : Exception do
    begin
      Log('Error: ' + e.Message);
      Halt(1);
    end;
  end;
end.

