unit uCNA2ActSetValue;

interface

uses
  Classes,
  SysUtils,
  ToolsAPI,
  TypInfo,
  uCNA2Actions,
  uCNA2Settings,
  uSettingsBase,
  Variants,
  WideStrings,
  uRTTIHelper,
  Dialogs;

type
  Tcna2ActSetValue = class(Tcna2Action)
  private
    FValue : Variant;
  public
    constructor Create(ATypeInfo : PTypeInfo); override;
    
    procedure LoadFromSettings(APath : TSettingName); override;
    procedure SaveToSettings(APath : TSettingName); override;

    function AsString() : WideString; override;
    procedure Configure(ATypeInfo : PTypeInfo); override;

    class function GetDisplayName : WideString; override;
    class function CanHandle(ATypeInfo : PTypeInfo) : Boolean; override;
    class function HasConfigDialog : Boolean; override;
  end;

implementation

uses uCNA2ActSetValueConfig, uCNA2ValueEditors;

{ Tcna2ActSetValue }

function Tcna2ActSetValue.AsString: WideString;
var
  sl : TWideStringList;
  idx : Integer;
  Element : Integer;
begin
  sl := TWideStringList.Create;
  try
    case FTypeInfo.Kind of
      tkInteger,
      tkFloat,
      tkInt64 : Result := 'Set to ' + VarToWideStr(FValue);
      tkChar,
      tkWChar,
      tkLString,
      tkWString,
      tkString: Result := 'Set to ''' + FValue + '''';
      tkEnumeration :
      begin
        rttihEnumToList(FTypeInfo, sl);
        idx := sl.IndexOfObject(TObject(Integer(FValue)));
        if idx = -1 then
        begin
          idx := 0;
          FValue := Integer(sl.Objects[idx]);
        end;
        Result := 'Set to ' + sl.Names[idx];
      end;
      tkSet :
      begin
        Result := 'Set to [';

        rttihSetToList(FTypeInfo, sl);
        for idx := 0 to sl.Count - 1 do
        begin
          Element := Integer(sl.Objects[idx]);
          if (Integer(FValue) and Element) = Element then
            Result := Result + sl.Names[idx] + ',';
        end;

        if Result[Length(Result)] = ',' then
          Result[Length(Result)]  := ']'
        else
          Result := Result + ']';
      end;      
    end;
  finally
    sl.Free;
  end;
end;

class function Tcna2ActSetValue.CanHandle(ATypeInfo: PTypeInfo): Boolean;
var
  EditorClass : Tcna2ValueEditorClass;
begin
  Result := cna2ValueEditors.FindByTypeInfo(ATypeInfo, EditorClass);
end;

procedure Tcna2ActSetValue.Configure(ATypeInfo : PTypeInfo);
begin
  FValue := Tform_ConfigSetValue.Execute(ATypeInfo, FValue);
end;

constructor Tcna2ActSetValue.Create(ATypeInfo: PTypeInfo);
var
  sl : TWideStringList;
begin
  inherited;

  sl := TWideStringList.Create;
  try
    case FTypeInfo.Kind of
      tkInteger,
      tkFloat,
      tkInt64 : FValue := 0;
      tkChar,
      tkWString,
      tkWChar,
      tkLString,
      tkString: FValue := '';
      tkEnumeration :
      begin
        rttihEnumToList(FTypeInfo, sl);
        FValue := Integer(sl.Objects[0]);
      end;
      tkSet:
      begin
        FValue := Integer(0);
      end;
    end;
  finally
    sl.Free;
  end;
end;

class function Tcna2ActSetValue.GetDisplayName: WideString;
begin
  Result := 'Set value';
end;

class function Tcna2ActSetValue.HasConfigDialog: Boolean;
begin
  Result := true;
end;

procedure Tcna2ActSetValue.LoadFromSettings(APath: TSettingName);
begin
  FValue := cna2Settings.GetValue(APath + '/Value', FValue);
end;

procedure Tcna2ActSetValue.SaveToSettings(APath: TSettingName);
begin
  cna2Settings.SetValue(APath + '/Value', FValue);
end;

end.
