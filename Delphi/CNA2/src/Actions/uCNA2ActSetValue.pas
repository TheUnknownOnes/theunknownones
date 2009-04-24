//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************
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
  uRTTIHelper,
  Graphics,
  dialogs;

type
  Tcna2ActSetValue = class(Tcna2Action)
  private
    FValue : Variant;
    FShowDialog : Boolean;
  public
    constructor Create(ATypeInfo : PTypeInfo); override;
    
    procedure LoadFromSettings(APath : TSettingName); override;
    procedure SaveToSettings(APath : TSettingName); override;

    procedure Execute(AEditor : IOTAFormEditor;
                      AComponent : IOTAComponent;
                      AProperty : WideString); override;

    function AsString() : WideString; override;
    procedure Configure(); override;

    class function GetDisplayName : WideString; override;
    class function CanHandle(ATypeInfo : PTypeInfo) : Boolean; override;
    class function HasConfigDialog : Boolean; override;

    procedure Init(AValue : Variant; AShowDialog : Boolean);
  end;

implementation

uses uCNA2ActSetValueConfig, uCNA2ValueEditors;

{ Tcna2ActSetValue }

function Tcna2ActSetValue.AsString: WideString;
var
  sl : TStringList;
  idx : Integer;
  Element : Integer;
begin
  sl := TStringList.Create;
  try
    if FShowDialog then
      Result := 'Show dialog'
    else
    begin
      case FTypeInfo.Kind of
        tkInteger,
        tkFloat,
        tkInt64 :
        begin
          if FTypeInfo.Name = 'TColor' then
            Result := 'Set to ' + ColorToString(FValue)
          else
            Result := 'Set to ' + VarToWideStr(FValue);
        end;
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

procedure Tcna2ActSetValue.Configure();
begin
  Tform_ConfigSetValue.Execute(FTypeInfo, FValue, FShowDialog, true, 'Choose value');
end;

constructor Tcna2ActSetValue.Create(ATypeInfo: PTypeInfo);
var
  sl : TStringList;
begin
  inherited;

  FShowDialog := false;

  sl := TStringList.Create;
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

procedure Tcna2ActSetValue.Execute(AEditor: IOTAFormEditor;
  AComponent: IOTAComponent; AProperty: WideString);
var
  Obj : TObject;
  NewValue : Variant;
  DoIt,
  EverythingOK : Boolean;
begin
  Obj := TObject(AComponent.GetComponentHandle);

  repeat
    EverythingOK := true;

    NewValue := FValue;

    if FShowDialog then
      DoIt := Tform_ConfigSetValue.Execute(FTypeInfo,
                                           NewValue,
                                           FShowDialog,
                                           false,
                                           'Choose value for ' + AProperty)
    else
      DoIt := true;

    try
      if DoIt then
        rttihSetPropertyValue(Obj, AProperty, NewValue);
    except
      on E : Exception do
      begin
        EverythingOK := false;
        MessageDlg(E.Message, mtError, [mbOK], 0);
      end;
    end;
  until EverythingOK;
end;

class function Tcna2ActSetValue.GetDisplayName: WideString;
begin
  Result := 'Set value/Show dialog';
end;

class function Tcna2ActSetValue.HasConfigDialog: Boolean;
begin
  Result := true;
end;

procedure Tcna2ActSetValue.Init(AValue: Variant; AShowDialog: Boolean);
begin
  FValue := AValue;
  FShowDialog := AShowDialog;
end;

procedure Tcna2ActSetValue.LoadFromSettings(APath: TSettingName);
begin
  FValue := cna2Settings.GetValue(APath + '/Value', FValue);
  FShowDialog := cna2Settings.GetValue(APath + '/ShowDialog', FShowDialog);
end;

procedure Tcna2ActSetValue.SaveToSettings(APath: TSettingName);
begin
  cna2Settings.SetValue(APath + '/Value', FValue);
  cna2Settings.SetValue(APath + '/ShowDialog', FShowDialog);
end;

end.
