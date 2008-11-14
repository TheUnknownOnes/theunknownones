//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uCNA2ActCreateObject;

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
  Graphics,
  Dialogs;

type
  Tcna2ActCreateObject = class(Tcna2Action)
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
  end;

implementation

{ Tcna2ActCreateObject }

function Tcna2ActCreateObject.AsString: WideString;
begin
  Result := 'Create object';
end;

class function Tcna2ActCreateObject.CanHandle(ATypeInfo: PTypeInfo): Boolean;
begin
  Result := ATypeInfo.Kind = tkClass;
end;

procedure Tcna2ActCreateObject.Configure;
begin
end;

constructor Tcna2ActCreateObject.Create(ATypeInfo: PTypeInfo);
begin
  inherited;

end;

procedure Tcna2ActCreateObject.Execute(AEditor: IOTAFormEditor;
  AComponent: IOTAComponent; AProperty: WideString);
var
  NewObj : IOTAComponent;
  Obj : TObject;
begin
  Obj := TObject(AComponent.GetComponentHandle);
  NewObj := AEditor.CreateComponent(AComponent.GetParent, FTypeInfo.Name, 0, 0, 0, 0);

  if Assigned(NewObj) then
  begin
    rttihSetPropertyValue(Obj, AProperty, Integer(NewObj.GetComponentHandle));

    NewObj.Select(true);
    AComponent.Select(true);
  end;
end;

class function Tcna2ActCreateObject.GetDisplayName: WideString;
begin
  Result := 'Create object';
end;

class function Tcna2ActCreateObject.HasConfigDialog: Boolean;
begin
  Result := false;
end;

procedure Tcna2ActCreateObject.LoadFromSettings(APath: TSettingName);
begin
end;

procedure Tcna2ActCreateObject.SaveToSettings(APath: TSettingName);
begin
end;

end.
