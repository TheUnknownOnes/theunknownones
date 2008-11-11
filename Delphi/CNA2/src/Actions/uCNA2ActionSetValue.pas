unit uCNA2ActionSetValue;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  uCNA2Actions,
  Typinfo,
  uSettingsBase,
  ToolsAPI, StdCtrls,
  uCNA2ValueEditor,
  uCNA2FrameValueEditorBase;

type
  TCNA2ActionSetValue = class(TCNA2Action)
  private
    Value : Variant;
  public
    procedure Execute(AComponent : IOTAComponent; AProperty : WideString); override;

    procedure SaveToSettings(ASettingsPath : TSettingName); override;
    procedure LoadFromSettings(ASettingsPath : TSettingName); override;

    function CanBeConfigured : Boolean; override;
    procedure Configure(ATypeInfo : PTypeInfo); override;

    function AsString : String; override;
  end;

  TCNA2ActionProviderSetValue = class(TCNA2ActionProvider)
  protected
    function GetID : TGUID; override;
    function GetName : WideString; override;
  public
    function CreateAction : TCNA2Action; override;
    function Handles(ATypeInfo : PTypeInfo) : Boolean; override;
  end;

  Tform_ActionSetValueConfig = class(TForm)
    Label1: TLabel;
    procedure FormShow(Sender: TObject);
  private
    FEditor : Tframe_ValueEditorBase;
  public
    Value : Variant;
    TypeInfo : PTypeInfo;
  end;

implementation

var
  Provider : TCNA2ActionProviderSetValue;

{$R *.dfm}

{ TCNA2ActionSetValue }

function TCNA2ActionSetValue.AsString: String;
begin
  Result := 'Set value';
end;

function TCNA2ActionSetValue.CanBeConfigured: Boolean;
begin
  Result := true;
end;

procedure TCNA2ActionSetValue.Configure(ATypeInfo : PTypeInfo);
var
  Form : Tform_ActionSetValueConfig;
begin
  Form := Tform_ActionSetValueConfig.Create(nil);
  try
    form.Value := Value;
    form.TypeInfo := ATypeInfo;
    if Form.ShowModal = mrOk then
      Value := Form.Value;
  finally
    Form.Free;
  end;
end;

procedure TCNA2ActionSetValue.Execute(AComponent: IOTAComponent;
  AProperty: WideString);
begin
  inherited;

end;

procedure TCNA2ActionSetValue.LoadFromSettings(ASettingsPath: TSettingName);
begin
  inherited;

end;

procedure TCNA2ActionSetValue.SaveToSettings(ASettingsPath: TSettingName);
begin
  inherited;

end;

{ TCNA2ActionProviderSetValue }

function TCNA2ActionProviderSetValue.CreateAction: TCNA2Action;
begin
  Result := TCNA2ActionSetValue.Create(Self);
end;

function TCNA2ActionProviderSetValue.GetID: TGUID;
begin
  Result := StringToGUID('{3B792B3D-F551-4941-8012-A08590421B62}');
end;

function TCNA2ActionProviderSetValue.GetName: WideString;
begin
  Result := 'Set value'
end;

function TCNA2ActionProviderSetValue.Handles(ATypeInfo: PTypeInfo): Boolean;
begin

  Result := ATypeInfo.Kind in [tkInteger,
                               tkChar,
                               tkEnumeration,
                               tkFloat,
                               tkString,
                               tkSet,
                               tkLString,
                               tkInt64];
end;

procedure Tform_ActionSetValueConfig.FormShow(Sender: TObject);
var
  FrameClass : TValueEditorFrameClass;
begin
  if cna2ValueEditor.GetEditorClass(TypeInfo.Kind, FrameClass) then
  begin
    FEditor := FrameClass.Create(Self);
    FEditor.Parent := Self;
    FEditor.Align := alClient;
    FEditor.Value := Value;
  end;
end;

end.
