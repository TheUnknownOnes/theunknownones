unit uCNA2ActSetValueConfig;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  TypInfo, StdCtrls, ExtCtrls,
  uCNA2ValueEditors;

type
  Tform_ConfigSetValue = class(TForm)
    pan_Buttons: TPanel;
    btn_OK: TButton;
    btn_Cancel: TButton;
    procedure FormShow(Sender: TObject);
  private
    FEditor : Tcna2ValueEditor;
  public
    procedure Init (ATypeInfo : PTypeInfo; AValue : Variant);
    function GetSelectedValue : Variant;

    class function Execute(ATypeInfo : PTypeInfo; AValue : Variant) : Variant;
  end;


implementation

{$R *.dfm}

{ Tform_ConfigSetValue }

class function Tform_ConfigSetValue.Execute(ATypeInfo: PTypeInfo;
  AValue: Variant): Variant;
var
  Form : Tform_ConfigSetValue;
begin
  Form := Tform_ConfigSetValue.Create(nil);
  try
    form.Init(ATypeInfo, AValue);
    if Form.ShowModal = mrOk then
      Result := Form.GetSelectedValue
    else
      Result := AValue;
  finally
    Form.Free;
  end;
end;

procedure Tform_ConfigSetValue.FormShow(Sender: TObject);
begin
  if Assigned(FEditor) then
    SelectNext(FEditor, true, true);
end;

function Tform_ConfigSetValue.GetSelectedValue: Variant;
begin
  if Assigned(FEditor) then
    Result := FEditor.Value
  else
    Result := null;
end;

procedure Tform_ConfigSetValue.Init(ATypeInfo: PTypeInfo; AValue: Variant);
var
  EditorClass : Tcna2ValueEditorClass;
begin
  FEditor := nil;
  
  if cna2ValueEditors.FindByTypeInfo(ATypeInfo, EditorClass) then
  begin
    FEditor := EditorClass.Create(self);
    FEditor.Parent := Self;
    ClientHeight := pan_Buttons.Height + FEditor.GetDesiredHeight;
    FEditor.TabOrder := 0;
    FEditor.Init(ATypeInfo, AValue, false);
  end;
end;

end.
