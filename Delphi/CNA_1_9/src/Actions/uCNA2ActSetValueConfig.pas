//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************
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
    cb_ShowDialog: TCheckBox;
    procedure FormShow(Sender: TObject);
  private
    FEditor : Tcna2ValueEditor;
  public
    procedure Init (ATypeInfo : PTypeInfo;
                    AValue : Variant;
                    AShowDialog : Boolean;
                    AIsConfig : Boolean;
                    ACaption : TCaption);
    function GetSelectedValue : Variant;
    function GetShowDialog : Boolean;

    class function Execute(ATypeInfo : PTypeInfo;
                           var AValue : Variant;
                           var AShowDialog : Boolean;
                           AIsConfig : Boolean;
                           ACaption : TCaption) : Boolean;
  end;


implementation

{$R *.dfm}

{ Tform_ConfigSetValue }

class function Tform_ConfigSetValue.Execute(ATypeInfo : PTypeInfo;
                                             var AValue : Variant;
                                             var AShowDialog : Boolean;
                                             AIsConfig : Boolean;
                                             ACaption : TCaption): Boolean;
var
  Form : Tform_ConfigSetValue;
begin
  Form := Tform_ConfigSetValue.Create(nil);
  try
    form.Init(ATypeInfo, AValue, AShowDialog, AIsConfig, ACaption);
    Result := Form.ShowModal = mrOk;
    if Result then
    begin
      AValue := Form.GetSelectedValue;
      AShowDialog := Form.GetShowDialog;
    end;
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

function Tform_ConfigSetValue.GetShowDialog: Boolean;
begin
  Result := cb_ShowDialog.Checked;
end;

procedure Tform_ConfigSetValue.Init(ATypeInfo: PTypeInfo;
                                    AValue: Variant;
                                    AShowDialog : Boolean;
                                    AIsConfig : Boolean;
                                    ACaption : TCaption);
var
  EditorClass : Tcna2ValueEditorClass;
begin
  FEditor := nil;

  Caption := ACaption;

  if cna2ValueEditors.FindByTypeInfo(ATypeInfo, EditorClass) then
  begin
    FEditor := EditorClass.Create(self);
    FEditor.Parent := Self;
    FEditor.TabOrder := 0;
    cb_ShowDialog.Visible := AIsConfig;
    cb_ShowDialog.Checked := AShowDialog;
    FEditor.Init(ATypeInfo, AValue, not AIsConfig);
    ClientHeight := pan_Buttons.Height + FEditor.GetDesiredHeight;
  end;
end;

end.
