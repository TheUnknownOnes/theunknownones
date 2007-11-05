//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit UnitCNAEditProperty;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TypInfo, StdCtrls, ExtCtrls;

type
  TFormCNAPropEdBase = class(TForm)
    panBottom: TPanel;
    btnCancel: TButton;
    btnOK: TButton;
    btnClearValue: TButton;
    cbShowInputBox: TCheckBox;
    panPropName: TPanel;
    procedure FormCreate(Sender: TObject);
  private
  protected
    function NewValue: String; virtual;
  public
    procedure GetNewValue(OldValue : String;
                          PropertyName : String;
                          PropInfoString : String;
                          IsRuntime : Boolean;
                          var Result : String;
                          var ShowInputBox : Boolean); virtual;
  end;

implementation

uses unitCNALangs;

{$R *.dfm}

{ TForm1 }

procedure TFormCNAPropEdBase.GetNewValue(OldValue, PropertyName,
  PropInfoString: String;IsRuntime : Boolean; var Result: String; var ShowInputBox : Boolean);
var
  Res : TModalResult;
begin
  Self.Caption:=GetLangString('SpecifyProperty');
  panPropName.Caption:=PropertyName;
  if (IsRuntime) then
  begin
    cbShowInputBox.Visible:=false;
    btnClearValue.Visible:=false;
  end;
  cbShowInputBox.Checked:=ShowInputBox;
  Res:=Self.ShowModal;
  ShowInputBox:=cbShowInputBox.Checked;
  
  case Res of
    mrOk:
      Result:=NewValue;
    mrCancel:
      Result:='<!--CANCELOFDLG-->';
    mrIgnore:
      Result:='<!--NILORNULL-->';
  else
    Result:=OldValue;
  end;
end;

procedure TFormCNAPropEdBase.FormCreate(Sender: TObject);
{.$Region 'GetStringsOfChild'}
  procedure GetStringsOfChildren(AParent: TComponent);
  var
    i : Integer;
  begin
    for i:=0 to AParent.ComponentCount-1 do
    begin
      if (not LangStringAvailable(AParent.Components[i],'Caption')) then
        Continue;
      if (AParent.Components[i] is TLabel) then
        TLabel(AParent.Components[i]).Caption:=GetLangString(AParent.Components[i],'Caption');
       if (not (AParent.Components[i] is TWincontrol)) then
        Continue;
      SetWindowText(TWinControl(AParent.Components[i]).Handle,
                      PChar(GetLangString(AParent.Components[i],'Caption')));
      if (AParent.Components[i].ComponentCount>0) then
        GetStringsOfChildren(AParent.Components[i]);
    end;
  end;
{.$EndRegion}
begin
  GetStringsOfChildren(Self);
end;

function TFormCNAPropEdBase.NewValue: String;
begin
  Result:='';
end;



end.
