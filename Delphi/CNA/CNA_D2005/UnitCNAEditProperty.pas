//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit UnitCNAEditProperty;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, TypInfo, StdCtrls, ExtCtrls, Themes;

type
  TFormCNAPropEdBase = class(TForm)
    panBottom: TPanel;
    btnCancel: TButton;
    btnOK: TButton;
    btnClearValue: TButton;
    cbShowInputBox: TCheckBox;
    panPropName: TPanel;
    PaintBox1: TPaintBox;
    procedure PaintBox1Paint(Sender: TObject);
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
{$region 'GetStringsOfChild'}
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
{$endregion}
begin
  GetStringsOfChildren(Self);
end;

function TFormCNAPropEdBase.NewValue: String;
begin
  Result:='';
end;

procedure TFormCNAPropEdBase.PaintBox1Paint(Sender: TObject);
var
  Element : TThemedElementDetails;
begin
  if ThemeServices.ThemesEnabled then
  begin
    Element:=ThemeServices.GetElementDetails(tebHeaderBackgroundNormal);

    ThemeServices.DrawElement(PaintBox1.Canvas.Handle,Element,PaintBox1.ClientRect);

    ThemeServices.DrawText(PaintBox1.Canvas.Handle,Element,panPropName.Caption,PaintBox1.ClientRect,4 or 32,0);
  end;
end;

end.
