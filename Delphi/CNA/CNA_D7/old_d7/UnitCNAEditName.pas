//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit UnitCNAEditName;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TformName = class(TForm)
    panBottom: TPanel;
    btnCancel: TButton;
    btnOK: TButton;
    lblNewName: TLabel;
    edName: TEdit;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    SelFrom,
    SelLength : Integer;
  end;


implementation

uses unitCNALangs;

{$R *.dfm}

procedure TformName.FormCreate(Sender: TObject);
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

procedure TformName.FormShow(Sender: TObject);
begin
  edName.SetFocus;
  edName.SelStart:=SelFrom;
  edName.SelLength:=SelLength;
end;

end.
