//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit UnitResEdDotNetEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, unitResourceDotNet, StdCtrls, ExtCtrls;

type
  TResEdDotNetEdit = class(TForm)
    MemoDesc: TMemo;
    MemoValue: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    LabelName: TLabel;
    LabelType: TLabel;
    LabelMime: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
  private
    { Private-Deklarationen }
  public
  end;

function EditDotNetResource(res: TDotNetResourceElement): Boolean;

implementation

uses unitResourceElement;

{$R *.dfm}

{ TResEdDotNetEdit }

function EditDotNetResource(
  res: TDotNetResourceElement): Boolean;
var
  myForm : TResEdDotNetEdit;
begin
  result:=False;

  myForm:=TResEdDotNetEdit.Create(nil);
  try
    myForm.LabelName.Caption:=res.ResourceName;
    myForm.LabelType.Caption:=res.rType;
    myForm.LabelMime.Caption:=res.MimeType;
    myForm.MemoDesc.Text:=res.Comment;
    myForm.MemoValue.Text:=res.Text;

    if (myForm.ShowModal=mrOk) then
    begin
      res.Comment:=myForm.MemoDesc.Text;
      res.Text:=myForm.MemoValue.Text;
      result:=True;
    end;
  finally
    myForm.Release;
  end;
end;

end.
