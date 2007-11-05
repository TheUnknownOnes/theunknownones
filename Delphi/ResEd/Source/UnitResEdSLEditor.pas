//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit UnitResEdSLEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ToolWin, Grids, ValEdit, ImgList, unitResourceMessages,
  StrUtils, StdCtrls, ExtCtrls;

type
  TFormResEdSLEditor = class(TForm)
    vlEditor: TValueListEditor;
    ToolBar1: TToolBar;
    tbDelRow: TToolButton;
    tbAddRow: TToolButton;
    ImageList1: TImageList;
    Panel1: TPanel;
    btnSave: TButton;
    procedure btnSaveClick(Sender: TObject);
    procedure tbDelRowClick(Sender: TObject);
    procedure tbAddRowClick(Sender: TObject);
    procedure vlEditorValidate(Sender: TObject; ACol, ARow: Integer;
      const KeyName, KeyValue: string);
  private
    
  public
    RD : TTextResourceElement;
    procedure EditTextResource;
  end;

implementation

{$R *.dfm}

{ TFormResEdSLEditor }

var
  IDFormat : String;

procedure TFormResEdSLEditor.EditTextResource;
var
  i : Integer;
  s : String;
begin
  vlEditor.Strings.Clear;

  if RD is TMessageResourceElement then
    IDFormat:='%.8d'
  else
  begin
    IDFormat:='%d';
    ToolBar1.Visible:=False;
    vlEditor.KeyOptions:=[];
  end;
   
  for i:=0 to RD.Count-1 do
  begin
    s:=RD.Strings[i];
    s:=AnsiReplaceStr(s,#13#10,'\n');
    vlEditor.Strings.Add(
       Format(IDFormat,[RD.Ids[i]])+
        '='+
       s);    
  end;

  self.ShowModal;
end;

procedure TFormResEdSLEditor.vlEditorValidate(Sender: TObject; ACol,
  ARow: Integer; const KeyName, KeyValue: string);
var
  dummy : integer;
begin
  if ACol=0 then
    if TryStrToInt(KeyName, dummy) = False then
      raise Exception.Create('invalid string ID ... must be a number');
end;

procedure TFormResEdSLEditor.tbAddRowClick(Sender: TObject);
begin
  vlEditor.InsertRow('','',false);
end;

procedure TFormResEdSLEditor.tbDelRowClick(Sender: TObject);
begin
  vlEditor.DeleteRow(vlEditor.Row);
end;

procedure TFormResEdSLEditor.btnSaveClick(Sender: TObject);
var
  i : integer;
  s : String;
begin
  while RD.Count>vlEditor.Strings.Count do
    rd.Delete(RD.Count-1);

  for i:=0 to vlEditor.Strings.Count-1 do
  begin       
    s:=vlEditor.Strings.ValueFromIndex[i];
    s:=AnsiReplaceText(s,'\n',#13#10);
    RD.Strings[i]:=s;
    RD.Ids[i]:=StrToInt(vlEditor.Strings.Names[i]);
  end;

  ModalResult:=mrOk;
end;

end.
