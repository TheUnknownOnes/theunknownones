//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit UnitResEdVIEditor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, unitResourceVersionInfo, ImgList, StdCtrls, ExtCtrls, Grids, ValEdit,
  ComCtrls, ToolWin;

type
  TFormResEdVIEditor = class(TForm)
    ToolBar1: TToolBar;
    tbAddRow: TToolButton;
    tbDelRow: TToolButton;
    vlFixed: TValueListEditor;
    Panel1: TPanel;
    btnSave: TButton;
    ImageList1: TImageList;
    vlVariable: TValueListEditor;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    procedure vlFixedGetPickList(Sender: TObject; const KeyName: string;
      Values: TStrings);
    procedure btnSaveClick(Sender: TObject);
    procedure tbDelRowClick(Sender: TObject);
    procedure tbAddRowClick(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    RD : TVersionInfoResourceElement;
    procedure EditVersionInfoResource;
  end;

var
  FormResEdVIEditor: TFormResEdVIEditor;

implementation

uses
  StrUtils;

{$R *.dfm}

resourcestring
  rstVersionFormatError   = 'Error in version string';
  rstChangeFlags          = 'version flags change';
  rstChangeProductVersion = 'product version change';
  rstChangeFileVersion    = 'file version change';
  rstDeleteString         = 'delete version string';
  rstChangeString         = 'modify version string';
  rstChangeStringName     = 'modify version string name';
  rstAddString            = 'add version string';
  rstNewString            = 'New String %d';

{ TFormResEdVIEditor }

(*----------------------------------------------------------------------------*
 | function VersionToString ()                                                |
 |                                                                            |
 | Convert a version large integer to a string                                |
 |                                                                            |
 | Parameters:                                                                |
 |   version : TULargeInteger     The version integer to convert              |
 |                                                                            |
 | The function returns string representation of the version no               |
 *----------------------------------------------------------------------------*)
function VersionToString (version : TULargeInteger) : string;
begin
  with version do
    result := Format ('%d.%d.%d.%d', [HiWord (HighPart), LoWord (HighPart), HiWord (LowPart), LoWord (LowPart)]);
end;

(*----------------------------------------------------------------------------*
 | function StringToVersion ()                                                |
 |                                                                            |
 | Convert a version string to a large integer                                |
 |                                                                            |
 | Parameters:                                                                |
 |   version : string       The version string to convert                     |
 |                                                                            |
 | The function returns the integer representation of the version string      |
 *----------------------------------------------------------------------------*)
function StringToVersion (const version : string) : TULargeInteger;
var
  p : Integer;
  s : string;
  hh, h, l, ll : word;
  ok : boolean;
begin
  hh := 0;
  ll := 0;
  h := 0;
  l := 0;

  s := version;
  p := Pos ('.', s);
  ok := False;
  if p > 0 then
  begin
    hh := StrToInt (Copy (s, 1, p - 1));
    s := Copy (s, p + 1, MaxInt);
    p := Pos ('.', s);
    if p > 0 then
    begin
      h := StrToInt (Copy (s, 1, p - 1));
      s := Copy (s, p + 1, MaxInt);
      p := Pos ('.', s);
      if p > 0 then
      begin
        l := StrToInt (Copy (s, 1, p - 1));
        ll := StrToInt (Copy (s, p + 1, MaxInt));
        ok := True;
      end
    end
  end;

  if not ok then
    raise exception.Create (rstVersionFormatError);

  result.HighPart := 65536 * hh + h;
  result.LowPart := 65536 * l + ll;
end;

procedure TFormResEdVIEditor.btnSaveClick(Sender: TObject);
var
  i : Integer;
begin
  RD.EmptyFixedInfo;

  RD.FileVersion:=StringToVersion(vlFixed.Values['File Version']);
  RD.ProductVersion:=StringToVersion(vlFixed.Values['Product Version']);

  if AnsiUpperCase(vlFixed.Values['Debug'])='TRUE' then RD.FileFlags := RD.FileFlags + [ffDebug] else RD.FileFlags := RD.FileFlags - [ffDebug];
  if AnsiUpperCase(vlFixed.Values['Inferred'])='TRUE' then RD.FileFlags := RD.FileFlags + [ffInfoInferred] else RD.FileFlags := RD.FileFlags - [ffInfoInferred];
  if AnsiUpperCase(vlFixed.Values['Patched'])='TRUE' then RD.FileFlags := RD.FileFlags + [ffPatched] else RD.FileFlags := RD.FileFlags - [ffPatched];
  if AnsiUpperCase(vlFixed.Values['Pre Release'])='TRUE' then RD.FileFlags := RD.FileFlags + [ffPreRelease] else RD.FileFlags := RD.FileFlags - [ffPreRelease];
  if AnsiUpperCase(vlFixed.Values['Private Build'])='TRUE' then RD.FileFlags := RD.FileFlags + [ffPrivateBuild] else RD.FileFlags := RD.FileFlags - [ffPrivateBuild];
  if AnsiUpperCase(vlFixed.Values['Special Build'])='TRUE' then RD.FileFlags := RD.FileFlags + [ffSpecialBuild] else RD.FileFlags := RD.FileFlags - [ffSpecialBuild];

  if AnsiUpperCase(vlFixed.Values['DLL'])='TRUE' then RD.FileType := VFT_DLL else RD.FileType := VFT_APP;


  while RD.KeyCount>0 do
    RD.DeleteKey(0);

  RD.SetKeyValue('FileVersion', vlFixed.Values['File Version']);
  RD.SetKeyValue('ProductVersion', vlFixed.Values['Product Version']);

  for I := 0 to vlVariable.Strings.Count - 1 do
  begin
    if vlVariable.Strings.Names[i]<>'' then
      RD.SetKeyValue(vlVariable.Strings.Names[i], vlVariable.Strings.ValueFromIndex[i]);
  end;

  ModalResult:=mrOk;
end;

procedure TFormResEdVIEditor.EditVersionInfoResource;
var
  i : Integer;
begin
  vlFixed.Values['Product Version']:=VersionToString(RD.ProductVersion);
  vlFixed.Values['File Version']:=VersionToString(RD.FileVersion);

  vlFixed.Values['Debug'] :=IfThen(ffDebug in RD.FileFlags,'True','False');
  vlFixed.Values['Inferred'] := IfThen(ffInfoInferred in RD.FileFlags,'True','False');
  vlFixed.Values['Patched'] := IfThen(ffPatched in RD.FileFlags,'True','False');
  vlFixed.Values['Pre Release'] := IfThen(ffPreRelease in RD.FileFlags,'True','False');
  vlFixed.Values['Private Build'] := IfThen(ffPrivateBuild in RD.FileFlags,'True','False');
  vlFixed.Values['Special Build'] := IfThen(ffSpecialBuild in RD.FileFlags,'True','False');
  vlFixed.Values['DLL'] := IfThen(RD.FileType=VFT_DLL,'True','False');

  for I := 0 to RD.KeyCount - 1 do
  begin
    vlVariable.InsertRow(RD.Key[I].KeyName, RD.Key[I].Value, true);
  end;

  self.ShowModal;
end;

procedure TFormResEdVIEditor.tbAddRowClick(Sender: TObject);
var
  idx: Integer;
  dummy: Integer;
begin
  idx:=1;
  repeat
    if not vlVariable.FindRow('New String '+IntToStr(idx),dummy) then
    begin
      vlVariable.InsertRow('New String '+IntToStr(idx),'',true);
      break;
    end;

    inc(idx);
  until idx=MaxInt;

end;

procedure TFormResEdVIEditor.tbDelRowClick(Sender: TObject);
begin
  vlVariable.DeleteRow(vlVariable.Row);
end;

procedure TFormResEdVIEditor.ToolButton2Click(Sender: TObject);
var
  sl : TStrings;
  idx: Integer;
  row: Integer;
begin
  sl:=TStringList.Create;
  sl.CommaText:='CompanyName,FileDescription,InternalName,LegalCopyright,LegalTrademark,OriginalFileName,ProductName';

  for idx := 0 to sl.Count - 1 do
  begin
    if not vlVariable.FindRow(sl[idx],row)  then
      vlVariable.InsertRow(sl[idx],'',True);
  end;                                      

  sl.Free;
end;

procedure TFormResEdVIEditor.vlFixedGetPickList(Sender: TObject;
  const KeyName: string; Values: TStrings);
begin
  if (KeyName='Debug') or
     (KeyName='Inferred') or
     (KeyName='Patched') or
     (KeyName='DLL') or
     (KeyName='Pre Release') or
     (KeyName='Private Build') or
     (KeyName='Special Build') then
  begin
    Values.Clear;
    Values.CommaText:='True,False';
  end;
end;

end.
