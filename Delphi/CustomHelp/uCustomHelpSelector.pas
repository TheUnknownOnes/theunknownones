unit uCustomHelpSelector;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, HelpIntfs, OleCtrls, StdCtrls, ExtCtrls, ActiveX, mshtml, ComCtrls;

type
  THelpSelector = class(TInterfacedObject, IHelpSelector)
  protected
    function SelectKeyword(Keywords: TStrings) : Integer;
    function TableOfContents(Contents: TStrings): Integer;
  public
  end;

  TFormHelpSelector = class(TForm)
    ListBox1: TListView;
    Panel1: TPanel;
    btnOk: TButton;
    procedure ListBox1DblClick(Sender: TObject);
    procedure ListBox1KeyPress(Sender: TObject; var Key: Char);
  private
    procedure MakeFriendlyCaptions;
  protected
  public
    { Public-Deklarationen }
  end;


implementation

uses
  UrlMon, StrUtils, ComObj, ShellAPI, uCustomHelpMain;

{$R *.dfm}

function THelpSelector.SelectKeyword(Keywords: TStrings): Integer;
var
  idx : integer;
begin
  with TFormHelpSelector.Create(nil) do
  begin
    for idx := 0 to KeyWords.Count - 1 do
      with ListBox1.Items.Add do
      begin
        Caption:=Keywords[idx];
        SubItems.Add('');
        SubItems.Add('')
      end;

    MakeFriendlyCaptions;
    Result:=-1;
    ListBox1.ItemIndex:=0;
    if ShowModal=mrOk then
      Result:=ListBox1.ItemIndex;
    Release;
  end;
end;

function THelpSelector.TableOfContents(Contents: TStrings): Integer;
begin
  Result:=0;
end;

function DownLoadInternetFile(URL : WideString) : String;
var Stream : IStream;
    Buffer : array of byte;
  numBytes : longint;
        hr : HRESULT;
    outStream : TStringStream;
begin
  if Pos('#',URL)>0 then
    Delete(URL, Pos('#',URL), Length(URL));

  SetLength( Buffer, 8192 );
  outStream:=TStringStream.Create('');

  try
  if (URLOpenBlockingStreamW( NIL, PWideChar(URL), Stream, 0, NIL) = S_OK) then
  begin
     hr:= Stream.Read (@Buffer[0], SizeOf(Buffer), @numBytes );
     while (hr = S_OK) do
     begin
       outStream.Write( Buffer[0], numBytes );
       hr:= Stream.Read (@Buffer[0], SizeOf(Buffer), @numBytes );
     end;

    result:= outStream.DataString;
  end
  else
    result:= '';

  finally
    outStream.Free;
  end;

  SetLength( Buffer, 0 );
end;

procedure TFormHelpSelector.ListBox1DblClick(Sender: TObject);
var
  URL : String;
begin
  URL:=ListBox1.ItemFocused.SubItems[1];
  if URL<>EmptyStr then
    ShellExecute(self.Handle, 'open', PChar(URL), '', '', SW_SHOWNORMAL);

  ModalResult:=mrOk;
end;

procedure TFormHelpSelector.ListBox1KeyPress(Sender: TObject; var Key: Char);
begin
  if Key=#13 then
    ListBox1DblClick(Sender);
end;

procedure TFormHelpSelector.MakeFriendlyCaptions;
var
  idx: Integer;
  intf : DispHTMLDocument;
  doc : IHTMLDocument2;
  c, d, u : String;
begin
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
  for idx := 0 to ListBox1.Items.Count - 1 do
  begin
    if TCustomHelp.DecodeURL(ListBox1.Items[idx].Caption, c, d, u) then
    begin
      ListBox1.Items[idx].Caption:=c;
      ListBox1.Items[idx].subItems[0]:=d;
      //ListBox1.Items[idx].subItems[1]:=u;
    end
    else
    begin
      if AnsiStartsText('ms-help://', ListBox1.Items[idx].Caption) then
      begin
        intf:=CoHTMLDocument.Create;
        Intf.write(DownLoadInternetFile(ListBox1.Items[idx].Caption));
        doc:=intf as IHTMLDocument2;

        ListBox1.Items[idx].SubItems[0]:=ListBox1.Items[idx].Caption;
        ListBox1.Items[idx].SubItems[1]:=ListBox1.Items[idx].Caption;
        ListBox1.Items[idx].Caption:=UTF8Decode(doc.title);
      end;
    end;

  end;
end;

end.
