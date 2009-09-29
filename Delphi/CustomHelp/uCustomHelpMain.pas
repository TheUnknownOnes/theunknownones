unit uCustomHelpMain;

interface

uses
  Classes,
  Forms,
  Dialogs,
  HelpIntfs, Windows;

type
  TCustomHelp = class
  protected
    FHelpManager : IHelpManager;
  public
    ViewerID : Integer;
    
    constructor Create();
    destructor Destroy(); override;

    class function DecodeURL(const URL: String; out Caption: String;
      out Description: String; out Link: String): boolean;
  end;

  TMyViewer = class(TInterfacedObject, ICustomHelpViewer)
  private
    {$REGION 'ICustomHelpViewer'}
    function  GetViewerName : String;
    function  UnderstandsKeyword(const HelpString: String): Integer;
    function  GetHelpStrings(const HelpString: String): TStringList;
    function  CanShowTableOfContents : Boolean;
    procedure ShowTableOfContents;
    procedure ShowHelp(const HelpString: String);
    procedure NotifyID(const ViewerID: Integer);
    procedure SoftShutDown;
    procedure ShutDown;
    {$ENDREGION}
  end;


implementation

uses
  SysUtils, uCustomHelpSelector, StrUtils, ShellAPI;

const
  CPROT = 'CustomHelp://';

var
  ch : TCustomHelp;
  vi : Integer;


{ TMyViewer }

function TMyViewer.CanShowTableOfContents: Boolean;
begin
  Result := true;
end;

function TMyViewer.GetHelpStrings(const HelpString: String): TStringList;

  function EncodedHelpString: String;
  var
    idx: integer;
  begin
    Result:='';
    for idx := 1 to Length(HelpString)do
      Result:=Result + '%'+Format('%.2x', [Ord(HelpString[idx])]);
  end;
  
begin
  Result := TStringList.Create;
  Result.Add(CPROT+'DelphiReference|Suche "'+HelpString+'" über Daniels Cool Tool|http://ref.dp200x.de/dp_reference.php?sourceid=captaincaveman&wsproencoding=ISO-8859-1&securitytoken=guest&tabbed=1&sbutton=Search&searchD2009=1&searchDPCodeLib=1&searchDPForums=1&searchMSDN=1&query='+EncodedHelpString);
  Result.Add(CProt+'Koders.com|Suche "'+HelpString+'" über koders.com|http://www.koders.com/default.aspx?submit=Search&la=Delphi&li=*&s='+EncodedHelpString);
  Result.Add(CProt+'google Code|Suche "'+HelpString+'" über google codesearch|http://www.google.com/codesearch?btnG=Code+suchen&hl=de&as_lang=pascal&as_license_restrict=i&as_license=&as_package=&as_filename=&as_case=&as_q='+EncodedHelpString);
end;

function TMyViewer.GetViewerName: String;
begin
  Result := 'MyViewer';
end;

procedure TMyViewer.NotifyID(const ViewerID: Integer);
begin
  vi := ViewerID;
end;

procedure TMyViewer.ShowHelp(const HelpString: String);
var
  c,d,u: String;
begin
  if TCustomHelp.DecodeURL(HelpString, c, d, u) then
  begin
    ShellExecute(Application.Handle, 'open', PChar(u), '', '', SW_SHOWNORMAL);
  end
end;

procedure TMyViewer.ShowTableOfContents;
begin
end;

procedure TMyViewer.ShutDown;
begin
end;

procedure TMyViewer.SoftShutDown;
begin  
end;

function TMyViewer.UnderstandsKeyword(const HelpString: String): Integer;
var
  hs : IHelpSystem;
begin
  Result := 1;

  if GetHelpSystem(hs) then
  begin
    hs.AssignHelpSelector(THelpSelector.Create);
  end;
end;

{ TCustomHelp }

constructor TCustomHelp.Create;
var
  intf : ICustomHelpViewer;
begin
  FHelpManager := nil;
  intf:=TMyViewer.Create;

  RegisterViewer(intf, FHelpManager);
  ViewerID:=vi;
end;

class function TCustomHelp.DecodeURL(const URL: String; out Caption,
  Description, Link: String): boolean;
var
  sl : TStringList;
begin
  Result:=False;
  if AnsiStartsText(CPROT, URL) then
  begin
    Result:=True;
    sl:=TStringList.Create;
    sl.QuoteChar:=#0;
    sl.Delimiter:='|';
    sl.StrictDelimiter:=True;
    sl.DelimitedText:=Copy(URL, Length(CProt)+1, Length(URL));
    Caption:=Sl[0];
    Description:=sl[1];
    Link:=sl[2];
    sl.Free;
    Result:=True;
  end;           
end;

destructor TCustomHelp.Destroy;
begin
  if Assigned(FHelpManager) then
    FHelpManager.Release(ViewerID);
  
  inherited;
end;

initialization
  ch:=TCustomHelp.Create;

finalization
  ch.Free;
end.
