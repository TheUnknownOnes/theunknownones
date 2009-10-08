unit unit_FormSB;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, HTMLLite, xmldom, XMLIntf, XMLDoc, strutils, msxmldom,
  StdCtrls, ComCtrls, ImgList, ToolWin, JvComponentBase, JvFormMagnet,
  JvTrayIcon, Menus, ShellAPI, Registry, pngimage, xpman, Buttons, uPNGBitmap;

type
  TSmiley = record
    Code : String;
    Imag : String;
    Hint : String;
  end;
  TSmilies = Array of TSmiley;
  
  Tform_SB = class(TForm)
    pan_Input: TPanel;
    SB: ThtmlLite;
    tm_RefreshSB: TTimer;
    Label1: TLabel;
    ed_Message: TEdit;
    TB: TToolBar;
    iml_TB: TImageList;
    btn_Send: TToolButton;
    btn_ToolButton1: TToolButton;
    btn_Smilies: TToolButton;
    btn_ToolButton2: TToolButton;
    btn_Settings: TToolButton;
    tm_Alpha: TTimer;
    tm_CheckWindowStyle: TTimer;
    Tray: TJvTrayIcon;
    pum_Tray: TPopupMenu;
    mi_Exit: TMenuItem;
    pum_SB: TPopupMenu;
    mi_PM: TMenuItem;
    mi_Map: TMenuItem;
    tm_Flood: TTimer;
    PB_Flood: TProgressBar;
    btn_MinInput: TSpeedButton;
    procedure tm_FloodTimer(Sender: TObject);
    procedure mi_MapClick(Sender: TObject);
    procedure mi_PMClick(Sender: TObject);
    procedure mi_PMAdvancedDrawItem(Sender: TObject; ACanvas: TCanvas;
      ARect: TRect; State: TOwnerDrawState);
    procedure SBRightClick(Sender: TObject; Parameters: TRightClickParameters);
    procedure mi_PMMeasureItem(Sender: TObject; ACanvas: TCanvas; var Width,
      Height: Integer);
    procedure SBHotSpotClick(Sender: TObject; const SRC: string;
      var Handled: Boolean);
    procedure TrayDblClick(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure mi_ExitClick(Sender: TObject);
    procedure tm_CheckWindowStyleTimer(Sender: TObject);
    procedure tm_AlphaTimer(Sender: TObject);
    procedure btn_SettingsClick(Sender: TObject);
    procedure ed_MessageKeyPress(Sender: TObject; var Key: Char);
    procedure btn_SendClick(Sender: TObject);
    procedure btn_SmiliesClick(Sender: TObject);
    procedure SBImageRequest(Sender: TObject; const SRC: string;
      var Stream: TMemoryStream);
    procedure tm_RefreshSBTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure btn_MinInputClick(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    NextRefresh : Integer;
    smilies : TSmilies;
    LastShout : String;

    procedure BuildSmiliesList;
    function BuildHTMLShoutBox(XML: String): String;
    procedure SetWindowStyle(AActive : Boolean);

    procedure OpenHttpSite(AURL: String);
  end;

var
  form_SB: Tform_SB;

implementation

uses unit_Data, Unit_FormSmileySelect, unit_FormOptions, IniFiles,
  unit_FormMain, Types;

{$R *.dfm}

procedure Tform_SB.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Self.ModalResult:=mrOk;
  SetLength(smilies,0);
  Data.Settings.WriteInteger('Form','Left',Self.Left);
  Data.Settings.WriteInteger('Form','Top',Self.Top);
  Data.Settings.WriteInteger('Form','Width',Self.Width);
  Data.Settings.WriteInteger('Form','Height',Self.Height);
  Data.Settings.WriteBool('Form','InputMax',(pan_Input.Height>0));

  Data.Logout;
end;

procedure Tform_SB.FormCreate(Sender: TObject);
begin
  Self.Color:=COLOR_Form;
  NextRefresh:=1;
  LastShout:='';
  BuildSmiliesList;

  Self.Left:=Data.Settings.ReadInteger('Form','Left',Self.Left);
  Self.Top:=Data.Settings.ReadInteger('Form','Top',Self.Top);
  Self.Width:=Data.Settings.ReadInteger('Form','Width',Self.Width);
  Self.Height:=Data.Settings.ReadInteger('Form','Height',Self.Height);

  if Data.Settings.ReadBool('Form','InputMax',true) then
  begin
    GetAsBitmap(HInstance,'UP',clBtnFace,btn_MinInput.Glyph)
  end
  else
  begin
    GetAsBitmap(HInstance,'DOWN',clBtnFace,btn_MinInput.Glyph);
    pan_Input.Height:=0;
  end;
end;

procedure Tform_SB.mi_ExitClick(Sender: TObject);
begin
  Self.ModalResult:=mrOk;
end;

procedure Tform_SB.mi_MapClick(Sender: TObject);
begin
  OpenHttpSite(Format(URL_MAP,[pum_SB.Tag]));
end;

procedure Tform_SB.mi_PMAdvancedDrawItem(Sender: TObject; ACanvas: TCanvas;
  ARect: TRect; State: TOwnerDrawState);
var
  PNG : TPNGObject;
  x, y : Integer;
begin
  PNG:=TPNGObject.Create;
  PNG.LoadFromResourceName(HInstance,(Sender as TMenuItem).Caption);
  if odSelected in State then
    ACanvas.Brush.Color:=clMenuHighlight
  else
    ACanvas.Brush.Color:=clMenu;

  ACanvas.FillRect(ARect);
  x:=((ARect.right-Arect.left) div 2)-(png.Width div 2);
  y:=Arect.Top+((ARect.Bottom-Arect.Top) div 2)-(png.Height div 2);
  ACanvas.Draw(x,y,PNG);
  PNG.Free;
end;

procedure Tform_SB.mi_PMClick(Sender: TObject);
begin
  OpenHttpSite(Format(URL_PN,[pum_SB.Tag]));
end;

procedure Tform_SB.mi_PMMeasureItem(Sender: TObject; ACanvas: TCanvas;
  var Width, Height: Integer);
var
  PNG : TPNGObject;
begin
  PNG:=TPNGObject.Create;
  png.LoadFromResourceName(HInstance,(Sender as TMenuItem).Caption);
  Height:=png.Height;
  Width:=png.Width;
  PNG.Free;
end;

procedure Tform_SB.SBHotSpotClick(Sender: TObject; const SRC: string;
  var Handled: Boolean);
begin
  if AnsiStartsText('http://',src) then
    OpenHttpSite(src);
end;

procedure Tform_SB.SBImageRequest(Sender: TObject; const SRC: string;
  var Stream: TMemoryStream);
var
  LocalFileName : String;
begin
  LocalFileName:=Data.Dir_Cache+
                 ExtractFileName(StringReplace(Src,'/','\',[rfReplaceAll]));
  if not FileExists(LocalFileName) then
  begin
    Data.GetFile(URL_ROOT+src,LocalFileName);
  end;

  Stream:=TMemoryStream.Create;
  Stream.LoadFromFile(LocalFileName);
end;

procedure Tform_SB.SBRightClick(Sender: TObject;
  Parameters: TRightClickParameters);
var
  CP : TPoint;
  UID : String;
  P : INteger;
begin
  if AnsiStartsText(URL_ROOT+'profile.php?mode=viewprofile',Parameters.URL) then
  begin
    P:=Pos('=u',ReverseString(Parameters.URL));
    UID:=AnsiRightStr(Parameters.URL,P-1);
    pum_SB.Tag:=StrToIntDef(UID,0);
    GetCursorPos(CP);
    pum_SB.Popup(Cp.X,CP.Y);
  end;
end;

procedure Tform_SB.SetWindowStyle(AActive: Boolean);
begin
  if data.Settings.ReadBool('Options','StayOnTop',true) then
  begin
    if GetActiveWindow<>Self.Handle then
    begin
      self.FormStyle:=fsNormal;
      Self.FormStyle:=fsStayOnTop;
      Application.MainForm.FormStyle:=fsStayOnTop;
    end;
  end
  else
  begin
    Self.FormStyle:=fsNormal;
    Application.MainForm.FormStyle:=fsNormal;
  end;
  if AActive then
  begin
    tm_Alpha.Tag:=Data.Settings.ReadInteger('Options','AlphaActive',255);
    tm_Alpha.Enabled:=true;
  end
  else
  begin
    tm_Alpha.Tag:=Data.Settings.ReadInteger('Options','AlphaInactive',180);
    tm_Alpha.Enabled:=true;
  end;
end;

procedure Tform_SB.tm_AlphaTimer(Sender: TObject);
var
  ALPHA_INTERVAL : INteger;
begin
  ALPHA_INTERVAL:=Data.Settings.ReadInteger('Options','Fading',10);

  if Self.AlphaBlendValue>tm_Alpha.Tag+ALPHA_INTERVAL then
    Self.AlphaBlendValue:=Self.AlphaBlendValue-ALPHA_INTERVAL
  else if Self.AlphaBlendValue<tm_Alpha.Tag-ALPHA_INTERVAL then
    Self.AlphaBlendValue:=Self.AlphaBlendValue+ALPHA_INTERVAL
  else
  begin
    Self.AlphaBlendValue:=tm_Alpha.Tag;
    tm_Alpha.Enabled:=false;
  end;
end;

procedure Tform_SB.tm_CheckWindowStyleTimer(Sender: TObject);
var
  CP : TPoint;
begin
  GetCursorPos(CP);

  if  (Cp.x>=Self.Left) and
      (CP.X<=Self.Left+Self.Width) and
      (Cp.Y>=Self.Top) and
      (CP.Y<=Self.Top+Self.Height) then
  begin
    SetWindowStyle(true);
  end
  else
  begin
    if Application.Active then
      SetWindowStyle(true)
    else
      SetWindowStyle(false);
  end;

  if Self.WindowState=wsMinimized then
    Self.Hide;

  form_Login.Hide;
end;


procedure Tform_SB.tm_FloodTimer(Sender: TObject);
begin
  PB_Flood.Position:=PB_Flood.Position-1;
  if PB_Flood.Position=0 then
  begin
    tm_Flood.Enabled:=false;
    btn_Send.Enabled:=true;
  end;
end;

procedure Tform_SB.tm_RefreshSBTimer(Sender: TObject);
begin
  Dec(NextRefresh);
  if (NextRefresh=0) then
  begin
    NextRefresh:=REFRESH_INTERVAL;
    SB.LoadFromString(BuildHTMLShoutBox(Data.GetSB),'');
  end;
  SetWindowText(Self.Handle,Pchar(Format(STR_FormCaption,[NextRefresh])));
end;

procedure Tform_SB.TrayDblClick(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Self.Visible then
    Self.WindowState:=wsMinimized
  else
  begin
    SetForegroundWindow(Self.Handle);

    self.WindowState:=wsNormal;
    Self.Show;
  end;
end;

procedure Tform_SB.BuildSmiliesList;
var
  doc : TXMLDocument;
  root: IXMLNode;
  smiley: IXMLNode;
  i     : Integer;
  XML : TStrings;
begin
  XML:=TStringList.Create;
  XML.LoadFromFile(Data.File_Smilies_xml);

  doc:=TXMLDocument.Create(self);
  doc.DOMVendor:=MSXML_DOM;
  doc.XML.Text:=XML.Text;
  xml.Free;
  doc.Active:=true;

  root:=doc.DocumentElement;

  SetLength(Smilies, root.ChildNodes.Count);

  for i := 0 to root.ChildNodes.Count - 1 do
  begin
    smiley:=root.ChildNodes[i];

    smilies[i].Code:=smiley.ChildNodes['code'].Text;
    smilies[i].Imag:=smiley.ChildNodes['img'].Text;
    smilies[i].Hint:=smiley.ChildNodes['text'].Text;
  end;

  doc.Free;
end;

procedure Tform_SB.ed_MessageKeyPress(Sender: TObject; var Key: Char);
begin
  if Key=#13 then
    btn_SendClick(Sender);
end;

procedure Tform_SB.btn_MinInputClick(Sender: TObject);
begin
  if pan_Input.Height>0 then
    pan_Input.Height:=0
  else
    pan_Input.Height:=pan_Input.Tag;

  if (pan_Input.Height>0) then
    GetAsBitmap(HInstance,'UP',clBtnFace,btn_MinInput.Glyph)
  else
    GetAsBitmap(HInstance,'DOWN',clBtnFace,btn_MinInput.Glyph);
end;

procedure Tform_SB.btn_SendClick(Sender: TObject);
begin
  if (not AnsiSameText(ed_Message.Text,EmptyStr)) and
      btn_Send.Enabled then
  begin
    if not Data.SendMessage(ed_Message.Text) then
      MessageDlg(STR_ShoutNotPosted, mtWarning, [mbOK], 0)
    else
    begin
      LastShout:='';
      NextRefresh:=1;
      ed_Message.SelectAll;
      btn_Send.Enabled:=false;
      PB_Flood.Max:=FLOOD_LIMIT;
      PB_Flood.Position:=FLOOD_LIMIT;
      tm_Flood.Enabled:=true;
    end
  end
  else
    NextRefresh:=1;
end;

procedure Tform_SB.btn_SettingsClick(Sender: TObject);
begin
  Application.CreateForm(Tform_Options, form_Options);
  try
    form_Options.tb_AlphaActive.Position:=Data.Settings.ReadInteger('Options','AlphaActive',255);
    form_Options.tb_AlphaInactive.Position:=Data.Settings.ReadInteger('Options','AlphaInactive',180);
    form_Options.tb_Fading.Position:=Data.Settings.ReadInteger('Options','Fading',10);
    form_Options.cb_ShowNewPosts.Checked:=Data.Settings.ReadBool('Options','ShowNewShouts',true);
    form_Options.cb_StayOnTop.Checked:=Data.Settings.ReadBool('Options','StayOnTop',true);
    form_Options.cb_HighlightMe.Checked:=Data.Settings.ReadBool('Options','ReplaceMe',true);
    
    if form_Options.ShowModal=mrOk then
    begin
      Data.Settings.WriteInteger('Options','AlphaActive',form_Options.tb_AlphaActive.Position);
      Data.Settings.WriteInteger('Options','AlphaInactive',form_Options.tb_AlphaInactive.Position);
      Data.Settings.WriteInteger('Options','Fading',form_Options.tb_Fading.Position);
      Data.Settings.WriteBool('Options','ShowNewShouts',form_Options.cb_ShowNewPosts.Checked);
      Data.Settings.WriteBool('Options','StayOnTop',form_Options.cb_StayOnTop.Checked);
      Data.Settings.WriteBool('Options','ReplaceMe',form_Options.cb_HighlightMe.Checked);
      SetWindowStyle(active);
    end;
  finally

  end;
end;

procedure Tform_SB.btn_SmiliesClick(Sender: TObject);
var
  form_SmileySelect : TForm_SmileySelect;
  TopLeft : TPoint;
  Text : String;
  SM : String;
  OldSelStart : Integer;
begin
  Application.CreateForm(TForm_SmileySelect, form_SmileySelect);
  try
    TopLeft.X:=0;
    TopLeft.Y:=ed_Message.Top+ed_Message.Height;
    TopLeft:=Self.ClientToScreen(TopLeft);
    tm_CheckWindowStyle.Enabled:=false;
    Text:=ed_Message.Text;
    Delete(Text,ed_Message.SelStart+1,ed_Message.SelLength);
    SM:=form_SmileySelect.SelectSmiley(TopLeft.X,TopLeft.Y);
    Insert(SM,Text,ed_Message.SelStart+1);
    OldSelStart:=ed_Message.SelStart;
    ed_Message.Text:=Text;
    ed_Message.SelStart:=OldSelStart+Length(SM);
    tm_CheckWindowStyle.Enabled:=true;
  finally
    form_SmileySelect.Release;
  end;
end;

function Tform_SB.BuildHTMLShoutBox(XML: String): String;
var
  doc : TXMLDocument;
  root: IXMLNode;

  html: TStrings;

  shout: IXMLNode;

  messg : String;
  i     : Integer;
  rowCnt: Integer;
  RowColor : String;
begin
  html:=TStringList.Create;

  //Headerdaten schreiben
  html.Add('<html>');
  html.Add('<body margin="0">');
  html.Add('<table width="100%">');

  doc:=TXMLDocument.Create(self);
  doc.DOMVendor:=MSXML_DOM;
  doc.XML.Text:=XML;
  doc.Active:=true;

  root:=doc.DocumentElement;

  shout:=root.ChildNodes.First;

  rowCnt:=0;
  while shout<>nil do
  begin

    //Balloonhint / delete input text
    if rowCnt=0 then
    begin
      if (Data.Settings.ReadBool('Options','ShowNewShouts',true)) and
          (not AnsiSameText(LastShout,shout.ChildNodes['message'].Text)) then
      begin
        if not AnsiSameStr(LastShout,'') then
          Tray.BalloonHint('DP-Shouter',
                            Format(STR_NewShout,[shout.ChildNodes['username'].Text]),
                            btInfo);
        LastShout:=shout.ChildNodes['message'].Text;
      end;

      if AnsiSameText(shout.ChildNodes['username'].Text,form_Login.ed_UsernameDP.Text) then
        ed_Message.Clear;
        
    end;

    inc(RowCnt);
    if (rowCnt mod 2) = 0 then
      RowColor:='#B8D8FF'
    else
      RowColor:='#B0D0FF';

    html.Add('<tr>');
    html.Add('<td bgcolor="'+RowColor+'" >');
    html.Add('<font face="Tahoma" size="8px">');

    html.Add('<a href="'+shout.ChildNodes['profile'].Text+'"><b>');
    html.Add(shout.ChildNodes['username'].Text);
    html.Add('</b></a>');
    html.Add(' - '+shout.ChildNodes['timestamp'].Text);

    html.Add('<br />');
    messg:=' '+shout.ChildNodes['message'].Text+' ';
    for I := Low(Smilies) to High(Smilies) do
      messg:=AnsiReplaceText(messg,
                             ' '+Smilies[i].Code+' ',
                             ' <img src="'+Smilies[i].Imag+'" /> ');

    if Data.Settings.ReadBool('Options','ReplaceMe',true) then
      messg:=AnsiReplaceText(messg,
                             ' /me ',
                             ' <b>'+shout.ChildNodes['username'].Text+'</b> ');

    html.Add(Trim(messg));

    html.Add('</font>');
    html.Add('</td>');
    html.Add('</tr>');
    shout:=shout.NextSibling;
  end;

  //Abschluﬂ
  html.Add('</table>');
  html.Add('</body>');
  html.Add('</html>');

  Result:=html.Text;
  html.Free;
  doc.Free;
end;

procedure Tform_SB.OpenHttpSite(AURL: String); //funktion zum webseite in neuem Fenster ˆffnen
var LRegistry: TRegistry;
    LBrowser: String;
    //LStart, LEnd: Integer;
begin
  try
    ShellExecute(Application.Handle,'open',PChar(AURL),'','',SW_SHOW);
  except end;
  exit;
  LBrowser:=EmptyStr;
  LRegistry := TRegistry.Create(KEY_READ);
  try
    LRegistry.RootKey := HKEY_CLASSES_ROOT;
    if LRegistry.OpenKey('http\shell\open\command', false) then
    begin
      LBrowser := LRegistry.ReadString('');
      {if pos('"', LBrowser) > 0 then
      begin
        LStart := pos('"', LBrowser);
        LEnd := PosEx('"', LBrowser, LStart + 1);
        if LEnd > 0 then
           LBrowser := copy(LBrowser, LStart + 1, LEnd - (LStart + 1));
      end;}
      LBrowser:=StringReplace(LBrowser,'%1',AURL,[rfReplaceAll]);
      LRegistry.CloseKey;
    end;
  finally
    LRegistry.Free;
  end;
  if (not AnsiSameText(Trim(LBrowser),EmptyStr)) then
    if ShellExecute(0, 'open', PChar(LBrowser), nil, nil, SW_SHOW)<=32 then
      ShellExecute(0, 'open', PChar(AURL), nil, Pchar(''), SW_SHOW)
    //MessageDlg('Kein Handler f¸r HTTP-Urls gefunden (HKEY_CLASSES_ROOT\http\shell\open\command)', mtWarning, [mbOK], 0);
end;

end.
