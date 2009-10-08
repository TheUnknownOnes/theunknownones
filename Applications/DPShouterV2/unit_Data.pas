unit unit_Data;

interface

uses
  SysUtils, Classes, IniFiles, IdCookieManager, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, IdHTTP, JclSysInfo, Dialogs, IDCookie,
  IDMultiPartFormData, forms, Windows, Controls, STrUtils;

type
  TActionState = (asTryLogin);
  TData = class(TDataModule)
    Browser: TIdHTTP;
    Cookies: TIdCookieManager;
    procedure BrowserWorkEnd(Sender: TObject; AWorkMode: TWorkMode);
    procedure BrowserWorkBegin(Sender: TObject; AWorkMode: TWorkMode;
      const AWorkCountMax: Integer);
    procedure BrowserRedirect(Sender: TObject; var dest: string;
      var NumRedirect: Integer; var Handled: Boolean;
      var VMethod: TIdHTTPMethod);
    procedure CookiesNewCookie(ASender: TObject; ACookie: TIdCookieRFC2109;
      var VAccept: Boolean);
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    Settings : TIniFile;
    Dir_Application,
    Dir_DPShouter,
    Dir_Cache,
    File_Smilies_xml : String;

    ActionState : TActionState;

    SID : String;

    ShutDown : Boolean; //If something goes wrong, shutdown program
    LoginSuccess : Boolean; //Check the redirect after login to get the success-state of the login

    function Login(AUserName : String;
                    APassword : String) : Boolean;
    procedure Logout;
    function GetSB() : String;
    procedure GetFile( AURL : String;
                      ASaveTo : String);
    function SendMessage(AMessage : String) : Boolean;

    function Crypt(AText : String) : String;
  end;

var
  Data: TData;

const
  URL_ROOT='http://www.delphipraxis.net/';
  URL_LOGIN=URL_ROOT+'login.php';
  URL_LOGOUT=URL_LOGIN+'?logout=true&sid=%s';
  URL_SHOUTBOX=URL_ROOT+'rdf.php?shoutbox=1';
  URL_POST_MESSAGE=URL_ROOT+'shoutbox_view.php';
  URL_INDEX=URL_ROOT+'index.html';
  URL_PN=URL_ROOT+'privmsg.php?mode=post&u=%d';
  URL_MAP=URL_ROOT+'map.php?highlight=%d';

  STR_CanNotCreateDir='Das Verzeichnis "%s" konnte nicht angelegt werden.';
  STR_MissingFile='Die Datei "%s" wurde nicht gefunden.';
  STR_LoginFailed='Login fehlgeschlagen.';
  STR_ShoutNotPosted='Der Shout konnte nicht gepostet werden.';
  STR_FormCaption='DP-Shouter (%ds)';
  STR_NewShout='Neuer Shout von %s';

  SMILIES='smilies.xml';

  COLOR_Form=$FFD8B8;

  REFRESH_INTERVAL=120;
  FLOOD_LIMIT=45;

implementation

{$R *.dfm}

procedure TData.DataModuleCreate(Sender: TObject);
begin
  //By default everything is ok
  ShutDown:=false;

  //Define the pathes and files
  Dir_Application:=IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
  Dir_DPShouter:=IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(GetAppdataFolder)+'DPShouter');
  Dir_Cache:=IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(Dir_DPShouter)+'Cache');
  File_Smilies_xml:=Dir_DPShouter+SMILIES;

  //Check if the pathes exists
  if not DirectoryExists(Dir_DPShouter) then
    if not CreateDir(Dir_DPShouter) then
    begin
      ShutDown:=true;
      MessageDlg(Format(STR_CanNotCreateDir,[Dir_DPShouter]), mtError, [mbOK], 0);
      exit;
    end;
  if not DirectoryExists(Dir_Cache) then
    if not CreateDir(Dir_Cache) then
    begin
      ShutDown:=true;
      MessageDlg(Format(STR_CanNotCreateDir,[Dir_Cache]), mtError, [mbOK], 0);
      exit;
    end;

  //make sure, that an smilies.xml exists
  if not FileExists(File_Smilies_xml) then
    if not FileExists(Dir_Application+SMILIES) then
    begin
      ShutDown:=true;
      MessageDlg(Format(STR_MissingFile,[Dir_Application+SMILIES]), mtError, [mbOK], 0);
      exit;
    end
    else
      CopyFile(Pchar(Dir_Application+SMILIES),Pchar(File_Smilies_xml),false);

  //Initialize the Settings
  try
    Settings:=TIniFile.Create(Dir_DPShouter+'dpshouter.ini');
  except
    ShutDown:=true;
    raise;
  end;
end;


{$REGION 'Browser handling'}

procedure TData.BrowserRedirect(Sender: TObject; var dest: string;
  var NumRedirect: Integer; var Handled: Boolean; var VMethod: TIdHTTPMethod);
begin
  case ActionState of
    asTryLogin: 
    begin
      if AnsiStartsText(URL_INDEX,dest) then
        LoginSuccess:=true;  
    end;
  end;
end;

procedure TData.BrowserWorkBegin(Sender: TObject; AWorkMode: TWorkMode;
  const AWorkCountMax: Integer);
begin
  Screen.Cursor:=crHourGlass;
end;

procedure TData.BrowserWorkEnd(Sender: TObject; AWorkMode: TWorkMode);
begin
  Screen.Cursor:=crDefault;
end;

procedure TData.CookiesNewCookie(ASender: TObject; ACookie: TIdCookieRFC2109;
  var VAccept: Boolean);
begin
  if AnsiContainsText(ACookie.CookieName,'sid') then
    SID:=ACookie.Value;
  VAccept:=true;
end;

function TData.Login(AUserName, APassword: String) : Boolean;
var
  FormData : TIdMultiPartFormDataStream;
  Answer : TMemoryStream;
begin
  Result:=true;

  FormData:=TIdMultiPartFormDataStream.Create;
  Answer:=TMemoryStream.Create;

  try
    Browser.Request.ContentType:='application/x-www-form-urlencoded';

    //fields of the original html-form; may be changed with changes delphipraxis-layout
    FormData.AddFormField('redirect','');
    FormData.AddFormField('login','login');
    FormData.AddFormField('autologin','0');
    FormData.AddFormField('username',AUserName);
    FormData.AddFormField('password',APassword);
    LoginSuccess:=false;
    try
      Browser.Post(URL_LOGIN,FormData,Answer);
    except
      Result:=false;
    end;
    Result:=LoginSuccess;
  finally
    FormData.Free;
    Answer.Free;
  end;
end;

procedure TData.Logout;
var
  Answer : TMemoryStream;
begin
  Screen.Cursor:=crHourGlass;
  Answer:=TMemoryStream.Create;
  try
    Browser.Get(Format(URL_LOGOUT,[SID]),Answer);
  finally
    Screen.Cursor:=crDefault;
    Answer.Free;
  end;
end;

function TData.SendMessage(AMessage: String) : Boolean;
var
  FormData : TIdMultiPartFormDataStream;
  Answer : TMemoryStream;
begin
  Result:=true;

  FormData:=TIdMultiPartFormDataStream.Create;
  Answer:=TMemoryStream.Create;

  try
    Browser.Request.ContentType:='application/x-www-form-urlencoded';

    //fields of the original html-form; may be changed with changes delphipraxis-layout
    FormData.AddFormField('shout','go');
    FormData.AddFormField('message',AMessage);
    try
      Browser.Post(URL_POST_MESSAGE,FormData,Answer);
    except
      Result:=false;
    end;
  finally
    FormData.Free;
    Answer.Free;
  end;
end;

procedure TData.GetFile(AURL, ASaveTo: String);
var
  Answer : TMemoryStream;
begin
  Answer:=TMemoryStream.Create;
  try
    Browser.Get(AURL,Answer);
    Answer.Seek(0,soFromBeginning);
    Answer.SaveToFile(ASaveTo);
  finally
    Answer.Free;
  end;
end;

function TData.GetSB: String;
var
  Answer : TStringStream;
begin
  Result:='';

  Answer:=TStringStream.Create('');
  try
    Browser.Get(URL_SHOUTBOX,Answer);
    Answer.Seek(0,soFromBeginning);
    Result:=Answer.DataString;
  finally
    Answer.Free;
  end;
end;

{$ENDREGION}

{$REGION 'Crypt'}

function TData.Crypt(AText: String): String;
var
  idx : Integer;
begin
  Result:='';
  for idx:=1 to Length(AText) do
    Result:=Result+Chr(Ord(AText[idx]) xor (idx));
end;

{$ENDREGION}

end.
