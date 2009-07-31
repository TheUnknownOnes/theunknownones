unit uData;

interface

uses
  SysUtils, Classes, uSettingsBase, uSettingsXML, uSysTools, ShlObj,
  Dialogs, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
  IdMessageClient, IdIMAP4, ExtCtrls, IDMailBox,
  Contnrs, windows;

type
  {$M+}
  TAccount = class
  private
    FCheckInterval: Integer;
    FHost: String;
    FUsername: String;
    FPassword : String;
    FPort: Integer;
    FMailboxes: String;
    function GetPassword: String;
    procedure SetPassword(const Value: String);
  public
    NextCheck : Integer;
    UnseenMessages : Integer;

    constructor Create; overload;
    constructor Create(ACopyFrom : TAccount); overload; 

    procedure Assign(AAccount : TAccount);
    procedure AssignTo(AIMAP : TIdIMAP4);
  published
    property Username : String read FUsername write FUsername;
    property Password : String read GetPassword write SetPassword;
    property Host : String read FHost write FHost;
    property Port : Integer read FPort write FPort;
    property CheckInterval : Integer read FCheckInterval write FCheckInterval;
    property Mailboxes : String read FMailboxes write FMailboxes;
  end;

  TSettings = class
  private
    FMailTool: String;
  published
    property MailTool : String read FMailTool write FMailTool;
  end;

  TData = class(TDataModule)
    SettingsFile: TSettingsXMLFile;
    IMAP: TIdIMAP4;
    Timer: TTimer;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FSettings : TSettings;
    FAccounts : TObjectList;

    procedure DoCheck(AAcount : TAccount);
  public

    procedure Log(ALogline : STring);

    procedure SaveSettings;

    function UnseenMessages : Integer;
    procedure ResetUnseenMessages;

    property Settings : TSettings read FSettings;
    property Accounts : TObjectList read FAccounts;
  end;



var
  Data: TData;

implementation

{$R *.dfm}

procedure TData.DataModuleCreate(Sender: TObject);
var
  idx : Integer;
  a : TAccount;
begin
  SettingsFile.FileName := IncludeTrailingPathDelimiter(GetShellFolder(CSIDL_APPDATA)) + 'IMAPMailChecker.xml';

  if FileExists(SettingsFile.FileName) then
    SettingsFile.Load;

  FSettings := TSettings.Create;
  
  SettingsFile.ReadObject('/Settings', Settings);

  FAccounts := TObjectList.Create(true);
  for idx := 0 to SettingsFile.GetValue('/Accounts/Count', 0) - 1 do
  begin
    a := TAccount.Create;
    FAccounts.Add(a);
    SettingsFile.ReadObject('/Accounts/' + IntToStr(idx), a);
  end;
    
end;

procedure TData.DataModuleDestroy(Sender: TObject);
begin
  FSettings.Free;
  FAccounts.Free;
end;

procedure TData.DoCheck(AAcount: TAccount);
begin
  AAcount.AssignTo(IMAP);

  imap.Connect(1000);
  try
    if IMAP.ConnectionState = csAuthenticated then
    begin
      if IMAP.SelectMailBox('INBOX') then
      begin
        imap.StatusMailBox('INBOX', IMAP.MailBox);

        AAcount.UnseenMessages := IMAP.MailBox.UnseenMsgs;
      end;
    end
  finally
    if IMAP.Connected then
      IMAP.Disconnect;
  end;
end;

procedure TData.Log(ALogline: STring);
var
  sl : TStringList;
  FName : String;
begin
  Fname := 'e:\log.txt';

  sl := TStringList.Create;

  if FileExists(FName) then
    sl.LoadFromFile(FName);

  sl.Add(ALogline);

  sl.SaveToFile(FName);

  sl.Free;
end;

procedure TData.ResetUnseenMessages;
var
  idx : Integer;
begin
  for idx := 0 to Accounts.Count - 1 do
  begin
    TAccount(Accounts[idx]).UnseenMessages := 0;
  end;
end;

procedure TData.SaveSettings;
var
  idx : Integer;
begin
  SettingsFile.WriteObject('/Settings', Settings);

  SettingsFile.Delete('/Accounts');                   
  for idx := 0 to FAccounts.Count - 1 do
  begin
    SettingsFile.WriteObject('/Accounts/' + IntToStr(idx), TAccount(FAccounts[idx]));
  end;
  SettingsFile.SetValue('/Accounts/Count', FAccounts.Count);    

  SettingsFile.Save;
end;

procedure TData.TimerTimer(Sender: TObject);
var
  idx : Integer;
  a : TAccount;
begin
  Timer.Enabled := false;
  try
  
    for idx := 0 to Accounts.Count - 1 do
    begin
      a := TAccount(Accounts[idx]);

      a.NextCheck := a.NextCheck + 1;

      if a.NextCheck >= a.CheckInterval then
      begin
        a.NextCheck := 0;
        DoCheck(a);
      end;
    end;

  finally
    Timer.Enabled := true;
  end;
end;

function TData.UnseenMessages: Integer;
var
  idx : Integer;
begin
  Result := 0;

  for idx := 0 to Accounts.Count - 1 do
    Inc(Result, TAccount(Accounts[idx]).UnseenMessages)
end;

{ TAccount }

procedure TAccount.Assign(AAccount: TAccount);
begin
  Host := AAccount.Host;
  Username := AAccount.Username;
  Password := AAccount.Password;
  CheckInterval := AAccount.CheckInterval;
  Port := AAccount.Port;
  NextCheck := AAccount.NextCheck;
  Mailboxes := AAccount.Mailboxes;
end;

constructor TAccount.Create;
begin
  FCheckInterval := 10;
  NextCheck := 0;
  UnseenMessages := 0;
  FPort := 143;
  FMailboxes := 'INBOX';
  FHost := 'imap.host';
  FUsername := 'username';
  FPassword := 'password';
end;

procedure TAccount.AssignTo(AIMAP: TIdIMAP4);
begin
  AIMAP.Host := Host;
  AIMAP.Password := Password;
  AIMAP.Port := Port;
  AIMAP.Username := Username;
end;

constructor TAccount.Create(ACopyFrom: TAccount);
begin
  Create;
  Assign(ACopyFrom);
end;

function TAccount.GetPassword: String;
begin
  Result := FPassword;
end;

procedure TAccount.SetPassword(const Value: String);
begin
  FPassword := Value;
end;

initialization
  Data := TData.Create(nil);

finalization
  Data.Free;

end.
