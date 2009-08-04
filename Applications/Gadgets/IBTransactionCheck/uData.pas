unit uData;

interface

uses
  SysUtils, ActiveX, ComObj, Classes, FIBDatabase, pFIBDatabase, FIBQuery,
  pFIBQuery, uSettingsBase, uSettingsStream, ShlObj, uSysTools, dialogs,
  DateUtils;

type
  TData = class(TDataModule)
    DB: TpFIBDatabase;
    Transe: TpFIBTransaction;
    qry_1: TpFIBQuery;
    SettingsFile: TSettingsFile;
    procedure DataModuleCreate(Sender: TObject);
  private
  public
    MinLen : Integer;
    RefreshInterval : Integer;

    function GetTransactions : String;
    procedure Shutdown(AAttachment : Integer);

    procedure SaveSettings;
    procedure ShowOptionsDialog;
  end;

var
  Data: TData;

implementation

uses uFormSettings, Controls;

{$R *.dfm}

{ TData }

procedure TData.DataModuleCreate(Sender: TObject);
begin
  SettingsFile.FileName := IncludeTrailingPathDelimiter(GetShellFolder(CSIDL_APPDATA)) + 'IBTransactionChecker.dat';

  if FileExists(SettingsFile.FileName) then
    SettingsFile.Load;

  SettingsFile.ReadObject('/DB', DB);

  MinLen := SettingsFile.GetValue('/MinLen', 1);
  RefreshInterval := SettingsFile.GetValue('/RefreshInterval', 60000);
end;

function TData.GetTransactions: String;
var
  lines : Integer;
  Len : Single;
const
  max_lines = 10;
begin
  lines := 0;

  try
  
    try

      Db.Connected := true;
      qry_1.Transaction.StartTransaction;
      qry_1.ExecQuery;

      while (not qry_1.Eof) and (lines < max_lines) do
      begin
        Len := MinuteSpan(qry_1.FieldByName('Starttime').AsDateTime, Now);
        if Len > MinLen then
        begin
          Result := Result + '<span class="attachment" ';
          Result := Result + 'title="' + qry_1.FieldByName('Hostname').AsString + '">';
          Result := Result + Format('%s (%.1f min)', [qry_1.FieldByName('Username').AsString,
                                                    Len]);
          Result := Result + '</span>';

          Result := Result + '<img class="ShutdownButton" src="shutdown.png" title="Shutdown!" ';
          Result := Result + 'onclick="Shutdown(' + qry_1.FieldByName('Attachment').AsString + ')" />';

          Inc(lines);
        end;
        
        qry_1.Next;
      end;

    except
      on E:Exception do
      begin
        Result := '<div class="error">'+e.Message+'</div>'
      end;
    end;

  finally
    if qry_1.Open then
      qry_1.Close;
      
    if qry_1.Transaction.Active then
      qry_1.Transaction.Rollback;

    if DB.Connected then
      db.Connected := false;
  end;

end;

procedure TData.SaveSettings;
begin
  SettingsFile.WriteObject('/DB', DB);
  SettingsFile.SetValue('/MinLen', MinLen);
  SettingsFile.SetValue('/RefreshInterval', RefreshInterval);

  SettingsFile.Save;
end;

procedure TData.ShowOptionsDialog;
begin
  Tform_Settings.Execute;
end;

procedure TData.Shutdown(AAttachment: Integer);
begin
  if (MessageDlg('Do you really want to shut down this attachment?', mtConfirmation, [mbYes, mbNo], 0) in [mrYes]) then
  begin
    try
      Db.Connected := true;
      db.Execute('update tmp$attachments set tmp$state = ''SHUTDOWN'' where TMP$ATTACHMENT_ID =' + IntToStr(AAttachment));
    finally
      if DB.Connected then
        db.Connected := false;
    end;
  end;
end;

initialization
  Data := TData.Create(nil);

finalization
  Data.Free;

end.
