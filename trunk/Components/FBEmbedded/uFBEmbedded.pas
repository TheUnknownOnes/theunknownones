unit uFBEmbedded;

interface

uses
  Windows, Classes, SysUtils, Types;

type
  TFBEmbedded = class(TComponent)
  protected
    FActive: Boolean;
    FRootPath: String;
    FConfig: TStrings;
    FLibraryName: String;
    FOldCurDir : String;

    procedure SetActive(const Value: Boolean);

    procedure Start; virtual;
    procedure Stop; virtual;

    procedure ExtractFiles(AFileMap : String); virtual;
    procedure RemoveFiles(AFileMap : String); virtual;
  published
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Active : Boolean read FActive write SetActive default false;
    property RootPath : String read FRootPath;
    property Config : TStrings read FConfig;
    property LibraryName : String read FLibraryName;
  end;

  TFBEmbedded_2_5_0 = class(TFBEmbedded)
  protected
    {$I FBEmbedded_32_2_5_0.inc}
    procedure Start; override;
    procedure Stop; override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TUO', [TFBEmbedded_2_5_0]);
end;

{ TFBEmbedded }

constructor TFBEmbedded.Create(AOwner: TComponent);
begin
  inherited;

  FConfig := TStringList.Create;
  FConfig.NameValueSeparator := '=';

  Active := false;
end;

destructor TFBEmbedded.Destroy;
begin
  FConfig.Free;

  Active := false;

  inherited;
end;

procedure TFBEmbedded.ExtractFiles(AFileMap: String);
var
  FileMap : TStringList;
  RS : TResourceStream;
  idx : Integer;
  TargetFile : String;
begin
  FileMap := TStringList.Create;
  try
    FileMap.Delimiter := ';';
    FileMap.StrictDelimiter := true;
    FileMap.DelimitedText := AFileMap;

    for idx := 0 to FileMap.Count - 1 do
    begin
      rs := TResourceStream.Create(HInstance, FileMap.Names[idx], PChar(RT_RCDATA));
      try
        TargetFile := FRootPath + FileMap.ValueFromIndex[idx];
        ForceDirectories(ExtractFilePath(TargetFile));
        rs.SaveToFile(TargetFile);
      finally
        rs.Free;
      end;
    end;
  finally
    FileMap.Free;
  end;
end;

procedure TFBEmbedded.RemoveFiles(AFileMap: String);
var
  FileMap : TStringList;
  idx : Integer;
  TargetFile : String;
begin
  FileMap := TStringList.Create;
  try
    FileMap.Delimiter := ';';
    FileMap.StrictDelimiter := true;
    FileMap.DelimitedText := AFileMap;

    for idx := 0 to FileMap.Count - 1 do
    begin
      TargetFile := FRootPath + FileMap.ValueFromIndex[idx];
      DeleteFile(TargetFile);
      RemoveDir(ExtractFilePath(TargetFile));
    end;
  finally
    FileMap.Free;
  end;
end;

procedure TFBEmbedded.SetActive(const Value: Boolean);
begin
  if csDesigning in ComponentState then exit;

  if FActive <> Value then
  begin
    if Value then
      Start
    else
      Stop;

    FActive := Value;
  end;
end;

procedure TFBEmbedded.Start;
var
  TempDir : PChar;
begin
  GetMem(TempDir, MAX_PATH);
  try
    GetTempPath(MAX_PATH, TempDir);
    FRootPath := IncludeTrailingPathDelimiter(TempDir) + 'FBE_' + IntToStr(GetCurrentThread) + PathDelim;
  finally
    FreeMem(TempDir, MAX_PATH);
  end;

  ForceDirectories(ExcludeTrailingPathDelimiter(RootPath));

  Config.Values['RootDirectory'] := FRootPath;
  Config.SaveToFile(FRootPath + 'firebird.conf');

  FOldCurDir := GetCurrentDir;
  SetCurrentDir(FRootPath);
end;

procedure TFBEmbedded.Stop;
begin
  SetCurrentDir(FOldCurDir);

  DeleteFile(FRootPath + 'firebird.conf');
  DeleteFile(FRootPath + 'firebird.log');
  RemoveDir(FRootPath);
end;

{ TFBEmbedded_2_5_0 }

procedure TFBEmbedded_2_5_0.Start;
begin
  inherited;

  ExtractFiles(FFilemap);
  FLibraryName := FRootPath + FLibName;
end;

procedure TFBEmbedded_2_5_0.Stop;
begin
  FLibraryName := '';
  RemoveFiles(FFilemap);

  inherited;
end;

end.
