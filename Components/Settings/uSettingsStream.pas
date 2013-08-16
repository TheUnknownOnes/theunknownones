{-----------------------------------------------------------------------------
 Project: Settings
 Purpose: Contains components for loading and saving Settings in Streams,Files ...
 Created: 21.05.2008 14:45:06

 (c) by TheUnknownOnes under Apache License 2.0
 see http://www.TheUnknownOnes.net
-----------------------------------------------------------------------------}

unit uSettingsStream;

interface

{$I ..\..\Common\Jedi\JEDI.inc}

uses
  Classes,
  SysUtils,
  WideStrings,
  Variants,
  WideStrUtils,
  uSettingsBase;

type
  TCustomSettingsStream = class(TCustomSettings) 
  protected
    procedure WriteValue(const AStream : TStream; AValue : Smallint); overload;
    procedure WriteValue(const AStream : TStream; AValue : Integer); overload;
    procedure WriteValue(const AStream : TStream; AValue : Single); overload;
    procedure WriteValue(const AStream : TStream; AValue : Double); overload;
    procedure WriteValue(const AStream : TStream; AValue : WideString); overload;
    procedure WriteValue(const AStream : TStream; AValue : Boolean); overload;
    procedure WriteValue(const AStream : TStream; AValue : ShortInt); overload;
    procedure WriteValue(const AStream : TStream; AValue : Byte); overload;
    procedure WriteValue(const AStream : TStream; AValue : Word); overload;
    procedure WriteValue(const AStream : TStream; AValue : LongWord); overload;
    procedure WriteValue(const AStream : TStream; AValue : Int64); overload;
    procedure WriteValue(const AStream : TStream; AValue : String); overload;
    procedure WriteValue(const AStream : TStream; AValue : Variant); overload;

    procedure ReadValue(const AStream : TStream; out AValue : Smallint); overload;
    procedure ReadValue(const AStream : TStream; out AValue : Integer); overload;
    procedure ReadValue(const AStream : TStream; out AValue : Single); overload;
    procedure ReadValue(const AStream : TStream; out AValue : Double); overload;
    procedure ReadValue(const AStream : TStream; out AValue : WideString); overload;
    procedure ReadValue(const AStream : TStream; out AValue : Boolean); overload;
    procedure ReadValue(const AStream : TStream; out AValue : ShortInt); overload;
    procedure ReadValue(const AStream : TStream; out AValue : Byte); overload;
    procedure ReadValue(const AStream : TStream; out AValue : Word); overload;
    procedure ReadValue(const AStream : TStream; out AValue : LongWord); overload;
    procedure ReadValue(const AStream : TStream; out AValue : Int64); overload;
    procedure ReadValue(const AStream : TStream; out AValue : String); overload;
    procedure ReadValue(const AStream : TStream; out AValue : Variant); overload;

    procedure SaveSetting(ASetting : TSetting; AStream : TStream);
    procedure LoadSetting(ASetting : TSetting; AStream : TStream);

    function DoLoad : Boolean; override;
    function DoSave : Boolean; override;

    function DoCreateStream(out AStream : TStream; ARead : Boolean) : Boolean; virtual; abstract;
    function DoLoadStreamContent(const AStream : TStream) : Boolean; virtual; abstract;
    function DoSaveStreamContent(const AStream : TStream) : Boolean; virtual; abstract;
  end;


//==============================================================================


  TSettingsFile = class(TCustomSettingsStream)
  private
    FFilename: String;
  protected
    function DoCreateStream(out AStream : TStream; ARead : Boolean) : Boolean; override;
    function DoLoadStreamContent(const AStream : TStream) : Boolean; override;
    function DoSaveStreamContent(const AStream : TStream) : Boolean; override;

  published
    property ParentSettings;
    property ParentMode;

    property FileName : String read FFilename write FFilename;
  end;


//==============================================================================


  TSettingsStreamNeedStreamProc = function(ARead : Boolean; out AStream : TStream) : Boolean of object;
  TSettingsStreamContentProc = function(const AStream : TStream) : Boolean of object;

  TSettingsStream = class(TCustomSettingsStream)
  protected
    FOnLoadStreamContent: TSettingsStreamContentProc;
    FOnSaveStreamContent: TSettingsStreamContentProc;
    FOnNeedStream: TSettingsStreamNeedStreamProc;

    function DoCreateStream(out AStream : TStream; ARead : Boolean) : Boolean; override;
    function DoLoadStreamContent(const AStream : TStream) : Boolean; override;
    function DoSaveStreamContent(const AStream : TStream) : Boolean; override;
  published
    property ParentSettings;
    property ParentMode;

    property OnNeedStream : TSettingsStreamNeedStreamProc read FOnNeedStream write FOnNeedStream;
    property OnLoadStreamContent : TSettingsStreamContentProc read FOnLoadStreamContent write FOnLoadStreamContent;
    property OnSaveStreamContent : TSettingsStreamContentProc read FOnSaveStreamContent write FOnSaveStreamContent;
  end;

implementation    

//==============================================================================

{ TCustomSettingsStream }

function TCustomSettingsStream.DoLoad : Boolean;
var
  Stream : TStream;
begin
  Result := DoCreateStream(Stream, true);

  if Result then
    Result := Assigned(Stream);

  if not Result then
    exit;

  try
    Result := DoLoadStreamContent(Stream);

    if Result then
      LoadSetting(FRootSetting, Stream);
  finally
    Stream.Free;
  end;

end;

function TCustomSettingsStream.DoSave : Boolean;
var
  Stream  : TStream;
begin
  Result := DoCreateStream(Stream, False);

  if Result then
    Result := Assigned(Stream);

  if not Result then
    exit;

  try
    SaveSetting(FRootSetting, Stream);

    Result := DoSaveStreamContent(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TCustomSettingsStream.LoadSetting(ASetting: TSetting;
  AStream: TStream);
var
  idx, ChildCount : Integer;
  Child : TSetting;
  Name : TSettingName;
  Value : TSettingValue;
begin
  ReadValue(AStream, Name);
  ASetting.Name := Name;

  ReadValue(AStream, Value);
  ASetting.Value := Value;

  ReadValue(AStream, ChildCount);

  for idx := 0 to ChildCount - 1 do
  begin
    Child := TSetting.Create(ASetting, EmptyWideStr);

    LoadSetting(Child, AStream);
  end;
end;

procedure TCustomSettingsStream.ReadValue(const AStream: TStream;
  out AValue: Double);
begin
  AStream.ReadBuffer(AValue, SizeOf(AValue));
end;

procedure TCustomSettingsStream.ReadValue(const AStream: TStream;
  out AValue: WideString);
var
  Buffer : array of Byte;
  Len : Integer;
begin
  ReadValue(AStream, Len);
  Inc(Len, SizeOf(WideChar));
  SetLength(Buffer, Len);
  FillChar(Buffer[0], Len, 0);
  AStream.ReadBuffer(Buffer[0], Len - SizeOf(WideChar));
  AValue := WideString(PWideChar(@Buffer[0]));
end;

procedure TCustomSettingsStream.ReadValue(const AStream: TStream;
  out AValue: Boolean);
begin
  AStream.ReadBuffer(AValue, SizeOf(AValue));
end;

procedure TCustomSettingsStream.ReadValue(const AStream: TStream;
  out AValue: Smallint);
begin
  AStream.ReadBuffer(AValue, SizeOf(AValue));
end;

procedure TCustomSettingsStream.ReadValue(const AStream: TStream;
  out AValue: Integer);
begin
  AStream.ReadBuffer(AValue, SizeOf(AValue));
end;

procedure TCustomSettingsStream.ReadValue(const AStream: TStream;
  out AValue: Single);
begin
  AStream.ReadBuffer(AValue, SizeOf(AValue));
end;

procedure TCustomSettingsStream.ReadValue(const AStream: TStream;
  out AValue: ShortInt);
begin
  AStream.ReadBuffer(AValue, SizeOf(AValue));
end;

procedure TCustomSettingsStream.ReadValue(const AStream: TStream;
  out AValue: Int64);
begin
  AStream.ReadBuffer(AValue, SizeOf(AValue));
end;

procedure TCustomSettingsStream.ReadValue(const AStream: TStream;
  out AValue: String);
var
  Buffer : PChar;
  Len : Integer;
begin
  ReadValue(AStream, Len);
  Len := Len * SizeOf(Char);
  Inc(Len, SizeOf(Char));
  GetMem(Buffer, Len);
  FillChar(Buffer^, Len, 0);
  AStream.ReadBuffer(Buffer^, Len - SizeOf(Char));
  AValue := String(Buffer);
  FreeMem(Buffer, Len);
end;

procedure TCustomSettingsStream.ReadValue(const AStream: TStream;
  out AValue: Variant);
var
  ValueType : TVarType;
  tempSmallInt : Smallint;
  tempInteger : Integer;
  tempSingle : Single;
  tempDouble : Double;
  tempDate : TDateTime;
  tempOleStr : WideString;
  tempBoolean : Boolean;
  tempShortInt : Shortint;
  tempByte : Byte;
  tempWord : Word;
  tempLongWord : LongWord;
  tempInt64 : Int64;
  tempString : String;
begin
  ReadValue(AStream, ValueType);

  case ValueType of
    varEmpty    : VarClear(AValue);
    varNull     : AValue := null;
    varSmallint :
    begin
      ReadValue(AStream, tempSmallInt);
      AValue := tempSmallInt;
    end;
    varInteger  :
    begin
      ReadValue(AStream, tempInteger);
      AValue := tempInteger;
    end;
    varSingle   :
    begin
      ReadValue(AStream, tempSingle);
      AValue := tempSingle;
    end;
    varDouble   :
    begin
      ReadValue(AStream, tempDouble);
      AValue := tempDouble;
    end;
    varDate     :
    begin
      ReadValue(AStream, Double(tempDate));
      AValue := tempDate;
    end;
    varOleStr   :
    begin
      ReadValue(AStream, tempOleStr);
      AValue := tempOleStr;
    end;
    varBoolean  :
    begin
      ReadValue(AStream, tempBoolean);
      AValue := tempBoolean;
    end;
    varShortInt :
    begin
      ReadValue(AStream, tempShortInt);
      AValue := tempShortInt;
    end;
    varByte     :
    begin
      ReadValue(AStream, tempByte);
      AValue := tempByte;
    end;
    varWord     :
    begin
      ReadValue(AStream, tempWord);
      AValue := tempWord;
    end;
    varLongWord :
    begin
      ReadValue(AStream, tempLongWord);
      AValue := tempLongWord;
    end;
    varInt64    :
    begin
      ReadValue(AStream, tempInt64);
      AValue := tempInt64;
    end;
    varString   :
    begin
      ReadValue(AStream, tempString);
      AValue := tempString;
    end;
    {$ifdef DELPHI12_UP}
    varUString   :
    begin
      ReadValue(AStream, tempOleStr);
      AValue := tempOleStr;
    end;
    {$endif}
  end;
end;

procedure TCustomSettingsStream.ReadValue(const AStream: TStream;
  out AValue: Byte);
begin
  AStream.ReadBuffer(AValue, SizeOf(AValue));
end;

procedure TCustomSettingsStream.ReadValue(const AStream: TStream;
  out AValue: Word);
begin
  AStream.ReadBuffer(AValue, SizeOf(AValue));
end;

procedure TCustomSettingsStream.ReadValue(const AStream: TStream;
  out AValue: LongWord);
begin
  AStream.ReadBuffer(AValue, SizeOf(AValue));
end;

procedure TCustomSettingsStream.SaveSetting(ASetting: TSetting;
  AStream: TStream);
var
  idx : Integer;
begin
  WriteValue(AStream, ASetting.Name);
  WriteValue(AStream, ASetting.Value);
  WriteValue(AStream, ASetting.Children.Count);

  for idx := 0 to ASetting.Children.Count - 1 do
    SaveSetting(ASetting.Children[idx], AStream);
end;

procedure TCustomSettingsStream.WriteValue(const AStream: TStream;
  AValue: Boolean);
begin
  Astream.WriteBuffer(AValue, SizeOf(AValue));
end;

procedure TCustomSettingsStream.WriteValue(const AStream: TStream;
  AValue: Double);
begin
  Astream.WriteBuffer(AValue, SizeOf(AValue));
end;

procedure TCustomSettingsStream.WriteValue(const AStream: TStream;
  AValue: Smallint);
begin
  Astream.WriteBuffer(AValue, SizeOf(AValue));
end;

procedure TCustomSettingsStream.WriteValue(const AStream: TStream;
  AValue: Single);
begin
  Astream.WriteBuffer(AValue, SizeOf(AValue));
end;

procedure TCustomSettingsStream.WriteValue(const AStream: TStream;
  AValue: Int64);
begin
  Astream.WriteBuffer(AValue, SizeOf(AValue));
end;

procedure TCustomSettingsStream.WriteValue(const AStream: TStream;
  AValue: String);
begin
  WriteValue(AStream, Length(AValue));
  Astream.WriteBuffer(PChar(AValue)^, Length(AValue) * SizeOf(Char));
end;

procedure TCustomSettingsStream.WriteValue(const AStream: TStream;
  AValue: LongWord);
begin
  Astream.WriteBuffer(AValue, SizeOf(AValue));
end;

procedure TCustomSettingsStream.WriteValue(const AStream: TStream;
  AValue: ShortInt);
begin
  Astream.WriteBuffer(AValue, SizeOf(AValue));
end;

procedure TCustomSettingsStream.WriteValue(const AStream: TStream;
  AValue: Byte);
begin
  Astream.WriteBuffer(AValue, SizeOf(AValue));
end;

procedure TCustomSettingsStream.WriteValue(const AStream: TStream;
  AValue: Variant);
var
  ValueType : Word;
  Data : TVarData;
begin
  ValueType := VarType(AValue);

  WriteValue(AStream, ValueType);

  Data := TVarData(AValue);

  case ValueType of
    varSmallint : WriteValue(AStream, Data.VSmallInt);
    varInteger  : WriteValue(AStream, Data.VInteger);
    varSingle   : WriteValue(AStream, Data.VSingle);
    varDouble   : WriteValue(AStream, Data.VDouble);
    varDate     : WriteValue(AStream, Data.VDate);
    varOleStr   : WriteValue(AStream, WideString(Data.VOleStr));
    varBoolean  : WriteValue(AStream, Data.VBoolean);
    varShortInt : WriteValue(AStream, Data.VShortInt);
    varByte     : WriteValue(AStream, Data.VByte);
    varWord     : WriteValue(AStream, Data.VWord);
    varLongWord : WriteValue(AStream, Data.VLongWord);
    varInt64    : WriteValue(AStream, Data.VInt64);
    varString   : WriteValue(AStream, String(Data.VString));
    {$ifdef DELPHI12_UP}
    varUString  : WriteValue(AStream, WideString(PWideChar(Data.VUString)));
    {$endif}
  end;
end;

procedure TCustomSettingsStream.WriteValue(const AStream: TStream;
  AValue: WideString);
begin
  WriteValue(AStream, Length(AValue) * SizeOf(WideChar));
  Astream.WriteBuffer(PWideChar(AValue)^, Length(AValue) * SizeOf(WideChar));
end;

procedure TCustomSettingsStream.WriteValue(const AStream: TStream;
  AValue: Integer);
begin
  Astream.WriteBuffer(AValue, SizeOf(AValue));
end;

procedure TCustomSettingsStream.WriteValue(const AStream: TStream;
  AValue: Word);
begin
  Astream.WriteBuffer(AValue, SizeOf(AValue));
end;

{ TSettingsFile }

function TSettingsFile.DoCreateStream(out AStream: TStream;
  ARead: Boolean): Boolean;
var
  Flags : Word;
begin
  Result := Trim(FFilename) <> EmptyStr;

  if ARead then
    Flags := fmOpenRead
  else
    Flags := fmOpenWrite or fmCreate or fmShareDenyWrite;

  if Result then
    AStream := TFileStream.Create(FFilename, Flags);
end;

function TSettingsFile.DoLoadStreamContent(const AStream: TStream): Boolean;
begin
  AStream.Seek(0, soFromBeginning);

  Result := true;
end;

function TSettingsFile.DoSaveStreamContent(const AStream: TStream): Boolean;
begin
  Result := true;
end;

{ TSettingsStream }

function TSettingsStream.DoCreateStream(out AStream: TStream;
  ARead: Boolean): Boolean;
begin
  Result := Assigned(FOnNeedStream);

  if Result then
    Result := FOnNeedStream(ARead, AStream);
end;

function TSettingsStream.DoLoadStreamContent(const AStream: TStream): Boolean;
begin
  Result := Assigned(FOnLoadStreamContent);

  if Result then
    Result := FOnLoadStreamContent(AStream);
end;

function TSettingsStream.DoSaveStreamContent(const AStream: TStream): Boolean;
begin
  Result := Assigned(FOnSaveStreamContent);

  if Result then
    Result := FOnSaveStreamContent(AStream);
end;

end.
