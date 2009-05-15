unit uBassCommon;

interface

uses
  bass,
  Windows,
  Classes;

type
  TBassSampleFormat = (bsfUnknown,
                       bsf8Bit = 8,
                       bsf16Bit = 16,
                       bsf32Bit = 32);

  TBassWaveDataList = class(TThreadList)
  protected
    FChannels : Cardinal;
    FSampleFormat : TBassSampleFormat;

    procedure FreeData(AData : Pointer);
  public
    constructor Create(AChannel : Cardinal); reintroduce;
    destructor Destroy(); override;

    property Channels : Cardinal read FChannels;
    property SampleFormat : TBassSampleFormat read FSampleFormat;

    procedure Clear;

    procedure CopyFrom(ABuffer : Pointer; ABufferLen : Integer);

    function DataSize : Integer;

    procedure DeleteRange(AFromIndex, AToIndex : Integer);
    procedure DeleteExceptLast(ALast : Integer);
  end;

function Bass_GetSampleFormatFromChannel(AChannel : Cardinal) : TBassSampleFormat;
function Bass_GetChannelsFromChannel(AChannel : Cardinal) : Cardinal;

implementation

function Bass_GetSampleFormatFromChannel(AChannel : Cardinal) : TBassSampleFormat;
var
  info : BASS_CHANNELINFO;
begin
  Assert(AChannel > 0, 'Channel is not valid');

  if BASS_ChannelGetInfo(AChannel,  info) then
  begin
    if info.flags and BASS_SAMPLE_8BITS = BASS_SAMPLE_8BITS then
      Result := bsf8Bit
    else
    if info.flags and BASS_SAMPLE_FLOAT = BASS_SAMPLE_FLOAT then
      Result := bsf32Bit
    else
      Result := bsf16Bit;
  end
  else
    Result := bsfUnknown;
end;

function Bass_GetChannelsFromChannel(AChannel : Cardinal) : Cardinal;
var
  info : BASS_CHANNELINFO;
begin
  Assert(AChannel > 0, 'Channel is not valid');

  if BASS_ChannelGetInfo(AChannel,  info) then
    Result := info.chans
  else
    Result := 0;
end;

{ TBassWaveDataList }

procedure TBassWaveDataList.Clear;
begin
  DeleteRange(-1, -1);
end;

procedure TBassWaveDataList.CopyFrom(ABuffer: Pointer; ABufferLen: Integer);
var
  Data : Pointer;
  List : TList;
  idx,
  cnt : Integer;
  lDataSize : Integer;
begin
  lDataSize := DataSize;
  List := LockList;
  try
    cnt := ABufferLen div lDataSize;
    
    for idx := 0 to cnt - 1 do
    begin
      GetMem(Data, lDataSize);
      CopyMemory(Data, ABuffer, lDataSize);
      List.Add(Data);

      Inc(PByte(ABuffer), lDataSize);
    end;
    
  finally
    UnlockList;
  end;
end;

constructor TBassWaveDataList.Create(AChannel: Cardinal);
begin
  inherited Create();

  FSampleFormat := Bass_GetSampleFormatFromChannel(AChannel);
  FChannels := Bass_GetChannelsFromChannel(AChannel);
end;

function TBassWaveDataList.DataSize: Integer;
begin
  Result := FChannels * Integer(FSampleFormat);
end;

procedure TBassWaveDataList.DeleteExceptLast(ALast: Integer);
var
  idx : Integer;
  List : TList;
begin
  List := LockList;
  try

    for idx := List.Count - 1 - ALast downto 0 do
    begin
      FreeData(List[idx]);
      List.Delete(idx);
    end;

  finally
    UnlockList;
  end;
end;

procedure TBassWaveDataList.DeleteRange(AFromIndex, AToIndex: Integer);
var
  list : TList;
  idx : Integer;
begin
  List := LockList;
  try
    if AFromIndex = -1 then
      AFromIndex := 0;

    if AToIndex = -1 then
      AToIndex := list.Count - 1;

    for idx := AToIndex downto AFromIndex do
    begin
      FreeData(List[idx]);
      List.Delete(idx);
    end;

  finally
    UnlockList;
  end;
end;

destructor TBassWaveDataList.Destroy;
begin
  Clear;
  
  inherited;
end;

procedure TBassWaveDataList.FreeData(AData: Pointer);
begin
  FreeMem(AData, DataSize);
end;

end.
