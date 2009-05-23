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

  TBassFFTResolution = (bfr128,
                   bfr256,
                   bfr512,
                   bfr1024,
                   bfr2048,
                   bfr4096);

function Bass_GetSampleFormatFromChannel(AChannel : Cardinal) : TBassSampleFormat;
function Bass_GetChannelsFromChannel(AChannel : Cardinal) : Cardinal;

function Bass_GetFFTLength(AFFTWidth : TBassFFTResolution) : Cardinal;
function Bass_GetFFTCount(AFFTWidth : TBassFFTResolution) : Integer;
function Bass_AllocFFTBuffer(AFFTWidth : TBassFFTResolution) : Pointer;
procedure Bass_FreeFFTBuffer(AFFTWidth : TBassFFTResolution; var ABuffer : Pointer);

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

function Bass_GetFFTLength(AFFTWidth : TBassFFTResolution) : Cardinal;
begin
  case AFFTWidth of
    bfr128: Result := BASS_DATA_FFT256;
    bfr256: Result := BASS_DATA_FFT512;
    bfr512: Result := BASS_DATA_FFT1024;
    bfr1024: Result := BASS_DATA_FFT2048;
    bfr2048: Result := BASS_DATA_FFT4096;
    bfr4096: Result := BASS_DATA_FFT8192;
    else
      Result := 0;
  end;
end;

function Bass_GetFFTCount(AFFTWidth : TBassFFTResolution) : Integer;
begin
  case AFFTWidth of
    bfr128: Result := 128;
    bfr256: Result := 256;
    bfr512: Result := 512;
    bfr1024: Result := 1024;
    bfr2048: Result := 2048;
    bfr4096: Result := 4096;
    else
      Result := 0;
  end;
end;

function Bass_AllocFFTBuffer(AFFTWidth : TBassFFTResolution) : Pointer;
begin
  case AFFTWidth of
    bfr128: GetMem(Result, 128);
    bfr256: GetMem(Result, 256);
    bfr512: GetMem(Result, 512);
    bfr1024: GetMem(Result, 1024);
    bfr2048: GetMem(Result, 2048);
    bfr4096: GetMem(Result, 4096);
    else
      Result := nil;
  end;
end;

procedure Bass_FreeFFTBuffer(AFFTWidth : TBassFFTResolution; var ABuffer : Pointer);
begin
  case AFFTWidth of
    bfr128: FreeMem(ABuffer, 128);
    bfr256: FreeMem(ABuffer, 256);
    bfr512: FreeMem(ABuffer, 512);
    bfr1024: FreeMem(ABuffer, 1024);
    bfr2048: FreeMem(ABuffer, 2048);
    bfr4096: FreeMem(ABuffer, 4096);
  end;
end;

end.
