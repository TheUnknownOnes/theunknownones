{----------------------------------------------------------------------------- 
 Purpose: Implement Beep function with Bass library 
 
 (c) by TheUnknownOnes under dwywbdbu license - see http://theunknownones.googlecode.com/svn/ for the license
 see http://www.TheUnknownOnes.net
-----------------------------------------------------------------------------}

unit uBassBeep;

interface

uses
  Windows, Sysutils, Bass, classes;

procedure BassBeep(AFrequency : Integer; ALength : Integer; AVolume : Byte = 255; AASync : Boolean = false);

implementation
                            
const
  SampleRate = 44100;

type
  TSineWaveInfo = record
    Frequency : Cardinal;
    TotalSamples,
    RestSamples : Integer;
    Offset : Integer;
    Amp : Smallint;
  end;
  PSineWaveInfo = ^TSineWaveInfo;

function WriteSineWave(Handle : HSTREAM; Buffer : Pointer; Len : DWORD; User : Pointer) : DWORD; stdcall;
var
  y : Single;
  BufferWidth : Integer;
  PointsPerWave : Integer;
  PointInBuffer : PSmallInt;
  Info : PSineWaveInfo;
  x : Integer;
  FadeFromSample : Integer;
const
  ZPi = 2 * Pi;
  FadeSteps = 128;
begin
  Info := User;

  PointInBuffer := Buffer;
  BufferWidth := Len div SizeOf(Word);
  FadeFromSample := info.TotalSamples div FadeSteps;

  PointsPerWave := SampleRate div info.Frequency;

  x := info.Offset;

  while (x < BufferWidth + info.Offset) do
  begin
    y := sin((x / PointsPerWave) * ZPi);

    if (info.RestSamples <= FadeFromSample) then
    begin
      if info.Amp > 0 then
        Dec(info.Amp, info.Amp div FadeSteps)
      else
        break
    end;

    PointInBuffer^ := Trunc(y * Info.Amp);
    Inc(PointInBuffer);

    Dec(info.RestSamples);
    Inc(x);
  end;

  Result := (x - Info.Offset) * SizeOf(Word);

  Info.Offset := x mod PointsPerWave;

  if info.RestSamples <= 0 then
  begin
    Result := Result or BASS_STREAMPROC_END;

    Dispose(Info);
  end;

end;

procedure BassBeep(AFrequency : Integer; ALength : Integer; AVolume : Byte = 255; AASync : Boolean = false);
var
  s : Cardinal;
  info : PSineWaveInfo;
  e : THandle;
begin
  New(info);
  info.Frequency := AFrequency;
  info.TotalSamples := Trunc((ALength / 1000) * SampleRate);
  info.RestSamples := info.TotalSamples;
  info.Offset := 0;
  info.Amp := Trunc((MAXWORD div 2) * (AVolume / 255));

  s := BASS_StreamCreate(SampleRate, 1, BASS_STREAM_AUTOFREE, @WriteSineWave, info);
  BASS_ChannelPlay(s, false);

  if not AASync then
  begin
    e := CreateEvent(nil, true, false, '');
    try
    
      while BASS_ChannelIsActive(s) <> BASS_ACTIVE_STOPPED do
      begin
        WaitForSingleObject(e, 1);
      end;

    finally
      CloseHandle(e);
    end;
    
  end;
end;


end.
