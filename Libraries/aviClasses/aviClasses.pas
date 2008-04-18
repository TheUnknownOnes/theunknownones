//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit aviClasses;

interface

uses
  Classes,
  Windows,
  Sysutils,
  VFW,
  Graphics;

type
  TaviFile = class;

{==============================================================================}
//aviStream

  TaviStreamType = (astAudio,
                    astMidi,
                    astText,
                    astVideo);

  TaviStreamFlag = (asfDisabled,
                    asfFormatChanges);
  TaviStreamFlags = set of TaviStreamFlag;

  TaviStream = class
  protected
    FInterface : IAVIStream;
  public
    constructor Create(AFile : IAVIFile; AInfo : TAVIStreamInfo); virtual;
    constructor CreateFromBitmap(AFile : TaviFile; ABitmap : TBitmap; AFramesPerSecond : DWORD = 25); virtual;
  end;


{==============================================================================}
//aviFile

  TaviFileFlag = (affHasIndex,
                  affMustUseIndex,
                  affIsInterleaved,
                  affWasCaptureFile,
                  affCopyrighted);
  TaviFileFlags = set of TaviFileFlag;

  TaviFileCap = (afcCanRead,
                 afcCanWrite,
                 afcAllKeyFrames,
                 afcNoCompression);
  TaviFileCaps = set of TaviFileCap;

  TaviFile = class
  protected
    FInterface : IAVIFile;

    FFileInfo : TAVIFileInfo;

    procedure DoReadFileInfo;

    function ReadFileInfoIntValue(AIndex : Integer) : DWORD;
    function GetFileType: String;
    function GetCaps: TaviFileCaps;
    function GetFlags: TaviFileFlags;
  public
    constructor Create(AFileName : String; AMode : Longint = OF_READ); virtual;
    destructor Destroy(); override;

    property MaxBytesPerSecond    : DWORD index 1 read ReadFileInfoIntValue;
    property Streams              : DWORD index 4 read ReadFileInfoIntValue;
    property SuggestedBufferSize  : DWORD index 5 read ReadFileInfoIntValue;
    property Width                : DWORD index 6 read ReadFileInfoIntValue;
    property Height               : DWORD index 7 read ReadFileInfoIntValue;
    property Scale                : DWORD index 8 read ReadFileInfoIntValue;
    property Rate                 : DWORD index 9 read ReadFileInfoIntValue;
    property Length               : DWORD index 10 read ReadFileInfoIntValue;
    property Editcount            : DWORD index 11 read ReadFileInfoIntValue;
    property FileType             : String read GetFileType;
    property Flags                : TaviFileFlags read GetFlags;
    property Caps                 : TaviFileCaps read GetCaps;
  end;


{==============================================================================}
//Exception types

  EaviUNSUPPORTED     = type Exception;
  EaviBADFORMAT       = type Exception;
  EaviMEMORY          = type Exception;
  EaviINTERNAL        = type Exception;
  EaviBADFLAGS        = type Exception;
  EaviBADPARAM        = type Exception;
  EaviBADSIZE         = type Exception;
  EaviBADHANDLE       = type Exception;
  EaviFILEREAD        = type Exception;
  EaviFILEWRITE       = type Exception;
  EaviFILEOPEN        = type Exception;
  EaviCOMPRESSOR      = type Exception;
  EaviNOCOMPRESSOR    = type Exception;
  EaviREADONLY        = type Exception;
  EaviNODATA          = type Exception;
  EaviBUFFERTOOSMALL  = type Exception;
  EaviCANTCOMPRESS    = type Exception;
  EaviUSERABORT       = type Exception;
  EaviERROR           = type Exception;




{==============================================================================}
//Global helper

  function aviCheckForError(AResult : HRESULT;
                            ARaiseException : Boolean = true) : Boolean;
    //Returns true if AResult represents no error code




implementation {===============================================================}


{==============================================================================}
//Global helper

function aviCheckForError(AResult : HRESULT;
                          ARaiseException : Boolean = true) : Boolean;
var
  Error : Exception;
begin
  if AResult = AVIERR_OK then
    Result := true
  else
  begin
    Result := false;

    if ARaiseException then
    begin
      if AResult = AVIERR_UNSUPPORTED then Error := EaviUNSUPPORTED.Create('Operation not supported')
      else if AResult = AVIERR_BADFORMAT then Error := EaviBADFORMAT.Create('Bad format')
      else if AResult = AVIERR_MEMORY then Error := EaviMEMORY.Create('Insufficient memory')
      else if AResult = AVIERR_INTERNAL then Error := EaviINTERNAL.Create('Internal error')
      else if AResult = AVIERR_BADFLAGS then Error := EaviBADFLAGS.Create('Bad flags')
      else if AResult = AVIERR_BADPARAM then Error := EaviBADPARAM.Create('Bad parameter')
      else if AResult = AVIERR_BADSIZE then Error := EaviBADSIZE.Create('Bad size')
      else if AResult = AVIERR_BADHANDLE then Error := EaviBADHANDLE.Create('Bad handle')
      else if AResult = AVIERR_FILEREAD then Error := EaviFILEREAD.Create('Error while reading file')
      else if AResult = AVIERR_FILEWRITE then Error := EaviFILEWRITE.Create('Error while writing file')
      else if AResult = AVIERR_FILEOPEN then Error := EaviFILEOPEN.Create('Error while opening file')
      else if AResult = AVIERR_COMPRESSOR then Error := EaviCOMPRESSOR.Create('Compressor error ')
      else if AResult = AVIERR_NOCOMPRESSOR then Error := EaviNOCOMPRESSOR.Create('No suitable compressor')
      else if AResult = AVIERR_READONLY then Error := EaviREADONLY.Create('File is read only')
      else if AResult = AVIERR_NODATA then Error := EaviNODATA.Create('No data supplied')
      else if AResult = AVIERR_BUFFERTOOSMALL then Error := EaviBUFFERTOOSMALL.Create('Buffer too small')
      else if AResult = AVIERR_CANTCOMPRESS then Error := EaviCANTCOMPRESS.Create('Can not compress')
      else if AResult = AVIERR_USERABORT then Error := EaviUSERABORT.Create('Abort by user')
      else Error := EaviERROR.Create('Generic error');

      raise Error;
    end;
    
  end;
end;




{==============================================================================}
{ TaviFile }

constructor TaviFile.Create(AFileName: String; AMode: Integer);
begin
  FInterface := nil;

  aviCheckForError(AVIFileOpen(FInterface, PChar(AFileName), AMode, nil));
end;

destructor TaviFile.Destroy;
begin

  inherited;
end;

procedure TaviFile.DoReadFileInfo;
begin
  FillMemory(@FFileInfo, SizeOf(FFileInfo), 0);
  
  aviCheckForError(AVIFileInfo(FInterface, FFileInfo, SizeOf(FFileInfo)))
end;

function TaviFile.GetCaps: TaviFileCaps;
begin
  DoReadFileInfo;

  Result := [];
  if FFileInfo.dwCaps and AVIFILECAPS_CANREAD = AVIFILECAPS_CANREAD then
    Include(Result, afcCanRead);
  if FFileInfo.dwCaps and AVIFILECAPS_CANWRITE = AVIFILECAPS_CANWRITE then
    Include(Result, afcCanWrite);
  if FFileInfo.dwCaps and AVIFILECAPS_ALLKEYFRAMES = AVIFILECAPS_ALLKEYFRAMES then
    Include(Result, afcAllKeyFrames);
  if FFileInfo.dwCaps and AVIFILECAPS_NOCOMPRESSION = AVIFILECAPS_NOCOMPRESSION then
    Include(Result, afcNoCompression); 
end;

function TaviFile.GetFileType: String;
begin
  DoReadFileInfo;

  Result := FFileInfo.szFileType;
end;

function TaviFile.GetFlags: TaviFileFlags;
begin
  DoReadFileInfo;

  Result := [];
  if FFileInfo.dwFlags and AVIFILEINFO_HASINDEX = AVIF_HASINDEX then
    Include(Result, affHasIndex);
  if FFileInfo.dwFlags and AVIFILEINFO_MUSTUSEINDEX = AVIFILEINFO_MUSTUSEINDEX then
    Include(Result, affMustUseIndex);
  if FFileInfo.dwFlags and AVIFILEINFO_ISINTERLEAVED = AVIFILEINFO_ISINTERLEAVED then
    Include(Result, affIsInterleaved);
  if FFileInfo.dwFlags and AVIFILEINFO_WASCAPTUREFILE = AVIFILEINFO_WASCAPTUREFILE then
    Include(Result, affWasCaptureFile);
  if FFileInfo.dwFlags and AVIFILEINFO_COPYRIGHTED = AVIFILEINFO_COPYRIGHTED then
    Include(Result, affCopyrighted);
end;

function TaviFile.ReadFileInfoIntValue(AIndex: Integer): DWORD;
begin
  DoReadFileInfo;

  case AIndex of
    1: Result := FFileInfo.dwMaxBytesPerSec;
    2: Result := FFileInfo.dwFlags;
    3: Result := FFileInfo.dwCaps;
    4: Result := FFileInfo.dwStreams;
    5: Result := FFileInfo.dwSuggestedBufferSize;
    6: Result := FFileInfo.dwWidth;
    7: Result := FFileInfo.dwHeight;
    8: Result := FFileInfo.dwScale;
    9: Result := FFileInfo.dwRate;
    10: Result := FFileInfo.dwLength;
    11: Result := FFileInfo.dwEditCount;
  end;
end;

{ TaviStream }

constructor TaviStream.Create(AFile: IAVIFile; AInfo: TAVIStreamInfo);
begin
  FInterface := nil;
  
  aviCheckForError(AVIFileCreateStream(AFile, FInterface, AInfo));
end;

constructor TaviStream.CreateFromBitmap(AFile: TaviFile; ABitmap: TBitmap;
  AFramesPerSecond: DWORD);
var
  Info : TAVIStreamInfo;
  BitmapBits : Pointer;
  BitmapInfo : PBitmapInfoHeader;
  BitmapBitsSize,
  BitmapInfoSize : DWORD;

begin
  FillMemory(@Info, SizeOf(Info), 0);

  GetDIBSizes(ABitmap.Handle, BitmapInfoSize, BitmapBitsSize);
  GetMem(BitmapInfo, BitmapInfoSize);
  GetMem(BitmapBits, BitmapBitsSize);
  try
    GetDIB(ABitmap.Handle, 0, BitmapInfo^, BitmapBits^);

    Info.fccType := streamtypeVIDEO;
    Info.fccHandler := 0;
    info.dwScale := 1;
    Info.dwRate := AFramesPerSecond;
    Info.dwSuggestedBufferSize := BitmapBitsSize;
    Info.rcFrame.Right := ABitmap.Width;
    Info.rcFrame.Bottom := ABitmap.Height;

    Create(AFile.FInterface, Info);

    aviCheckForError(AVIStreamSetFormat(FInterface, 0, BitmapInfo, BitmapInfoSize));
  finally
    FreeMem(BitmapInfo, BitmapInfoSize);
    FreeMem(BitmapBits, BitmapBitsSize);
  end;
end;

initialization
  AVIFileInit;

finalization
  AVIFileExit;
    
end.
