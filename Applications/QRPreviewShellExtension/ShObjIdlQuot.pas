{-------------------------------------------------------------------------------
 Unit Name: ShObjIdlQuot
 Author   : hans gulo (HG)
 Purpose  : Some types and constants translated to Delphi from ShObjIdl.h
            and ShlObj.h
-------------------------------------------------------------------------------}

unit ShObjIdlQuot;

interface

uses Windows, ActiveX;

type
  { from ShlObjIdl.h }
  IExtractImage = interface
    ['{BB2E617C-0920-11D1-9A0B-00C04FC2D6C1}']
    function GetLocation(Buffer: POleStr;
                         BufferSize: DWORD;
                         var Priority: DWORD;
                         var Size: TSize;
                         ColorDepth: DWORD;
                         var Flags: DWORD): HResult; stdcall;
    function Extract(var BitmapHandle: HBITMAP): HResult; stdcall;
  end;

  IRunnableTask = interface
    ['{85788D00-6807-11D0-B810-00C04FD706EC}']
    function Run: HResult; stdcall;
    function Kill(fWait: BOOL): HResult; stdcall;
    function Suspend: HResult; stdcall;
    function Resume: HResult; stdcall;
    function IsRunning: Longint; stdcall;
  end;

const
  { from ShlObjIdl.h }
  ITSAT_MAX_PRIORITY      = 2;
  ITSAT_MIN_PRIORITY      = 1;
  ITSAT_DEFAULT_PRIORITY  = 0;

  IEI_PRIORITY_MAX        = ITSAT_MAX_PRIORITY;
  IEI_PRIORITY_MIN        = ITSAT_MIN_PRIORITY;
  IEIT_PRIORITY_NORMAL    = ITSAT_DEFAULT_PRIORITY;

  IEIFLAG_ASYNC     = $001; // ask the extractor if it supports ASYNC extract
                            // (free threaded)
  IEIFLAG_CACHE     = $002; // returned from the extractor if it does NOT cache
                            // the thumbnail
  IEIFLAG_ASPECT    = $004; // passed to the extractor to beg it to render to
                            // the aspect ratio of the supplied rect
  IEIFLAG_OFFLINE   = $008; // if the extractor shouldn't hit the net to get
                            // any content needs for the rendering
  IEIFLAG_GLEAM     = $010; // does the image have a gleam? this will be
                            // returned if it does
  IEIFLAG_SCREEN    = $020; // render as if for the screen  (this is exlusive
                            // with IEIFLAG_ASPECT )
  IEIFLAG_ORIGSIZE  = $040; // render to the approx size passed, but crop if
                            // neccessary
  IEIFLAG_NOSTAMP   = $080; // returned from the extractor if it does NOT want
                            // an icon stamp on the thumbnail
  IEIFLAG_NOBORDER  = $100; // returned from the extractor if it does NOT want
                            // an a border around the thumbnail
  IEIFLAG_QUALITY   = $200; // passed to the Extract method to indicate that
                            // a slower, higher quality image is desired,
                            // re-compute the thumbnail

implementation

end.
