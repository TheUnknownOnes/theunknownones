unit uPS_Types;

interface

uses
  uPSCompiler,
  uPSRuntime,
  uPSUtils,
  Types,
  uPS_Helpers;

procedure PS_Register_Types_C(ACompiler : TPSPascalCompiler);
procedure PS_Register_Types_R(AExec : TPSExec; ARCi : TPSRuntimeClassImporter);

implementation

procedure PS_Register_Types_C(ACompiler : TPSPascalCompiler);
begin
  ACompiler.AddTypeS('TPoint', 'record X, Y : Longint; end;');
  ACompiler.AddTypeS('TRect', 'record Left, Top, Right, Bottom: Longint; end;');
  ACompiler.AddTypeS('TSize', 'record cx, cy: Longint; end;');
  ACompiler.AddTypeS('TSmallPoint', 'record X, Y : Smallint; end;');
  ACompiler.AddType('LargeInt', btS64);
  RegisterEnum(ACompiler, TypeInfo(TSplitRectType));

  ACompiler.AddDelphiFunction('function EqualRect(const R1, R2: TRect): Boolean;');
  ACompiler.AddDelphiFunction('function Rect(Left, Top, Right, Bottom: Integer): TRect;');
  ACompiler.AddDelphiFunction('function Bounds(ALeft, ATop, AWidth, AHeight: Integer): TRect;');
  ACompiler.AddDelphiFunction('function Point(X, Y: Integer): TPoint;');
  ACompiler.AddDelphiFunction('function SmallPoint(XY: LongWord): TSmallPoint;');
  ACompiler.AddDelphiFunction('function PtInRect(const Rect: TRect; const P: TPoint): Boolean;');
  ACompiler.AddDelphiFunction('function IntersectRect(out Rect: TRect; const R1, R2: TRect): Boolean;');
  ACompiler.AddDelphiFunction('function UnionRect(out Rect: TRect; const R1, R2: TRect): Boolean;');
  ACompiler.AddDelphiFunction('function IsRectEmpty(const Rect: TRect): Boolean;');
  ACompiler.AddDelphiFunction('function OffsetRect(var Rect: TRect; DX: Integer; DY: Integer): Boolean;');
  ACompiler.AddDelphiFunction('function CenterPoint(const Rect: TRect): TPoint;');
  ACompiler.AddDelphiFunction('function RectWidth(const Rect: TRect): Integer;');
  ACompiler.AddDelphiFunction('function RectHeight(const Rect: TRect): Integer;');
  ACompiler.AddDelphiFunction('function SplitRectSize(Rect: TRect; SplitType: TSplitRectType; Size: Integer): TRect;');
  ACompiler.AddDelphiFunction('function SplitRectPercent(Rect: TRect; SplitType: TSplitRectType; Percent: Double): TRect');
  ACompiler.AddDelphiFunction('function CenteredRect(SourceRect: TRect; CenteredRect: TRect): TRect;');
end;

procedure PS_Register_Types_R(AExec : TPSExec; ARCi : TPSRuntimeClassImporter);
type
  TFLongWordTSmallPoint = function(a1 : LongWord) : TSmallPoint;
  TFTRectTSplitRectTypeIntgerTRect = function(Rect: TRect; SplitType: TSplitRectType; Size: Integer): TRect;
  TFTRectTSplitRectTypeDoubleTRect = function(Rect: TRect; SplitType: TSplitRectType; Percent: Double): TRect;
var
  fLongWordTSmallPoint : TfLongWordTSmallPoint;
  FTRectTSplitRectTypeIntgerTRect : TFTRectTSplitRectTypeIntgerTRect;
  FTRectTSplitRectTypeDoubleTRect : TFTRectTSplitRectTypeDoubleTRect;
begin
  AExec.RegisterDelphiFunction(@EqualRect, 'EqualRect', cdRegister);
  AExec.RegisterDelphiFunction(@Rect, 'Rect', cdRegister);
  AExec.RegisterDelphiFunction(@Bounds, 'Bounds', cdRegister);
  AExec.RegisterDelphiFunction(@Point, 'Point', cdRegister);
  fLongWordTSmallPoint := SmallPoint; AExec.RegisterDelphiFunction(@fLongWordTSmallPoint, 'SmallPoint', cdRegister);
  AExec.RegisterDelphiFunction(@PtInRect, 'PtInRect', cdRegister);
  AExec.RegisterDelphiFunction(@IntersectRect, 'IntersectRect', cdRegister);
  AExec.RegisterDelphiFunction(@UnionRect, 'UnionRect', cdRegister);
  AExec.RegisterDelphiFunction(@IsRectEmpty, 'IsRectEmpty', cdRegister);
  AExec.RegisterDelphiFunction(@OffsetRect, 'OffsetRect', cdRegister);
  AExec.RegisterDelphiFunction(@CenterPoint, 'CenterPoint', cdRegister);
  AExec.RegisterDelphiFunction(@RectWidth, 'RectWidth', cdRegister);
  AExec.RegisterDelphiFunction(@RectHeight, 'RectHeight', cdRegister);
  FTRectTSplitRectTypeIntgerTRect := SplitRect; AExec.RegisterDelphiFunction(@FTRectTSplitRectTypeIntgerTRect, 'SplitRectSize', cdRegister);
  FTRectTSplitRectTypeDoubleTRect := SplitRect; AExec.RegisterDelphiFunction(@FTRectTSplitRectTypeDoubleTRect, 'SplitRectPercent', cdRegister);
  AExec.RegisterDelphiFunction(@CenteredRect, 'CenteredRect', cdRegister);
end;

end.
