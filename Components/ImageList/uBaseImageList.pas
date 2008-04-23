//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uBaseImageList;

interface

uses
  Classes,
  Graphics,
  Windows;

type
  TBaseImagelist = class;

  TImagelistChangeProc = procedure(AImagelist : TBaseImagelist; AIndex : Integer) of object;
  
  TImageListChangeLink = class
  {- Use a object of this class, to get informed about changes of the imagelist
   - just create a object and register it with "RegisterChangeLink"}
  private
    FImageList: TBaseImageList;

    FOnChange: TImagelistChangeProc;

    procedure DoChange(AIndex : Integer);
  public
    destructor Destroy(); override;

    property OnChange : TImagelistChangeProc read FOnChange write FOnChange;
    property ImageList : TBaseImagelist read FImageList;
  end;


  TilDrawState = (ildEnabled,
                  ildDisabled,
                  ildHighlight,
                  ildFocused,
                  ildSelected);
  TilDrawStates = set of TilDrawState;

  TBaseImagelist = class(TComponent)
  {- the mother of all image lists}
  protected
    FChangeLinks : TList;

    FOnChange: TImagelistChangeProc;

    function GetCount: Integer; virtual;

    procedure DoChange(AIndex : Integer);
    function IsValidIndex(AIndex: Integer): Boolean;
    procedure DoDraw(AIndex : Integer; ACanvas : TCanvas; APos : TPoint; AStates : TilDrawStates); virtual;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;

    property Count : Integer read GetCount;

    procedure Draw(AIndex : Integer; ACanvas : HDC; APos : TPoint; AStates : TilDrawStates); overload;
    procedure Draw(AIndex : Integer; ACanvas : TCanvas; APos : TPoint; AStates : TilDrawStates); overload;

    procedure RegisterChangeLink(AChangeLink : TImageListChangeLink); virtual;
    procedure UnregisterChangeLink(AChangeLink : TImageListChangeLink); virtual;

    property OnChange : TImagelistChangeProc read FOnChange write FOnChange;

    procedure GetImageSize(AIndex : Integer; out AWidth, AHeight : Integer); virtual;
  end;



implementation

uses
  SysUtils;

{ TBaseImageList }

constructor TBaseImagelist.Create(AOwner: TComponent);
begin
  FChangeLinks:=TList.Create;

  inherited;
end;

destructor TBaseImagelist.Destroy;
begin
  while FChangeLinks.Count>0 do
  begin
    TImageListChangeLink(FChangeLinks[0]).FImageList:=nil;
    FChangeLinks.Delete(0);
  end;

  FChangeLinks.Free;

  inherited;
end;


procedure TBaseImagelist.DoChange(AIndex: Integer);
var
  idx : Integer;
begin
  if Assigned(FOnChange) then
    FOnChange(Self, AIndex);

  for idx := 0 to FChangeLinks.Count - 1 do
    TImageListChangeLink(FChangeLinks[idx]).DoChange(AIndex);
end;

procedure TBaseImagelist.DoDraw(AIndex: Integer; ACanvas: TCanvas; APos: TPoint;
  AStates: TilDrawStates);
begin

end;

function TBaseImageList.IsValidIndex(AIndex : Integer) : Boolean;
begin
  Result := (AIndex>=0) and (AIndex < Count);
end;

procedure TBaseImagelist.Draw(AIndex: Integer; ACanvas: TCanvas; APos: TPoint;
  AStates: TilDrawStates);
begin
  if IsValidIndex(AIndex) then
    DoDraw(AIndex, ACanvas, APos, AStates);
end;

procedure TBaseImagelist.Draw(AIndex: Integer; ACanvas: HDC; APos: TPoint;
  AStates: TilDrawStates);
var
  Canv : TCanvas;
begin
  inherited;

  Canv:=TCanvas.Create;
  try
    Canv.Handle:=ACanvas;
    Draw(AIndex, Canv, APos, AStates);
  finally
    Canv.Free;
  end;
end;

function TBaseImagelist.GetCount: Integer;
begin
  Result:=-1;
end;

procedure TBaseImagelist.GetImageSize(AIndex: Integer; out AWidth,
  AHeight: Integer);
begin
  AWidth:=-1;
  AHeight:=-1;
end;

procedure TBaseImagelist.RegisterChangeLink(AChangeLink: TImageListChangeLink);
begin
  if FChangeLinks.IndexOf(AChangeLink)<=-1 then
  begin
    if Assigned(AChangeLink.FImageList) then
      AChangeLink.FImageList.UnregisterChangeLink(AChangeLink);

    AChangeLink.FImageList:=Self;
    FChangeLinks.Add(AChangeLink);
  end;
end;

procedure TBaseImagelist.UnregisterChangeLink(
  AChangeLink: TImageListChangeLink);
var
  idx : Integer;
begin
  idx:=FChangeLinks.IndexOf(AChangeLink);

  if idx>-1 then
  begin
    TImageListChangeLink(FChangeLinks[idx]).FImageList:=nil;
    FChangeLinks.Delete(idx);
  end;
end;

{ TImageListChangeLink }

destructor TImageListChangeLink.Destroy;
begin
  if Assigned(FImageList) then
    FImageList.UnregisterChangeLink(Self);

  inherited;
end;

procedure TImageListChangeLink.DoChange(AIndex: Integer);
begin
  if Assigned(FOnChange) then
    FOnChange(FImageList, AIndex)
end;

end.
