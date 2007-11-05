//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit PNGImageList;

interface

{$R 'PNGImagelist.res'}

uses
  Classes,
  Controls,
  SysUtils,
  PNGImage,
  PNGCommon,
  Windows,
  Graphics,
  BaseImageList;

type
  {All forward declarations}
  TCustomPNGImageList = class;


  {TCustomPNGImageList
   - the mother of all PNGImage lists
   - use this ton implement your own list}
  TCustomPNGImageList = class(TBaseImagelist)
  protected
    FImages : TPNGObjectList;

    procedure OnListChange(Sender : TPNGObjectList; AIndex : Integer);

    function GetItem(AIndex: Integer): TPNGObject; virtual;
    procedure SetItem(AIndex: Integer; const Value: TPNGObject); virtual;
    property Items[AIndex : Integer] : TPNGObject read GetItem write SetItem; default;

    function GetCount: Integer; override;

    procedure SetImages(const Value: TPNGObjectList); virtual;

    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override;

    procedure Draw(AIndex : Integer; ACanvas : HDC; APos : TPoint; AStates : TilDrawStates); overload; override;
    procedure Draw(AIndex : Integer; ACanvas : TCanvas; APos : TPoint; AStates : TilDrawStates); overload; override;

    procedure GetImageSize(AIndex : Integer; out AWidth, AHeight : Integer); override;

    property Images : TPNGObjectList read FImages write SetImages;
  end;


  {TPNGImageList
   - the PNGImagelist for generic use}
  TPNGImageList = class(TCustomPNGImageList)
  public
    property Items;
    property Count;
  published
    property Images;
    
    property OnChange;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Imagelists',[TPNGImageList]);
end;

{ TCustomPNGImageList }

constructor TCustomPNGImageList.Create(AOwner: TComponent);
begin
  FImages:=TPNGObjectList.Create;
  FImages.OnChange:=OnListChange;

  inherited;
end;

procedure TCustomPNGImageList.DefineProperties(Filer: TFiler);
begin
  inherited;

  Filer.DefineBinaryProperty('Items', FImages.LoadFromStream, FImages.SaveToStream, Images.Count>0);
end;

destructor TCustomPNGImageList.Destroy;
begin
  FImages.Free;

  inherited;
end;

procedure TCustomPNGImageList.Draw(AIndex: Integer; ACanvas: TCanvas;
  APos: TPoint; AStates: TilDrawStates);
var
  PNG : TPNGObject;
begin
  PNG:=Items[AIndex];
  ACanvas.Draw(APos.X, APos.Y, PNG);
end;

procedure TCustomPNGImageList.Draw(AIndex: Integer; ACanvas: HDC; APos: TPoint;
  AStates: TilDrawStates);
var
  Canv : TCanvas;
begin
  Canv:=TCanvas.Create;
  try
    Canv.Handle:=ACanvas;
    Draw(AIndex, Canv, APos, AStates);
  finally
    Canv.Free;
  end;
end;

function TCustomPNGImageList.GetCount: Integer;
begin
  Result:=FImages.Count;
end;

procedure TCustomPNGImageList.GetImageSize(AIndex: Integer; out AWidth,
  AHeight: Integer);
var
  PNG : TPNGObject;
begin
  PNG:=Items[AIndex];

  AWidth:=PNG.Width;
  AHeight:=PNG.Height;
end;

function TCustomPNGImageList.GetItem(AIndex: Integer): TPNGObject;
begin
  Result:=FImages[AIndex];
end;

procedure TCustomPNGImageList.OnListChange(Sender: TPNGObjectList;
  AIndex: Integer);
begin
  DoChange(AIndex);
end;

procedure TCustomPNGImageList.SetItem(AIndex: Integer; const Value: TPNGObject);
begin
  FImages[AIndex].Assign(Value);
end;                 

procedure TCustomPNGImageList.SetImages(const Value: TPNGObjectList);
begin
  FImages.Assign(Value);
end;

end.
