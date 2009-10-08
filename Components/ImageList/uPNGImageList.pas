//**********************************************************
// Developed by TheUnkownOnes.net
// 
// for more information look at www.TheUnknownOnes.net
//**********************************************************
unit uPNGImageList;

interface

{$R 'PNGImagelist.res'}

uses
  Classes,
  Controls,
  SysUtils,
  PNGImage,
  uPNGCommon,
  Windows,
  Graphics,
  uBaseImageList;

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
    procedure DoDraw(AIndex: Integer; ACanvas: TCanvas; APos: TPoint;
      AStates: TilDrawStates);  override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy(); override; 

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

implementation

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

procedure TCustomPNGImageList.DoDraw(AIndex: Integer; ACanvas: TCanvas;
  APos: TPoint; AStates: TilDrawStates);
var
  PNG : TPNGObject;
begin
  inherited;

  PNG:=Items[AIndex];
  ACanvas.Draw(APos.X, APos.Y, PNG);
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
