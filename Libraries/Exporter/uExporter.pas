unit uExporter;

interface

uses
  Classes, Variants, SysUtils;

type
  TExporterNavigateDirections = (ndBeginning, ndPrevious, ndNext, ndEnd);
  TExporterNavigateResult = (nrBOF, nrOK, nrEOF);

  TExporterPos = array of Integer;

  TExporterSourceBase = class(TInterfacedObject)
  private
    FCurrentPos: TExporterPos;
  protected
    constructor Create;

    function GetDimensionCount: Integer; virtual;
  public
    function GetValue: Variant; virtual;
    function GetCaption(ADimension: Integer): String; virtual;
    function Navigate(ADimension: Integer; ANavigateDirection: TExporterNavigateDirections): TExporterNavigateResult; virtual; abstract;

    property CurrentPos: TExporterPos read FCurrentPos;
  end;

  TExporterSource1DBase = class(TExporterSourceBase)
  protected
    function GetDimensionCount: Integer; override;
  public
    function NavigateRow(ANavigateDirection: TExporterNavigateDirections): TExporterNavigateResult; virtual; abstract;
  end;

  TExporterSource2DBase = class(TExporterSourceBase)
  protected
    function GetDimensionCount: Integer; override;
  public
    function NavigateColumn(ANavigateDirection: TExporterNavigateDirections): TExporterNavigateResult; virtual; abstract;
    function NavigateRow(ANavigateDirection: TExporterNavigateDirections): TExporterNavigateResult; virtual; abstract;
  end;

  TExporterSource3DBase = class(TExporterSourceBase)
  protected
    function GetDimensionCount: Integer; override;
  public
    function NavigatePage(ANavigateDirection: TExporterNavigateDirections): TExporterNavigateResult; virtual; abstract;
    function NavigateColumn(ANavigateDirection: TExporterNavigateDirections): TExporterNavigateResult; virtual; abstract;
    function NavigateRow(ANavigateDirection: TExporterNavigateDirections): TExporterNavigateResult; virtual; abstract;
  end;

  TExporterDestinationBase = class(TInterfacedObject)
  protected
    function Execute(ASource: TExporterSourceBase): Boolean; virtual; abstract;
  end;

  TExporterDestinationBaseClass = class of TExporterDestinationBase;

  TExporter = class(TInterfacedObject)
  private
    FDestination: TExporterDestinationBase;
    FSource: TExporterSourceBase;
  public
    property Source: TExporterSourceBase read FSource write FSource;
    property Destination: TExporterDestinationBase read FDestination write FDestination;

    function Execute: Boolean; overload;
    class function Execute(ASource: TExporterSourceBase;
                           ADestination: TExporterDestinationBase): Boolean; overload;
  end;

implementation

uses
  Forms, Controls;

{ TExporterSourceBase }

constructor TExporterSourceBase.Create;
begin
  inherited;
  SetLength(FCurrentPos, GetDimensionCount);
end;

function TExporterSourceBase.GetCaption(ADimension: Integer): String;
begin
  Result:='';
end;

function TExporterSourceBase.GetDimensionCount: Integer;
begin
  Result:=0;
end;

function TExporterSourceBase.GetValue: Variant;
begin
  Result:=Null;
end;

{ TExporter }

function TExporter.Execute: Boolean;
begin
  if not Assigned(FDestination) then
    raise Exception.Create('Ungültiges Exportziel');

  if not Assigned(FSource) then
    raise Exception.Create('Ungültige Exportquelle');

  Screen.Cursor:=crHourGlass;
  try
    Result:=FDestination.Execute(FSource);
  finally
    Screen.Cursor:=crDefault;
  end;
end;

class function TExporter.Execute(ASource: TExporterSourceBase;
  ADestination: TExporterDestinationBase): Boolean;
var
  exp : TExporter;
begin
  exp:=TExporter.Create;
  try
    exp.Destination:=ADestination;
    exp.Source:=ASource;
    Result:=exp.Execute;
  finally
    exp.Free;
  end;
end;

{ TExporterSource2DBase }

function TExporterSource2DBase.GetDimensionCount: Integer;
begin
  Result:=2;
end;

{ TExporterSource3DBase }

function TExporterSource3DBase.GetDimensionCount: Integer;
begin
  Result:=3;
end;

{ TExporterSource1DBase }

function TExporterSource1DBase.GetDimensionCount: Integer;
begin
  Result:=1;
end;


end.
