unit uExporterSourceVST;

interface

uses
  uExporter, VirtualTrees;

type
  TExporterSourceVST = class(TExporterSource2DBase)
  private
    FTree: TVirtualStringTree;
    FCurrentNode : PVirtualNode;
  protected
    procedure ValidateDataSet;

    function GetValue: Variant; override;
    function GetCaption(ADimension: Integer): String; override;

    function Navigate(ADimension: Integer; ANavigateDirection: TExporterNavigateDirections): TExporterNavigateResult; override;
    function NavigateRow(ANavigateDirection: TExporterNavigateDirections): TExporterNavigateResult; override;
    function NavigateColumn(ANavigateDirection: TExporterNavigateDirections): TExporterNavigateResult; override;
  public
    constructor Create(ATree: TVirtualStringTree);
    property Tree: TVirtualStringTree read FTree write FTree;
  end;

implementation

uses
  SysUtils;

{ TExporterSourceVST }

constructor TExporterSourceVST.Create(ATree: TVirtualStringTree);
begin
  inherited Create;
  FTree:=ATree;
end;

function TExporterSourceVST.GetCaption(ADimension: Integer): String;
begin
  ValidateDataSet;
  Result:='';

  if ADimension=1 then
    Result:=FTree.Header.Columns[CurrentPos[1]].CaptionText;
end;

function TExporterSourceVST.GetValue: Variant;
var
  StrResult: String;
  DoubleResult : Double;
  DTResult: TDateTime;
begin
  FTree.OnGetText(FTree, FCurrentNode, Self.CurrentPos[1], ttNormal, StrResult);
  if TryStrToFloat(StrResult, DoubleResult) then
    Result:=DoubleResult
  else
  if TryStrToDateTime(StrResult, DTResult) then
    Result:=DTResult
  else
    Result:=StrResult;
end;

function TExporterSourceVST.Navigate(ADimension: Integer;
  ANavigateDirection: TExporterNavigateDirections): TExporterNavigateResult;
begin
  ValidateDataSet;

  case ADimension of
    0 : Result:=NavigateRow(ANavigateDirection);
    1 : Result:=NavigateColumn(ANavigateDirection);
  end;
end;

function TExporterSourceVST.NavigateColumn(
  ANavigateDirection: TExporterNavigateDirections): TExporterNavigateResult;
begin
  case ANavigateDirection of
    ndBeginning: CurrentPos[1]:=0;
    ndPrevious: dec(CurrentPos[1]);
    ndNext: inc(CurrentPos[1]);
    ndEnd: CurrentPos[1]:=FTree.Header.Columns.Count-1;
  end;

  if CurrentPos[1]<0 then
    Result:=nrBOF
  else
  if CurrentPos[1]>FTree.Header.Columns.Count-1 then
    Result:=nrEOF
  else
    Result:=nrOK;
end;

function TExporterSourceVST.NavigateRow(
  ANavigateDirection: TExporterNavigateDirections): TExporterNavigateResult;
begin
  case ANavigateDirection of
    ndBeginning: begin
                   FCurrentNode:=FTree.GetFirst;
                   CurrentPos[0]:=0;
                 end;
    ndPrevious: begin
                  FCurrentNode:=FTree.GetPrevious(FCurrentNode);
                  dec(CurrentPos[0]);
                end;
    ndNext:     begin
                  FCurrentNode:=FTree.GetNext(FCurrentNode);
                  inc(CurrentPos[0]);
                end;
    ndEnd:      begin
                  FCurrentNode:=FTree.GetLast;
                  CurrentPos[0]:=FTree.GetLast.Index;
                end;
  end;

  if (ANavigateDirection=ndNext) and (FCurrentNode=nil) then
    Result:=nrEOF
  else
  if (ANavigateDirection=ndPrevious) and (FCurrentNode=nil) then
    Result:=nrBOF
  else
    Result:=nrOK;
end;

procedure TExporterSourceVST.ValidateDataSet;
begin
  if not Assigned(FTree) then
    raise Exception.Create('Datenquelle nicht verfügbar');
end;

end.
