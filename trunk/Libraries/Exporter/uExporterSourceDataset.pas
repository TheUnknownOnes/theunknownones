unit uExporterSourceDataset;

interface

uses
  uExporter, DB;

type
  TExporterSourceDataSet = class(TExporterSource2DBase)
  private
    FDataSet: TDataSet;
  protected
    procedure ValidateDataSet;

    function GetValue: Variant; override;
    function GetCaption(ADimension: Integer): WideString; override;

    function Navigate(ADimension: Integer; ANavigateDirection: TExporterNavigateDirections): TExporterNavigateResult; override;
    function NavigateRow(ANavigateDirection: TExporterNavigateDirections): TExporterNavigateResult; override;
    function NavigateColumn(ANavigateDirection: TExporterNavigateDirections): TExporterNavigateResult; override;
  public
    constructor Create(ADataSet: TDataSet);
    property DataSet: TDataSet read FDataSet write FDataSet;
  end;

implementation

uses
  SysUtils;

{ TExporterSourceDataSet }

constructor TExporterSourceDataSet.Create(ADataSet: TDataSet);
begin
  inherited Create;
  FDataSet:=ADataSet;
end;

function TExporterSourceDataSet.GetCaption(ADimension: Integer): WideString;
begin
  ValidateDataSet;
  Result:='';

  if ADimension=1 then
    Result:=FDataSet.Fields[CurrentPos[1]].FieldName;
end;

function TExporterSourceDataSet.GetValue: Variant;
begin
  Result:=FDataSet.Fields[Self.CurrentPos[1]].Value;
end;

function TExporterSourceDataSet.Navigate(ADimension: Integer;
  ANavigateDirection: TExporterNavigateDirections): TExporterNavigateResult;
begin
  ValidateDataSet;

  case ADimension of
    0 : Result:=NavigateRow(ANavigateDirection);
    1 : Result:=NavigateColumn(ANavigateDirection);
  end;
end;

function TExporterSourceDataSet.NavigateColumn(
  ANavigateDirection: TExporterNavigateDirections): TExporterNavigateResult;
begin
  case ANavigateDirection of
    ndBeginning: CurrentPos[1]:=0;
    ndPrevious: dec(CurrentPos[1]);
    ndNext: inc(CurrentPos[1]);
    ndEnd: CurrentPos[1]:=FDataSet.FieldCount-1;
  end;

  if CurrentPos[1]<0 then
    Result:=nrBOF
  else
  if CurrentPos[1]>FDataSet.FieldCount-1 then
    Result:=nrEOF
  else
    Result:=nrOK;
end;

function TExporterSourceDataSet.NavigateRow(
  ANavigateDirection: TExporterNavigateDirections): TExporterNavigateResult;
begin
  case ANavigateDirection of
    ndBeginning: begin
                   FDataSet.First;
                   CurrentPos[0]:=0;
                 end;
    ndPrevious: begin
                  FDataSet.Prior;
                  dec(CurrentPos[0]);
                end;
    ndNext:     begin
                  FDataSet.Next;
                  inc(CurrentPos[0]);
                end;
    ndEnd:      begin
                  FDataSet.Last;
                  CurrentPos[0]:=FDataSet.RecordCount-1;
                end;
  end;

  if FDataSet.Eof then
    Result:=nrEOF
  else
  if FDataSet.Bof then
    Result:=nrBOF
  else
    Result:=nrOK;
end;

procedure TExporterSourceDataSet.ValidateDataSet;
begin
  if not Assigned(FDataSet) then
    raise Exception.Create('Datenquelle nicht verfügbar');
end;

end.
