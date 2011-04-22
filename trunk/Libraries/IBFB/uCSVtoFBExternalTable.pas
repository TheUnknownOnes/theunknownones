//**********************************************************
// Developed by TheUnkownOnes.net
//
// for more information look at www.TheUnknownOnes.net
//**********************************************************

unit uCSVtoFBExternalTable;



interface

uses
  Windows, Types, Classes, SysUtils, StrUtils,
  uib, uFBEmbedded, dialogs;

type
  TCSVtoFBExternalTable = class
  private
    FCSV_Delimiter: Char;
    FCSV_QuoteChar: Char;
    FTable_FieldDef: String;
    FCSV_SkipLines: Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Convert(ACSVFile : String; AExternalTable : String);

    property CSV_Delimiter : Char read FCSV_Delimiter write FCSV_Delimiter;
    property CSV_QuoteChar : Char read FCSV_QuoteChar write FCSV_QuoteChar;
    property CSV_SkipLines : Integer read FCSV_SkipLines write FCSV_SkipLines;
    property Table_FieldDef : String read FTable_FieldDef write FTable_FieldDef;
  end;

implementation

{ TCSVtoFBExternalTable }

procedure TCSVtoFBExternalTable.Convert(ACSVFile, AExternalTable: String);
var
  DB : TUIBDataBase;
  Trans : TUIBTransaction;
  Qry : TUIBQuery;
  sl : TStringList;
  idx : Integer;
  sr : TStreamReader;
  FBE : TFBEmbedded_2_5_0;
const
  table_name = 'IMPORT';
  qry_fieldnames = 'select trim(f.rdb$field_name) '+
                   'from rdb$relation_fields f '+
                   'where f.rdb$relation_name = ''' + table_name + ''' ' +
                   'order by f.rdb$field_position';
begin
  DB := TUIBDataBase.Create(nil);
  Trans := TUIBTransaction.Create(DB);
  Qry := TUIBQuery.Create(DB);
  sl := TStringList.Create;
  FBE := TFBEmbedded_2_5_0.Create(nil);
  try
    fbe.Config.Values['ExternalFileAccess'] := 'Full';
    fbe.Active := true;

    DB.UserName := 'SYSDBA';
    DB.LibraryName := fbe.LibraryName;
    DB.DatabaseName := IntToStr(GetCurrentThreadId) + '.fdb';
    if FileExists(Db.DatabaseName) then
      DeleteFile(db.DatabaseName);

    db.CreateDatabase(db.CharacterSet);

    db.Connected := true;

    Trans.DataBase := DB;
    Trans.StartTransaction;
    Trans.ExecuteImmediate('create table ' + table_name + ' external ''' +AExternalTable + ''' (' + FTable_FieldDef + ')');
    Trans.Commit;

    qry.DataBase := Db;
    qry.Transaction := Trans;
    qry.SQl.Text := qry_fieldnames;
    qry.Open();
    while not qry.Eof do
    begin
      sl.Add(qry.Fields.AsString[0]);
      qry.next;
    end;
    qry.Close();

    qry.SQL.Text := 'insert into ' + table_name + '(' + sl.CommaText + ') values (';
    for idx := 0 to sl.Count - 1 do
      qry.SQL.Add(ifthen(idx > 0, ',') + ':' + sl[idx]);
    qry.SQL.Add(')');

    qry.Prepare();

    sl.Delimiter := FCSV_Delimiter;
    sl.QuoteChar := FCSV_QuoteChar;
    sl.StrictDelimiter := true;

    sr := TStreamReader.Create(ACSVFile);
    try
      for idx := 1 to FCSV_SkipLines do sr.ReadLine;

      while not sr.EndOfStream do
      begin
        sl.DelimitedText := sr.ReadLine;
        for idx := 0 to sl.Count - 1 do
          qry.Params.AsString[idx] := sl[idx];
        Qry.Execute;
      end;
    finally
      sr.Free;
    end;

    trans.Commit;

    db.Connected := false;
    DeleteFile(db.DatabaseName);
  finally
    DB.Free;
    sl.Free;
    fbe.Free;
  end;
end;

constructor TCSVtoFBExternalTable.Create;
begin
  FCSV_Delimiter := ',';
  FCSV_QuoteChar := #0;
  FCSV_SkipLines := 1;
end;

destructor TCSVtoFBExternalTable.Destroy;
begin

  inherited;
end;


end.
