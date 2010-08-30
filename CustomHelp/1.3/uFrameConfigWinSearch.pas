unit uFrameConfigWinSearch;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uFrameConfigColor, StdCtrls, ExtCtrls, CategoryButtons, Spin,
  uCustomHelpConsts, Buttons;

type
  TFrameConfigWinSearch = class(TFrame)
    Panel1: TPanel;
    EditName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    seMaxResults: TSpinEdit;
    SpeedButton1: TSpeedButton;
    mem_Query: TMemo;
    procedure SpeedButton1Click(Sender: TObject);
  private
    function GetProviderName: string;
    procedure SetProviderName(const Value: string);
    function GetMaxResults: Integer;
    function GetTimeout: Integer;
    procedure SetMaxResults(const Value: Integer);
    procedure SetTimeout(const Value: Integer);
    function GetSQL: String;
    procedure SetSQL(const Value: String);
    procedure SetDescription(const Value: String);
    function GetDescription: String;
    { Private-Deklarationen }
  public
    property ProviderName: string read GetProviderName write SetProviderName;
    property Timeout: Integer read GetTimeout write SetTimeout;
    property MaxResults: Integer read GetMaxResults write SetMaxResults;
    property SQL: String read GetSQL write SetSQL;
    property Description: String read GetDescription write SetDescription;

    procedure Save(AIndex: Integer);
  end;

implementation

uses uCustomHelpMain;

{$R *.dfm}

{ TFrameConfigWinSearch }

function TFrameConfigWinSearch.GetDescription: String;
begin

end;

function TFrameConfigWinSearch.GetMaxResults: Integer;
begin
  Result:=seMaxResults.Value;
end;

function TFrameConfigWinSearch.GetProviderName: string;
begin
  Result:=EditName.Text;
end;

function TFrameConfigWinSearch.GetSQL: String;
begin
  Result:= mem_Query.Text;
end;

function TFrameConfigWinSearch.GetTimeout: Integer;
begin
  Result:=-1;
end;

procedure TFrameConfigWinSearch.Save(AIndex: Integer);
begin
  GlobalCustomHelp.WriteProviderToRegistry(IntToStr(AIndex),
          Self.ProviderName,
          '',
          SQL,
          nstoTrimAll,
          ptWinSearch,
          True,
          Timeout,
          MaxResults);
end;

procedure TFrameConfigWinSearch.SetDescription(const Value: String);
begin

end;

procedure TFrameConfigWinSearch.SetMaxResults(const Value: Integer);
begin
  seMaxResults.Value:=Value;
end;

procedure TFrameConfigWinSearch.SetProviderName(const Value: string);
begin
  EditName.Text:=Value;
end;

procedure TFrameConfigWinSearch.SetSQL(const Value: String);
begin
  mem_Query.Text:=Value;
end;

procedure TFrameConfigWinSearch.SetTimeout(const Value: Integer);
begin

end;

procedure TFrameConfigWinSearch.SpeedButton1Click(Sender: TObject);
begin
  Self.Free;
end;

end.
