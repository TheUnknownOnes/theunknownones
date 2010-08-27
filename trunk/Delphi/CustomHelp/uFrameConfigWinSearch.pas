unit uFrameConfigWinSearch;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uFrameConfigColor, StdCtrls, ExtCtrls, CategoryButtons, Spin,
  uCustomHelpConsts, Buttons;

type
  TFrameConfigWinSearch = class(TFrame)
    Panel1: TPanel;
    fccColor: TFrameConfigColor;
    EditName: TEdit;
    Label1: TLabel;
    Memo1: TMemo;
    Label2: TLabel;
    Label4: TLabel;
    seMaxResults: TSpinEdit;
    SpeedButton1: TSpeedButton;
    procedure SpeedButton1Click(Sender: TObject);
  private
    function GetColor: TColor;
    function GetProviderName: string;
    procedure SetColor(const Value: TColor);
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
    property SelectedColor: TColor read GetColor write SetColor;
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

function TFrameConfigWinSearch.GetColor: TColor;
begin
  Result:=fccColor.Color;
end;

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
  Result:=Memo1.Text;
end;

function TFrameConfigWinSearch.GetTimeout: Integer;
begin
  Result:=-1;
end;

procedure TFrameConfigWinSearch.Save(AIndex: Integer);
begin
  GlobalCustomHelp.Color[ProviderName] := SelectedColor;
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

procedure TFrameConfigWinSearch.SetColor(const Value: TColor);
begin
  fccColor.Color:=Value;
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
  Memo1.Text:=Value;
end;

procedure TFrameConfigWinSearch.SetTimeout(const Value: Integer);
begin

end;

procedure TFrameConfigWinSearch.SpeedButton1Click(Sender: TObject);
begin
  Self.Free;
end;

end.
